;;;; mini-setup.scm - a minimal setup program
;
; - intentionally overmodularized for testing purposes


(require-library posix srfi-13 srfi-1)


;;; Miscellaneous support routines

(module misc ((run execute)
	      program-path
	      qs 
	      once
	      ->symbol
	      quit)
  
  (import scheme chicken posix srfi-13 ports utils data-structures)

  (define (quit . args)
    (with-output-to-port (current-error-port)
      (cut apply print args))
    (exit 1))

  (define program-path
    (make-parameter 
     (and-let* ((path (pathname-directory (car (argv)))))
       (make-pathname
	(if (absolute-pathname? path)
	    path
	    (current-directory) )
	path))))

  (define (execute cmd)
    (let ((cmd (string-intersperse (map ->string cmd) " ")))
      (print "  " cmd)
      (let ((s (system cmd)))
	(unless (zero? s)
	  (quit "shell command terminated with nonzero exit status " s ":\n\n  " cmd)))))

  (define (qs str)
    (string-concatenate
     (map (lambda (c)
	    (if (or (char-whitespace? c)
		    (memq c '(#\# #\" #\' #\` #\´ #\~ #\& #\% #\$ #\! #\* #\; #\< #\> #\\
			      #\( #\) #\{ #\} #\[ #\])))
		(string #\\ c)
		(string c)))
	  (string->list str))))

  (define-syntax run
    (syntax-rules ()
      ((_ (cmd ...)) (execute `(cmd ...)))))

  (define-syntax once
    (syntax-rules ()
      ((_ expr) (force (delay expr)))))

  (define (->symbol x)
    (cond ((symbol? x) x)
	  ((string? x) (string->symbol x))
	  (else (with-output-to-string (cut display x)))))

)


;;; Keep and locate egg meta-information

(module egg-info (register-info
		  find-info
		  find-uninstalled-dependencies)

  (import scheme chicken srfi-1 misc)

  (define *repository* #f)

  (define (register-info name info)
    (print "registering meta info for " name)
    (set! *repository* (alist-cons (->symbol name) info *repository*)))
  
  (define (find-info name)
    (let ((id (->symbol name)))
      (cond ((assq id *repository*) => cdr)
	    (else (extension-information id)))))

  (define (find-uninstalled-dependencies egg)
    (let ((info (cdr (or (assq (->symbol egg) *repository*)
			 (error "unregistered egg information required" egg)))))
      (remove
       extension-information
       (cond ((assq 'needs info) => 
	      (lambda (a) (map symbol->string (cdr a))))
	     (else '())))))

)


;;; Manage local source tree

(module source-tree (locate-egg-directory
		     tree-directory)

  (import scheme chicken regex posix extras utils data-structures)
  (import egg-info)

  (define *repository* #f)
  (define tree-directory (make-parameter "."))

  (define (version>? v1 v2)
    (define (version->list v)
      (map (lambda (x) (or (string->number x) x))
	   (string-split-fields "[-\\._]" v)))
    (let loop ((p1 (version->list v1))
	       (p2 (version->list v2)))
      (cond ((null? p1) (null? p2))
	    ((null? p2))
	    ((number? (car p1))
	     (and (if (number? (car p2))
		      (> (car p1) (car p2))
		      (string>? (number->string (car p1)) (car p2)))
		  (loop (cdr p1) (cdr p2))))
	    ((number? (car p2))
	     (and (string>? (car p1) (number->string (car p2)))
		  (loop (cdr p1) (cdr p2))))
	    ((string>? (car p1) (car p2)) (loop (cdr p1) (cdr p2)))
	    (else #f))))

  (define (locate-egg-directory egg trunk?)
    (let* ((eggdir (make-pathname (tree-directory) egg))
	   (files (directory eggdir))
	   (trunkdir (make-pathname eggdir "trunk"))
	   (tagdir (make-pathname eggdir "tags"))
	   (hastrunk (and (file-exists? trunkdir) (directory? trunkdir)))
	   (filedir
	    (or (and trunk? hastrunk trunkdir)
		(and (file-exists? tagdir) (directory? tagdir)
		     (let ((vs (sort version>? (directory tagdir))))
		       (and (pair? vs)
			    (let ((vfile (make-pathname tagdir "version")))
			      (unless (file-exists? vfile)
				(print "creating " vfile)
				(with-output-to-file vfile (cut print (car vs))))
			      (make-pathname tagdir (car vs))))))
		(and hastrunk trunkdir)
		eggdir))
	   (meta (make-pathname filedir egg "meta"))
	   (info
	    (cond ((file-exists? meta) (car (read-file meta)))
		  (else
		   (print "Warning: egg `" egg "' has no .meta file")
		   '()))))
      (register-info egg info)
      filedir))

)


;;; Invoke .setup script

(module run-script (run-setup-script
		    program-path 
		    ensure-setup-api-installed)

  (import scheme chicken posix utils misc)

  (define (ensure-setup-api-installed)
    (unless (extension-information 'setup-api)
      (quit "extension `setup-api' not installed - please run mini-setup with the `-i' option") ) )

  (define (run-setup-script egg dir)
    (parameterize ((current-directory dir))
      (run (,(qs (make-pathname (program-path) "csi"))
	    -e ,(qs "(require-library setup-api)") -e ,(qs "(import setup-api)")
	    ,(qs (make-pathname #f egg "setup") ) ) ) ) )

)


;;; Install setup-api egg manually

(module setup-api (install-setup-api)

  (import scheme chicken misc extras utils)

  (define (install-setup-api)
    (run (,(qs (make-pathname (program-path) "csc"))
	  -s setup-api.scm))
    (run (cp setup-api.so ,(qs (repository-path))))
    (with-output-to-file (make-pathname (repository-path) "setup-api.setup-info")
      (cut pp `((files ,(make-pathname (repository-path) "setup-api.so"))))))

)


;;; Entry point

(module main (main)
 
  (import scheme chicken)
  (import egg-info source-tree run-script misc setup-api data-structures)

  (define *trunk-ok* #f)
 
  (define (usage code)
    (print "usage: mini-setup [-h] [-i] [-t] [-p PROGRAMPATH] [-r TREEDIRECTORY] EGGNAME ...")
    (exit code))

  (define (install egg)
    (print "checking " egg " ...")
    (once (ensure-setup-api-installed))
    (let* ((dir (locate-egg-directory egg *trunk-ok*))
	   (info (find-info egg))
	   (deps (find-uninstalled-dependencies egg)))
      (print egg " is located at " dir)
      (when (pair? deps)
	(print egg " has uninstalled dependencies: " (string-intersperse deps ", "))
	(for-each (cut locate-egg-directory <> *trunk-ok*) deps) ; installs info in *repository*
	(for-each install deps))
      (print "installing " egg " ...")
      (run-setup-script egg dir)))

  (define (main args)
    (when (null? args) (usage 0))
    (let loop ((args args))
      (unless (null? args)
	(let ((arg (car args))
	      (rest (cdr args)))
	  (cond ((string=? "-h" arg) (usage 0))
		((string=? "-p" arg)
		 (if (pair? rest)
		     (program-path (car rest))
		     (usage 1))
		 (loop (cdr rest)))
		((string=? "-i" arg)
		 (install-setup-api) 
		 (loop rest))
		((string=? "-t" arg)
		 (set! *trunk-ok* #t)
		 (loop rest))
		((string=? "-r" arg)
		 (if (pair? rest)
		     (tree-directory (car rest))
		     (usage 1))
		 (loop (cdr rest)))
		((and (positive? (string-length arg)) (char=? #\- (string-ref arg 0)))
		 (usage 1))
		(else 
		 (install arg)
		 (loop rest)))))))

)

(import main)

(main (command-line-arguments))
