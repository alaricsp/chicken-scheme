;;;; mini-setup.scm - a minimal setup program


(require-library posix)


;;; Keep and locate egg meta-information

(module egg-info (register-info
		  find-info
		  find-uninstalled-dependencies)

  (import scheme chicken srfi-1)

  (define *repository* #f)

  (define (register-info name info)
    (set! *repository* (alist-cons name info *repository*)))
  
  (define (find-info name)
    (cond ((assq name *repository*) => cdr)
	  (else (extension-information name))))

  (define (find-uninstalled-dependencies egg)
    (let ((info (cdr (or (assq egg *repository*)
			 (error "unregistered egg information required" egg)))))
      (remove
       extension-information
       (cond ((assq 'needs info) => cdr)
	     (else '())))))

)


;;; Manage local source tree

(module source-tree (locate-egg-directory
		     tree-directory)

  (import scheme chicken regex posix extras utils)
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

  (define (locate-egg-directory egg)
    (let* ((eggdir (make-pathname (tree-directory) egg))
	   (files (directory eggdir))
	   (top 
	    (let ((tagdir (make-pathname eggdir "tags")))
	      (and (file-exists? tagdir) (directory? tagdir)
		   (let ((vs (sort version>? (directory tagdir))))
		     (and (pair? vs) (car vs))) )))
	   (filedir (if top (make-pathname (append eggdir "tags") top) eggdir))
	   (meta (make-pathname filedir egg "meta")))
      (and (or top (file-exists? meta))
	   (let ((info (car (read-file meta))))
	     (register-info info)
	     filedir))))

)


;;; Invoke .setup script

(module run-script (run-setup-script
		    program-path)

  (import scheme chicken posix utils)

  (define (quit . args)
    (with-output-to-port (current-error-port)
      (cut apply print args))
    (exit 1))

  (define program-path (make-parameter "."))

  (define (ensure-setup-api-installed)
    (unless (extension-information 'setup-api)
      (quit "extension `setup-api' not installed") ) )

  (define (run-setup-script egg dir)
    (parameterize ((current-directory dir))
      (let ((s (system 
		(string-append
		 (make-pathname (program-path) "csi")
		 " -bnq -R setup-api "
		 (make-pathname dir egg "setup") ) ) ) )
	(unless (zero? s)
	  (quit "setup script terminated with non-zero exit status " s) ) ) ) )

)


;;; Entry point

(module main ()
 
  (import scheme chicken)
  (import egg-info source-tree run-script)
 
  (define (usage code)
    (print "usage: mini-setup [-h] [-p PROGRAMPATH] [-t TREEDIRECTORY] EGGNAME ...")
    (exit code))

  (define (install egg)
    (let* ((dir (locate-egg-directory egg))
	   (eggsym (string->symbol egg))
	   (info (find-info eggsym))
	   (deps (find-uninstalled-dependencies eggsym)))
      (for-each install (map symbol->string deps))
      (run-setup-script egg dir)))

  (define (main args)
    (ensure-setup-api-installed)
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
		((string=? "-t" arg)
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
