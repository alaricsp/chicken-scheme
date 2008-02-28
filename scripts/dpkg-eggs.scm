;;
;; Given a directory tree with egg directories, build Debian packages
;; for all eggs that have a debian subdirectory.
;;
;; Usage: dpkg-eggs --eggdir=DIR --output-dir=DIR
;;

(require-extension srfi-1)
(require-extension srfi-13)
(require-extension posix)
(require-extension regex)
(require-extension utils)
(require-extension args)

(include "tools.scm")

(define s+ string-append)

(define opts
  `(
    ,(args:make-option (extension-path)       (required: "DIR")    
		       (s+ "path to stream-wiki extensions"))
    ,(args:make-option (wiki-dir)       (required: "DIR")    
		       (s+ "use wiki documentation in directory DIR"))
    ,(args:make-option (egg-dir)       (required: "DIR")    
		       (s+ "operate on eggs in directory DIR"))
    ,(args:make-option (output-dir)       (required: "DIR")    
		       (s+ "place Debian packages in directory DIR (will be created if it does not exist)"))
    ,(args:make-option (verbose)       #:none
		       (s+ "enable verbose mode")
		       (set! *verbose* #t))
    ,(args:make-option (exclude)       (required: "EGGS")    
		       (s+ "a comma separated list of eggs to exclude from building"))
    ,(args:make-option (h help)  #:none               "Print help"
		       (usage))

    ))


;; Use args:usage to generate a formatted list of options (from OPTS),
;; suitable for embedding into help text.
(define (usage)
  (print "Usage: " (car (argv)) " options... [list of eggs to be built] ")
  (newline)
  (print "The following options are recognized: ")
  (newline)
  (print (parameterize ((args:indent 5)) (args:usage opts)))
  (exit 1))


;; Process arguments and collate options and arguments into OPTIONS
;; alist, and operands (filenames) into OPERANDS.  You can handle
;; options as they are processed, or afterwards.
(define args    (command-line-arguments))
(set!-values (options operands)  (args:parse args opts))

(define dirsep (string ##sys#pathname-directory-separator))

(define (read-subdirs path)
  (find-files path directory? cons (list) 0))

;; Compare versions of the format x.x...
(define (version< v1 v2)
  (let ((v1 (string-split v1 "."))
	(v2 (string-split v2 ".")))
    (every (lambda (s1 s2) 
	     (let ((n1 (string->number s1))
		   (n2 (string->number s2)))
	       (cond ((and n1 n2)  (<= n1 n2))
		     (else (string<= s1 s2)))))
	   v1 v2)))
	    
;; Find the latest release in a given egg directory 
(define (find-latest-release path)
  (let ((tags (s+ path dirsep "tags")))
    (cond ((file-exists? tags) 
	   (let ((lst (filter-map (lambda (x) (and (not (string=? (pathname-strip-directory x) ".svn")) x))
				  (read-subdirs tags)))
		 (cmp (lambda (x y) (version< (pathname-strip-directory x) (pathname-strip-directory y)))))
	     (if (pair? lst) (car (reverse (sort lst cmp))) path)))
	  (else path))))
	    
;; Find the debian subdirectory in a given egg directory
(define (find-debian-subdir path . rest)
  (let-optionals rest ((release (find-latest-release path)))
    (cond ((file-exists? (s+ path dirsep "trunk" dirsep "debian")) => identity)
	  ((file-exists? (s+ release dirsep "debian")) => identity)
	  (else #f))))
	    
;; Find wiki documentation for given egg
(define (find-wiki-doc name wikidir)
  (cond ((file-exists? (s+ wikidir dirsep name)) => identity)
	(else #f)))

(define (build-deb eggdir wiki-dir output-dir ext-path path)
  (let* ((name     (pathname-strip-directory path))
	 (release  (find-latest-release path))
	 (debdir   (find-debian-subdir path release)))
    (if debdir
	(let ((start      (cwd))
	      (build-dir  (s+ output-dir dirsep name))
	      (doc        (cond ((file-exists? (s+ release dirsep name ".html")) => identity)
				((and wiki-dir (file-exists? (s+ wiki-dir dirsep name))) => identity)
				(else #f))))
	  (message "Release directory is ~a" release)
	  (message "debian subdirectory found in ~a" path)
	  (run (rm -rf ,build-dir))
	  (run (cp -R ,release ,build-dir))
	  (run (cp -R ,debdir ,build-dir))
	  (if (and doc (not (string-suffix? ".html" doc)))
	      (let ((html-path (s+ "html/" name ".html")))
		(run (csi -s ,(cond ((file-exists? (s+ start "/makehtml.scm")) => identity)
				    (else 'makehtml.scm))
			  ,(s+ "--extension-path=" ext-path) 
			  ,(s+ "--wikipath=" wiki-dir) 
			  ,(s+ "--only=" name)))
		(run (cp ,html-path ,build-dir))))
	  (cd build-dir)
	  (run (chmod a+rx debian/rules))
	  (run (,(s+ "EGG_TREE=\"" eggdir "\"") dpkg-buildpackage -us -uc))
	  (cd start))
	(message "No debian subdirectory found in ~a" path))))

(define (main options operands)
  (let ((opt_wikidir   (alist-ref 'wiki-dir options))
	(opt_eggdir    (alist-ref 'egg-dir options))
	(opt_extpath   (alist-ref 'extension-path options))
	(opt_exclude ((lambda (x) (and x (string-split x ","))) (alist-ref 'exclude options)))
	(opt_output-dir (alist-ref 'output-dir options)))
    (if (not (and opt_eggdir opt_output-dir))
	(begin
	  (error-message "Both egg directory and output directory must be specified!")
	  (usage)))
    (message "Egg directory tree: ~a" opt_eggdir)
    (message "Output directory tree: ~a" opt_output-dir)
    ;; make sure target dir exists
    (if (not (file-exists? opt_output-dir))
	(begin
	  (message "Creating directory ~a" opt_output-dir)
	  (create-directory opt_output-dir)))
    (let ((eggdirs (filter-map 
		    (lambda (x) (and (not (member (pathname-strip-directory x) opt_exclude)) x))
		    (or (and (pair? operands) (map (lambda (x) (s+ opt_eggdir dirsep (->string x))) operands))
			(read-subdirs opt_eggdir)))))
      (if (null? eggdirs)
	  (message "No egg directories found in ~a" opt_eggdir)
	  (message "Found egg directories: ~a" eggdirs))
      (for-each (lambda (x) (build-deb opt_eggdir opt_wikidir opt_output-dir opt_extpath x))
		eggdirs))))

(main options operands)
