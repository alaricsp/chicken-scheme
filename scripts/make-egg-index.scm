;;;; make-egg-index.scm - create index page for extension release directory

(load-relative "tools.scm")

(use setup-download matchable htmlprag data-structures)

(define *major-version* (##sys#fudge 41))

(define +stylesheet+ "")

(define +categories+
  '((lang-exts "Language extensions")
    (graphics "Graphics")
    (debugging "Debugging tools")
    (logic "Logic programming")
    (net "Networking")
    (io "Input/Output")
    (db "Databases")
    (os "OS interface")
    (ffi "Interfacing to other languages")
    (web "Web programing")
    (xml "XML processing")
    (doc-tools "Documentation tools")
    (egg-tools "Egg tools")
    (math "Mathematical libraries")
    (oop "Object-oriented programming")
    (data "Algorithms and data-structures")
    (parsing "Data formats and parsing")
    (tools "Tools")
    (sound "Sound")
    (testing "Unit-testing")
    (crypt "Cryptography")
    (ui "User interface toolkits")
    (code-generation "Code generation")
    (macros "Macros and meta-syntax")
    (misc "Miscellaneous")
    (hell "Concurrency and parallelism")
    (uncategorized "Not categerized")
    (obsolete "Unsupported or redundant") ) )

(define (d fstr . args)
  (fprintf (current-error-port) "~?~%" fstr args))

(define (usage code)
  (print "make-egg-index.scm [--major-version=MAJOR] [DIR]")
  (exit code))

(define (make-egg-index dir)
  (let ((title 
	 (sprintf "Eggs Unlimited (release branch ~a, updated ~a)"
		  *major-version*
		  (string-chomp (seconds->string (current-seconds)))))
	(eggs (gather-egg-information dir)))
    (write-shtml-as-html
     `(html
       ,(header title)
       (body
	,@(prelude title)
	,@(emit-egg-information eggs)
	,@(trailer))))))

(define (header title)
  `(head
    (style (@ (type "text/css")) 
      ,+stylesheet+)
    (title ,title)))

(define (prelude title)
  `((h1 ,title)
    (p "A library of extensions for the Chicken Scheme system.")
    (h3 "Installation")
    (p "Just enter")
    (pre "  chicken-install EXTENSIONNAME\n")
    (p "This will download anything needed to compile and install the library. "
       "If your " (i "extension repository") " is placed at a location for which "
       "you don't have write permissions, then run " (tt "chicken-install") 
       "with the " (tt "-sudo") " option or run it as root (not recommended).")
    (p "You can obtain the repository location by running")
    (pre "  csi -p \"(repository-path)\"\n")
    (p "If you only want to download the extension and install it later, pass the "
       (tt "-retrieve") " option to " (tt "chicken-install") ":")
    (pre "  chicken-install -retrieve EXTENSIONNAME\n")
    (p "By default the archive will be unpacked into a temporary directory (named "
       (tt "EXTENSIONNAME.egg-dir") " and the directory will be removed if the "
       "installation completed successfully. To keep the extracted files add "
       (tt "-keep") "to the options passed to " (tt "chicken-install") ".")
    (p "For more information, enter")
    (pre "  chicken-install -help\n")
    (p "If you would like to access the subversion repository, see "
       (a (@ (href "http://chicken.wiki.br/eggs tutorial")) "the "
	  (i "Egg tutorial")) ".")
    (p "If you are looking for 3rd party libraries used by one the extensions, "
       "check out the CHICKEN "
       (a (@ (href "http://www.call-with-current-continuation.org/tarballs/") )
	  (i "tarball repository")))
    (h3 "List of available eggs")))

(define (trailer)
  '())

(define (emit-egg-information eggs)
  (append-map
   (match-lambda
     ((cat catname)
      (d "category: ~a" catname)
      `((h3 ,catname)
	(table
	 (tr (th "Name") (th "Description") (th "License") (th "author") (th "maintainer") (th "version"))
	 ,@(append-map
	    make-egg-entry
	    (sort
	     (filter (lambda (info) 
		       (and (eq? cat (cadr (or (assq 'category (cdr info))
					       '(#f uncategorized))))
			    (not (assq 'hidden (cdr info)))))
		     eggs) 
	     (lambda (e1 e2)
	       (string<? (symbol->string (car e1)) (symbol->string (car e2))))))))))
   +categories+))

(define (make-egg-entry egg)
  (call/cc 
   (lambda (return)
     (define (prop name def pred)
       (cond ((assq name (cdr egg)) => (o (cut check pred <> name) cadr))
	     (else def)))
     (define (check pred x p)
       (cond ((pred x) x)
	     (else
	      (warning "extension has incorrectly typed .meta entry and will not be listed" (car egg) p x)
	      (return '()))))
     (d "  ~a   ~a" (car egg) (prop 'version "HEAD" any?))
     `((tr (td ,(symbol->string (car egg)))
	   (td ,(prop 'synopsis "unknown" string?))
	   (td ,(prop 'license "unknown" name?))
	   (td ,(prop 'author "unknown" name?))
	   (td ,(prop 'maintainer "" name?))
	   (td ,(prop 'version "" version?)))))))

(define name?
  (disjoin string? symbol?))

(define version?
  (disjoin string? number?))

(define (main args)
  (match args
    ((dir)
     (make-egg-index dir))
    (() (make-egg-index "."))
    (_ (usage 1))))

(main (simple-args (command-line-arguments)))
