;;;; make-egg-rss-feed.scm - create RSS 2.0 feed for extension release directory

(load-relative "tools.scm")

;; uses sxml-transforms since htmlprag idiotically attempts to be clever
;; about empty elements (i.e. "link").

(use setup-download matchable sxml-transforms data-structures regex srfi-1)

(import irregex)

(define *help* #f)
(define *major-version* (##sys#fudge 41))

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
    (uncategorized "Not categorized")
    (obsolete "Unsupported or redundant") ) )

(define (d fstr . args)
  (fprintf (current-error-port) "~?~%" fstr args))

(define (usage code)
  (print "make-egg-rss-feed.scm [--help] [--major-version=MAJOR] [DIR]")
  (exit code))

(define (make-egg-rss-feed dir)
  (let ((title (sprintf "Eggs Unlimited (release branch ~a)" *major-version*))
	(eggs (gather-egg-information dir)))
    (display "<?xml version='1.0'?>\n")
    (SXML->HTML
     `(rss
       (@ (version "2.0"))
       (channel
	,@(channel title)
	,@(items eggs))))))

(define (channel title)
  (let ((date (seconds->string (current-seconds))))
    `((title ,title)
      (link "http://galinha.ucpel.tche.br/chicken-projects/egg-rss-feed-4.xml")
      (description "RSS feed for publishing latest CHICKEN extensions")
      (language "en-us")
      (copyright "(c)2009 The CHICKEN Team")
      (pubDate ,date)
      (lastBuildDate ,date))))

(define (items eggs)
  (map
   (lambda (egg)
     (call/cc 
      (lambda (return)
	(define (prop name def pred)
	  (cond ((assq name (cdr egg)) => (o (cut check pred <> name) cadr))
		(else def)))
	(define (check pred x p)
	  (cond ((pred x) x)
		(else
		 (warning "extension has .meta entry of incorrect type and will not be listed" (car egg) p x)
		 (return '()))))
	(d "  ~a   ~a" (car egg) (prop 'version "HEAD" any?))
	`(item 
	  (title ,(sprintf "~a ~a (~a)" 
			   (car egg) 
			   (prop 'version "" version?)
			   (let* ((c1 (prop 'category 'uncategorized name?))
				  (c (assq c1 +categories+)))
			     (if c (cadr c) (sprintf "unknown category: ~a" c1)))))
	  (guid (@ (isPermaLink "false")) ,(symbol->string (car egg)))
	  (link ,(sprintf "http://chicken.wiki.br/eggref/~a/~a" *major-version* (car egg)))
	  (description ,(prop 'synopsis "unknown" string?))
	  (author ,(prop 'author "unknown" name?))))))
   eggs))

(define name?
  (disjoin string? symbol?))

(define version?
  (disjoin string? number?))

(define (main args)
  (when *help* (usage 0))
  (match args
    ((dir)
     (make-egg-rss-feed dir))
    (() (make-egg-rss-feed "."))
    (_ (usage 1))))

(main (simple-args (command-line-arguments)))
