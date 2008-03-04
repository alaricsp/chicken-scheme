#!/bin/sh
#| ;;; makehtml.scm -*- Scheme -*-
exec csi -s $0 "$@"
|#


;;; this should use build.scm, really...

(use syntax-case)
(use srfi-40)
(use html-stream stream-ext stream-wiki utils srfi-13 posix regex)
(use matchable)

(define s+ string-append)

(define (simple-args #!optional (args (command-line-arguments)) (error error))
  (define (assign var val)
    (##sys#setslot 
     (string->symbol (s+ "*" var "*"))
     0
     (if (string? val) 
	 (or (string->number val) val)
	 val)))
  (let loop ((args args) (vals '()))
    (cond ((null? args) (reverse vals))
	  ((string-match "(-{1,2})?([-_A-Za-z0-9]+)(=)?\\s*(.+)?" (car args)) 
	   =>
	   (lambda (m)
	     (let*-values (((next) (cdr args))
			   ((var val)
			    (match m
			      ((_ _ opt "=" val)
			       (cond (val (values opt val))
				     (else 
				      (when (null? next)
					(error "missing argument for option" (car args)) )
				      (let ((x (car next)))
					(set! next (cdr next))
					(values opt x)))))
			      ((_ (? string?) opt #f #f) (values opt #t))
			      (_ (values #f #f)) ) ) )
	       (cond (var 
		      (assign var val)
		      (loop next vals) )
		     (else (loop next (cons (car args) vals)))))))
	  (else (loop (cdr args) (cons (car args) vals))))))

(define-constant +outpath+ "html")
(define-constant +index-page+ "The User's Manual")

(define *pdf* #f)
(define *extension-path* #f)
(define *pages* (if (file-exists? "manual") (directory "manual") (list)))
(define *only* #f)
(define *wikipath* "~/eggs/wiki")
(define *fetch-manual* #f)

(define (hyphen s)
  (string-substitute " " "-" s #t) )

(define (clean-link lnk)
  (cond ((or (string-prefix? "toc:" lnk) (string-prefix? "tags:" lnk))
	 lnk)
	((string-ci=? lnk +index-page+) "index.html")
	((any (lambda (f) (string-ci=? f lnk)) *pages*)
	 (s+ (hyphen (string-downcase lnk)) ".html|" lnk) )
	((string-prefix? "http:" lnk) lnk)
	(else (s+ "http://galinha.ucpel.tche.br/" lnk) ) ) )

(define (convert-page page)
  (let ((data (read-all page)))
    (string-concatenate
     (let loop ((i 0) (all '()))
       (match (string-search-positions "\\[\\[([^]]+)\\]\\]" data i)
	 (((s e) (ls le))
	  (let ((lnk (substring data ls le)))
	    (loop e (cons
		(s+ (substring data i (+ 2 s))
		    (clean-link lnk)
		    "]]")
		all) ) ) )
	 (_ (reverse (cons (substring data i) all))) ) ) ) ) )

;; We need this to keep the order of chapters in the PDF file.
(define manual-wiki-files
  '("The User's Manual"
    "Overview"
    "Basic mode of operation"
    "Using the compiler"
    "Using the interpreter"
    "Supported language"
    "Deviations from the standard"
    "Extensions to the standard"
    "Non-standard read syntax"
    "Non-standard macros and special forms"
    "Pattern matching"
    "Declarations"
    "Parameters"
    "Unit library"
    "Unit eval"
    "Unit extras"
    "Unit srfi-1"
    "Unit srfi-4"
    "Unit srfi-13"
    "Unit srfi-14"
    "Unit match"
    "Unit regex"
    "Unit srfi-18"
    "Unit posix"
    "Unit utils"
    "Unit tcp"
    "Unit lolevel"
    "Interface to external functions and variables"
    "Accessing external objects"
    "Foreign type specifiers"
    "Embedding"
    "Callbacks"
    "Locations"
    "Other support procedures"
    "C interface"
    "chicken-setup"
    "Data representation"
    "Bugs and limitations"
    "faq"
    "Acknowledgements"
    "Bibliography"
    ))

(define (chapters-sanity-check)
  "Checks if all the wiki files listed in `*pages*' are in
`manual-wiki-files', just in case we forget to update this
variable when new chapters are added; and if all the files listed
in `manual-wiki-files' can be found in `*pages*'."
  (for-each (lambda (file)
              (when (not (member file manual-wiki-files))
                (error (conc file " was not found in `manual-wiki-files'."))))
            *pages*)
  (for-each (lambda (file)
              (when (not (member file *pages*))
                (error (conc "File \"" file "\" was not found under the manual directory."))))
            manual-wiki-files))
  
(define (html-files->pdf)
  "Requires htmldoc (http://www.htmldoc.org)."
  (system (conc "htmldoc --book --numbered --size a4 --title "
                "--toctitle \"Chicken User's Manual\" "
                " --header t "
                "--linkstyle plain --outfile chicken.pdf "
                (if *only*
                    (html-pagename *only*)
                    (string-intersperse
                     (map html-pagename manual-wiki-files))))))

(define (html-pagename pagename)
  (hyphen (string-downcase 
           (make-pathname
            +outpath+
            (if (string=? pagename +index-page+) "index" pagename) "html"))))

(define (wiki-pagename pagename)
  (if *only* 
      (make-pathname *wikipath* pagename)
      (make-pathname "manual" pagename) ))

(define *loaded-extensions* (make-hash-table))

(define (wiki-files->html)
  (for-each
   (lambda (p)
     (let* ((pagename p)
	    (pw (wiki-pagename pagename))
            (po (html-pagename pagename)))
       (when (or (not (file-exists? po))
		 (> (file-modification-time pw) (file-modification-time po)) )
	 (print p " -> " po " ...")
	 (with-output-to-file po
	   (lambda ()
	     (if *only*
		 (printf "<html><head><title>~a</title></head><body>~%" pagename)
		 (printf "<html><head><title>CHICKEN User's Manual - ~a</title></head><body>~%" pagename))
	     (write-stream
	      (wiki->html
	       (string->stream (convert-page pw))
	       stream-null
	       ""
	       (constantly stream-null)
	       (constantly stream-null)
	       (make-hash-table)
	       (make-html-header 1)
	       (constantly stream-null)
	       (constantly #t)
	       *loaded-extensions*) )
	     (printf "~%</body></html>") ) ) ) ) )
   (if *only* (list *only*) *pages*) ) )

(define (usage code)
  (print "makedoc --fetch-manual")
  (print "makedoc --extension-path=EXTPATH [--pdf] [--wikipath=PATH] [--only=PAGENAME]") 
  (exit code) )

(simple-args)

(when *fetch-manual*
  (for-each 
   (lambda (f) 
     (system* "cp ~a manual"
	      (string-concatenate
	       (map (lambda (c)
		      (if (not (memq c '(#\' #\" #\space)))
			  (string c)
			  (string #\\ c) ) )
		    (string->list (make-pathname *wikipath* f)) ))))
   manual-wiki-files) 
  (exit) )

(unless *extension-path* (usage 1))
(system* "mkdir -p html")
(for-each
 (lambda (f)
   (unless (string-suffix? ".svn" f)
     (load-extensions-from-file *loaded-extensions* f)))
 (glob (conc *extension-path* "/*")) )
(when *pdf* (chapters-sanity-check))
(wiki-files->html)
(when *pdf* (html-files->pdf))
