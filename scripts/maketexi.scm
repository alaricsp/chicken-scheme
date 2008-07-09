;;;; maketexi.scm
;
; (by Ivan Raikov)

(require-extension syntax-case)
(require-extension srfi-1)
(require-extension srfi-69)
(require-extension posix)
(require-extension utils)
(require-extension srfi-40)
(require-extension html-stream)
(require-extension stream-ext)
(require-extension stream-wiki)

(define extensions (make-hash-table))

(load-extensions-from-file extensions (or (file-exists? "enscript-texinfo.scm")
					  (file-exists? "scripts/enscript-texinfo.scm")))

(define wikipath (optional (command-line-arguments) "manual"))

(define file-list (map (lambda (x) (make-pathname wikipath x))
		       (list "The User's Manual"
			     "Getting started"
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
			     "Unit data-structures"
			     "Unit ports"
			     "Unit files"
			     "Unit extras"
			     "Unit srfi-1"
			     "Unit srfi-4"
			     "Unit srfi-13"
			     "Unit srfi-14"
			     "Unit srfi-69"
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
			     "Bibliography")))

(define str (stream-concatenate (list->stream (map (compose port->stream open-input-file) file-list))))

(write-stream
 (texi-page 
  "Felix Winkelmann and the Chicken Team"
  "Chicken Scheme Reference Manual"
  "Copyright 2007-2008 Felix Winkelmann and the Chicken Team"
  "The User's Manual"
  (wiki->texi str
	      stream-null ;; tail
	      "" ;; name
	      (constantly stream-null) ;; open
	      (lambda (name tail) tail) ;; include
	      (make-hash-table) ;; linktypes
	      extensions))
 (open-output-file "chicken.texi"))
