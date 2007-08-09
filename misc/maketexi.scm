;;;; maketexi.scm
;
; (by Ivan Raikov)


(require-extension srfi-1)
(require-extension posix)
(require-extension utils)
(require-extension stream-wiki)
(require-extension stream-ext)

(define wikipath (optional (command-line-arguments) "chicken-manual"))

(define file-list (map (lambda (x) (make-pathname wikipath x))
		       (list "The User's Manual"
			     "Basic mode of operation"
			     "Using the compiler"
			     "Using the interpreter"
			     "Supported language"
			     "Deviations from the standard"
			     "Extensions to the standard"
			     "Non standard read syntax"
			     "Non standard macros and special forms"
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
			     "Bibliography")))

(define str (stream-concatenate (list->stream (map (compose port->stream open-input-file) file-list))))

(write-stream
 (texi-page 
  "Felix Winkelmann"
  "Chicken Scheme Reference Manual"
  "Copyright 2007 Felix Winkelmann"
  "The User's Manual"
  (wiki->texi str))
 (open-output-file "chicken.texi"))
