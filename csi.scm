;;;; csi.scm - Interpreter stub for CHICKEN
;
; Copyright (c) 2000-2007, Felix L. Winkelmann
; Copyright (c) 2008-2009, The Chicken Team
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following
; conditions are met:
;
;   Redistributions of source code must retain the above copyright notice, this list of conditions and the following
;     disclaimer. 
;   Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
;     disclaimer in the documentation and/or other materials provided with the distribution. 
;   Neither the name of the author nor the names of its contributors may be used to endorse or promote
;     products derived from this software without specific prior written permission. 
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
; AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.


(declare
  (uses chicken-syntax srfi-69 ports) ; is here because a bootstrap from an older chicken may not make
  (usual-integrations)			;  this used automatically
  (disable-interrupts)
  (disable-warning var)
  (run-time-macros)			;*** later: compile-syntax
  (foreign-declare #<<EOF
#if (defined(_MSC_VER) && defined(_WIN32)) || defined(HAVE_DIRECT_H)
# include <direct.h>
#else
# define _getcwd(buf, len)       NULL
#endif
EOF
) )

(include "banner")

(private csi 
  print-usage print-banner
  run hexdump del 
  parse-option-string chop-separator lookup-script-file
  report describe dump hexdump bytevector-data get-config
  deldups tty-input?
  history-list history-count history-add history-ref
  trace-indent trace-indent-level traced-procedure-entry traced-procedure-exit)

(declare
  (always-bound
    ##sys#windows-platform)
  (hide parse-option-string bytevector-data member* canonicalize-args do-trace do-untrace
	traced-procedures describer-table dirseparator? resolve-var
	findall trace-indent command-table do-break do-unbreak broken-procedures) )


;;; Parameters:

(define-constant init-file ".csirc")

(set! ##sys#repl-print-length-limit 2048)
(set! ##sys#features (cons #:csi ##sys#features))


;;; Print all sorts of information:

(define (print-usage)
  (display #<<EOF
usage: csi [FILENAME | OPTION ...]

  `csi' is the CHICKEN interpreter.
  
  FILENAME is a Scheme source file name with optional extension. OPTION may be
  one of the following:

    -h  -help  --help             display this text and exit
    -v  -version                  display version and exit
        -release                  print release number and exit
    -i  -case-insensitive         enable case-insensitive reading
    -e  -eval EXPRESSION          evaluate given expression
    -p  -print EXPRESSION         evaluate and print result(s)
    -P  -pretty-print EXPRESSION  evaluate and print result(s) prettily
    -D  -feature SYMBOL           register feature identifier
    -q  -quiet                    do not print banner

EOF
)
  (display #<#EOF
    -n  -no-init                  do not load initialization file #{#\`} #{init-file} #{#\'}

EOF
)
  (display  #<<EOF
    -b  -batch                    terminate after command-line processing
    -w  -no-warnings              disable all warnings
    -k  -keyword-style STYLE      enable alternative keyword-syntax
                                   (prefix, suffix or none)
        -no-parentheses-synonyms  disables list delimiter synonyms
        -no-symbol-escape         disables support for escaped symbols
        -r5rs-syntax              disables the Chicken extensions to
                                   R5RS syntax
    -s  -script PATHNAME          use interpreter for shell scripts
        -ss PATHNAME              shell script with `main' procedure
        -sx PATHNAME              same as `-s', but print each expression
                                   as it is evaluated
    -R  -require-extension NAME   require extension and import before
                                   executing code
    -I  -include-path PATHNAME    add PATHNAME to include path
    --                            ignore all following options

EOF
) )

(define (print-banner)
  (newline)
  #; ;UNUSED
  (when (and (tty-input?) (##sys#fudge 11))
    (let* ((t (string-copy +product+))
	   (len (string-length t))
	   (c (make-string len #\x08)))
      (do ((i (sub1 (* 2 len)) (sub1 i)))
	  ((zero? i))
	(let* ((p (abs (- i len)))
	       (o (string-ref t p)))
	  (string-set! t p #\@)
	  (print* t)
	  (string-set! t p o)
	  (let ((t0 (+ (current-milliseconds) 20)))
	    (let loop ()		; crude, but doesn't need srfi-18
	      (when (< (current-milliseconds) t0)
		(loop))))
	  (print* c) ) ) ) )
  (print +product+)
  (print +banner+ (chicken-version #t) "\n") )


;;; Reader for REPL history:

(set! ##sys#user-read-hook
  (let ([read-char read-char]
	[read read]
	[old-hook ##sys#user-read-hook] )
    (lambda (char port)
      (cond [(or (char=? #\) char) (char-whitespace? char))
	     `',(history-ref (fx- history-count 1)) ]
	    [else (old-hook char port)] ) ) ) )

(set! ##sys#sharp-number-hook
  (lambda (port n)
    `',(history-ref n) ) )


;;; Chop terminating separator from pathname:

(define (dirseparator? c)
  (or (char=? c #\\) (char=? c #\/)))

(define chop-separator 
  (let ([substring substring] )
    (lambda (str)
      (let* ((len (sub1 (##sys#size str)))
	     (c (string-ref str len)))
	(if (and (fx> len 0) (dirseparator? c))
	    (substring str 0 len)
	    str) ) ) ) )


;;; Find script in PATH (only used for Windows/DOS):

(define @ #f)

(define lookup-script-file 
  (let* ([buf (make-string 256)]
	 [_getcwd (foreign-lambda nonnull-c-string "_getcwd" scheme-pointer int)] )
    (define (addext name)
      (if (file-exists? name)
	  name
	  (let ([n2 (string-append name ".bat")])
	    (and (file-exists? n2) n2) ) ) )
    (define (string-index proc str1)
      (let ((len (##sys#size str1)))
	(let loop ((i 0))
	  (cond ((fx>= i len) #f)
		((proc (##core#inline "C_subchar" str1 i)) i)
		(else (loop (fx+ i 1))) ) ) ) )
    (lambda (name)
      (let ([path (get-environment-variable "PATH")])
	(and (> (##sys#size name) 0)
	     (cond [(dirseparator? (string-ref name 0)) (addext name)]
		   [(string-index dirseparator? name)
		    (and-let* ([p (_getcwd buf 256)])
		      (addext (string-append (chop-separator p) "/" name)) ) ]
		   [(addext name)]
		   [else
		    (let ([name2 (string-append "/" name)])
		      (let loop ([ps (string-split path ";")])
			(and (pair? ps)
			     (let ([name2 (string-append (chop-separator (##sys#slot ps 0)) name2)])
			       (or (addext name2)
				   (loop (##sys#slot ps 1)) ) ) ) ) ) ] ) ) ) ) ) )
				   

;;; REPL customization:

(define history-list (make-vector 32))
(define history-count 1)

(define history-add
  (let ([vector-resize vector-resize])
    (lambda (vals)
      (let ([x (if (null? vals) (##sys#void) (##sys#slot vals 0))]
	    [size (##sys#size history-list)] )
	(when (fx>= history-count size)
	  (set! history-list (vector-resize history-list (fx* 2 size))) )
	(vector-set! history-list history-count x)
	(set! history-count (fx+ history-count 1))
	x) ) ) )

(define (history-ref index)
  (let ([i (inexact->exact index)])
    (if (and (fx> i 0) (fx<= i history-count))
	(vector-ref history-list i) 
	(##sys#error "history entry index out of range" index) ) ) )

(repl-prompt
 (let ([sprintf sprintf])
   (lambda ()
     (sprintf "#;~A> " history-count) ) ) )

(define (tty-input?)
  (or (##sys#fudge 12) (##sys#tty-port? ##sys#standard-input)) )

(set! ##sys#break-on-error #f)

(set! ##sys#read-prompt-hook
  (let ([old ##sys#read-prompt-hook])
    (lambda ()
      (when (tty-input?) (old)) ) ) )

(define command-table (make-vector 37 '()))

(define (toplevel-command name proc #!optional help)
  (##sys#check-symbol name 'toplevel-command)
  (when help (##sys#check-string help 'toplevel-command))
  (##sys#hash-table-set! command-table name (cons proc help)) )

(set! ##sys#repl-eval-hook
  (let ((eval eval)
	(load-noisily load-noisily)
	(read read)
	(singlestep singlestep)
	(read-line read-line)
	(length length)
	(display display)
	(write write)
	(string-split string-split)
	(printf printf)
	(expand expand)
	(pretty-print pretty-print)
	(integer? integer?)
	(values values) )
    (lambda (form)
      (set! trace-indent-level 0)
      (cond ((eof-object? form) (exit))
	    ((and (pair? form)
		  (eq? 'unquote (##sys#slot form 0)) )
	     (let ((cmd (cadr form)))
	       (cond ((and (symbol? cmd) (##sys#hash-table-ref command-table cmd)) =>
		      (lambda (p)
			((car p))
			(##sys#void) ) )
		     (else
		      (case cmd
			((x)
			 (let ([x (read)])
			   (pretty-print (##sys#strip-syntax (expand x)))
			   (##sys#void) ) )
			((p)
			 (let* ([x (read)]
				[xe (eval x)] )
			   (pretty-print xe)
			   (##sys#void) ) )
			((d)
			 (let* ([x (read)]
				[xe (eval x)] )
			   (describe xe) ) )
			((du)
			 (let* ([x (read)]
				[xe (eval x)] )
			   (dump xe) ) )
			((dur)
			 (let* ([x (read)]
				[n (read)]
				[xe (eval x)] 
				[xn (eval n)] )
			   (dump xe xn) ) )
			((r) (report))
			((q) (exit))
			((l)
			 (let ((fns (string-split (read-line))))
			   (for-each load fns)
			   (##sys#void) ) )
			((ln) 
			 (let ((fns (string-split (read-line))))
			   (for-each (cut load-noisily <> printer: (lambda (x) (pretty-print x) (print* "==> "))) fns)
			   (##sys#void) ) )
			((t)
			 (let ((x (read)))
			   (receive rs (time (eval x))
			     (history-add rs)
			     (apply values rs) ) ) )
			((tr) (do-trace (map resolve-var (string-split (read-line)))))
			((utr) (do-untrace (map resolve-var (string-split (read-line)))))
			((br) (do-break (map resolve-var (string-split (read-line)))))
			((ubr) (do-unbreak (map resolve-var (string-split (read-line)))))
			((uba) (do-unbreak-all))
			((breakall) 
			 (set! ##sys#break-in-thread #f) ) 
			((breakonly)
			 (set! ##sys#break-in-thread (eval (read))) )
			((info)
			 (when (pair? traced-procedures)
			   (printf "Traced: ~s~%" (map car traced-procedures)) )
			 (when (pair? broken-procedures)
			   (printf "Breakpoints: ~s~%" (map car broken-procedures)) ) )
			((c)
			 (cond (##sys#last-breakpoint
				(let ((exn ##sys#last-breakpoint))
				  (set! ##sys#last-breakpoint #f)
				  (##sys#break-resume exn) ) )
			       (else (display "no breakpoint pending\n") ) ) )
			((exn) 
			 (when ##sys#last-exception
			   (history-add (list ##sys#last-exception))
			   (describe ##sys#last-exception) ) )
			((step)
			 (let ((x (read)))
			   (read-line)
			   (singlestep (eval `(lambda () ,x))) ) )
			((s)
			 (let* ((str (read-line))
				(r (system str)) )
			   (history-add (list r))
			   r) )
			((?)
			 (display 
			  "Toplevel commands:

 ,?                Show this text
 ,p EXP            Pretty print evaluated expression EXP
 ,d EXP            Describe result of evaluated expression EXP
 ,du EXP           Dump data of expression EXP
 ,dur EXP N        Dump range
 ,q                Quit interpreter
 ,l FILENAME ...   Load one or more files
 ,ln FILENAME ...  Load one or more files and print result of each top-level expression
 ,r                Show system information
 ,s TEXT ...       Execute shell-command
 ,tr NAME ...      Trace procedures
 ,utr NAME ...     Untrace procedures
 ,br NAME ...      Set breakpoints
 ,ubr NAME ...     Remove breakpoints
 ,uba              Remove all breakpoints
 ,breakall         Break in all threads (default)
 ,breakonly THREAD Break only in specified thread
 ,c                Continue from breakpoint
 ,info             List traced procedures and breakpoints
 ,step EXPR        Execute EXPR in single-stepping mode
 ,exn              Describe last exception
 ,t EXP            Evaluate form and print elapsed time
 ,x EXP            Pretty print expanded expression EXP\n")
			 (##sys#hash-table-for-each
			  (lambda (k v) 
			    (let ((help (cdr v)))
			      (if help
				  (print #\space help)
				  (print " ," k) ) ) )
			  command-table)
			 (##sys#void) )
			(else
			 (printf "Undefined toplevel command ~s - enter `,?' for help~%" form) 
			 (##sys#void) ) ) ) ) ) )
	    (else
	     (receive rs (eval form)
	       (history-add rs)
	       (apply values rs) ) ) ) ) ) )

(define (resolve-var str)
  (##sys#strip-syntax (string->symbol str) (##sys#current-environment) #t))


;;; Tracing:

(define (del x lst tst)
  (let loop ([lst lst])
    (if (null? lst)
	'()
	(let ([y (car lst)])
	  (if (tst x y)
	      (cdr lst)
	      (cons y (loop (cdr lst))) ) ) ) ) )

(define trace-indent-level 0)
(define traced-procedures '())
(define broken-procedures '())

(define trace-indent
  (lambda ()
    (write-char #\|)
    (do ((i trace-indent-level (sub1 i)))
	((<= i 0))
      (write-char #\space) ) ) )

(define traced-procedure-entry
  (lambda (name args)
    (trace-indent)
    (set! trace-indent-level (add1 trace-indent-level))
    (write (cons name args))
    (##sys#write-char-0 #\newline ##sys#standard-output) 
    (flush-output) ) )

(define traced-procedure-exit
  (lambda (name results)
    (set! trace-indent-level (sub1 trace-indent-level))
    (trace-indent)
    (write name)
    (display " -> ")
    (for-each
     (lambda (x)
       (write x)
       (write-char #\space) )
     results)
    (##sys#write-char-0 #\newline ##sys#standard-output) 
    (flush-output) ) )

(define do-trace
  (lambda (names)
    (if (null? names)
	(for-each (lambda (a) (print (car a))) traced-procedures) 
	(for-each
	 (lambda (s)
	   (let ((s (expand s)))
	     (cond ((assq s traced-procedures)
		    (##sys#warn "procedure already traced" s) )
		   ((assq s broken-procedures)
		    (##sys#warn "procedure already has breakpoint") )
		   (else
		    (let ((old (##sys#slot s 0)))
		      (cond ((not (procedure? old)) (##sys#error "cannot trace non-procedure" s))
			    (else
			     (set! traced-procedures (cons (cons s old) traced-procedures))
			     (##sys#setslot
			      s 0
			      (lambda args
				(traced-procedure-entry s args)
				(call-with-values (lambda () (apply old args))
				  (lambda results
				    (traced-procedure-exit s results) 
				    (apply values results) ) ) ) ) ) ) ) ) ) ) )
	 names) ) ) )

(define do-untrace 
  (lambda (names)
    (for-each
     (lambda (s)
       (let* ((s (expand s))
	      (p (assq s traced-procedures)) )
	 (cond ((not p) (##sys#warn "procedure not traced" s))
	       (else
		(##sys#setslot s 0 (cdr p))
		(set! traced-procedures (del p traced-procedures eq?) ) ) ) ) )
     names) ) )

(define do-break
  (lambda (names)
    (if (null? names)
	(for-each (lambda (b) (print (car a))) broken-procedures) 
	(for-each
	 (lambda (s)
	   (let* ((s (expand s))
		  (a (assq s traced-procedures)))
	     (when a
	       (##sys#warn "un-tracing procedure" s)
	       (##sys#setslot s 0 (cdr a))
	       (set! traced-procedures (del a traced-procedures eq?)) )
	     (let ((old (##sys#slot s 0)))
	       (cond ((not (procedure? old)) (##sys#error "cannot set breakpoint on non-procedure" s))
		     (else
		      (set! broken-procedures (cons (cons s old) broken-procedures))
		      (##sys#setslot
		       s 0
		       (lambda args
			 (##sys#break-entry s args)
			 (##sys#apply old args) ) ) ) ) ) ) )
	 names) ) ) )

(define do-unbreak
  (lambda (names)
    (for-each
     (lambda (s)
       (let* ((s (expand s))
	      (p (assq s broken-procedures)) )
	 (cond ((not p) (##sys#warn "procedure has no breakpoint" s))
	       (else
		(##sys#setslot s 0 (cdr p))
		(set! broken-procedures (del p broken-procedures eq?) ) ) ) ) )
     names) ) )

(define do-unbreak-all
  (lambda ()
    (for-each (lambda (bp)
                (##sys#setslot (car bp) 0 (cdr bp)))
              broken-procedures)
    (set! broken-procedures '())
    (##sys#void)))

;;; Parse options from string:

(define (parse-option-string str)
  (let ([ins (open-input-string str)])
    (map (lambda (o)
	   (if (string? o)
	       o
	       (let ([os (open-output-string)])
		 (write o os)
		 (get-output-string os) ) ) )
	 (handle-exceptions ex (##sys#error "invalid option syntax" str)
	   (do ([x (read ins) (read ins)]
		[xs '() (cons x xs)] )
	       ((eof-object? x) (reverse xs)) ) ) ) ) )


;;; Print status information:

(define report
  (let ((printf printf)
	(chop chop)
	(sort sort)
	(with-output-to-port with-output-to-port)
	(current-output-port current-output-port) )
    (lambda port
      (with-output-to-port (if (pair? port) (car port) (current-output-port))
	(lambda ()
	  (gc)
	  (let ([sinfo (##sys#symbol-table-info)]
		[minfo (memory-statistics)] )
	    (define (shorten n) (/ (truncate (* n 100)) 100))
	    (printf "Features:")
	    (for-each
	     (lambda (lst) 
	       (display "\n  ")
	       (for-each 
		(lambda (f)
		  (printf "~a~a" f (make-string (fxmax 1 (fx- 16 (string-length f))) #\space)) )
		lst) )
	     (chop (sort (map keyword->string ##sys#features) string<?) 5))
	    (printf "~%~
                   Machine type:    \t~A ~A~%~
                   Software type:   \t~A~%~
                   Software version:\t~A~%~
                   Build platform:  \t~A~%~
                   Include path:    \t~A~%~
                   Symbol-table load:\t~S~%  ~
                     Avg bucket length:\t~S~%  ~
                     Total symbol count:\t~S~%~
                   Memory:\theap size is ~S bytes~A with ~S bytes currently in use~%~  
                     nursery size is ~S bytes, stack grows ~A~%"
		    (machine-type)
		    (if (##sys#fudge 3) "(64-bit)" "")
		    (software-type)
		    (software-version)
		    (build-platform)
		    ##sys#include-pathnames
		    (shorten (vector-ref sinfo 0))
		    (shorten (vector-ref sinfo 1))
		    (vector-ref sinfo 2)
		    (vector-ref minfo 0)
		    (if (##sys#fudge 17) " (fixed)" "")
		    (vector-ref minfo 1)
		    (vector-ref minfo 2)
		    (if (= 1 (##sys#fudge 18)) "downward" "upward") )
	    (##sys#write-char-0 #\newline ##sys#standard-output)
	    (when (##sys#fudge 14) (display "interrupts are enabled\n"))
	    (when (##sys#fudge 15) (display "symbol gc is enabled\n")) 
	    (##core#undefined) ) ) ) ) ) )


;;; Describe & dump:

(define bytevector-data
  '((u8vector "vector of unsigned bytes" u8vector-length u8vector-ref)
    (s8vector "vector of signed bytes" s8vector-length s8vector-ref)
    (u16vector "vector of unsigned 16-bit words" u16vector-length u16vector-ref)
    (s16vector "vector of signed 16-bit words" s16vector-length s16vector-ref)
    (u32vector "vector of unsigned 32-bit words" u32vector-length u32vector-ref)
    (s32vector "vector of signed 32-bit words" s32vector-length s32vector-ref)
    (f32vector "vector of 32-bit floats" f32vector-length f32vector-ref)
    (f64vector "vector of 64-bit floats" f64vector-length f64vector-ref) ) )

(define-constant max-describe-lines 40)

(define describer-table (make-vector 37 '()))

(define describe
  (let ([sprintf sprintf]
	[printf printf] 
	[fprintf fprintf]
	[length length]
	[list-ref list-ref]
	[string-ref string-ref])
    (lambda (x #!optional (out ##sys#standard-output))
      (define (descseq name plen pref start)
	(let ((len (fx- (plen x) start)))
	  (when name (fprintf out "~A of length ~S~%" name len))
	  (let loop1 ((i 0))
	    (cond ((fx>= i len))
		  ((fx>= i max-describe-lines)
		   (fprintf out "~% (~A elements not displayed)~%" (fx- len i)) )
		  (else
		   (let ((v (pref x (fx+ start i))))
		     (let loop2 ((n 1) (j (fx+ i (fx+ start 1))))
		       (cond ((fx>= j len)
			      (fprintf out " ~S: ~S" i v)
			      (if (fx> n 1)
				  (fprintf out "\t(followed by ~A identical instance~a)~% ...~%" 
					   (fx- n 1)
					   (if (eq? n 2) "" "s"))
				  (newline out) )
			      (loop1 (fx+ i n)) )
			     ((eq? v (pref x j)) (loop2 (fx+ n 1) (fx+ j 1)))
			     (else (loop2 n len)) ) ) ) ) ) ) ) )
      (when (##sys#permanent? x)
	(fprintf out "statically allocated (0x~X) " (##sys#block-address x)) )
      (cond [(char? x)
	     (let ([code (char->integer x)])
	       (fprintf out "character ~S, code: ~S, #x~X, #o~O~%" x code code code) ) ]
	    [(eq? x #t) (fprintf out "boolean true~%")]
	    [(eq? x #f) (fprintf out "boolean false~%")]
	    [(null? x) (fprintf out "empty list~%")]
	    [(eof-object? x) (fprintf out "end-of-file object~%")]
	    [(eq? (##sys#void) x) (fprintf out "unspecified object~%")]
	    [(fixnum? x)
	     (fprintf out "exact integer ~S, #x~X, #o~O, #b~B" x x x x)
	     (let ([code (integer->char x)])
	       (when (fx< x #x10000) (fprintf out ", character ~S" code)) )
	     (##sys#write-char-0 #\newline ##sys#standard-output) ]
	    [(eq? x (##sys#slot '##sys#arbitrary-unbound-symbol 0))
	     (fprintf out "unbound value~%") ]
	    [(##sys#number? x) (fprintf out "number ~S~%" x)]
	    [(string? x) (descseq "string" ##sys#size string-ref 0)]
	    [(vector? x) (descseq "vector" ##sys#size ##sys#slot 0)]
	    [(symbol? x)
	     (unless (##sys#symbol-has-toplevel-binding? x) (display "unbound " out))
	     (when (and (symbol? x) (fx= 0 (##sys#byte (##sys#slot x 1) 0)))
	       (display "keyword " out) )
	     (fprintf out "~asymbol with name ~S~%"
		      (if (##sys#interned-symbol? x) "" "uninterned ")
		      (##sys#symbol->string x))
	     (let ((plist (##sys#slot x 2)))
	       (unless (null? plist)
		 (display "  \nproperties:\n\n" out)
		 (do ((plist plist (cddr plist)))
		     ((null? plist))
		   (fprintf out "  ~s\t" (car plist))
		   (##sys#with-print-length-limit
		    1000
		    (lambda ()
		      (write (cadr plist) out) ) )
		   (newline out) ) ) ) ]
	    [(list? x) (descseq "list" length list-ref 0)]
	    [(pair? x) (fprintf out "pair with car ~S and cdr ~S~%" (car x) (cdr x))]
	    [(procedure? x)
	     (let ([len (##sys#size x)])
	       (if (and (> len 3)
			(memq #:tinyclos ##sys#features)
			(eq? ##tinyclos#entity-tag (##sys#slot x (fx- len 1))) ) ;XXX handle this in tinyclos egg (difficult)
		   (describe-object x out)
		   (descseq 
		    (sprintf "procedure with code pointer ~X" (##sys#peek-unsigned-integer x 0))
		    ##sys#size ##sys#slot 1) ) ) ]
	    [(port? x)
	     (fprintf out
		      "~A port of type ~A with name ~S and file pointer ~X~%"
		      (if (##sys#slot x 1) "input" "output")
		      (##sys#slot x 7)
		      (##sys#slot x 3)
		      (##sys#peek-unsigned-integer x 0) ) ]
	    [(and (memq #:tinyclos ##sys#features) (instance? x)) ; XXX put into tinyclos egg
	     (describe-object x out) ]
	    [(##sys#locative? x)
	     (fprintf out "locative~%  pointer ~X~%  index ~A~%  type ~A~%"
		      (##sys#peek-unsigned-integer x 0)
		      (##sys#slot x 1)
		      (case (##sys#slot x 2) 
			[(0) "slot"]
			[(1) "char"]
			[(2) "u8vector"]
			[(3) "s8vector"]
			[(4) "u16vector"]
			[(5) "s16vector"]
			[(6) "u32vector"]
			[(7) "s32vector"]
			[(8) "f32vector"]
			[(9) "f64vector"] ) ) ]
	    [(##sys#pointer? x) (fprintf out "machine pointer ~X~%" (##sys#peek-unsigned-integer x 0))]
	    [(##sys#bytevector? x)
	     (let ([len (##sys#size x)])
	       (fprintf out "blob of size ~S:~%" len)
	       (hexdump x len ##sys#byte out) ) ]
	    [(##core#inline "C_lambdainfop" x)
	     (fprintf out "lambda information: ~s~%" (##sys#lambda-info->string x)) ]
	    [(##sys#structure? x 'hash-table)
	     (let ((n (##sys#slot x 2)))
	       (fprintf out "hash-table with ~S element~a~%  comparison procedure: ~A~%"
			n (if (fx= n 1) "" "s")  (##sys#slot x 3)) )
	     (fprintf out "  hash function: ~a~%" (##sys#slot x 4))
	     (hash-table-walk		; blindly assumes it is bound
	      x
	      (lambda (k v) (fprintf out " ~S\t-> ~S~%" k v)) ) ]
	    [(##sys#structure? x 'condition)
	     (fprintf out "condition: ~s~%" (##sys#slot x 1))
	     (for-each
	      (lambda (k)
		(fprintf out " ~s~%" k)
		(let loop ((props (##sys#slot x 2)))
		  (unless (null? props)
		    (when (eq? k (caar props))
		      (fprintf out "\t~s: ~s~%" (cdar props) (cadr props)) )
		    (loop (cddr props)) ) ) )
	      (##sys#slot x 1) ) ]
	    [(and (##sys#structure? x 'meroon-instance) (provided? 'meroon)) ; XXX put this into meroon egg (really!)
	     (unveil x out) ]
	    [(##sys#generic-structure? x)
	     (let ([st (##sys#slot x 0)])
	       (cond ((##sys#hash-table-ref describer-table st) => (cut <> x out))
		     ((assq st bytevector-data) =>
		      (lambda (data)
			(apply descseq (append (map eval (cdr data)) (list 0)))) )
		     (else
		      (fprintf out "structure of type `~S':~%" (##sys#slot x 0))
		      (descseq #f ##sys#size ##sys#slot 1) ) ) ) ]
	    [else (fprintf out "unknown object~%")] )
      (##sys#void) ) ) )

(define (set-describer! tag proc)
  (##sys#check-symbol tag 'symbol 'set-describer!)
  (##sys#hash-table-set! describer-table tag proc) )


;;; Display hexdump:

(define dump
  (lambda (x . len-out)
    (let-optionals len-out
	([len #f]
	 [out ##sys#standard-output] )
      (define (bestlen n) (if len (min len n) n))
      (cond [(##sys#immediate? x) (##sys#error 'dump "cannot dump immediate object" x)]
	    [(##sys#bytevector? x) (hexdump x (bestlen (##sys#size x)) ##sys#byte out)]
	    [(string? x) (hexdump x (bestlen (##sys#size x)) ##sys#byte out)]
	    [(and (not (##sys#immediate? x)) (##sys#pointer? x))
	     (hexdump x 32 ##sys#peek-byte out) ]
	    [(and (##sys#generic-structure? x) (assq (##sys#slot x 0) bytevector-data))
	     (let ([bv (##sys#slot x 1)])
	       (hexdump bv (bestlen (##sys#size bv)) ##sys#byte out) ) ]
	    [else (##sys#error 'dump "cannot dump object" x)] ) ) ) )

(define hexdump
  (let ([display display]
	[string-append string-append]
	[make-string make-string]
	[write-char write-char] )
    (lambda (bv len ref out)

      (define (justify n m base lead)
	(let* ([s (number->string n base)]
	       [len (##sys#size s)] )
	  (if (fx< len m)
	      (string-append (make-string (fx- m len) lead) s)
	      s) ) )

      (do ([a 0 (fx+ a 16)])
	  ((fx>= a len))
	(display (justify a 4 10 #\space) out)
	(write-char #\: out)
	(do ([j 0 (fx+ j 1)]
	     [a a (fx+ a 1)] )
	    ((or (fx>= j 16) (fx>= a len))
	     (and-let* ([(fx>= a len)]
			[o (fxmod len 16)]
			[(not (fx= o 0))] )
	       (do ([k (fx- 16 o) (fx- k 1)])
		   ((fx= k 0))
		 (display "   " out) ) ) )
	  (write-char #\space out)
	  (display (justify (ref bv a) 2 16 #\0) out) )
	(write-char #\space out)
	(do ([j 0 (fx+ j 1)]
	     [a a (fx+ a 1)] )
	    ((or (fx>= j 16) (fx>= a len)))
	  (let ([c (ref bv a)])
	    (if (and (fx>= c 32) (fx< c 128))
		(write-char (integer->char c) out)
		(write-char #\. out) ) ) ) 
	(##sys#write-char-0 #\newline out) ) ) ) )


;;; Start interpreting:

(define (deldups lis . maybe-=)
  (let ((elt= (optional maybe-= equal?)))
    (let recur ((lis lis))
      (if (null? lis) lis
	  (let* ((x (car lis))
		 (tail (cdr lis))
		 (new-tail (recur (del x tail elt=))))
	    (if (eq? tail new-tail) lis (cons x new-tail)))))))

(define (member* keys set)
  (let loop ((set set))
    (and (pair? set)
	 (let find ((ks keys))
	   (cond ((null? ks) (loop (cdr set)))
		 ((equal? (car ks) (car set)) set)
		 (else (find (cdr ks))) ) ) ) ) )

(define-constant short-options 
  '(#\k #\s #\v #\h #\D #\e #\i #\R #\b #\n #\q #\w #\- #\I #\p #\P) )

(define-constant long-options
  '("-ss" "-sx" "-script" "-version" "-help" "--help" "-feature" "-eval"
    "-case-insensitive" "-keyword-style" "-no-parentheses-synonyms" "-no-symbol-escape"
    "-r5rs-syntax"
    "-require-extension" "-batch" "-quiet" "-no-warnings" "-no-init" 
    "-include-path" "-release" "-print" "-pretty-print" "--") )

(define (canonicalize-args args)
  (let loop ((args args))
    (if (null? args)
	'()
	(let ((x (car args)))
	  (cond ((member x '("-s" "-ss" "-script" "-sx" "--")) args)
                ((and (fx> (##sys#size x) 2)
                       (char=? #\- (##core#inline "C_subchar" x 0))
                       (not (member x long-options)) )
                 (if (char=? #\: (##core#inline "C_subchar" x 1))
                     (loop (cdr args))
                     (let ((cs (string->list (substring x 1))))
                       (if (findall cs short-options)
                           (append (map (cut string #\- <>) cs) (loop (cdr args)))
                           (##sys#error "invalid option" x) ) ) ) )
	        (else (cons x (loop (cdr args)))))))))

(define (findall chars clist)
  (let loop ((chars chars))
    (or (null? chars)
	(and (memq (car chars) clist)
	     (loop (cdr chars))))))

(define-constant simple-options
  '("--" "-b" "-batch" "-q" "-quiet" "-n" "-no-init" "-w" "-no-warnings" "-i" "-case-insensitive"
    "-no-parentheses-synonyms" "-no-symbol-escape" "-r5rs-syntax"
    ; Not "simple" but processed early
    "-ss" "-sx" "-s" "-script") )

(define-constant complex-options
  '("-D" "-feature" "-I" "-include-path" "-k" "-keyword-style") )

(define (run)
  (let* ([extraopts (parse-option-string (or (get-environment-variable "CSI_OPTIONS") ""))]
	 [args (canonicalize-args (command-line-arguments))]
	 ; Check for these before 'args' is updated by any 'extraopts'
	 [kwstyle (member* '("-k" "-keyword-style") args)]
	 [script (member* '("-ss" "-sx" "-s" "-script") args)])
    (cond [script
	   (when (or (not (pair? (cdr script)))
		     (zero? (string-length (cadr script)))
		     (char=? #\- (string-ref (cadr script) 0)) )
	     (##sys#error "missing or invalid script argument"))
	   (program-name (cadr script))
	   (command-line-arguments (cddr script))
	   (register-feature! 'script)
	   (set-cdr! (cdr script) '()) 
	   (when ##sys#windows-platform
	     (and-let* ((sname (lookup-script-file (cadr script))))
	       (set-car! (cdr script) sname) ) ) ]
	  [else
	   (set! args (append (canonicalize-args extraopts) args))
	   (and-let* ([p (member "--" args)])
	     (set-cdr! p '()) ) ] )
    (let* ([eval? (member* '("-e" "-p" "-P" "-eval" "-print" "-pretty-print") args)]
	   [batch (or script (member* '("-b" "-batch") args) eval?)]
	   [quietflag (member* '("-q" "-quiet") args)]
	   [quiet (or script quietflag eval?)]
	   [ipath (map chop-separator (string-split (or (get-environment-variable "CHICKEN_INCLUDE_PATH") "") ";"))] )      
      (define (collect-options opt)
	(let loop ([opts args])
	  (cond [(member opt opts) 
		 => (lambda (p)
		      (if (null? (cdr p))
			  (##sys#error "missing argument to command-line option" opt)
			  (cons (cadr p) (loop (cddr p)))) ) ]
		[else '()] ) ) )
      (define (loadinit)
	(let ([fn (##sys#string-append "./" init-file)])
	  (if (file-exists? fn)
	      (load fn)
	      (let* ([prefix (chop-separator (or (get-environment-variable "HOME") "."))]
		     [fn (string-append prefix "/" init-file)] )
		(when (file-exists? fn) 
		  (load fn) ) ) ) ) )
      (define (evalstring str #!optional (rec (lambda _ (void))))
	(let ((in (open-input-string str)))
	  (do ([x (read in) (read in)])
	      ((eof-object? x))
	    (rec (receive (eval x))) ) ) )
      (when quietflag (set! ##sys#eval-debug-level 0))
      (when (member* '("-h" "-help" "--help") args)
	(print-usage)
	(exit 0) )
      (when (member* '("-v" "-version") args)
	(print-banner)
	(exit 0) )
      (when (member "-release" args)
	(print (chicken-version))
	(exit 0) )
      (when (member* '("-w" "-no-warnings") args)
	(unless quiet (display "Warnings are disabled\n"))
	(set! ##sys#warnings-enabled #f) )
      (unless quiet
	(load-verbose #t)
	(print-banner) )
      (when (member* '("-i" "-case-insensitive") args)
	(unless quiet (display "Identifiers and symbols are case insensitive\n"))
	(register-feature! 'case-insensitive)
	(case-sensitive #f) )
      (for-each register-feature! (collect-options "-feature"))
      (for-each register-feature! (collect-options "-D"))
      (set! ##sys#include-pathnames 
	(deldups
	 (append (map chop-separator (collect-options "-include-path"))
		 (map chop-separator (collect-options "-I"))
		 ##sys#include-pathnames
		 ipath)
	 string=?) )
      (when kwstyle
	(cond [(not (pair? (cdr kwstyle)))
	       (##sys#error "missing argument to `-keyword-style' option") ]
	      [(string=? "prefix" (cadr kwstyle))
	       (keyword-style #:prefix) ]
	      [(string=? "none" (cadr kwstyle))
	       (keyword-style #:none) ]
	      [(string=? "suffix" (cadr kwstyle))
	       (keyword-style #:suffix) ] ) )
      (when (member* '("-no-parentheses-synonyms") args)
	(unless quiet (display "Disabled support for parentheses synonyms\n"))
        (parentheses-synonyms #f) )
      (when (member* '("-no-symbol-escape") args)
	(unless quiet (display "Disabled support for escaped symbols\n"))
	(symbol-escape #f) )
      (when (member* '("-r5rs-syntax") args)
	(unless quiet (display "Disabled the Chicken extensions to R5RS syntax\n"))
	(case-sensitive #f)
	(keyword-style #:none)
	(parentheses-synonyms #f)
	(symbol-escape #f) )
      (unless (or (member* '("-n" "-no-init") args) script) (loadinit))
      (do ([args args (cdr args)])
	  ((null? args)
	   (unless batch 
	     (repl)
	     (##sys#write-char-0 #\newline ##sys#standard-output) ) )
	(let* ([arg (car args)]
	       #;[len (string-length arg)] )
	  (cond ((member arg simple-options) )
		((member arg complex-options)
		 (set! args (cdr args)) )
		((or (string=? "-R" arg) (string=? "-require-extension" arg))
		 (eval `(##core#require-extension (,(string->symbol (cadr args))) #t))
		 (set! args (cdr args)) )
		((or (string=? "-e" arg) (string=? "-eval" arg))
		 (evalstring (cadr args))
		 (set! args (cdr args)) )
		((or (string=? "-p" arg) (string=? "-print" arg))
		 (evalstring (cadr args) (cut for-each print <...>))
		 (set! args (cdr args)) )
		((or (string=? "-P" arg) (string=? "-pretty-print" arg))
		 (evalstring (cadr args) (cut for-each pretty-print <...>) )
		 (set! args (cdr args)) )
		(else
		 (let ((scr (and script (car script))))
		   (##sys#load 
		    arg 
		    (and (equal? "-sx" scr)
			 (lambda (x)
			   (pretty-print x ##sys#standard-error)
			   (newline ##sys#standard-error)
			   (eval x)))
		    #f)
		   (when (equal? "-ss" scr)
		     (call-with-values (cut main (command-line-arguments))
		       (lambda results
			 (exit
			  (if (and (pair? results) (fixnum? (car results)))
			      (car results)
			      0) ) ) ) ) ) ) ) ) ) ) ) )

(run)
