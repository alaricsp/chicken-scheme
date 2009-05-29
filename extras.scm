;;; extras.scm - Optional non-standard extensions
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
 (unit extras)
 (uses data-structures ports)
 (usual-integrations)
 (disable-warning redef) )

(cond-expand
 [paranoia]
 [else
  (declare
    (no-bound-checks)
    (no-procedure-checks-for-usual-bindings)
    (bound-to-procedure
      ##sys#check-char ##sys#check-exact ##sys#check-port ##sys#check-string
      ##sys#substring ##sys#for-each ##sys#map ##sys#setslot
      ##sys#allocate-vector ##sys#check-pair ##sys#error-not-a-proper-list
      ##sys#member ##sys#assoc ##sys#error ##sys#signal-hook ##sys#read-string!
      ##sys#check-symbol ##sys#check-vector ##sys#floor ##sys#ceiling
      ##sys#truncate ##sys#round ##sys#check-number ##sys#cons-flonum
      ##sys#flonum-fraction ##sys#make-port ##sys#fetch-and-check-port-arg
      ##sys#print ##sys#check-structure ##sys#make-structure make-parameter
      ##sys#flush-output ##sys#write-char-0 ##sys#number->string
      ##sys#fragments->string ##sys#symbol->qualified-string
      reverse-string-append ##sys#number? ##sys#procedure->string
      ##sys#pointer->string ##sys#user-print-hook ##sys#peek-char-0
      ##sys#read-char-0 ##sys#write-char ##sys#string-append ##sys#gcd ##sys#lcm
      ##sys#fudge ##sys#check-list ##sys#user-read-hook ##sys#check-closure ##sys#check-inexact
      input-port? make-vector list->vector sort! merge! open-output-string floor
      get-output-string current-output-port display write port? list->string
      make-string string pretty-print-width newline char-name read random
      open-input-string make-string call-with-input-file read-line reverse ) ) ] )

(declare
  (hide
    fprintf0 generic-write reverse-string-append) )

(include "unsafe-declarations.scm")

(register-feature! 'extras)


;;; Read expressions from file:

(define read-file
  (let ([read read]
	[reverse reverse] 
	[call-with-input-file call-with-input-file] )
    (lambda (#!optional (port ##sys#standard-input) (reader read) max)
      (define (slurp port)
	(do ((x (reader port) (reader port))
	     (i 0 (fx+ i 1))
	     (xs '() (cons x xs)) )
	    ((or (eof-object? x) (and max (fx>= i max))) (reverse xs)) ) )
      (if (port? port)
	  (slurp port)
	  (call-with-input-file port slurp) ) ) ) )


;;; Random numbers:

(cond-expand
  (unix

    (define random-seed)
    (define randomize)

    (let ((srandom (foreign-lambda void "srandom" unsigned-integer)))

      (set! random-seed
        (lambda (#!optional (seed (current-seconds)))
          (##sys#check-integer seed 'random-seed)
          (srandom seed) ) )

      (set! randomize
        (lambda (#!optional (seed (##sys#fudge 2)))
          (##sys#check-exact seed 'randomize)
          (srandom seed) ) ) )

    (define (random n)
      (##sys#check-integer n 'random)
      (if (eq? 0 n)
          0
          ((foreign-lambda* long ((integer64 n)) "return( random() % ((uint64_t) n) );") n) ) ) )
  (else

    (define random-seed
      (let ((srand (foreign-lambda void "srand" unsigned-integer)))
        (lambda n
          (let ((t (if (null? n) (current-seconds) (car n))))
            (##sys#check-integer t 'random-seed)
            (srand t) ) ) ) )

    (define (randomize . n)
      (let ((nn (if (null? n) (##sys#fudge 2) (car n))))
        (##sys#check-exact nn 'randomize)
        (##core#inline "C_randomize" nn) ) )

    (define (random n)
      (##sys#check-exact n 'random)
      (if (eq? n 0)
          0
          (##core#inline "C_random_fixnum" n) ) ) ) )


;;; Line I/O:

(define read-line
  (let ([make-string make-string])
    (define (fixup str len)
      (##sys#substring
       str 0
       (if (and (fx>= len 1) (char=? #\return (##core#inline "C_subchar" str (fx- len 1))))
	   (fx- len 1)
	   len) ) )
    (lambda args
      (let* ([parg (pair? args)]
	     [p (if parg (car args) ##sys#standard-input)]
	     [limit (and parg (pair? (cdr args)) (cadr args))])
	(##sys#check-port p 'read-line)
	(cond ((##sys#slot (##sys#slot p 2) 8) => (lambda (rl) (rl p limit)))
	      (else
	       (let* ((buffer-len (if limit limit 256))
		      (buffer (##sys#make-string buffer-len)))
		 (let loop ([i 0])
		   (if (and limit (fx>= i limit))
		       (##sys#substring buffer 0 i)
		       (let ([c (##sys#read-char-0 p)])
			 (if (eof-object? c)
			     (if (fx= i 0)
				 c
				 (##sys#substring buffer 0 i) ) 
			     (case c
			       [(#\newline) (##sys#substring buffer 0 i)]
			       [(#\return)
				(let ([c (peek-char p)])
				  (if (char=? c #\newline)
				      (begin (##sys#read-char-0 p)
					     (##sys#substring buffer 0 i))
				      (##sys#substring buffer 0 i) ) ) ]
			       [else
				(when (fx>= i buffer-len)
				  (set! buffer (##sys#string-append buffer (make-string buffer-len)))
				  (set! buffer-len (fx+ buffer-len buffer-len)) )
				(##core#inline "C_setsubchar" buffer i c)
				(loop (fx+ i 1)) ] ) ) ) ) ) ) ) ) ) ) ) )

(define read-lines
  (let ((read-line read-line)
	(call-with-input-file call-with-input-file) 
	(reverse reverse) )
    (lambda port-and-max
      (let* ((port (if (pair? port-and-max) (##sys#slot port-and-max 0) ##sys#standard-input))
	     (rest (and (pair? port-and-max) (##sys#slot port-and-max 1)))
	     (max (if (pair? rest) (##sys#slot rest 0) #f)) )
	(define (doread port)
	  (let loop ((lns '())
		     (n (or max 1000000000)) ) ; this is silly
	    (if (eq? n 0)
		(reverse lns)
		(let ((ln (read-line port)))
		  (if (eof-object? ln)
		      (reverse lns)
		      (loop (cons ln lns) (fx- n 1)) ) ) ) ) )
	(if (string? port)
	    (call-with-input-file port doread)
	    (begin
	      (##sys#check-port port 'read-lines)
	      (doread port) ) ) ) ) ) )


;;; Extended I/O 

(define (##sys#read-string! n dest port start)
  (cond ((eq? n 0) 0)
	(else
	 (when (##sys#slot port 6)	; peeked?
	   (##core#inline "C_setsubchar" dest start (##sys#read-char-0 port))
	   (set! start (fx+ start 1)) )
	 (let ((rdstring (##sys#slot (##sys#slot port 2) 7)))
           (if rdstring
	       (let loop ((start start) (n n) (m 0))
		 (let ((n2 (rdstring port n dest start)))
		   (##sys#setislot port 5 ; update port-position
				   (fx+ (##sys#slot port 5) n2))
		   (cond ((eq? n2 0) m)
			 ((or (not n) (fx< n2 n)) 
			  (loop (fx+ start n2) (and n (fx- n n2)) (fx+ m n2)))
			 (else (fx+ n2 m)))))
	       (let loop ((start start) (n n) (m 0))
		 (let ((n2 (let ((c (##sys#read-char-0 port)))
			     (if (eof-object? c)
				 0
				 (begin
				   (##core#inline "C_setsubchar" dest start c)
				   1) ) )  ) )
		   (cond ((eq? n2 0) m)
			 ((or (not n) (fx< n2 n)) 
			  (loop (fx+ start n2) (and n (fx- n n2)) (fx+ m n2)) )
			 (else (fx+ n2 m))) )))))))

(define (read-string! n dest #!optional (port ##sys#standard-input) (start 0))
  (##sys#check-port port 'read-string!)
  (##sys#check-string dest 'read-string!)
  (when n
    (##sys#check-exact n 'read-string!)
    (when (fx> (fx+ start n) (##sys#size dest))
      (set! n (fx- (##sys#size dest) start))))
  (##sys#check-exact start 'read-string!)
  (##sys#read-string! n dest port start) )

(define-constant read-string-buffer-size 2048)

(define ##sys#read-string/port
  (let ((open-output-string open-output-string)
	(get-output-string get-output-string) )
    (lambda (n p)
      (##sys#check-port p 'read-string)
      (cond (n (##sys#check-exact n 'read-string)
	       (let* ((str (##sys#make-string n))
		      (n2 (##sys#read-string! n str p 0)) )
		 (if (eq? n n2)
		     str
		     (##sys#substring str 0 n2))))
	    (else
	     (let ([out (open-output-string)]
		   (buf (make-string read-string-buffer-size)))
	       (let loop ()
		 (let ((n (##sys#read-string! read-string-buffer-size
					      buf p 0)))
		   (cond ((eq? n 0)
			  (get-output-string out))
			 (else
			  (write-string buf n out)
			  (loop)))))))))))

(define (read-string #!optional n (port ##sys#standard-input))
  (##sys#read-string/port n port) )

(define read-token
  (let ([open-output-string open-output-string]
	[get-output-string get-output-string] )
    (lambda (pred . port)
      (let ([port (optional port ##sys#standard-input)])
	(##sys#check-port port 'read-token)
	(let ([out (open-output-string)])
	  (let loop ()
	    (let ([c (##sys#peek-char-0 port)])
	      (if (and (not (eof-object? c)) (pred c))
		  (begin
		    (##sys#write-char-0 (##sys#read-char-0 port) out)
		    (loop) )
		  (get-output-string out) ) ) ) ) ) ) ) )

(define write-string 
  (let ([display display])
    (lambda (s . more)
      (##sys#check-string s 'write-string)
      (let-optionals more ([n #f] [port ##sys#standard-output])
	(##sys#check-port port 'write-string)
	(when n (##sys#check-exact n 'write-string))
	(display 
	 (if (and n (fx< n (##sys#size s)))
	     (##sys#substring s 0 n)
	     s)
	 port) ) ) ) )

(define write-line
  (let ((display display)
	(newline newline) )
    (lambda (str . port)
      (let ((p (if (##core#inline "C_eqp" port '())
		   ##sys#standard-output
		   (##sys#slot port 0) ) ) )
	(##sys#check-port p 'write-line)
	(##sys#check-string str 'write-line)
	(display str p)
	(newline p) ) ) ) )


;;; Binary I/O

(define (read-byte #!optional (port ##sys#standard-input))
  (##sys#check-port port 'read-byte)
  (let ((x (##sys#read-char-0 port)))
    (if (eof-object? x)
	x
	(char->integer x) ) ) )

(define (write-byte byte #!optional (port ##sys#standard-output))
  (##sys#check-exact byte 'write-byte)
  (##sys#check-port port 'write-byte)
  (##sys#write-char-0 (integer->char byte) port) )




;;; Pretty print:
;
; Copyright (c) 1991, Marc Feeley
; Author: Marc Feeley (feeley@iro.umontreal.ca)
; Distribution restrictions: none
;
; Modified by felix for use with CHICKEN
;

(define generic-write
  (let ([open-output-string open-output-string]
	[get-output-string get-output-string] )
    (lambda (obj display? width output)

      (define (read-macro? l)
	(define (length1? l) (and (pair? l) (null? (cdr l))))
	(let ((head (car l)) (tail (cdr l)))
	  (case head
	    ((quote quasiquote unquote unquote-splicing) (length1? tail))
	    (else                                        #f))))

      (define (read-macro-body l)
	(cadr l))

      (define (read-macro-prefix l)
	(let ((head (car l)) (tail (cdr l)))
	  (case head
	    ((quote)            "'")
	    ((quasiquote)       "`")
	    ((unquote)          ",")
	    ((unquote-splicing) ",@"))))

      (define (out str col)
	(and col (output str) (+ col (string-length str))))

      (define (wr obj col)

	(define (wr-expr expr col)
	  (if (read-macro? expr)
	      (wr (read-macro-body expr) (out (read-macro-prefix expr) col))
	      (wr-lst expr col)))

	(define (wr-lst l col)
	  (if (pair? l)
	      (let loop ((l (cdr l))
			 (col (and col (wr (car l) (out "(" col)))))
		(cond ((not col) col)
		      ((pair? l)
		       (loop (cdr l) (wr (car l) (out " " col))))
		      ((null? l) (out ")" col))
		      (else      (out ")" (wr l (out " . " col))))))
	      (out "()" col)))

	(cond ((pair? obj)        (wr-expr obj col))
	      ((null? obj)        (wr-lst obj col))
	      ((eof-object? obj)  (out "#!eof" col))
	      ((vector? obj)      (wr-lst (vector->list obj) (out "#" col)))
	      ((boolean? obj)     (out (if obj "#t" "#f") col))
	      ((##sys#number? obj)      (out (##sys#number->string obj) col))
	      ((symbol? obj)
	       (let ([s (open-output-string)])
		 (##sys#print obj #t s)
		 (out (get-output-string s) col) ) )
	      ((procedure? obj)   (out (##sys#procedure->string obj) col))
	      ((string? obj)      (if display?
				      (out obj col)
				      (let loop ((i 0) (j 0) (col (out "\"" col)))
					(if (and col (< j (string-length obj)))
					    (let ((c (string-ref obj j)))
					      (if (or (char=? c #\\)
						      (char=? c #\"))
						  (loop j
							(+ j 1)
							(out "\\"
							     (out (##sys#substring obj i j)
								  col)))
						  (loop i (+ j 1) col)))
					    (out "\""
						 (out (##sys#substring obj i j) col))))))
	      ((char? obj)        (if display?
				      (out (make-string 1 obj) col)
				      (let ([code (char->integer obj)])
					(out "#\\" col)
					(cond [(char-name obj) 
					       => (lambda (cn) 
						    (out (##sys#slot cn 1) col) ) ]
					      [(fx< code 32)
					       (out "x" col)
					       (out (number->string code 16) col) ]
					      [(fx> code 255)
					       (out (if (fx> code #xffff) "U" "u") col)
					       (out (number->string code 16) col) ]
					      [else (out (make-string 1 obj) col)] ) ) ) )
	      ((eof-object? obj)  (out "#<eof>" col))
	      ((##core#inline "C_undefinedp" obj) (out "#<unspecified>" col))
	      ((##core#inline "C_anypointerp" obj) (out (##sys#pointer->string obj) col))
	      ((eq? obj (##sys#slot '##sys#arbitrary-unbound-symbol 0))
	       (out "#<unbound value>" col) )
	      ((##sys#generic-structure? obj)
	       (let ([o (open-output-string)])
		 (##sys#user-print-hook obj #t o)
		 (out (get-output-string o) col) ) )
	      ((port? obj) (out (string-append "#<port " (##sys#slot obj 3) ">") col))
	      ((##core#inline "C_bytevectorp" obj)
	       (if (##core#inline "C_permanentp" obj)
		   (out "#<static blob of size" col)
		   (out "#<blob of size " col) )
	       (out (number->string (##core#inline "C_block_size" obj)) col)
	       (out ">" col) )
	      ((##core#inline "C_lambdainfop" obj)
	       (out "#<lambda info " col)
	       (out (##sys#lambda-info->string obj) col)
	       (out "#>" col) )
	      (else (out "#<unprintable object>" col)) ) )

      (define (pp obj col)

	(define (spaces n col)
	  (if (> n 0)
	      (if (> n 7)
		  (spaces (- n 8) (out "        " col))
		  (out (##sys#substring "        " 0 n) col))
	      col))

	(define (indent to col)
	  (and col
	       (if (< to col)
		   (and (out (make-string 1 #\newline) col) (spaces to 0))
		   (spaces (- to col) col))))

	(define (pr obj col extra pp-pair)
	  (if (or (pair? obj) (vector? obj)) ; may have to split on multiple lines
	      (let ((result '())
		    (left (max (+ (- (- width col) extra) 1) max-expr-width)))
		(generic-write obj display? #f
			       (lambda (str)
				 (set! result (cons str result))
				 (set! left (- left (string-length str)))
				 (> left 0)))
		(if (> left 0)		; all can be printed on one line
		    (out (reverse-string-append result) col)
		    (if (pair? obj)
			(pp-pair obj col extra)
			(pp-list (vector->list obj) (out "#" col) extra pp-expr))))
	      (wr obj col)))

	(define (pp-expr expr col extra)
	  (if (read-macro? expr)
	      (pr (read-macro-body expr)
		  (out (read-macro-prefix expr) col)
		  extra
		  pp-expr)
	      (let ((head (car expr)))
		(if (symbol? head)
		    (let ((proc (style head)))
		      (if proc
			  (proc expr col extra)
			  (if (> (string-length (##sys#symbol->qualified-string head))
				 max-call-head-width)
			      (pp-general expr col extra #f #f #f pp-expr)
			      (pp-call expr col extra pp-expr))))
		    (pp-list expr col extra pp-expr)))))

					; (head item1
					;       item2
					;       item3)
	(define (pp-call expr col extra pp-item)
	  (let ((col* (wr (car expr) (out "(" col))))
	    (and col
		 (pp-down (cdr expr) col* (+ col* 1) extra pp-item))))

					; (item1
					;  item2
					;  item3)
	(define (pp-list l col extra pp-item)
	  (let ((col (out "(" col)))
	    (pp-down l col col extra pp-item)))

	(define (pp-down l col1 col2 extra pp-item)
	  (let loop ((l l) (col col1))
	    (and col
		 (cond ((pair? l)
			(let ((rest (cdr l)))
			  (let ((extra (if (null? rest) (+ extra 1) 0)))
			    (loop rest
				  (pr (car l) (indent col2 col) extra pp-item)))))
		       ((null? l)
			(out ")" col))
		       (else
			(out ")"
			     (pr l
				 (indent col2 (out "." (indent col2 col)))
				 (+ extra 1)
				 pp-item)))))))

	(define (pp-general expr col extra named? pp-1 pp-2 pp-3)

	  (define (tail1 rest col1 col2 col3)
	    (if (and pp-1 (pair? rest))
		(let* ((val1 (car rest))
		       (rest (cdr rest))
		       (extra (if (null? rest) (+ extra 1) 0)))
		  (tail2 rest col1 (pr val1 (indent col3 col2) extra pp-1) col3))
		(tail2 rest col1 col2 col3)))

	  (define (tail2 rest col1 col2 col3)
	    (if (and pp-2 (pair? rest))
		(let* ((val1 (car rest))
		       (rest (cdr rest))
		       (extra (if (null? rest) (+ extra 1) 0)))
		  (tail3 rest col1 (pr val1 (indent col3 col2) extra pp-2)))
		(tail3 rest col1 col2)))

	  (define (tail3 rest col1 col2)
	    (pp-down rest col2 col1 extra pp-3))

	  (let* ((head (car expr))
		 (rest (cdr expr))
		 (col* (wr head (out "(" col))))
	    (if (and named? (pair? rest))
		(let* ((name (car rest))
		       (rest (cdr rest))
		       (col** (wr name (out " " col*))))
		  (tail1 rest (+ col indent-general) col** (+ col** 1)))
		(tail1 rest (+ col indent-general) col* (+ col* 1)))))

	(define (pp-expr-list l col extra)
	  (pp-list l col extra pp-expr))

	(define (pp-lambda expr col extra)
	  (pp-general expr col extra #f pp-expr-list #f pp-expr))

	(define (pp-if expr col extra)
	  (pp-general expr col extra #f pp-expr #f pp-expr))

	(define (pp-cond expr col extra)
	  (pp-call expr col extra pp-expr-list))

	(define (pp-case expr col extra)
	  (pp-general expr col extra #f pp-expr #f pp-expr-list))

	(define (pp-and expr col extra)
	  (pp-call expr col extra pp-expr))

	(define (pp-let expr col extra)
	  (let* ((rest (cdr expr))
		 (named? (and (pair? rest) (symbol? (car rest)))))
	    (pp-general expr col extra named? pp-expr-list #f pp-expr)))

	(define (pp-begin expr col extra)
	  (pp-general expr col extra #f #f #f pp-expr))

	(define (pp-do expr col extra)
	  (pp-general expr col extra #f pp-expr-list pp-expr-list pp-expr))

					; define formatting style (change these to suit your style)

	(define indent-general 2)

	(define max-call-head-width 5)

	(define max-expr-width 50)

	(define (style head)
	  (case head
	    ((lambda let* letrec define) pp-lambda)
	    ((if set!)                   pp-if)
	    ((cond)                      pp-cond)
	    ((case)                      pp-case)
	    ((and or)                    pp-and)
	    ((let)                       pp-let)
	    ((begin)                     pp-begin)
	    ((do)                        pp-do)
	    (else                        #f)))

	(pr obj col 0 pp-expr))

      (if width
	  (out (make-string 1 #\newline) (pp obj 0))
	  (wr obj 0)))) )

; (reverse-string-append l) = (apply string-append (reverse l))

(define (reverse-string-append l)

  (define (rev-string-append l i)
    (if (pair? l)
      (let* ((str (car l))
	     (len (string-length str))
	     (result (rev-string-append (cdr l) (+ i len))))
	(let loop ((j 0) (k (- (- (string-length result) i) len)))
	  (if (< j len)
	    (begin
	      (string-set! result k (string-ref str j))
	      (loop (+ j 1) (+ k 1)))
	    result)))
      (make-string i)))

  (rev-string-append l 0))

; (pretty-print obj port) pretty prints 'obj' on 'port'.  The current
; output port is used if 'port' is not specified.

(define pretty-print-width (make-parameter 79))

(define (pretty-print obj . opt)
  (let ((port (if (pair? opt) (car opt) (current-output-port))))
    (generic-write obj #f (pretty-print-width) (lambda (s) (display s port) #t))
    (##core#undefined) ) )

(define pp pretty-print)


;;; Write simple formatted output:

(define fprintf0
  (let ((write write)
	(newline newline)
	(display display) 
	(open-output-string open-output-string)
	(get-output-string get-output-string))
    (lambda (loc port msg args)
      (when port (##sys#check-port port loc))
      (let ((out (if (and port (##sys#tty-port? port))
		     port
		     (open-output-string))))
      (let rec ([msg msg] [args args])
	(##sys#check-string msg loc)
	(let ((index 0)
	      (len (##sys#size msg)) )
	  (define (fetch)
	    (let ((c (##core#inline "C_subchar" msg index)))
	      (set! index (fx+ index 1))
	      c) )
	  (define (next)
	    (if (cond-expand [unsafe #f] [else (##core#inline "C_eqp" args '())])
		(##sys#error loc "too few arguments to formatted output procedure")
		(let ((x (##sys#slot args 0)))
		  (set! args (##sys#slot args 1)) 
		  x) ) )
	  (let loop ()
	    (unless (fx>= index len)
	      (let ((c (fetch)))
		(if (and (eq? c #\~) (fx< index len))
		    (let ((dchar (fetch)))
		      (case (char-upcase dchar)
			((#\S) (write (next) out))
			((#\A) (display (next) out))
			((#\C) (##sys#write-char-0 (next) out))
			((#\B) (display (##sys#number->string (next) 2) out))
			((#\O) (display (##sys#number->string (next) 8) out))
			((#\X) (display (##sys#number->string (next) 16) out))
			((#\!) (##sys#flush-output out))
			((#\?)
			 (let* ([fstr (next)]
				[lst (next)] )
			   (##sys#check-list lst loc)
			   (rec fstr lst) out) )
			((#\~) (##sys#write-char-0 #\~ out))
			((#\% #\N) (newline out))
			(else
			 (if (char-whitespace? dchar)
			     (let skip ((c (fetch)))
			       (if (char-whitespace? c)
				   (skip (fetch))
				   (set! index (fx- index 1)) ) )
			     (##sys#error loc "illegal format-string character" dchar) ) ) ) )
		    (##sys#write-char-0 c out) )
		(loop) ) ) ) ) )
      (cond ((not port) (get-output-string out))
	    ((not (eq? out port))
	     (##sys#print (get-output-string out) #f port) ) ) ) ) ) )

(define (fprintf port fstr . args)
  (fprintf0 'fprintf port fstr args) )

(define (printf fstr . args)
  (fprintf0 'printf ##sys#standard-output fstr args) )

(define (sprintf fstr . args)
  (fprintf0 'sprintf #f fstr args) )

(define format
  (let ([fprintf fprintf]
	[sprintf sprintf]
	[printf printf] )
    (lambda (fmt-or-dst . args)
      (apply (cond [(not fmt-or-dst)		 sprintf]
		   [(boolean? fmt-or-dst)	 printf]
		   [(string? fmt-or-dst)	 (set! args (cons fmt-or-dst args)) sprintf]
		   [(output-port? fmt-or-dst)	 (set! args (cons fmt-or-dst args)) fprintf]
		   [else
		    (##sys#error 'format "illegal destination" fmt-or-dst args)])
	     args) ) ) )

(register-feature! 'srfi-28)

