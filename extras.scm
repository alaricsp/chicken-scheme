;;; extras.scm - Optional non-standard extensions
;
; Copyright (c) 2000-2007, Felix L. Winkelmann
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
;
; Send bugs, suggestions and ideas to: 
;
; felix@call-with-current-continuation.org
;
; Felix L. Winkelmann
; Unter den Gleichen 1
; 37130 Gleichen
; Germany


(declare
 (unit extras)
 (usual-integrations)
 (disable-warning redef)
 (foreign-declare #<<EOF
#define C_hashptr(x)   C_fix(x & C_MOST_POSITIVE_FIXNUM)
#define C_mem_compare(to, from, n)   C_fix(C_memcmp(C_c_string(to), C_c_string(from), C_unfix(n)))
EOF
) )

(cond-expand
 [paranoia]
 [else
  (declare
    (no-bound-checks)
    (no-procedure-checks-for-usual-bindings)
    (bound-to-procedure
     ##sys#check-char ##sys#check-exact ##sys#check-port ##sys#check-string ##sys#substring
     ##sys#for-each ##sys#map ##sys#setslot ##sys#allocate-vector ##sys#check-pair ##sys#not-a-proper-list-error 
     ##sys#member ##sys#assoc ##sys#error ##sys#signal-hook ##sys#read-string!
     ##sys#check-symbol ##sys#check-vector ##sys#floor ##sys#ceiling ##sys#truncate ##sys#round 
     ##sys#check-number ##sys#cons-flonum
     ##sys#flonum-fraction ##sys#make-port ##sys#fetch-and-check-port-arg ##sys#print ##sys#check-structure 
     ##sys#make-structure make-parameter hash-table-set! ##sys#hash-new-len hash-table-ref 
     hash-table-update! floor input-port? make-vector list->vector sort! merge! open-output-string
     get-output-string current-output-port ##sys#flush-output ##sys#write-char-0 newline
     ##sys#number->string display write ##sys#fragments->string list->string make-string string
     pretty-print-width ##sys#symbol->qualified-string ##extras#reverse-string-append ##sys#number?
     ##sys#procedure->string ##sys#pointer->string port? ##sys#user-print-hook char-name 
     read open-input-string ##sys#peek-char-0 ##sys#read-char-0 ##sys#write-char call-with-input-file
     read-line reverse make-string ##sys#string-append random
     ##sys#gcd ##sys#lcm ##sys#fudge ##sys#check-list ##sys#user-read-hook) ) ] )

#{extras
  reverse-string-append generic-write hashtab-default-size hashtab-threshold hashtab-rehash hashtab-primes-table}

(declare
  (hide hashtab-threshold hashtab-rehash generic-write %hash) )

(cond-expand
 [unsafe
  (eval-when (compile)
    (define-macro (##sys#check-structure . _) '(##core#undefined))
    (define-macro (##sys#check-range . _) '(##core#undefined))
    (define-macro (##sys#check-pair . _) '(##core#undefined))
    (define-macro (##sys#check-list . _) '(##core#undefined))
    (define-macro (##sys#check-symbol . _) '(##core#undefined))
    (define-macro (##sys#check-string . _) '(##core#undefined))
    (define-macro (##sys#check-char . _) '(##core#undefined))
    (define-macro (##sys#check-exact . _) '(##core#undefined))
    (define-macro (##sys#check-port . _) '(##core#undefined))
    (define-macro (##sys#check-number . _) '(##core#undefined))
    (define-macro (##sys#check-byte-vector . _) '(##core#undefined)) ) ]
 [else
  (declare (emit-exports "extras.exports")) ] )


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


;;; Combinators:

(define (identity x) x)

(define (project n)
  (lambda args (list-ref args n)) )

(define (conjoin . preds)
  (lambda (x)
    (let loop ([preds preds])
      (or (null? preds)
	  (and ((##sys#slot preds 0) x)
	       (loop (##sys#slot preds 1)) ) ) ) ) )

(define (disjoin . preds)
  (lambda (x)
    (let loop ([preds preds])
      (and (not (null? preds))
	   (or ((##sys#slot preds 0) x)
	       (loop (##sys#slot preds 1)) ) ) ) ) )

(define (constantly . xs)
  (if (eq? 1 (length xs))
      (let ([x (car xs)])
	(lambda _ x) )
      (lambda _ (apply values xs)) ) )

(define (flip proc) (lambda (x y) (proc y x)))

(define complement
  (lambda (p)
    (lambda args (not (apply p args))) ) )

(define (compose . fns)
  (define (rec f0 . fns)
    (if (null? fns)
	f0
	(lambda args
	  (call-with-values
	      (lambda () (apply (apply rec fns) args))
	    f0) ) ) )
  (if (null? fns)
      values
      (apply rec fns) ) )

(define (o . fns)
  (if (null? fns)
      identity
      (let loop ((fns fns))
	(let ((h (##sys#slot fns 0))
	      (t (##sys#slot fns 1)) )
	  (if (null? t)
	      h
	      (lambda (x) (h ((loop t) x))))))))

(define (list-of pred)
  (lambda (lst)
    (let loop ([lst lst])
      (cond [(null? lst) #t]
	    [(not-pair? lst) #f]
	    [(pred (##sys#slot lst 0)) (loop (##sys#slot lst 1))]
	    [else #f] ) ) ) )

(define (noop . _) (void))

(define (each . procs)
  (cond ((null? procs) (lambda _ (void)))
	((null? (##sys#slot procs 1)) (##sys#slot procs 0))
	(else
	 (lambda args
	   (let loop ((procs procs))
	     (let ((h (##sys#slot procs 0))
		   (t (##sys#slot procs 1)) )
	       (if (null? t)
		   (apply h args)
		   (begin
		     (apply h args)
		     (loop t) ) ) ) ) ) ) ) )

(define (any? x) #t)


;;; List operators:

(define (atom? x) (##core#inline "C_i_not_pair_p" x))

(define (tail? x y)
  (##sys#check-list y 'tail?)
  (or (##core#inline "C_eqp" x '())
      (let loop ((y y))
	(cond ((##core#inline "C_eqp" y '()) #f)
	      ((##core#inline "C_eqp" x y) #t)
	      (else (loop (##sys#slot y 1))) ) ) ) )

(define intersperse 
  (lambda (lst x)
    (let loop ((ns lst))
      (if (##core#inline "C_eqp" ns '())
	  ns
	  (let ((tail (cdr ns)))
	    (if (##core#inline "C_eqp" tail '())
		ns
		(cons (##sys#slot ns 0) (cons x (loop tail))) ) ) ) ) ) )

(define (butlast lst)
  (##sys#check-pair lst 'butlast)
  (let loop ((lst lst))
    (let ((next (##sys#slot lst 1)))
      (if (and (##core#inline "C_blockp" next) (##core#inline "C_pairp" next))
	  (cons (##sys#slot lst 0) (loop next))
	  '() ) ) ) )

(define (flatten . lists0)
  (let loop ([lists lists0] [rest '()])
    (cond [(null? lists) rest]
	  [else
	   (let ([head (##sys#slot lists 0)]
		 [tail (##sys#slot lists 1)] )
	     (if (list? head)
		 (loop head (loop tail rest))
		 (cons head (loop tail rest)) ) ) ] ) ) )

(define chop
  (let ([reverse reverse])
    (lambda (lst n)
      (##sys#check-exact n 'chop)
      (cond-expand
       [(not unsafe) (when (fx<= n 0) (##sys#error 'chop "invalid numeric argument" n))]
       [else] )
      (let ([len (length lst)])
	(let loop ([lst lst] [i len])
	  (cond [(null? lst) '()]
		[(fx< i n) (list lst)]
		[else
		 (do ([hd '() (cons (##sys#slot tl 0) hd)]
		      [tl lst (##sys#slot tl 1)] 
		      [c n (fx- c 1)] )
		     ((fx= c 0)
		      (cons (reverse hd) (loop tl (fx- i n))) ) ) ] ) ) ) ) ) )

(define (join lsts . lst)
  (let ([lst (if (pair? lst) (car lst) '())])
    (##sys#check-list lst 'join)
    (let loop ([lsts lsts])
      (cond [(null? lsts) '()]
	    [(cond-expand [unsafe #f] [else (not (pair? lsts))])
	     (##sys#not-a-proper-list-error lsts) ]
	    [else
	     (let ([l (##sys#slot lsts 0)]
		   [r (##sys#slot lsts 1)] )
	       (if (null? r)
		   l
		   (##sys#append l lst (loop r)) ) ) ] ) ) ) )

(define compress
  (lambda (blst lst)
    (let ([msg "bad argument type - not a proper list"])
      (##sys#check-list lst 'compress)
      (let loop ([blst blst] [lst lst])
	(cond [(null? blst) '()]
	      [(cond-expand [unsafe #f] [else (not (pair? blst))])
	       (##sys#signal-hook #:type-error 'compress msg blst) ]
	      [(cond-expand [unsafe #f] [else (not (pair? lst))])
	       (##sys#signal-hook #:type-error 'compress msg lst) ]
	      [(##sys#slot blst 0) (cons (##sys#slot lst 0) (loop (##sys#slot blst 1) (##sys#slot lst 1)))]
	      [else (loop (##sys#slot blst 1) (##sys#slot lst 1))] ) ) ) ) )

(define shuffle
  ;; this should really shadow SORT! and RANDOM...
  (lambda (l)
    (map cdr
	 (sort! (map (lambda (x) (cons (random 10000) x)) l)
		(lambda (x y) (< (car x) (car y)))) ) ) )


;;; Alists:

(define (alist-update! x y lst . cmp)
  (let* ([cmp (if (pair? cmp) (car cmp) eqv?)]
	 [aq (cond [(eq? eq? cmp) assq]
		   [(eq? eqv? cmp) assv]
		   [(eq? equal? cmp) assoc]
		   [else 
		    (lambda (x lst)
		      (let loop ([lst lst])
			(and (pair? lst)
			     (let ([a (##sys#slot lst 0)])
			       (if (and (pair? a) (cmp (##sys#slot a 0) x))
				   a
				   (loop (##sys#slot lst 1)) ) ) ) ) ) ] ) ] 
	 [item (aq x lst)] )
    (if item
	(begin
	  (##sys#setslot item 1 y)
	  lst)
	(cons (cons x y) lst) ) ) )

(define (alist-ref x lst #!optional (cmp eqv?) (default #f))
  (let* ([aq (cond [(eq? eq? cmp) assq]
		   [(eq? eqv? cmp) assv]
		   [(eq? equal? cmp) assoc]
		   [else 
		    (lambda (x lst)
		      (let loop ([lst lst])
			(and (pair? lst)
			     (let ([a (##sys#slot lst 0)])
			       (if (and (pair? a) (cmp (##sys#slot a 0) x))
				   a
				   (loop (##sys#slot lst 1)) ) ) ) ) ) ] ) ] 
	 [item (aq x lst)] )
    (if item
	(##sys#slot item 1)
	default) ) )

(define (rassoc x lst . tst)
  (cond-expand [(not unsafe) (##sys#check-list lst 'rassoc)][else])
  (let ([tst (if (pair? tst) (car tst) eqv?)])
    (let loop ([l lst])
      (and (pair? l)
	   (let ([a (##sys#slot l 0)])
	     (cond-expand [(not unsafe) (##sys#check-pair a 'rassoc)][else])
	     (if (tst x (##sys#slot a 1))
		 a
		 (loop (##sys#slot l 1)) ) ) ) ) ) )


;;; Random numbers:

(define (random n)
  (##sys#check-exact n 'random)
  (if (eq? n 0)
      0
      (##core#inline "C_random_fixnum" n) ) )

(define (randomize . n)
  (##core#inline
   "C_randomize"
   (if (##core#inline "C_eqp" n '())
       (##sys#fudge 2)
       (let ((nn (##sys#slot n 0)))
	 (##sys#check-exact nn 'randomize)
	 nn) ) ) )


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
	(cond ((##sys#slot p 8) => (lambda (rl) (rl p limit)))
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
		     (n (or max 1000000)) )
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

(define (##sys#read-string! n dest port start)
  (when (##sys#slot port 6)		; peeked?
    (##core#inline "C_setsubchar" dest start (##sys#read-char-0 port))
    (set! start (fx+ start 1)) )
  (let ((rdstring (##sys#slot (##sys#slot port 2) 7)))
    (let loop ((start start) (n n) (m 0))
      (let ((n2 (if rdstring
		    (rdstring port n dest start) ; *** doesn't update port-position!
		    (let ((c (##sys#read-char-0 port)))
		      (if (eof-object? c)
			  0
			  (begin
			    (##core#inline "C_setsubchar" dest start c)
			    1) ) ) ) ) )
	(cond ((eq? n2 0) m)
	      ((or (not n) (fx< n2 n)) 
	       (loop (fx+ start n2) (and n (fx- n n2)) (fx+ m n2)) )
	      (else (fx+ n2 m))) ) ) ))

(define (read-string! n dest #!optional (port ##sys#standard-input) (start 0))
  (##sys#check-port port 'read-string!)
  (##sys#check-string dest 'read-string!)
  (when n
    (##sys#check-exact n 'read-string!)
    (when (fx> (fx+ start n) (##sys#size dest))
      (set! n (fx- (##sys#size dest) start))))
  (##sys#check-exact start 'read-string!)
  (##sys#read-string! n dest port start) )

(define read-string
  (let ((open-output-string open-output-string)
	(get-output-string get-output-string) )
    (lambda (#!optional n (p ##sys#standard-input))
      (##sys#check-port p 'read-string)
      (cond (n (##sys#check-exact n 'read-string)
	       (let* ((str (##sys#make-string n))
		      (n2 (##sys#read-string! n str p 0)) )
		 (if (eq? n n2)
		     str
		     (##sys#substring str 0 n2))))
	    (else
	     (let ([str (open-output-string)])
	       (let loop ([n n])
		 (or (and (eq? n 0) (get-output-string str))
		     (let ([c (##sys#read-char-0 p)])
		       (if (eof-object? c)
			   (get-output-string str)
			   (begin
			     (##sys#write-char c str) 
			     (loop (and n (fx- n 1))) ) ) ) ) ) ) ) ) ) ) )

(define read-token
  (let ([open-output-string open-output-string]
	[get-output-string get-output-string] )
    (lambda (pred . port)
      (let ([port (:optional port ##sys#standard-input)])
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
	(##sys#check-port port 'read-string)
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


;;; Redirect standard ports:

(define (with-input-from-port port thunk)
  (##sys#check-port port 'with-input-from-port)
  (fluid-let ([##sys#standard-input port])
    (thunk) ) )

(define (with-output-to-port port thunk)
  (##sys#check-port port 'with-output-from-port)
  (fluid-let ([##sys#standard-output port])
    (thunk) ) )

(define (with-error-output-to-port port thunk)
  (##sys#check-port port 'with-error-output-from-port)
  (fluid-let ([##sys#standard-error port])
    (thunk) ) )


;;; Extended string-port operations:
  
(define call-with-input-string 
  (let ([open-input-string open-input-string])
    (lambda (str proc)
      (let ((in (open-input-string str)))
	(proc in) ) ) ) )

(define call-with-output-string
  (let ((open-output-string open-output-string)
	(get-output-string get-output-string) )
    (lambda (proc)
      (let ((out (open-output-string)))
	(proc out)
	(get-output-string out) ) ) ) )

(define with-input-from-string
  (let ((open-input-string open-input-string))
    (lambda (str thunk)
      (fluid-let ([##sys#standard-input (open-input-string str)])
	(thunk) ) ) ) )

(define with-output-to-string
  (let ([open-output-string open-output-string]
	[get-output-string get-output-string] )
    (lambda (thunk)
      (fluid-let ([##sys#standard-output (open-output-string)])
	(thunk) 
	(get-output-string ##sys#standard-output) ) ) ) )


;;; Custom ports:
;
; - Port-slots:
;
;   10: last

(define make-input-port
  (lambda (read ready? close #!optional peek read-string read-line)
    (let* ((class
	    (vector 
	     (lambda (p)		; read-char
	       (let ([last (##sys#slot p 10)])
		 (cond [peek (read)]
		       [last
			(##sys#setislot p 10 #f)
			last]
		       [else (read)] ) ) )
	     (lambda (p)		; peek-char
	       (let ([last (##sys#slot p 10)])
		 (cond [peek (peek)]
		       [last last]
		       [else
			(let ([last (read)])
			  (##sys#setslot p 10 last)
			  last) ] ) ) )
	     #f				; write-char
	     #f				; write-string
	     (lambda (p)		; close
	       (close)
	       (##sys#setislot p 8 #t) )
	     #f				; flush-output
	     (lambda (p)		; char-ready?
	       (ready?) )
	     read-string 		; read-string
	     read-line) )		; read-line
	   (data (vector #f))
	   (port (##sys#make-port #t class "(custom)" 'custom)) )
      (##sys#setslot port 9 data) 
      port) ) )

(define make-output-port
  (let ([string string])
    (lambda (write close . flush)
      (let* ((flush (and (pair? flush) (car flush)))
	     (class
	      (vector
	       #f			; read-char
	       #f			; peek-char
	       (lambda (p c)		; write-char
		 (write (string c)) )
	       (lambda (p s)		; write-string
		 (write s) )
	       (lambda (p)		; close
		 (close)
		 (##sys#setislot p 8 #t) )
	       (lambda (p)		; flush-output
		 (when flush (flush)) )
	       #f			; char-ready?
	       #f			; read-string
	       #f) )			; read-line
	     (data (vector #f))
	     (port (##sys#make-port #f class "(custom)" 'custom)) )
	(##sys#setslot port 9 data) 
	port) ) ) )


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
	      ((eof-object? obj)  (out "#<eof>" col))
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
	      ((##sys#generic-structure? obj)
	       (let ([o (open-output-string)])
		 (##sys#user-print-hook obj #t o)
		 (out (get-output-string o) col) ) )
	      ((port? obj) (out (string-append "#<port " (##sys#slot obj 3) ">") col))
	      ((##core#inline "C_lambdainfop" obj)
	       (out "#<lambda info " col)
	       (out (##sys#lambda-info->string obj) col)
	       (out "#>" col) )
	      ((eq? obj (##sys#slot '##sys#arbitrary-unbound-symbol 0))
	       (out "#<unbound value>" col) )
	      (else               (out "#<unprintable object>" col)) ) )

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


;;; Anything->string conversion:

(define ->string 
  (let ([open-output-string open-output-string]
	[display display]
	[string string]
	[get-output-string get-output-string] )
    (lambda (x)
      (cond [(string? x) x]
	    [(symbol? x) (symbol->string x)]
	    [(char? x) (string x)]
	    [(number? x) (##sys#number->string x)]
	    [else 
	     (let ([o (open-output-string)])
	       (display x o)
	       (get-output-string o) ) ] ) ) ) )

(define conc
  (let ([string-append string-append])
    (lambda args
      (apply string-append (map ->string args)) ) ) )


;;; Search one string inside another:

(let ()
  (define (traverse which where start test loc)
    (##sys#check-string which loc)
    (##sys#check-string where loc)
    (let ([wherelen (##sys#size where)]
	  [whichlen (##sys#size which)] )
      (##sys#check-exact start loc)
      (let loop ([istart start] [iend whichlen])
	(cond [(fx> iend wherelen) #f]
	      [(test istart whichlen) istart]
	      [else 
	       (loop (fx+ istart 1)
		     (fx+ iend 1) ) ] ) ) ) )
  (set! substring-index 
    (lambda (which where . start)
      (traverse 
       which where (if (pair? start) (car start) 0) 
       (lambda (i l) (##core#inline "C_substring_compare" which where 0 i l))
       'substring-index) ) )
  (set! substring-index-ci 
    (lambda (which where . start)
      (traverse
       which where (if (pair? start) (car start) 0) 
       (lambda (i l) (##core#inline "C_substring_compare_case_insensitive" which where 0 i l)) 
       'substring-index-ci) ) ) )


;;; 3-Way string comparison:

(define (string-compare3 s1 s2)
  (##sys#check-string s1 'string-compare3)
  (##sys#check-string s2 'string-compare3)
  (let ((len1 (##sys#size s1))
        (len2 (##sys#size s2)) )
    (let* ((len-diff (fx- len1 len2)) 
	   (cmp (##core#inline "C_mem_compare" s1 s2 (if (fx< len-diff 0) len1 len2))))
      (if (fx= cmp 0) 
	  len-diff 
	  cmp))))

(define (string-compare3-ci s1 s2)
  (##sys#check-string s1 'string-compare3-ci)
  (##sys#check-string s2 'string-compare3-ci)
  (let ((len1 (##sys#size s1))
        (len2 (##sys#size s2)) )
    (let* ((len-diff (fx- len1 len2)) 
	   (cmp (##core#inline "C_string_compare_case_insensitive" s1 s2 (if (fx< len-diff 0) len1 len2))))
      (if (fx= cmp 0) 
	  len-diff 
	  cmp))))


;;; Substring comparison:

(define (substring=? s1 s2 . start)
  (##sys#check-string s1 'substring=?)
  (##sys#check-string s2 'substring=?)
  (let* ([n (length start)]
	 [start1 (if (fx>= n 1) (car start) 0)]
	 [start2 (if (fx>= n 2) (cadr start) 0)] 
	 [len (if (fx> n 2) 
		  (caddr start) 
		  (fxmin (fx- (##sys#size s1) start1)
			 (fx- (##sys#size s2) start2) ) ) ] )
    (##sys#check-exact start1 'substring=?)
    (##sys#check-exact start2 'substring=?)
    (##core#inline "C_substring_compare" s1 s2 start1 start2 len) ) )

(define (substring-ci=? s1 s2 . start)
  (##sys#check-string s1 'substring-ci=?)
  (##sys#check-string s2 'substring-ci=?)
  (let* ([n (length start)]
	 [start1 (if (fx>= n 1) (car start) 0)]
	 [start2 (if (fx>= n 2) (cadr start) 0)] 
	 [len (if (fx> n 2) 
		  (caddr start) 
		  (fxmin (fx- (##sys#size s1) start1)
			 (fx- (##sys#size s2) start2) ) ) ] )
    (##sys#check-exact start1 'substring-ci=?)
    (##sys#check-exact start2 'substring-ci=?)
    (##core#inline "C_substring_compare_case_insensitive"
		   s1 s2 start1 start2 len) ) )


;;; Split string into substrings:

(define string-split
  (lambda (str . delstr-and-flag)
    (##sys#check-string str 'string-split)
    (let* ([del (if (null? delstr-and-flag) "\t\n " (car delstr-and-flag))]
	   [flag (if (fx= (length delstr-and-flag) 2) (cadr delstr-and-flag) #f)]
	   [strlen (##sys#size str)] )
      (##sys#check-string del 'string-split)
      (let ([dellen (##sys#size del)] 
	    [first #f] )
	(define (add from to last)
	  (let ([node (cons (##sys#substring str from to) '())])
	    (if first
		(##sys#setslot last 1 node)
		(set! first node) ) 
	    node) )
	(let loop ([i 0] [last #f] [from 0])
	  (cond [(fx>= i strlen)
		 (when (or (fx> i from) flag) (add from i last))
		 (or first '()) ]
		[else
		 (let ([c (##core#inline "C_subchar" str i)])
		   (let scan ([j 0])
		     (cond [(fx>= j dellen) (loop (fx+ i 1) last from)]
			   [(eq? c (##core#inline "C_subchar" del j))
			    (let ([i2 (fx+ i 1)])
			      (if (or (fx> i from) flag)
				  (loop i2 (add from i last) i2)
				  (loop i2 last i2) ) ) ]
			   [else (scan (fx+ j 1))] ) ) ) ] ) ) ) ) ) )


;;; Concatenate list of strings:

(define (string-intersperse strs #!optional (ds " "))
  (##sys#check-list strs 'string-intersperse)
  (##sys#check-string ds 'string-intersperse)
  (let ((dslen (##sys#size ds)))
    (let loop1 ((ss strs) (n 0))
      (cond ((##core#inline "C_eqp" ss '())
	     (if (##core#inline "C_eqp" strs '())
		 ""
		 (let ((str2 (##sys#allocate-vector (fx- n dslen) #t #\space #f)))
		   (let loop2 ((ss2 strs) (n2 0))
		     (let* ((stri (##sys#slot ss2 0))
			    (next (##sys#slot ss2 1)) 
			    (strilen (##sys#size stri)) )
		       (##core#inline "C_substring_copy" stri str2 0 strilen n2)
		       (let ((n3 (fx+ n2 strilen)))
			 (if (##core#inline "C_eqp" next '())
			     str2
			     (begin
			       (##core#inline "C_substring_copy" ds str2 0 dslen n3)
			       (loop2 next (fx+ n3 dslen)) ) ) ) ) ) ) ) )
	    ((and (##core#inline "C_blockp" ss) (##core#inline "C_pairp" ss))
	     (let ((stri (##sys#slot ss 0)))
	       (##sys#check-string stri 'string-intersperse)
	       (loop1 (##sys#slot ss 1)
		      (fx+ (##sys#size stri) (fx+ dslen n)) ) ) )
	    (else (##sys#not-a-proper-list-error strs)) ) ) ) )


;;; Translate elements of a string:

(define string-translate 
  (let ([make-string make-string]
	[list->string list->string] )
    (lambda (str from . to)

      (define (instring s)
	(let ([len (##sys#size s)])
	  (lambda (c)
	    (let loop ([i 0])
	      (cond [(fx>= i len) #f]
		    [(eq? c (##core#inline "C_subchar" s i)) i]
		    [else (loop (fx+ i 1))] ) ) ) ) )

      (let* ([from
	      (cond [(char? from) (lambda (c) (eq? c from))]
		    [(pair? from) (instring (list->string from))]
		    [else
		     (##sys#check-string from 'string-translate)
		     (instring from) ] ) ]
	     [to
	      (and (pair? to)
		   (let ([tx (##sys#slot to 0)])
		     (cond [(char? tx) tx]
			   [(pair? tx) (list->string tx)]
			   [else
			    (##sys#check-string tx 'string-translate)
			    tx] ) ) ) ] 
	     [tlen (and (string? to) (##sys#size to))] )
	(##sys#check-string str 'string-translate)
	(let* ([slen (##sys#size str)]
	       [str2 (make-string slen)] )
	  (let loop ([i 0] [j 0])
	    (if (fx>= i slen)
		(if (fx< j i)
		    (##sys#substring str2 0 j)
		    str2)
		(let* ([ci (##core#inline "C_subchar" str i)]
		       [found (from ci)] )
		  (cond [(not found)
			 (##core#inline "C_setsubchar" str2 j ci)
			 (loop (fx+ i 1) (fx+ j 1)) ]
			[(not to) (loop (fx+ i 1) j)]
			[(char? to)
			 (##core#inline "C_setsubchar" str2 j to)
			 (loop (fx+ i 1) (fx+ j 1)) ]
			[(cond-expand [unsafe #f] [else (fx>= found tlen)])
			 (##sys#error 'string-translate "invalid translation destination" i to) ]
			[else 
			 (##core#inline "C_setsubchar" str2 j (##core#inline "C_subchar" to found))
			 (loop (fx+ i 1) (fx+ j 1)) ] ) ) ) ) ) ) ) ) )

(define (string-translate* str smap)
  (##sys#check-string str 'string-translate*)
  (##sys#check-list smap 'string-translate*)
  (let ([len (##sys#size str)])
    (define (collect i from total fs)
      (if (fx>= i len)
	  (##sys#fragments->string
	   total
	   (reverse 
	    (if (fx> i from) 
		(cons (##sys#substring str from i) fs)
		fs) ) )
	  (let loop ([smap smap])
	    (if (null? smap) 
		(collect (fx+ i 1) from (fx+ total 1) fs)
		(let* ([p (car smap)]
		       [sm (car p)]
		       [smlen (string-length sm)]
		       [st (cdr p)] )
		  (if (##core#inline "C_substring_compare" str sm i 0 smlen)
		      (let ([i2 (fx+ i smlen)])
			(when (fx> i from)
			  (set! fs (cons (##sys#substring str from i) fs)) )
			(collect 
			 i2 i2
			 (fx+ total (string-length st))
			 (cons st fs) ) ) 
		      (loop (cdr smap)) ) ) ) ) ) )
    (collect 0 0 0 '()) ) )


;;; Chop string into substrings:

(define (string-chop str len)
  (##sys#check-string str 'string-chop)
  (##sys#check-exact len 'string-chop)
  (let ([total (##sys#size str)])
    (let loop ([total total] [pos 0])
      (cond [(fx<= total 0) '()]
	    [(fx<= total len) (list (##sys#substring str pos (fx+ pos total)))]
	    [else (cons (##sys#substring str pos (fx+ pos len)) (loop (fx- total len) (fx+ pos len)))] ) ) ) )
	   

;;; Remove suffix

(define (string-chomp str #!optional (suffix "\n"))
  (##sys#check-string str 'string-chomp)
  (##sys#check-string suffix 'string-chomp)
  (let* ((len (##sys#size str))
	 (slen (##sys#size suffix)) 
	 (diff (fx- len slen)) )
    (if (and (fx>= len slen)
	     (##core#inline "C_substring_compare" str suffix diff 0 slen) )
	(##sys#substring str 0 diff)
	str) ) )


;;; Write simple formatted output:

(define fprintf
  (let ([write write]
	[newline newline]
	[display display] )
    (lambda (port msg . args)
      (let rec ([msg msg] [args args])
	(##sys#check-string msg 'fprintf)
	(##sys#check-port port 'fprintf)
	(let ((index 0)
	      (len (##sys#size msg)) )
	  (define (fetch)
	    (let ((c (##core#inline "C_subchar" msg index)))
	      (set! index (fx+ index 1))
	      c) )
	  (define (next)
	    (if (cond-expand [unsafe #f] [else (##core#inline "C_eqp" args '())])
		(##sys#error 'fprintf "too few arguments to formatted output procedure")
		(let ((x (##sys#slot args 0)))
		  (set! args (##sys#slot args 1)) 
		  x) ) )
	  (let loop ()
	    (unless (fx>= index len)
	      (let ((c (fetch)))
		(if (and (eq? c #\~) (fx< index len))
		    (let ((dchar (fetch)))
		      (case (char-upcase dchar)
			((#\S) (write (next) port))
			((#\A) (display (next) port))
			((#\C) (##sys#write-char-0 (next) port))
			((#\B) (display (##sys#number->string (next) 2) port))
			((#\O) (display (##sys#number->string (next) 8) port))
			((#\X) (display (##sys#number->string (next) 16) port))
			((#\!) (##sys#flush-output port))
			((#\?)
			 (let* ([fstr (next)]
				[lst (next)] )
			   (##sys#check-list lst 'fprintf)
			   (rec fstr lst) ) )
			((#\~) (##sys#write-char-0 #\~ port))
			((#\%) (newline port))
			((#\% #\N) (newline port))
			(else
			 (if (char-whitespace? dchar)
			     (let skip ((c (fetch)))
			       (if (char-whitespace? c)
				   (skip (fetch))
				   (set! index (fx- index 1)) ) )
			     (##sys#error 'fprintf "illegal format-string character" dchar) ) ) ) )
		    (##sys#write-char-0 c port) )
		(loop) ) ) ) ) ) ) ) )


(define printf
  (let ((fprintf fprintf)
	(current-output-port current-output-port) )
    (lambda (msg . args)
      (apply fprintf (current-output-port) msg args) ) ) )


(define sprintf
  (let ((open-output-string open-output-string)
	(get-output-string get-output-string)
	(fprintf fprintf) )
    (lambda (fstr . args)
      (let ((out (open-output-string)))
	(apply fprintf out fstr args)
	(get-output-string out) ) ) ) )

(define format sprintf)

(register-feature! 'srfi-28)


;;; Defines: sorted?, merge, merge!, sort, sort!
;;; Author : Richard A. O'Keefe (based on Prolog code by D.H.D.Warren)
;;;
;;; This code is in the public domain.

;;; Updated: 11 June 1991
;;; Modified for scheme library: Aubrey Jaffer 19 Sept. 1991
;;; Updated: 19 June 1995

;;; (sorted? sequence less?)
;;; is true when sequence is a list (x0 x1 ... xm) or a vector #(x0 ... xm)
;;; such that for all 1 <= i <= m,
;;;	(not (less? (list-ref list i) (list-ref list (- i 1)))).

; Modified by flw for use with CHICKEN:
;


(define (sorted? seq less?)
    (cond
	((null? seq)
	    #t)
	((vector? seq)
	    (let ((n (vector-length seq)))
		(if (<= n 1)
		    #t
		    (do ((i 1 (+ i 1)))
			((or (= i n)
			     (less? (vector-ref seq i)
				    (vector-ref seq (- i 1))))
			    (= i n)) )) ))
	(else
	    (let loop ((last (car seq)) (next (cdr seq)))
		(or (null? next)
		    (and (not (less? (car next) last))
			 (loop (car next) (cdr next)) )) )) ))


;;; (merge a b less?)
;;; takes two lists a and b such that (sorted? a less?) and (sorted? b less?)
;;; and returns a new list in which the elements of a and b have been stably
;;; interleaved so that (sorted? (merge a b less?) less?).
;;; Note:  this does _not_ accept vectors.  See below.

(define (merge a b less?)
    (cond
	((null? a) b)
	((null? b) a)
	(else (let loop ((x (car a)) (a (cdr a)) (y (car b)) (b (cdr b)))
	    ;; The loop handles the merging of non-empty lists.  It has
	    ;; been written this way to save testing and car/cdring.
	    (if (less? y x)
		(if (null? b)
		    (cons y (cons x a))
		    (cons y (loop x a (car b) (cdr b)) ))
		;; x <= y
		(if (null? a)
		    (cons x (cons y b))
		    (cons x (loop (car a) (cdr a) y b)) )) )) ))


;;; (merge! a b less?)
;;; takes two sorted lists a and b and smashes their cdr fields to form a
;;; single sorted list including the elements of both.
;;; Note:  this does _not_ accept vectors.

(define (merge! a b less?)
    (define (loop r a b)
	(if (less? (car b) (car a))
	    (begin
		(set-cdr! r b)
		(if (null? (cdr b))
		    (set-cdr! b a)
		    (loop b a (cdr b)) ))
	    ;; (car a) <= (car b)
	    (begin
		(set-cdr! r a)
		(if (null? (cdr a))
		    (set-cdr! a b)
		    (loop a (cdr a) b)) )) )
    (cond
	((null? a) b)
	((null? b) a)
	((less? (car b) (car a))
	    (if (null? (cdr b))
		(set-cdr! b a)
		(loop b a (cdr b)))
	    b)
	(else ; (car a) <= (car b)
	    (if (null? (cdr a))
		(set-cdr! a b)
		(loop a (cdr a) b))
	    a)))


;;; (sort! sequence less?)
;;; sorts the list or vector sequence destructively.  It uses a version
;;; of merge-sort invented, to the best of my knowledge, by David H. D.
;;; Warren, and first used in the DEC-10 Prolog system.  R. A. O'Keefe
;;; adapted it to work destructively in Scheme.

(define (sort! seq less?)
    (define (step n)
	(cond
	    ((> n 2)
		(let* ((j (quotient n 2))
		       (a (step j))
		       (k (- n j))
		       (b (step k)))
		    (merge! a b less?)))
	    ((= n 2)
		(let ((x (car seq))
		      (y (cadr seq))
		      (p seq))
		    (set! seq (cddr seq))
		    (if (less? y x) (begin
			(set-car! p y)
			(set-car! (cdr p) x)))
		    (set-cdr! (cdr p) '())
		    p))
	    ((= n 1)
		(let ((p seq))
		    (set! seq (cdr seq))
		    (set-cdr! p '())
		    p))
	    (else
		'()) ))
    (if (vector? seq)
	(let ((n (vector-length seq))
	      (vec seq))
	  (set! seq (vector->list seq))
	  (do ((p (step n) (cdr p))
	       (i 0 (+ i 1)))
	      ((null? p) vec)
	    (vector-set! vec i (car p)) ))
	;; otherwise, assume it is a list
	(step (length seq)) ))

;;; (sort sequence less?)
;;; sorts a vector or list non-destructively.  It does this by sorting a
;;; copy of the sequence.  My understanding is that the Standard says
;;; that the result of append is always "newly allocated" except for
;;; sharing structure with "the last argument", so (append x '()) ought
;;; to be a standard way of copying a list x.

(define (sort seq less?)
    (if (vector? seq)
	(list->vector (sort! (vector->list seq) less?))
	(sort! (append seq '()) less?)))


;;; Binary search:

(define binary-search
  (let ([list->vector list->vector])
    (lambda (vec proc)
      (if (pair? vec)
	  (set! vec (list->vector vec))
	  (##sys#check-vector vec 'binary-search) )
      (let ([len (##sys#size vec)])
	(and (fx> len 0)
	     (let loop ([ps 0]
			[pe len] )
	       (let ([p (fx+ ps (##core#inline "C_fixnum_divide" (fx- pe ps) 2))])
		 (let* ([x (##sys#slot vec p)]
			[r (proc x)] )
		   (cond [(fx= r 0) p]
			 [(fx< r 0) (and (not (fx= pe p)) (loop ps p))]
			 [else (and (not (fx= ps p)) (loop p pe))] ) ) ) ) ) ) ) ) )


;;; Hashtables:

;;; Utility definitions:

(define-constant hashtab-default-size 307)
(define-constant hashtab-threshold 0.5)

; Predefined sizes for the hash tables:
;
; Start in 307; each element is the smallest prime that is at least twice as
; bigger as the previous element in the list.  The last number is an
; exception: it is the largest fixnum we can repressent.

(define-constant hashtab-primes-table
  '(307 617 1237 2477 4957 9923 19853 39709 79423 158849 317701 635413 1270849
	2541701 5083423 10166857 20333759 40667527 81335063 162670129 325340273 
	650680571 1073741823))

(define-constant hashtab-max-size 1073741823)

(define (hash-table? x) (##sys#structure? x 'hash-table))


;;; Creation and erasure:

(define make-hash-table
  (let ([make-vector make-vector])
    (lambda test-and-size
      (let-optionals test-and-size ([test equal?] [hashf %hash] [len hashtab-default-size])
	(##sys#check-exact len 'make-hash-table)
	(##sys#make-structure 'hash-table (make-vector len '()) 0 test hashf) ) ) ) )

(define hash-table-copy 
  (let ((make-vector make-vector))
    (lambda (ht)
      (##sys#check-structure ht 'hash-table 'hash-table-copy)
      (let* ((vec1 (##sys#slot ht 1))
	     (len (##sys#size vec1))
	     (vec2 (make-vector len '())) )
	(do ((i 0 (fx+ i 1)))
	    ((fx>= i len)
	     (##sys#make-structure
	      'hash-table vec2 (##sys#slot ht 2) (##sys#slot ht 3) (##sys#slot ht 4)) )
	  (##sys#setslot 
	   vec2 i
	   (let copy ((lst (##sys#slot vec1 i)))
	     (if (null? lst)
		 '()
		 (let ((b (##sys#slot lst 0)))
		   (cons (cons (##sys#slot b 0) (##sys#slot b 1))
			 (copy (##sys#slot lst 1))))))))))) )


;;; Hash-table property-accessors

(define (hash-table-equivalence-function ht)
  (##sys#check-structure ht 'hash-table 'hash-table-equivalence-function) 
  (##sys#slot ht 3) )

(define (hash-table-hash-function ht)
  (##sys#check-structure ht 'hash-table 'hash-table-hash-function) 
  (##sys#slot ht 4) )


;;; Generation of hash-values:

(define-constant hash-depth-limit 4)
(define-constant default-hash-bound 536870912)

(define %hash
  (lambda (x limit)
    (define (hash-with-test x d)
      (if (or (not (##core#inline "C_blockp" x)) (##core#inline "C_byteblockp" x) (symbol? x))
	  (rechash x (fx+ d 1))
	  99) )
    (define (rechash x d)
      (cond ((fx>= d hash-depth-limit) 0)
            ((##core#inline "C_fixnump" x) x)
	    ((##core#inline "C_charp" x) (char->integer x))
	    ((eq? x #t) 256)
	    ((eq? x #f) 257)
	    ((eq? x '()) 258)
	    ((##core#inline "C_eofp" x) 259)
	    ((not (##core#inline "C_blockp" x)) 262)
	    ((##core#inline "C_symbolp" x) (##core#inline "C_hash_string" (##sys#slot x 1)))
	    ((list? x) (fx+ (length x) (hash-with-test (##sys#slot x 0) d)))
	    ((pair? x) 
	     (fx+ (arithmetic-shift (hash-with-test (##sys#slot x 0) d) 16)
		  (hash-with-test (##sys#slot x 1) d) ) )
	    ((##core#inline "C_portp" x) (if (input-port? x) 260 261))
	    ((##core#inline "C_byteblockp" x) (##core#inline "C_hash_string" x))
	    (else
	     (let ([len (##sys#size x)]
		   [start (if (##core#inline "C_specialp" x) 1 0)] )
	       (let loop ([k (fx+ len (if (##core#inline "C_specialp" x) (##core#inline "C_peek_fixnum" x 0) 0))]
			  [i start]
			  [len (fx- (if (fx> len 4) 4 len) start)] )
		 (if (fx= len 0)
		     k
		     (loop (fx+ k (fx+ (fx* k 16) (##core#inline "C_fix" (rechash (##sys#slot x i) (fx+ d 1)))))
			   (fx+ i 1)
			   (fx- len 1) ) ) ) ) ) ) )
    (##sys#check-exact limit 'hash)
    (##core#inline "C_fixnum_modulo" (fxand (foreign-value "C_MOST_POSITIVE_FIXNUM" int) (rechash x 0)) limit) ) )

(define (hash x #!optional (bound default-hash-bound))
  (##sys#check-exact bound 'hash)
  (%hash x bound) )

(define hash-by-identity hash)

(define (string-hash s #!optional (bound default-hash-bound))
  (##core#inline 
   "C_fixnum_modulo"
   (##core#inline "C_hash_string" s)
   bound) )

(define (string-ci-hash s #!optional (bound default-hash-bound))
  (##core#inline 
   "C_fixnum_modulo"
   (##core#inline "C_hash_string_ci" s)
   bound) )


;;; Access:

(define (hash-table-size ht)
  (##sys#check-structure ht 'hash-table 'hash-table-size)
  (##sys#slot ht 2) )

(define hash-table-update! 
  ;; This one was suggested by Sven Hartrumpf (and subsequently added in SRFI-69)
  (let ([eq0 eq?]
	[floor floor] )
    (lambda (ht key proc #!optional
	   (init (lambda () (##sys#signal-hook #:access-error 'hash-table-update!
			    "hash-table does not contain key" key ht))))
      (##sys#check-structure ht 'hash-table 'hash-table-update!)
      (let restart ()
	(let* ((vec (##sys#slot ht 1))
	       (hashf (##sys#slot ht 4))
	       (len (##sys#size vec))
	       (test (##sys#slot ht 3))
	       (k (hashf key len))
	       (c (fx+ (##sys#slot ht 2) 1)) )
	  (if (and (fx>= c (inexact->exact (floor (* len hashtab-threshold))))
		   (fx< len hashtab-max-size))
	      (let ((vec2 (make-vector
                            (##sys#hash-new-len hashtab-primes-table
						(fxmin hashtab-max-size (fx* len 2)))
                            '())))
		(hashtab-rehash vec vec2 hashf)
		(##sys#setslot ht 1 vec2)
		(restart) ) 
	      (let ((bucket0 (##sys#slot vec k)))
		(if (eq? eq0 test)
		    ;; Fast path (eq? test):
		    (let loop ((bucket bucket0))
		      (cond ((eq? bucket '())
			     (let ((val (proc (init))))
			       (##sys#setslot vec k (cons (cons key val) bucket0))
			       (##sys#setslot ht 2 c) 
			       val) )
			    (else
			     (let ((b (##sys#slot bucket 0)))
			       (if (eq? key (##sys#slot b 0))
				   (let ((val (proc (##sys#slot b 1))))
				     (##sys#setslot b 1 val)
				     val)
				   (loop (##sys#slot bucket 1)) ) ) ) ) )
		    (let loop ((bucket bucket0))
		      (cond ((eq? bucket '())
			     (let ((val (proc (init))))
			       (##sys#setslot vec k (cons (cons key val) bucket0))
			       (##sys#setslot ht 2 c) 
			       val) )
			    (else
			     (let ((b (##sys#slot bucket 0)))
			       (if (test key (##sys#slot b 0))
				   (let ((val (proc (##sys#slot b 1))))
				     (##sys#setslot b 1 val)
				     val) 
				   (loop (##sys#slot bucket 1)) ) ) ) ) ) ) ) ) ) ) ) ) )

(define hash-table-update!/default 
  (let ((hash-table-update! hash-table-update!))
    (lambda (ht key func def)
      (hash-table-update! ht key func (lambda () def)) ) ) )

(define hash-table-set! 
  (let ([hash-table-update! hash-table-update!])
    (lambda (ht key val)
      (hash-table-update! ht key (lambda (x) val) (lambda () val)) ) ) )

(define hash-table-ref
  (let ([eq0 eq?])
    (getter-with-setter
     (lambda (ht key . default)
       (##sys#check-structure ht 'hash-table 'hash-table-ref)
       (let* ([vec (##sys#slot ht 1)]
	      [hashf (##sys#slot ht 4)]
	      [k (hashf key (##sys#size vec))] 
	      [def (if (pair? default) 
		       (car default)
		       (cut ##sys#signal-hook #:access-error 'hash-table-ref "hash-table does not contain key" key ht) ) ]
	      [test (##sys#slot ht 3)] )
	 (if (eq? eq0 test)
	     ;; Fast path (eq? test):
	     (let loop ((bucket (##sys#slot vec k)))
	       (if (eq? bucket '())
		   (def)
		   (let ((b (##sys#slot bucket 0)))
		     (if (eq? key (##sys#slot b 0))
			 (##sys#slot b 1)
			 (loop (##sys#slot bucket 1)) ) ) ) )
	     (let loop ((bucket (##sys#slot vec k)))
	       (if (eq? bucket '())
		   (def)
		   (let ((b (##sys#slot bucket 0)))
		     (if (test key (##sys#slot b 0))
			 (##sys#slot b 1)
			 (loop (##sys#slot bucket 1)) ) ) ) ) ) ) )
     hash-table-set!) ) )

(define hash-table-ref/default
  (let ((hash-table-ref hash-table-ref))
    (lambda (ht key def)
      (hash-table-ref ht key (lambda () def)) ) ) )

(define hash-table-exists?
  (let ((unique (vector 42))
	(ref hash-table-ref/default) )
    (lambda (ht key)
      (##sys#check-structure ht 'hash-table 'hash-table-exists?)
      (not (eq? unique (ref ht key unique))) ) ) )

(define (##sys#hash-new-len tab req)
  (if (or (fx>= (##sys#slot tab 0) req)
	  (eq? (##sys#slot tab 1) '()))
      (##sys#slot tab 0)
      (##sys#hash-new-len (##sys#slot tab 1) req)))

(define hash-table-delete!
  (let ([eq0 eq?]
	[floor floor])
    (lambda (ht key)
      (##sys#check-structure ht 'hash-table 'hash-table-delete!)
      (let* ((vec (##sys#slot ht 1))
	     (hashf (##sys#slot ht 4))
	     (len (##sys#size vec))
	     (test (##sys#slot ht 3))
	     (k (hashf key len))
	     (c (fx- (##sys#slot ht 2) 1)))
	(let ((bucket0 (##sys#slot vec k)))
	  (if (eq? eq0 test)
	      ;; Fast path (eq? test):
	      (let loop ((prev '())
			 (bucket bucket0))
		(if (null? bucket)
		    #f
		    (let ((b (##sys#slot bucket 0)))
		      (if (eq? key (##sys#slot b 0))
			  (begin
			    (if (null? prev)
				(##sys#setslot vec k (##sys#slot bucket 1))
				(##sys#setslot prev 1 (##sys#slot bucket 1)))
			    (##sys#setslot ht 2 c)
			    #t)
			  (loop bucket (##sys#slot bucket 1))))))
	      (let loop ((prev '())
			 (bucket bucket0))
		(if (null? bucket)
		    #f
		    (let ((b (##sys#slot bucket 0)))
		      (if (test key (##sys#slot b 0))
			  (begin
			    (if (null? prev)
				(##sys#setslot vec k (##sys#slot bucket 1))
				(##sys#setslot prev 1 (##sys#slot bucket 1)))
			    (##sys#setslot ht 2 c)
			    #t)
			  (loop bucket (##sys#slot bucket 1))))))))))))

(define hashtab-rehash
  (lambda (vec1 vec2 hashf)
    (let ([len1 (##sys#size vec1)]
	  [len2 (##sys#size vec2)] )
      (do ([i 0 (fx+ i 1)])
	  ((fx>= i len1))
	(let loop ([bucket (##sys#slot vec1 i)])
	  (unless (null? bucket)
	    (let* ([b (##sys#slot bucket 0)]
		   [x (##sys#slot b 0)] 
		   [k (hashf x len2)] )
	      (##sys#setslot vec2 k (cons (cons x (##sys#slot b 1)) (##sys#slot vec2 k)))
	      (loop (##sys#slot bucket 1)) ) ) ) ) ) ) )

(define hash-table-merge!
  (let ((hash-table-set! hash-table-set!))
    (lambda (ht1 ht2)
      (##sys#check-structure ht1 'hash-table 'hash-table-merge!)
      (##sys#check-structure ht2 'hash-table 'hash-table-merge!)
      (let* ((vec (##sys#slot ht2 1))
	     (len (##sys#size vec)) )
	(do ((i 0 (fx+ i 1)))
	    ((fx>= i len) ht1)
	  (do ((lst (##sys#slot vec i) (##sys#slot lst 1)))
	      ((null? lst))
	    (let ((b (##sys#slot lst 0)))
	      (hash-table-set! ht1 (##sys#slot b 0) (##sys#slot b 1)) ) ) ) ) ) ) )


;;; Conversion to and from lists:

(define (hash-table->alist ht)
  (##sys#check-structure ht 'hash-table 'hash-table->alist)
  (let* ([vec (##sys#slot ht 1)]
	 [len (##sys#size vec)] )
    (let loop ([i 0] [lst '()])
      (if (fx>= i len)
	  lst
	  (let loop2 ([bucket (##sys#slot vec i)] [lst lst])
	    (if (null? bucket)
		(loop (fx+ i 1) lst)
		(loop2 (##sys#slot bucket 1)
		       (let ([x (##sys#slot bucket 0)])
			 (cons (cons (##sys#slot x 0) (##sys#slot x 1)) lst) ) ) ) ) ) ) ) )

(define alist->hash-table
  (let ((make-hash-table make-hash-table)
	(hash-table-set! hash-table-set!) )
    (lambda (alist . rest)
      (let ((hash (apply make-hash-table rest)))
	(for-each (lambda (x) (hash-table-set! hash (car x) (cdr x))) alist)
	hash))))

(define (hash-table-keys ht)
  (##sys#check-structure ht 'hash-table 'hash-table-keys)
  (let* ([vec (##sys#slot ht 1)]
	 [len (##sys#size vec)] )
    (let loop ([i 0] [lst '()])
      (if (fx>= i len)
	  lst
	  (let loop2 ([bucket (##sys#slot vec i)] [lst lst])
	    (if (null? bucket)
		(loop (fx+ i 1) lst)
		(loop2 (##sys#slot bucket 1)
		       (let ([x (##sys#slot bucket 0)])
			 (cons (##sys#slot x 0) lst) ) ) ) ) ) ) ) )

(define (hash-table-values ht)
  (##sys#check-structure ht 'hash-table 'hash-table-values)
  (let* ([vec (##sys#slot ht 1)]
	 [len (##sys#size vec)] )
    (let loop ([i 0] [lst '()])
      (if (fx>= i len)
	  lst
	  (let loop2 ([bucket (##sys#slot vec i)] [lst lst])
	    (if (null? bucket)
		(loop (fx+ i 1) lst)
		(loop2 (##sys#slot bucket 1)
		       (let ([x (##sys#slot bucket 0)])
			 (cons (##sys#slot x 1) lst) ) ) ) ) ) ) ) )

(register-feature! 'srfi-69)


;;; Mapping over keys and elements:

(define hash-table-walk
  (lambda (ht p)
    (##sys#check-structure ht 'hash-table 'hash-table-walk)
    (let* ((vec (##sys#slot ht 1))
	   (len (##sys#size vec)))
      (do ((i 0 (fx+ i 1)))
	  ((fx>= i len))
	(##sys#for-each (lambda (bucket) 
		      (p (##sys#slot bucket 0)
			 (##sys#slot bucket 1) ) )
		    (##sys#slot vec i) ) ) ) ) )

(define hash-table-fold
  (lambda (ht p init)
    (##sys#check-structure ht 'hash-table 'hash-table-fold)
    (let* ((vec (##sys#slot ht 1))
	   (len (##sys#size vec)))
      (let loop ((i 0) (acc init))
	(if (fx>= i len)
	    acc
	    (let fold2 ((buckets (##sys#slot vec i)) (acc acc))
	      (if (null? buckets)
		  (loop (fx+ i 1) acc)
		  (let ((b (##sys#slot buckets 0)))
		    (fold2 (##sys#slot buckets 1)
			   (p (##sys#slot b 0)
			      (##sys#slot b 1) 
			      acc) ) ) ) ) ) ) ) ) )


; Support for queues
;
; Written by Andrew Wilcox (awilcox@astro.psu.edu) on April 1, 1992.
;
; This code is in the public domain.
; 
; (heavily adapated for use with CHICKEN by felix)
;


; Elements in a queue are stored in a list.  The last pair in the list
; is stored in the queue type so that datums can be added in constant
; time.

(define (make-queue) (##sys#make-structure 'queue '() '()))
(define (queue? x) (##sys#structure? x 'queue))

(define (queue-empty? q)
  (##sys#check-structure q 'queue 'queue-empty?)
  (eq? '() (##sys#slot q 1)) )

(define queue-first
  (lambda (q)
    (##sys#check-structure q 'queue 'queue-first)
    (let ((first-pair (##sys#slot q 1)))
      (cond-expand 
       [(not unsafe)
	(when (eq? '() first-pair)
	  (##sys#error 'queue-first "queue is empty" q)) ]
       [else] )
      (##sys#slot first-pair 0) ) ) )

(define queue-last
  (lambda (q)
    (##sys#check-structure q 'queue 'queue-last)
    (let ((last-pair (##sys#slot q 2)))
      (cond-expand
       [(not unsafe)
	(when (eq? '() last-pair)
	  (##sys#error 'queue-last "queue is empty" q)) ]
       [else] )
      (##sys#slot last-pair 0) ) ) )

(define (queue-add! q datum)
  (##sys#check-structure q 'queue 'queue-add!)
  (let ((new-pair (cons datum '())))
    (cond ((eq? '() (##sys#slot q 1)) (##sys#setslot q 1 new-pair))
	  (else (##sys#setslot (##sys#slot q 2) 1 new-pair)) )
    (##sys#setslot q 2 new-pair) 
    (##core#undefined) ) )

(define queue-remove!
  (lambda (q)
    (##sys#check-structure q 'queue 'queue-remove!)
    (let ((first-pair (##sys#slot q 1)))
      (cond-expand
       [(not unsafe)
	(when (eq? '() first-pair)
	  (##sys#error 'queue-remove! "queue is empty" q) ) ]
       [else] )
      (let ((first-cdr (##sys#slot first-pair 1)))
	(##sys#setslot q 1 first-cdr)
	(if (eq? '() first-cdr)
	    (##sys#setslot q 2 '()) )
	(##sys#slot first-pair 0) ) ) ) )

(define (queue->list q)
  (##sys#check-structure q 'queue 'queue->list)
  (##sys#slot q 1) )

(define (list->queue lst0)
  (##sys#check-list lst0 'list->queue)
  (##sys#make-structure 
   'queue lst0
   (if (eq? lst0 '())
       '()
       (do ((lst lst0 (##sys#slot lst 1)))
	   ((eq? (##sys#slot lst 1) '()) lst)
	 (if (or (not (##core#inline "C_blockp" lst))
		 (not (##core#inline "C_pairp" lst)) )
	     (##sys#not-a-proper-list-error lst0 'list->queue) ) ) ) ) )


; (queue-push-back! queue item)
; Pushes an item into the first position of a queue.

(define (queue-push-back! q item)
  (##sys#check-structure q 'queue 'queue-push-back!)
  (let ((newlist (cons item (##sys#slot q 1))))
    (##sys#setslot q 1 newlist)
    (if (eq? '() (##sys#slot q 2))
        (##sys#setslot q 2 newlist))))

; (queue-push-back-list! queue item-list)
; Pushes the items in item-list back onto the queue,
; so that (car item-list) becomes the next removable item.

(define-macro (last-pair lst0)
  `(do ((lst ,lst0 (##sys#slot lst 1)))
       ((eq? (##sys#slot lst 1) '()) lst)))

(define (queue-push-back-list! q itemlist)
  (##sys#check-structure q 'queue 'queue-push-back-list!)
  (##sys#check-list itemlist 'queue-push-back-list!)
  (let* ((newlist (append itemlist (##sys#slot q 1)))
         (newtail (if (eq? newlist '())
                       '()
                       (last-pair newlist))))
    (##sys#setslot q 1 newlist)
    (##sys#setslot q 2 newtail)))
