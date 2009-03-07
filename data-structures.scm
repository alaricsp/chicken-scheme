;;; data-structures.scm - Optional data structures extensions
;
; Copyright (c) 2008-2009, The Chicken Team
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
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
 (unit data-structures)
 (usual-integrations)
 (disable-warning redef)
 (foreign-declare #<<EOF
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
      ##extras#reverse-string-append ##sys#number? ##sys#procedure->string
      ##sys#pointer->string ##sys#user-print-hook ##sys#peek-char-0
      ##sys#read-char-0 ##sys#write-char ##sys#string-append ##sys#gcd ##sys#lcm
      ##sys#fudge ##sys#check-list ##sys#user-read-hook ##sys#check-closure ##sys#check-inexact
      input-port? make-vector list->vector sort! merge! open-output-string floor
      get-output-string current-output-port display write port? list->string
      make-string string pretty-print-width newline char-name read random
      open-input-string make-string call-with-input-file read-line reverse ) ) ] )

(private data-structures
  reverse-string-append
  fprintf0 generic-write )

(declare
  (hide
    fprintf0 generic-write ) )

(include "unsafe-declarations.scm")

(register-feature! 'data-structures)



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

(define (list-of? pred)
  (lambda (lst)
    (let loop ([lst lst])
      (cond [(null? lst) #t]
	    [(not-pair? lst) #f]
	    [(pred (##sys#slot lst 0)) (loop (##sys#slot lst 1))]
	    [else #f] ) ) ) )

(define list-of list-of?)		; DEPRECATED

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

(define (none? x) #f)

(define (always? . _) #t)

(define (never? . _) #f)

(define (left-section proc . args)
  (##sys#check-closure proc 'left-section)
  (lambda xs
    (##sys#apply proc (##sys#append args xs)) ) )

(define right-section
  (let ([##sys#reverse reverse])
    (lambda (proc . args)
      (##sys#check-closure proc 'right-section)
      (let ([revdargs (##sys#reverse args)])
        (lambda xs
          (##sys#apply proc (##sys#reverse (##sys#append revdargs (##sys#reverse xs)))) ) ) ) ) )


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
	     (##sys#error-not-a-proper-list lsts) ]
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
  (lambda (l random)
    (let ((len (length l)))
      (map cdr
	   (sort! (map (lambda (x) (cons (random len) x)) l)
		  (lambda (x y) (< (car x) (car y)))) ) ) ) )


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
  (set! ##sys#substring-index 
    (lambda (which where start)
      (traverse 
       which where start
       (lambda (i l) (##core#inline "C_substring_compare" which where 0 i l))
       'substring-index) ) )
  (set! ##sys#substring-index-ci 
    (lambda (which where start)
      (traverse
       which where start
       (lambda (i l) (##core#inline "C_substring_compare_case_insensitive" which where 0 i l)) 
       'substring-index-ci) ) ) )

(define (substring-index which where #!optional (start 0))
  (##sys#substring-index which where start) )

(define (substring-index-ci which where #!optional (start 0))
  (##sys#substring-index-ci which where start) )


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

(define (##sys#substring=? s1 s2 start1 start2 n)
  (##sys#check-string s1 'substring=?)
  (##sys#check-string s2 'substring=?)
  (let ((len (or n
		 (fxmin (fx- (##sys#size s1) start1)
			(fx- (##sys#size s2) start2) ) ) ) )
    (##sys#check-exact start1 'substring=?)
    (##sys#check-exact start2 'substring=?)
    (##core#inline "C_substring_compare" s1 s2 start1 start2 len) ) )

(define (substring=? s1 s2 #!optional (start1 0) (start2 0) len)
  (##sys#substring=? s1 s2 start1 start2 len) )

(define (##sys#substring-ci=? s1 s2 start1 start2 n)
  (##sys#check-string s1 'substring-ci=?)
  (##sys#check-string s2 'substring-ci=?)
  (let ((len (or n
		 (fxmin (fx- (##sys#size s1) start1)
			(fx- (##sys#size s2) start2) ) ) ) )
    (##sys#check-exact start1 'substring-ci=?)
    (##sys#check-exact start2 'substring-ci=?)
    (##core#inline "C_substring_compare_case_insensitive"
		   s1 s2 start1 start2 len) ) )

(define (substring-ci=? s1 s2 #!optional (start1 0) (start2 0) len)
  (##sys#substring-ci=? s1 s2 start1 start2 len) )


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
	    (else (##sys#error-not-a-proper-list strs)) ) ) ) )


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
	    ;; The loop handles the merging of non-empty lists.	 It has
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
;;; Warren, and first used in the DEC-10 Prolog system.	 R. A. O'Keefe
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
	     (##sys#error-not-a-proper-list lst0 'list->queue) ) ) ) ) )


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

(define-inline (last-pair lst0)
  (do ((lst lst0 (##sys#slot lst 1)))
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
