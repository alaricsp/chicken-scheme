;;; srfi-69.scm - Optional non-standard extensions
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
 (unit srfi-69)
 (usual-integrations)
 (disable-warning redef) ) ; hash-table-ref is an extended binding!

(cond-expand
 [paranoia]
 [else
  (declare
    (no-bound-checks)
    (no-procedure-checks-for-usual-bindings) ) ] )

(declare
  (bound-to-procedure
    ##sys#signal-hook
    ##sys#peek-fixnum
    ##sys#make-structure
    ##sys#size
    ##sys#slot ##sys#setslot
    *equal?-hash )
  (hide
    *eq?-hash *eqv?-hash *equal?-hash
    *make-hash-table
    *hash-table-copy *hash-table-merge! *hash-table-update!/default
    *hash-table-for-each *hash-table-fold
    hash-table-canonical-length hash-table-rehash! hash-table-check-resize! ) )

(cond-expand
 [unsafe]
 [else
   (declare
     (bound-to-procedure
       ##sys#check-string ##sys#check-symbol
       ##sys#check-exact ##sy#check-inexact
       ##sys#check-closure ##sys#check-structure ) ) ] )

(include "unsafe-declarations.scm")

(register-feature! 'srfi-69)


;;; Naming Conventions:

;; %foo - inline primitive
;; %%foo - local inline (no such thing but at least it looks different)
;; $foo - local macro
;; *foo - local unchecked variant of a checked procedure
;; ##sys#foo - public, but undocumented, un-checked procedure
;; foo - public checked procedure
;;


;;; Core Inlines:

(define-inline (%fix wrd)
  (##core#inline "C_fix" wrd) )

(define-inline (%block? obj)
  (##core#inline "C_blockp" obj) )

(define-inline (%immediate? obj)
  (not (##core#inline "C_blockp" obj)) )

(define-inline (%special? obj)
  (##core#inline "C_specialp" obj) )

(define-inline (%port? obj)
  (##core#inline "C_portp" obj) )

(define-inline (%byte-block? obj)
  (##core#inline "C_byteblockp" obj) )

(define-inline (%string-hash str)
  (##core#inline "C_hash_string" str) )

(define-inline (%string-ci-hash str)
  (##core#inline "C_hash_string_ci" str) )

(define-inline (%subbyte bytvec i)
  (##core#inline "C_subbyte" bytvec i) )


;;; Generation of hash-values:

;; All '%foo-hash' return a fixnum, not necessarily positive. The "overflow" of
;; a, supposedly, unsigned hash value into negative is not checked during
;; intermediate computation.
;;
;; The body of '*eq?-hash' is duplicated in '*eqv?-hash' and the body of '*eqv?-hash'
;; is duplicated in '*equal?-hash' to save on procedure calls.

;; Fixed hash-values:

(define-constant other-hash-value 99)
(define-constant true-hash-value 256)
(define-constant false-hash-value 257)
(define-constant null-hash-value 258)
(define-constant eof-hash-value 259)
(define-constant input-port-hash-value 260)
(define-constant output-port-hash-value 261)
(define-constant unknown-immediate-hash-value 262)

(define-constant hash-default-bound 536870912)

;; Force Hash to Bounded Fixnum:

(define-inline (%fxabs fxn)
  (if (fx< fxn 0) (fxneg fxn) fxn ) )

(define-inline (%hash/limit hsh lim)
  (fxmod (fxand (foreign-value "C_MOST_POSITIVE_FIXNUM" int)
		(%fxabs hsh))
	 lim) )

;; Number Hash:

(define-constant flonum-magic 331804471)

(define-syntax $flonum-hash
  (lambda (form r c)
    (let ( (flo (cadr form))
           (_%subbyte (r '%subbyte))
           (_flonum-magic (r 'flonum-magic))
           (_fx+ (r 'fx+))
           (_fx* (r 'fx*))
           (_fxshl (r 'fxshl)) )
    `(,_fx* ,_flonum-magic
            ,(let loop ( (idx (fx- (##sys#size 1.0) 1)) )
               (if (fx= 0 idx)
                   `(,_%subbyte ,flo 0)
                   `(,_fx+ (,_%subbyte ,flo ,idx)
                           (,_fxshl ,(loop (fx- idx 1)) 1)) ) ) ) ) ) )

(define (##sys#number-hash-hook obj)
  (*equal?-hash obj) )

(define-inline (%non-fixnum-number-hash obj)
  (cond [(flonum? obj)	($flonum-hash obj)]
	[else		(%fix (##sys#number-hash-hook obj))] ) )

(define-inline (%number-hash obj)
  (cond [(fixnum? obj)	obj]
	[else		(%non-fixnum-number-hash obj)] ) )

(define (number-hash obj #!optional (bound hash-default-bound))
  (unless (number? obj)
    (##sys#signal-hook #:type 'number-hash "invalid number" obj) )
  (##sys#check-exact bound 'number-hash)
  (%hash/limit (%number-hash obj) bound) )

;; Object UID Hash:

#; ;NOT YET (no weak-reference)
(define-inline (%object-uid-hash obj)
  (%uid-hash (##sys#object->uid obj)) )

(define-inline (%object-uid-hash obj)
  (*equal?-hash obj) )

(define (object-uid-hash obj #!optional (bound hash-default-bound))
  (##sys#check-exact bound 'object-uid-hash)
  (%hash/limit (%object-uid-hash obj) bound) )

;; Symbol Hash:

#; ;NOT YET (no unique-symbol-hash)
(define-inline (%symbol-hash obj)
  (##sys#slot obj INDEX-OF-UNIQUE-HASH-VALUE-COMPUTED-DURING-SYMBOL-CREATION) )

(define-inline (%symbol-hash obj)
  (%string-hash (##sys#slot obj 1)) )

(define (symbol-hash obj #!optional (bound hash-default-bound))
  (##sys#check-symbol obj 'symbol-hash)
  (##sys#check-exact bound 'symbol-hash)
  (%hash/limit (%symbol-hash obj) bound) )

;; Keyword Hash:

(define (##sys#check-keyword x . y)
  (unless (keyword? x)
    (##sys#signal-hook #:type-error
		       (and (not (null? y)) (car y))
		       "bad argument type - not a keyword" x) ) )

#; ;NOT YET (no unique-keyword-hash)
(define-inline (%keyword-hash obj)
  (##sys#slot obj INDEX-OF-UNIQUE-HASH-VALUE-COMPUTED-DURING-KEYWORD-CREATION) )

(define-inline (%keyword-hash obj)
  (%string-hash (##sys#slot obj 1)) )

(define (keyword-hash obj #!optional (bound hash-default-bound))
  (##sys#check-keyword obj 'keyword-hash)
  (##sys#check-exact bound 'keyword-hash)
  (%hash/limit (%keyword-hash obj) bound) )

;; Eq Hash:

(define-inline (%eq?-hash-object? obj)
  (or (%immediate? obj)
       (symbol? obj)
       #; ;NOT YET (no keyword vs. symbol issue)
       (keyword? obj) ) )

(define (*eq?-hash obj)
  (cond [(fixnum? obj)		obj]
	[(char? obj)		(char->integer obj)]
	[(eq? obj #t)		true-hash-value]
	[(eq? obj #f)		false-hash-value]
	[(null? obj)		null-hash-value]
	[(eof-object? obj)	eof-hash-value]
	[(symbol? obj)		(%symbol-hash obj)]
	#; ;NOT YET (no keyword vs. symbol issue)
	[(keyword? obj)		(%keyword-hash obj)]
	[(%immediate? obj)	unknown-immediate-hash-value]
	[else			(%object-uid-hash obj) ] ) )

(define (eq?-hash obj #!optional (bound hash-default-bound))
  (##sys#check-exact bound 'eq?-hash)
  (%hash/limit (*eq?-hash obj) bound) )

(define hash-by-identity eq?-hash)

;; Eqv Hash:

(define-inline (%eqv?-hash-object? obj)
  (or (%eq?-hash-object? obj)
      (number? obj) ) )

(define (*eqv?-hash obj)
  (cond [(fixnum? obj)		obj]
	[(char? obj)		(char->integer obj)]
	[(eq? obj #t)		true-hash-value]
	[(eq? obj #f)		false-hash-value]
	[(null? obj)		null-hash-value]
	[(eof-object? obj)	eof-hash-value]
	[(symbol? obj)		(%symbol-hash obj)]
	#; ;NOT YET (no keyword vs. symbol issue)
	[(keyword? obj)		(%keyword-hash obj)]
	[(number? obj)		(%non-fixnum-number-hash obj)]
	[(%immediate? obj)	unknown-immediate-hash-value]
	[else			(%object-uid-hash obj) ] ) )

(define (eqv?-hash obj #!optional (bound hash-default-bound))
  (##sys#check-exact bound 'eqv?-hash)
  (%hash/limit (*eqv?-hash obj) bound) )

;; Equal Hash:

;XXX Be nice if these were parameters
(define-constant recursive-hash-max-depth 4)
(define-constant recursive-hash-max-length 4)

;; NOTE - These refer to identifiers available only within the body of '*equal?-hash'.

(define-inline (%%list-hash obj)
  (fx+ (length obj)
       (recursive-atomic-hash (##sys#slot obj 0) depth)) )

(define-inline (%%pair-hash obj)
  (fx+ (fxshl (recursive-atomic-hash (##sys#slot obj 0) depth) 16)
	(recursive-atomic-hash (##sys#slot obj 1) depth)) )

(define-inline (%%port-hash obj)
  (fx+ (fxshl (##sys#peek-fixnum obj 0) 4) ; Little extra "identity"
	(if (input-port? obj)
	    input-port-hash-value
	    output-port-hash-value)) )

(define-inline (%%special-vector-hash obj)
  (vector-hash obj (##sys#peek-fixnum obj 0) depth 1) )

(define-inline (%%regular-vector-hash obj)
  (vector-hash obj 0 depth 0) )

(define (*equal?-hash obj)

  ; Recurse into some portion of the vector's slots
  (define (vector-hash obj seed depth start)
    (let ([len (##sys#size obj)])
      (let loop ([hsh (fx+ len seed)]
		 [i start]
		 [len (fx- (fxmin recursive-hash-max-length len) start)] )
	(if (fx= len 0)
	    hsh
	    (loop (fx+ hsh
		       (fx+ (fxshl hsh 4)
			    (recursive-hash (##sys#slot obj i) (fx+ depth 1))))
		  (fx+ i 1)
		  (fx- len 1) ) ) ) ) )

  ; Don't recurse into structured objects
  (define (recursive-atomic-hash obj depth)
    (if (or (%eqv?-hash-object? obj)
	    (%byte-block? obj))
	(recursive-hash obj (fx+ depth 1))
	other-hash-value ) )

  ; Recurse into structured objects
  (define (recursive-hash obj depth)
    (cond [(fx>= depth recursive-hash-max-depth)
				  other-hash-value]
	  [(fixnum? obj)	  obj]
	  [(char? obj)		  (char->integer obj)]
	  [(eq? obj #t)		  true-hash-value]
	  [(eq? obj #f)		  false-hash-value]
	  [(null? obj)		  null-hash-value]
	  [(eof-object? obj)	  eof-hash-value]
	  [(symbol? obj)	  (%symbol-hash obj)]
	  #; ;NOT YET (no keyword vs. symbol issue)
	  [(keyword? obj)	  (%keyword-hash obj)]
	  [(number? obj)	  (%non-fixnum-number-hash obj)]
	  [(%immediate? obj)	  unknown-immediate-hash-value]
	  [(%byte-block? obj)	  (%string-hash obj)]
	  [(list? obj)		  (%%list-hash obj)]
	  [(pair? obj)		  (%%pair-hash obj)]
	  [(%port? obj)		  (%%port-hash obj)]
	  [(%special? obj)	  (%%special-vector-hash obj)]
	  [else			  (%%regular-vector-hash obj)] ) )

  ;
  (recursive-hash obj 0) )

(define (equal?-hash obj #!optional (bound hash-default-bound))
  (##sys#check-exact bound 'hash)
  (%hash/limit (*equal?-hash obj) bound) )

(define hash equal?-hash)

;; String Hash:

(define (string-hash str #!optional (bound hash-default-bound) . start+end)
  (##sys#check-string str 'string-hash)
  (##sys#check-exact bound 'string-hash)
  (let ((str (if (pair? start+end)
		 (let-optionals start+end ((start 0)
					   (end (##sys#size str)))
		   (##sys#check-range start 0 (##sys#size str) 'string-hash) 
		   (##sys#check-range end 0 (##sys#size str) 'string-hash) 
		   (##sys#substring str start end) )
		 str) ) )
    (%hash/limit (%string-hash str) bound) ) )

(define (string-ci-hash str #!optional (bound hash-default-bound) . start+end)
  (##sys#check-string str 'string-ci-hash)
  (##sys#check-exact bound 'string-ci-hash)
  (let ((str (if (pair? start+end)
		 (let-optionals start+end ((start 0)
					   (end (##sys#size str)))
		   (##sys#check-range start 0 (##sys#size str) 'string-hash-ci) 
		   (##sys#check-range end 0 (##sys#size str) 'string-hash-ci) 
		   (##sys#substring str start end) )
		 str) ) )
  (%hash/limit (%string-ci-hash str) bound) ) )

(define string-hash-ci string-ci-hash)


;;; Hash-Tables:

; Predefined sizes for the hash tables:
;
; Starts with 307; each element is the smallest prime that is at least twice in
; magnitude as the previous element in the list.
;
; The last number is an exception: it is the largest 32-bit fixnum we can represent.

(define-constant hash-table-prime-lengths
  '(307 617
    1237 2477 4957 9923
    19853 39709 79423
    158849 317701 635413
    1270849 2541701 5083423
    10166857 20333759 40667527 81335063 162670129
    325340273 650680571
    ;
    1073741823))

(define-constant hash-table-default-length 307)
(define-constant hash-table-max-length 1073741823)
(define-constant hash-table-new-length-factor 2)

(define-constant hash-table-default-min-load 0.5)
(define-constant hash-table-default-max-load 0.8)

;; Restrict hash-table length to tabled lengths:

(define (hash-table-canonical-length tab req)
  (let loop ([tab tab])
    (let ([cur (##sys#slot tab 0)]
	  [nxt (##sys#slot tab 1)])
      (if (or (fx>= cur req)
	      (null? nxt))
	  cur
	  (loop nxt) ) ) ) )

;; "Raw" make-hash-table:

(define *make-hash-table
  (let ([make-vector make-vector])
    (lambda (test hash len min-load max-load weak-keys weak-values initial
	     #!optional (vec (make-vector len '())))
      (##sys#make-structure 'hash-table
       vec 0 test hash min-load max-load #f #f initial) ) ) )

;; SRFI-69 & SRFI-90'ish.
;;
;; Argument list is the pattern
;;
;; (make-hash-table #!optional test hash size
;;		    #!key test hash size initial min-load max-load weak-keys weak-values)
;;
;; where a keyword argument takes precedence over the corresponding optional
;; argument. Keyword arguments MUST come after optional & required
;; arugments.
;;
;; Wish DSSSL (extended) argument list processing Did-What-I-Want (DWIW).

(define make-hash-table
  (let ([core-eq? eq?]
	[core-eqv? eqv?]
	[core-equal? equal?]
	[core-string=? string=?]
	[core-string-ci=? string-ci=?]
	[core= =] )
    (lambda arguments0
      (let ([arguments arguments0]
	    [test equal?]
	    [hash #f]
	    [size hash-table-default-length]
	    [initial #f]
	    [min-load hash-table-default-min-load]
	    [max-load hash-table-default-max-load]
	    [weak-keys #f]
	    [weak-values #f])
	(let ([hash-for-test
		(lambda ()
		  (cond [(or (eq? core-eq? test)
			     (eq? eq? test))		  eq?-hash]
			[(or (eq? core-eqv? test)
			     (eq? eqv? test))		  eqv?-hash]
			[(or (eq? core-equal? test)
			     (eq? equal? test))		  equal?-hash]
			[(or (eq? core-string=? test)
			     (eq? string=? test))	  string-hash]
			[(or (eq? core-string-ci=? test)
			     (eq? string-ci=? test))	  string-hash-ci]
			[(or (eq? core= test)
			     (eq? = test))		  number-hash]
			[else				  #f] ) ) ] )
	  ; Process optional arguments
	  (unless (null? arguments)
	    (let ([arg (car arguments)])
	      (unless (keyword? arg)
		(##sys#check-closure arg 'make-hash-table)
		(set! test arg)
		(set! arguments (cdr arguments)) ) ) )
	  (unless (null? arguments)
	    (let ([arg (car arguments)])
	      (unless (keyword? arg)
		(##sys#check-closure arg 'make-hash-table)
		(set! hash arg)
		(set! arguments (cdr arguments)) ) ) )
	  (unless (null? arguments)
	    (let ([arg (car arguments)])
	      (unless (keyword? arg)
		(##sys#check-exact arg 'make-hash-table)
		(unless (fx< 0 arg)
		  (error 'make-hash-table "invalid size" arg) )
		(set! size (fxmin hash-table-max-size arg))
		(set! arguments (cdr arguments)) ) ) )
	  ; Process keyword arguments
	  (let loop ([args arguments])
	    (unless (null? args)
	      (let ([arg (car args)])
		(let ([invarg-err
			(lambda (msg)
			  (error 'make-hash-table msg arg arguments0))])
		  (if (keyword? arg)
		      (let* ([nxt (cdr args)]
			     [val (if (pair? nxt)
				      (car nxt)
				      (invarg-err "missing keyword value"))])
			(case arg
			  [(#:test)
			    (##sys#check-closure val 'make-hash-table)
			    (set! test val)]
			  [(#:hash)
			    (##sys#check-closure val 'make-hash-table)
			    (set! hash val)]
			  [(#:size)
			    (##sys#check-exact val 'make-hash-table)
			    (unless (fx< 0 val)
			      (error 'make-hash-table "invalid size" val) )
			    (set! size (fxmin hash-table-max-size val))]
			  [(#:initial)
			    (set! initial (lambda () val))]
			  [(#:min-load)
			    (##sys#check-inexact val 'make-hash-table)
			    (unless (and (fp< 0.0 val) (fp< val 1.0))
			      (error 'make-hash-table "invalid min-load" val) )
			    (set! min-load val)]
			  [(#:max-load)
			    (##sys#check-inexact val 'make-hash-table)
			    (unless (and (fp< 0.0 val) (fp< val 1.0))
			      (error 'make-hash-table "invalid max-load" val) )
			    (set! max-load val)]
			  [(#:weak-keys)
			    (set! weak-keys (and val #t))]
			  [(#:weak-values)
			    (set! weak-values (and val #t))]
			  [else
			    (invarg-err "unknown keyword")])
			(loop (cdr nxt)) )
		      (invarg-err "missing keyword") ) ) ) ) )
	  ; Load must be a proper interval
	  (when (fp< max-load min-load)
	    (error 'make-hash-table "min-load greater than max-load" min-load max-load) )
	  ; Force canonical hash-table vector length
	  (set! size (hash-table-canonical-length hash-table-prime-lengths size))
	  ; Decide on a hash function when not supplied
	  (unless hash
	    (let ([func (hash-for-test)])
	      (if func
		  (set! hash func)
		  (begin
		    (warning 'make-hash-table "user test without user hash")
		    (set! hash equal?-hash) ) ) ) )
	  ; Done
	  (*make-hash-table test hash size min-load max-load weak-keys weak-values initial) ) ) ) ) )

;; Hash-Table Predicate:

(define (hash-table? obj)
  (##sys#structure? obj 'hash-table) )

;; Hash-Table Properties:

(define (hash-table-size ht)
  (##sys#check-structure ht 'hash-table 'hash-table-size)
  (##sys#slot ht 2) )

(define (hash-table-equivalence-function ht)
  (##sys#check-structure ht 'hash-table 'hash-table-equivalence-function)
  (##sys#slot ht 3) )

(define (hash-table-hash-function ht)
  (##sys#check-structure ht 'hash-table 'hash-table-hash-function)
  (##sys#slot ht 4) )

(define (hash-table-min-load ht)
  (##sys#check-structure ht 'hash-table 'hash-table-min-load)
  (##sys#slot ht 5) )

(define (hash-table-max-load ht)
  (##sys#check-structure ht 'hash-table 'hash-table-max-load)
  (##sys#slot ht 6) )

(define (hash-table-weak-keys ht)
  (##sys#check-structure ht 'hash-table 'hash-table-weak-keys)
  (##sys#slot ht 7) )

(define (hash-table-weak-values ht)
  (##sys#check-structure ht 'hash-table 'hash-table-weak-values)
  (##sys#slot ht 8) )

(define (hash-table-has-initial? ht)
  (##sys#check-structure ht 'hash-table 'hash-table-has-initial?)
  (and (##sys#slot ht 9)
       #t ) )

(define (hash-table-initial ht)
  (##sys#check-structure ht 'hash-table 'hash-table-initial)
  (and-let* ([thunk (##sys#slot ht 9)])
    (thunk) ) )

;; hash-table-rehash!:

(define (hash-table-rehash! vec1 vec2 hash)
  (let ([len1 (##sys#size vec1)]
	[len2 (##sys#size vec2)] )
    (do ([i 0 (fx+ i 1)])
	[(fx>= i len1)]
      (let loop ([bucket (##sys#slot vec1 i)])
	(unless (null? bucket)
	  (let* ([pare (##sys#slot bucket 0)]
		 [key (##sys#slot pare 0)]
		 [hshidx (hash key len2)] )
	    (##sys#setslot vec2 hshidx
			   (cons (cons key (##sys#slot pare 1)) (##sys#slot vec2 hshidx)))
	    (loop (##sys#slot bucket 1)) ) ) ) ) ) )

;; hash-table-resize!:

(define (hash-table-resize! ht vec len)
  (let* ([deslen (fxmin hash-table-max-length (fx* len hash-table-new-length-factor))]
         [newlen (hash-table-canonical-length hash-table-prime-lengths deslen)]
         [vec2 (make-vector newlen '())] )
    (hash-table-rehash! vec vec2 (##sys#slot ht 4))
    (##sys#setslot ht 1 vec2) ) )

;; hash-table-check-resize!:

(define-inline (hash-table-check-resize! ht newsiz)
  (let ([vec (##sys#slot ht 1)]
        [min-load (##sys#slot ht 5)]
        [max-load (##sys#slot ht 6)] )
    (let ([len (##sys#size vec)] )
      (let ([min-load-len (inexact->exact (floor (* len min-load)))]
            [max-load-len (inexact->exact (floor (* len max-load)))] )
        (if (and (fx< len hash-table-max-length)
                 (fx<= min-load-len newsiz) (fx<= newsiz max-load-len))
          (hash-table-resize! ht vec len) ) ) ) ) )

;; hash-table-copy:

(define *hash-table-copy
  (let ([make-vector make-vector])
    (lambda (ht)
      (let* ([vec1 (##sys#slot ht 1)]
	     [len (##sys#size vec1)]
	     [vec2 (make-vector len '())] )
	(do ([i 0 (fx+ i 1)])
	    [(fx>= i len)
	     (*make-hash-table
	      (##sys#slot ht 3) (##sys#slot ht 4)
	      (##sys#slot ht 2)
	      (##sys#slot ht 5) (##sys#slot ht 6)
	      (##sys#slot ht 7) (##sys#slot ht 8)
	      (##sys#slot ht 9)
	      vec2)]
	  (##sys#setslot vec2 i
	   (let copy-loop ([bucket (##sys#slot vec1 i)])
	     (if (null? bucket)
		 '()
		 (let ([pare (##sys#slot bucket 0)])
		   (cons (cons (##sys#slot pare 0) (##sys#slot pare 1))
			 (copy-loop (##sys#slot bucket 1))))))) ) ) ) ) )

(define (hash-table-copy ht)
  (##sys#check-structure ht 'hash-table 'hash-table-copy)
  (*hash-table-copy ht) )

;; hash-table-update!:
;;
;; This one was suggested by Sven Hartrumpf (and subsequently added in SRFI-69).
;; Modified for ht props min & max load.

(define hash-table-update!
  (let ([core-eq? eq?] )
    (lambda (ht key
	     #!optional (func identity)
		        (thunk
		         (let ([thunk (##sys#slot ht 9)])
		           (or thunk
			       (lambda ()
			         (##sys#signal-hook #:access-error
			          'hash-table-update!
			          "hash-table does not contain key" key ht))))))
      (##sys#check-structure ht 'hash-table 'hash-table-update!)
      (##sys#check-closure func 'hash-table-update!)
      (##sys#check-closure thunk 'hash-table-update!)
      (let ([newsiz (fx+ (##sys#slot ht 2) 1)] )
	(hash-table-check-resize! ht newsiz)
	(let ([hash (##sys#slot ht 4)]
	      [test (##sys#slot ht 3)]
	      [vec (##sys#slot ht 1)] )
	  (let* ([len (##sys#size vec)]
	         [hshidx (hash key len)]
	         [bucket0 (##sys#slot vec hshidx)] )
            (if (eq? core-eq? test)
                ; Fast path (eq? is rewritten by the compiler):
                (let loop ([bucket bucket0])
                  (if (null? bucket)
                      (let ([val (func (thunk))])
                        (##sys#setslot vec hshidx (cons (cons key val) bucket0))
                        (##sys#setislot ht 2 newsiz)
                        val )
                      (let ([pare (##sys#slot bucket 0)])
                         (if (eq? key (##sys#slot pare 0))
                             (let ([val (func (##sys#slot pare 1))])
                               (##sys#setslot pare 1 val)
                               val)
                             (loop (##sys#slot bucket 1)) ) ) ) )
                ; Slow path
                (let loop ([bucket bucket0])
                  (if (null? bucket)
                      (let ([val (func (thunk))])
                        (##sys#setslot vec hshidx (cons (cons key val) bucket0))
                        (##sys#setislot ht 2 newsiz)
                        val )
                      (let ([pare (##sys#slot bucket 0)])
                         (if (test key (##sys#slot pare 0))
                             (let ([val (func (##sys#slot pare 1))])
                               (##sys#setslot pare 1 val)
                               val )
                             (loop (##sys#slot bucket 1)) ) ) ) ) ) ) ) ) ) ) )

(define *hash-table-update!/default
  (let ([core-eq? eq?] )
    (lambda (ht key func def)
      (let ([newsiz (fx+ (##sys#slot ht 2) 1)] )
	(hash-table-check-resize! ht newsiz)
	(let ([hash (##sys#slot ht 4)]
	      [test (##sys#slot ht 3)]
	      [vec (##sys#slot ht 1)] )
	  (let* ([len (##sys#size vec)]
	         [hshidx (hash key len)]
	         [bucket0 (##sys#slot vec hshidx)] )
            (if (eq? core-eq? test)
                ; Fast path (eq? is rewritten by the compiler):
                (let loop ([bucket bucket0])
                  (if (null? bucket)
                      (let ([val (func def)])
                        (##sys#setslot vec hshidx (cons (cons key val) bucket0))
                        (##sys#setislot ht 2 newsiz)
                        val )
                      (let ([pare (##sys#slot bucket 0)])
                         (if (eq? key (##sys#slot pare 0))
                             (let ([val (func (##sys#slot pare 1))])
                               (##sys#setslot pare 1 val)
                               val)
                             (loop (##sys#slot bucket 1)) ) ) ) )
                ; Slow path
                (let loop ([bucket bucket0])
                  (if (null? bucket)
                      (let ([val (func def)])
                        (##sys#setslot vec hshidx (cons (cons key val) bucket0))
                        (##sys#setislot ht 2 newsiz)
                        val )
                      (let ([pare (##sys#slot bucket 0)])
                         (if (test key (##sys#slot pare 0))
                             (let ([val (func (##sys#slot pare 1))])
                               (##sys#setslot pare 1 val)
                               val )
                             (loop (##sys#slot bucket 1)) ) ) ) ) ) ) ) ) ) ) )

(define (hash-table-update!/default ht key func def)
  (##sys#check-structure ht 'hash-table 'hash-table-update!/default)
  (##sys#check-closure func 'hash-table-update!/default)
  (*hash-table-update!/default ht key func def) )

(define hash-table-set!
  (let ([core-eq? eq?] )
    (lambda (ht key val)
      (##sys#check-structure ht 'hash-table 'hash-table-set!)
      (let ([newsiz (fx+ (##sys#slot ht 2) 1)] )
	(hash-table-check-resize! ht newsiz)
	(let ([hash (##sys#slot ht 4)]
	      [test (##sys#slot ht 3)]
	      [vec (##sys#slot ht 1)] )
	  (let* ([len (##sys#size vec)]
	         [hshidx (hash key len)]
	         [bucket0 (##sys#slot vec hshidx)] )
            (if (eq? core-eq? test)
                ; Fast path (eq? is rewritten by the compiler):
                (let loop ([bucket bucket0])
                  (if (null? bucket)
                      (begin
                        (##sys#setslot vec hshidx (cons (cons key val) bucket0))
                        (##sys#setislot ht 2 newsiz) )
                      (let ([pare (##sys#slot bucket 0)])
                         (if (eq? key (##sys#slot pare 0))
                             (##sys#setslot pare 1 val)
                             (loop (##sys#slot bucket 1)) ) ) ) )
                ; Slow path
                (let loop ([bucket bucket0])
                  (if (null? bucket)
                      (begin
                        (##sys#setslot vec hshidx (cons (cons key val) bucket0))
                        (##sys#setislot ht 2 newsiz) )
                      (let ([pare (##sys#slot bucket 0)])
                         (if (test key (##sys#slot pare 0))
                             (##sys#setslot pare 1 val)
                             (loop (##sys#slot bucket 1)) ) ) ) ) )
            (void) ) ) ) ) ) )

;; Hash-Table Reference:

(define hash-table-ref
  (getter-with-setter
    (let ([core-eq? eq?])
      (lambda (ht key #!optional (def (lambda ()
				        (##sys#signal-hook #:access-error
				         'hash-table-ref
				         "hash-table does not contain key" key ht))))
        (##sys#check-structure ht 'hash-table 'hash-table-ref)
        (##sys#check-closure def 'hash-table-ref)
        (let  ([vec (##sys#slot ht 1)]
	       [test (##sys#slot ht 3)] )
          (let* ([hash (##sys#slot ht 4)]
		 [hshidx (hash key (##sys#size vec))] )
	    (if (eq? core-eq? test)
	        ; Fast path (eq? is rewritten by the compiler):
	        (let loop ([bucket (##sys#slot vec hshidx)])
		  (if (null? bucket)
		      (def)
		      (let ([pare (##sys#slot bucket 0)])
		        (if (eq? key (##sys#slot pare 0))
			    (##sys#slot pare 1)
			    (loop (##sys#slot bucket 1)) ) ) ) )
	        ; Slow path
	        (let loop ([bucket (##sys#slot vec hshidx)])
		  (if (null? bucket)
		      (def)
		      (let ([pare (##sys#slot bucket 0)])
		        (if (test key (##sys#slot pare 0))
			    (##sys#slot pare 1)
			    (loop (##sys#slot bucket 1)) ) ) ) ) ) ) ) ) )
   hash-table-set!) )

(define hash-table-ref/default
  (let ([core-eq? eq?])
    (lambda (ht key def)
      (##sys#check-structure ht 'hash-table 'hash-table-ref/default)
      (let  ([vec (##sys#slot ht 1)]
	     [test (##sys#slot ht 3)] )
	(let* ([hash (##sys#slot ht 4)]
	       [hshidx (hash key (##sys#size vec))] )
	   (if (eq? core-eq? test)
	       ; Fast path (eq? is rewritten by the compiler):
	       (let loop ([bucket (##sys#slot vec hshidx)])
		 (if (null? bucket)
		     def
		     (let ([pare (##sys#slot bucket 0)])
		       (if (eq? key (##sys#slot pare 0))
			   (##sys#slot pare 1)
			   (loop (##sys#slot bucket 1)) ) ) ) )
	       ; Slow path
	       (let loop ([bucket (##sys#slot vec hshidx)])
		 (if (null? bucket)
		     def
		     (let ([pare (##sys#slot bucket 0)])
		       (if (test key (##sys#slot pare 0))
			   (##sys#slot pare 1)
			   (loop (##sys#slot bucket 1)) ) ) ) ) ) ) ) ) ) )

(define hash-table-exists?
  (let ([core-eq? eq?])
    (lambda (ht key)
      (##sys#check-structure ht 'hash-table 'hash-table-exists?)
      (let  ([vec (##sys#slot ht 1)]
	     [test (##sys#slot ht 3)] )
	(let* ([hash (##sys#slot ht 4)]
	       [hshidx (hash key (##sys#size vec))] )
	  (if (eq? core-eq? test)
	       ; Fast path (eq? is rewritten by the compiler):
	       (let loop ([bucket (##sys#slot vec hshidx)])
		 (and (not (null? bucket))
		      (let ([pare (##sys#slot bucket 0)])
		        (or (eq? key (##sys#slot pare 0))
			    (loop (##sys#slot bucket 1)) ) ) ) )
	       ; Slow path
	       (let loop ([bucket (##sys#slot vec hshidx)])
		 (and (not (null? bucket))
		      (let ([pare (##sys#slot bucket 0)])
		        (or (test key (##sys#slot pare 0))
			    (loop (##sys#slot bucket 1)) ) ) ) ) ) ) ) ) ) )

;; hash-table-delete!:

(define hash-table-delete!
  (let ([core-eq? eq?])
    (lambda (ht key)
      (##sys#check-structure ht 'hash-table 'hash-table-delete!)
      (let* ([vec (##sys#slot ht 1)]
             [len (##sys#size vec)]
             [hash (##sys#slot ht 4)]
             [hshidx (hash key len)] )
        (let ([test (##sys#slot ht 3)]
              [newsiz (fx- (##sys#slot ht 2) 1)]
              [bucket0 (##sys#slot vec hshidx)] )
          (if (eq? core-eq? test)
              ; Fast path (eq? is rewritten by the compiler):
              (let loop ([prev #f] [bucket bucket0])
                (and (not (null? bucket))
                     (let ([pare (##sys#slot bucket 0)]
                           [nxt (##sys#slot bucket 1)])
                       (if (eq? key (##sys#slot pare 0))
                           (begin
                             (if prev
                                 (##sys#setslot prev 1 nxt)
                                 (##sys#setslot vec hshidx nxt) )
                             (##sys#setislot ht 2 newsiz)
                             #t )
                           (loop bucket nxt) ) ) ) )
              ; Slow path
              (let loop ([prev #f] [bucket bucket0])
                (and (not (null? bucket))
                     (let ([pare (##sys#slot bucket 0)]
                           [nxt (##sys#slot bucket 1)])
                       (if (test key (##sys#slot pare 0))
                           (begin
                             (if prev
                                 (##sys#setslot prev 1 nxt)
                                 (##sys#setslot vec hshidx nxt) )
                             (##sys#setislot ht 2 newsiz)
                             #t )
                           (loop bucket nxt) ) ) ) ) ) ) ) ) ) )

;; hash-table-remove!:

(define (hash-table-remove! ht func)
  (##sys#check-structure ht 'hash-table 'hash-table-remove!)
  (##sys#check-closure func 'hash-table-remove!)
  (let* ([vec (##sys#slot ht 1)]
	 [len (##sys#size vec)] )
    (let ([siz (##sys#slot ht 2)])
      (do ([i 0 (fx+ i 1)])
	  [(fx>= i len) (##sys#setislot ht 2 siz)]
	(let loop ([prev #f] [bucket (##sys#slot vec i)])
	  (and (not (null? bucket))
	       (let ([pare (##sys#slot bucket 0)]
		     [nxt (##sys#slot bucket 1)])
		 (if (func (##sys#slot pare 0) (##sys#slot pare 1))
		     (begin
		       (if prev
			   (##sys#setslot prev 1 nxt)
			   (##sys#setslot vec i nxt) )
		       (set! siz (fx- siz 1))
		       #t )
		     (loop bucket nxt ) ) ) ) ) ) ) ) )

;; hash-table-clear!:

(define (hash-table-clear! ht)
  (##sys#check-structure ht 'hash-table 'hash-table-clear!)
  (vector-fill! (##sys#slot ht 1) '())
  (##sys#setislot ht 2 0) )

;; Hash Table Merge:

(define (*hash-table-merge! ht1 ht2)
  (let* ([vec (##sys#slot ht2 1)]
	 [len (##sys#size vec)] )
    (do ([i 0 (fx+ i 1)])
	[(fx>= i len) ht1]
      (do ([lst (##sys#slot vec i) (##sys#slot lst 1)])
	  [(null? lst)]
	(let ([b (##sys#slot lst 0)])
	  (*hash-table-update!/default ht1 (##sys#slot b 0) identity (##sys#slot b 1)) ) ) ) ) )

(define (hash-table-merge! ht1 ht2)
  (##sys#check-structure ht1 'hash-table 'hash-table-merge!)
  (##sys#check-structure ht2 'hash-table 'hash-table-merge!)
  (*hash-table-merge! ht1 ht2) )

(define (hash-table-merge ht1 ht2)
  (##sys#check-structure ht1 'hash-table 'hash-table-merge)
  (##sys#check-structure ht2 'hash-table 'hash-table-merge)
  (*hash-table-merge! (*hash-table-copy ht1) ht2) )

;; Hash-Table <-> Association-List:

(define (hash-table->alist ht)
  (##sys#check-structure ht 'hash-table 'hash-table->alist)
  (let* ([vec (##sys#slot ht 1)]
	 [len (##sys#size vec)] )
    (let loop ([i 0] [lst '()])
      (if (fx>= i len)
	  lst
	  (let loop2 ([bucket (##sys#slot vec i)]
		      [lst lst])
	    (if (null? bucket)
		(loop (fx+ i 1) lst)
		(loop2 (##sys#slot bucket 1)
		       (let ([x (##sys#slot bucket 0)])
			 (cons (cons (##sys#slot x 0) (##sys#slot x 1)) lst) ) ) ) ) ) ) ) )

(define alist->hash-table
  (let ([make-hash-table make-hash-table])
    (lambda (alist . rest)
      (##sys#check-list alist 'alist->hash-table)
      (let ([ht (apply make-hash-table rest)])
	(for-each
	 (lambda (x)
	   (##sys#check-pair x 'alist->hash-table)
	   (*hash-table-update!/default  ht (##sys#slot x 0) identity (##sys#slot x 1)) )
	 alist)
	ht ) ) ) )

;; Hash-Table Keys & Values:

(define (hash-table-keys ht)
  (##sys#check-structure ht 'hash-table 'hash-table-keys)
  (let* ([vec (##sys#slot ht 1)]
	 [len (##sys#size vec)] )
    (let loop ([i 0] [lst '()])
      (if (fx>= i len)
	  lst
	  (let loop2 ([bucket (##sys#slot vec i)]
		      [lst lst])
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
	  (let loop2 ([bucket (##sys#slot vec i)]
		      [lst lst])
	    (if (null? bucket)
		(loop (fx+ i 1) lst)
		(loop2 (##sys#slot bucket 1)
		       (let ([x (##sys#slot bucket 0)])
			 (cons (##sys#slot x 1) lst) ) ) ) ) ) ) ) )

;; Mapping Over Hash-Table Keys & Values:
;;
;; hash-table-for-each:
;; hash-table-walk:
;; hash-table-fold:
;; hash-table-map:

(define (*hash-table-for-each ht proc)
  (let* ([vec (##sys#slot ht 1)]
	 [len (##sys#size vec)] )
    (do ([i 0 (fx+ i 1)] )
	[(fx>= i len)]
      (##sys#for-each (lambda (bucket)
			(proc (##sys#slot bucket 0) (##sys#slot bucket 1)) )
		      (##sys#slot vec i)) ) ) )

(define (*hash-table-fold ht func init)
  (let* ([vec (##sys#slot ht 1)]
	 [len (##sys#size vec)] )
    (let loop ([i 0] [acc init])
      (if (fx>= i len)
	  acc
	  (let fold2 ([bucket (##sys#slot vec i)]
		      [acc acc])
	    (if (null? bucket)
		(loop (fx+ i 1) acc)
		(let ([pare (##sys#slot bucket 0)])
		  (fold2 (##sys#slot bucket 1)
			 (func (##sys#slot pare 0) (##sys#slot pare 1) acc) ) ) ) ) ) ) ) )

(define (hash-table-fold ht func init)
  (##sys#check-structure ht 'hash-table 'hash-table-fold)
  (##sys#check-closure func 'hash-table-fold)
  (*hash-table-fold ht func init) )

(define (hash-table-for-each ht proc)
  (##sys#check-structure ht 'hash-table 'hash-table-for-each)
  (##sys#check-closure proc 'hash-table-for-each)
  (*hash-table-for-each ht proc) )

(define (hash-table-walk ht proc)
  (##sys#check-structure ht 'hash-table 'hash-table-walk)
  (##sys#check-closure proc 'hash-table-walk)
  (*hash-table-for-each ht proc) )

(define (hash-table-map ht func)
  (##sys#check-structure ht 'hash-table 'hash-table-map)
  (##sys#check-closure func 'hash-table-map)
  (*hash-table-fold ht (lambda (k v a) (cons (func k v) a)) '()) )
