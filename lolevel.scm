;;;; lolevel.scm - Low-level routines for CHICKEN
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
  (unit lolevel)
  (uses extras)
  (usual-integrations)
  (disable-warning var redef)
  (hide ipc-hook-0 xproc-tag)
  (foreign-declare #<<EOF
#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)
# include <sys/types.h>
#endif
#ifndef C_NONUNIX
# include <sys/mman.h>
#endif

#define C_pointer_to_object(ptr)   ((C_word*)C_block_item(ptr, 0))
#define C_w2b(x)                   C_fix(C_wordstobytes(C_unfix(x)))
#define C_pointer_eqp(x, y)        C_mk_bool(C_c_pointer_nn(x) == C_c_pointer_nn(y))
EOF
) )

(cond-expand
 [paranoia]
 [else
  (declare
    (no-bound-checks)
    (no-procedure-checks-for-usual-bindings)
    (bound-to-procedure
     ##sys#symbol-hash-toplevel-binding? ##sys#make-locative ##sys#become! make-hash-table 
     hash-table-ref/default ##sys#make-string make-vector hash-table-set! hash-table-set!
     make-property-condition make-composite-condition signal ##sys#set-pointer-address! ##sys#make-vector
     ##sys#make-pointer byte-vector-fill! make-string make-byte-vector ##sys#not-a-proper-list-error ##sys#check-pointer
     ##sys#locative? ##sys#bytevector?
     extend-procedure ##sys#lambda-decoration ##sys#decorate-lambda ##sys#make-tagged-pointer ##sys#check-special
     ##sys#vector->closure! ##sys#error ##sys#signal-hook ##sys#address->pointer ##sys#pointer->address) ) ] )

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
    (define-macro (##sys#check-pointer . _) '(##core#undefined))
    (define-macro (##sys#check-special . _) '(##core#undefined))
    (define-macro (##sys#check-byte-vector . _) '(##core#undefined)) ) ]
 [else
  (declare (emit-exports "lolevel.exports"))] )

(register-feature! 'lolevel)


;;; Move arbitrary blocks of memory around:

(define move-memory!
  (let ([memmove1 (foreign-lambda void "C_memmove" c-pointer c-pointer int)]
	[memmove2 (foreign-lambda void "C_memmove" c-pointer scheme-pointer int)]
	[memmove3 (foreign-lambda void "C_memmove" scheme-pointer c-pointer int)]
	[memmove4 (foreign-lambda void "C_memmove" scheme-pointer scheme-pointer int)]
	[slot1structs '(mmap u8vector u16vector u32vector s8vector s16vector s32vector f32vector f64vector)] )
    (lambda (from to . n)
      (define (err) (##sys#error 'move-memory! "need number of bytes to move" from to))
      (define (xerr x) (##sys#signal-hook #:type-error 'move-memory! "invalid argument type" x))
      (define (checkn n nmax)
	(if (cond-expand [unsafe #t] [else (fx<= n nmax)])
	    n
	    (##sys#error 'move-memory! "number of bytes to move too large" from to n nmax) ) )
      (define (checkn2 n nmax nmax2)
	(if (cond-expand [unsafe #t] [else (and (fx<= n nmax) (fx<= n nmax2))])
	    n
	    (##sys#error 'move-memory! "number of bytes to move too large" from to n nmax nmax2) ) )
      (let move ([from from] [to to])
	(cond [(##sys#generic-structure? from)
	       (if (memq (##sys#slot from 0) slot1structs)
		   (move (##sys#slot from 1) to)
		   (xerr from) ) ]
	      [(##sys#generic-structure? to)
	       (if (memq (##sys#slot to 0) slot1structs)
		   (move from (##sys#slot to 1))
		   (xerr to) ) ]
	      [(or (##sys#pointer? from) (##sys#locative? from))
	       (cond [(or (##sys#pointer? to) (##sys#locative? to)) (memmove1 to from (:optional n (err)))]
		     [(or (##sys#bytevector? to) (string? to))
		      (memmove3 to from (checkn (:optional n (err)) (##sys#size to))) ]
		     [else (xerr to)] ) ]
	      [(or (##sys#bytevector? from) (string? from))
	       (let ([nfrom (##sys#size from)])
		 (cond [(or (##sys#pointer? to) (##sys#locative? to))
			(memmove2 to from (checkn (:optional n nfrom) nfrom))]
		       [(or (##sys#bytevector? to) (string? to))
			(memmove4 to from (checkn2 (:optional n nfrom) nfrom (##sys#size to))) ]
		       [else (xerr to)] ) ) ]
	      [else (xerr from)] ) ) ) ) )


;;; Pointer operations:

(define (##sys#check-pointer ptr loc)
  (unless (and (##core#inline "C_blockp" ptr)
	       (or (##core#inline "C_pointerp" ptr)
		   (##core#inline "C_swigpointerp" ptr)
		   (##core#inline "C_taggedpointerp" ptr) ) )
    (##sys#signal-hook #:type-error loc "bad argument type - not a pointer" ptr) ) )

(define null-pointer ##sys#null-pointer)

(define (pointer? x)
  (and (##core#inline "C_blockp" x)
       (or (##core#inline "C_pointerp" x)
	   (##core#inline "C_taggedpointerp" x) ) ) )

(define (address->pointer addr)
  (cond-expand
   [(not unsafe)
    (when (not (integer? addr))
      (##sys#signal-hook #:type-error 'address->pointer "bad argument type - not an integer" addr) ) ]
   [else] )
  (##sys#address->pointer addr) )

(define (pointer->address ptr)
  (##sys#check-special ptr 'pointer->address)
  (##sys#pointer->address ptr) )

(define (null-pointer? ptr)
  (##sys#check-special ptr 'null-pointer?)
  (eq? 0 (##sys#pointer->address ptr) ) )

(define (object->pointer x)
  (and (##core#inline "C_blockp" x)
       ((foreign-lambda* nonnull-c-pointer ((scheme-object x))
	  "return((void *)x);") 
	x) ) )

(define (pointer->object ptr)
  (##sys#check-pointer ptr 'pointer->object)
  (##core#inline "C_pointer_to_object" ptr) )

(define (pointer=? p1 p2)
  (##sys#check-special p1 'pointer=?)
  (##sys#check-special p2 'pointer=?)
  (##core#inline "C_pointer_eqp" p1 p2) )

(define allocate (foreign-lambda c-pointer "C_malloc" int))
(define free (foreign-lambda void "C_free" c-pointer))

(define align-to-word
  (let ([align (foreign-lambda integer "C_align" integer)])
    (lambda (x)
      (cond [(number? x) (align x)]
	    [(and (##core#inline "C_blockp" x) (##core#inline "C_specialp" x))
	     (##sys#address->pointer (align (##sys#pointer->address x))) ]
	    [else (##sys#signal-hook #:type-error 'align-to-word "bad argument type - not a pointer or fixnum" x)] ) ) ) )

(define pointer-offset
  (foreign-lambda* nonnull-c-pointer ([c-pointer ptr] [integer off])
    "return((unsigned char *)ptr + off);") )

(define pointer-u8-set! (foreign-lambda* void ([c-pointer p] [int n]) "*((unsigned char *)p) = n;"))
(define pointer-s8-set! (foreign-lambda* void ([c-pointer p] [int n]) "*((char *)p) = n;"))
(define pointer-u16-set! (foreign-lambda* void ([c-pointer p] [int n]) "*((unsigned short *)p) = n;"))
(define pointer-s16-set! (foreign-lambda* void ([c-pointer p] [int n]) "*((short *)p) = n;"))
(define pointer-u32-set! (foreign-lambda* void ([c-pointer p] [int n]) "*((C_u32 *)p) = n;"))
(define pointer-s32-set! (foreign-lambda* void ([c-pointer p] [int n]) "*((C_s32 *)p) = n;"))
(define pointer-f32-set! (foreign-lambda* void ([c-pointer p] [double n]) "*((float *)p) = n;"))
(define pointer-f64-set! (foreign-lambda* void ([c-pointer p] [float n]) "*((double *)p) = n;"))

(define pointer-u8-ref
  (getter-with-setter
   (foreign-lambda* int ([c-pointer p]) "return(*((unsigned char *)p));")
   pointer-u8-set!) )

(define pointer-s8-ref
  (getter-with-setter
   (foreign-lambda* int ([c-pointer p]) "return(*((char *)p));")
   pointer-s8-set!) )

(define pointer-u16-ref
  (getter-with-setter
   (foreign-lambda* int ([c-pointer p]) "return(*((unsigned short *)p));")
   pointer-u16-set!) )

(define pointer-s16-ref
  (getter-with-setter
   (foreign-lambda* int ([c-pointer p]) "return(*((short *)p));")
   pointer-s6-set!) )

(define pointer-u32-ref
  (getter-with-setter
   (foreign-lambda* integer ([c-pointer p]) "return(*((C_u32 *)p));")
   pointer-u32-set!) )

(define pointer-s32-ref
  (getter-with-setter
   (foreign-lambda* integer ([c-pointer p]) "return(*((C_s32 *)p));")
   pointer-s32-set!) )

(define pointer-f32-ref
  (getter-with-setter
   (foreign-lambda* float ([c-pointer p]) "return(*((float *)p));")
   pointer-f32-set!) )

(define pointer-f64-ref
  (getter-with-setter
   (foreign-lambda* double ([c-pointer p]) "return(*((double *)p));")
   pointer-f64-set!) )

(define (tag-pointer ptr tag)
  (let ([tp (##sys#make-tagged-pointer tag)])
    (if (and (##core#inline "C_blockp" ptr) (##core#inline "C_specialp" ptr))
	(##core#inline "C_copy_pointer" ptr tp)
	(##sys#signal-hook #:type-error 'tag-pointer "bad argument type - not a pointer" ptr) )
    tp) )

(define (tagged-pointer? x tag)
  (and (##core#inline "C_blockp" x) 
       (##core#inline "C_taggedpointerp" x)
       (equal? tag (##sys#slot x 1)) ) )

(define (pointer-tag x)
  (if (and (##core#inline "C_blockp" x) (##core#inline "C_specialp" x))
      (and (##core#inline "C_taggedpointerp" x)
	   (##sys#slot x 1) )
      (##sys#signal-hook #:type-error 'tag-pointer "bad argument type - not a pointer" x) ) )


;;; Procedures extended with data:

(define xproc-tag (vector 'extended))

(define (extend-procedure proc data)
  #+(not unsafe)
  (unless (##core#inline "C_closurep" proc)
    (##sys#signal-hook #:type-error 'extend-procedure "bad argument type - not a procedure" proc) )
  (##sys#decorate-lambda
   proc
   (lambda (x) (and (pair? x) (eq? xproc-tag (##sys#slot x 0)))) 
   (lambda (x i)
     (##sys#setslot x i (cons xproc-tag data))
     x) ) )

(define (extended-procedure? x)
  (and (##core#inline "C_blockp" x)
       (##core#inline "C_closurep" x)
       (##sys#lambda-decoration x (lambda (x) (and (pair? x) (eq? xproc-tag (##sys#slot x 0)))))
       #t) )

(define (procedure-data x)
  (and (##core#inline "C_blockp" x)
       (##core#inline "C_closurep" x)
       (and-let* ((d (##sys#lambda-decoration x (lambda (x) (and (pair? x) (eq? xproc-tag (##sys#slot x 0)))))))
	 (##sys#slot d 1) ) ) )

(define set-procedure-data!
  (let ((extend-procedure extend-procedure))
    (lambda (proc x)
      (let ((p2 (extend-procedure proc x)))
	(if (eq? p2 proc)
	    proc
	    (##sys#signal-hook #:type-error 'set-procedure-data! "bad argument type - not an extended procedure" proc) ) ) ) ) )


;;; Bytevector stuff:

(define (byte-vector? x)
  (and (##core#inline "C_blockp" x)
       (##core#inline "C_bytevectorp" x) ) )

(define (byte-vector-fill! bv n)
  (##sys#check-byte-vector bv 'byte-vector-fill!)
  (##sys#check-exact n 'byte-vector-fill!)
  (let ([len (##sys#size bv)])
    (do ([i 0 (fx+ i 1)])
	((fx>= i len))
      (##sys#setbyte bv i n) ) ) )

(define make-byte-vector
  (let ([byte-vector-fill! byte-vector-fill!])
    (lambda (size . init)
      (##sys#check-exact size 'make-byte-vector)
      (let ([bv (##sys#allocate-vector size #t #f #t)])
	(##core#inline "C_string_to_bytevector" bv)
	(when (pair? init) (byte-vector-fill! bv (car init)))
	bv) ) ) )

(define byte-vector
  (let ([make-byte-vector make-byte-vector])
    (lambda bytes
      (let* ([n (length bytes)]
	     [bv (make-byte-vector n)] )
	(do ([i 0 (fx+ i 1)]
	     [bytes bytes (##sys#slot bytes 1)] )
	    ((fx>= i n) bv)
	  (##sys#setbyte bv i (##sys#slot bytes 0)) ) ) ) ) )

(define byte-vector-set!
  (lambda (bv i x)
    (##sys#check-byte-vector bv 'byte-vector-set!)
    (##sys#check-exact i 'byte-vector-set!)
    (##sys#check-exact x 'byte-vector-set!)
    (let ([n (##sys#size bv)])
      (if (or (fx< i 0) (fx>= i n))
	  (##sys#error 'byte-vector-set! "out of range" bv i)
	  (##sys#setbyte bv i x) ) ) ) )

(define byte-vector-ref
  (getter-with-setter
   (lambda (bv i)
     (##sys#check-byte-vector bv 'byte-vector-ref)
     (##sys#check-exact i 'byte-vector-ref)
     (let ([n (##sys#size bv)])
       (if (or (fx< i 0) (fx>= i n))
	   (##sys#error 'byte-vector-ref "out of range" bv i)
	   (##sys#byte bv i) ) ) )
   byte-vector-set!) )

(define (byte-vector->list bv)
  (##sys#check-byte-vector bv 'byte-vector->list)
  (let ([len (##sys#size bv)])
    (let loop ([i 0])
      (if (fx>= i len)
	  '()
	  (cons (##sys#byte bv i) 
		(loop (fx+ i 1)) ) ) ) ) )

(define list->byte-vector
  (let ([make-byte-vector make-byte-vector])
    (lambda (lst)
      (##sys#check-list lst 'list->byte-vector)
      (let* ([n (length lst)]
	     [v (make-byte-vector n)] )
	(do ([p lst (##sys#slot p 1)]
	     [i 0 (fx+ i 1)] )
	    ((eq? p '()) v)
	  (if (pair? p)
	      (let ([b (##sys#slot p 0)])
		(##sys#check-exact b 'list->byte-vector)
		(##sys#setbyte v i b) )
	      (##sys#not-a-proper-list-error lst) ) ) ) ) ) )

(define string->byte-vector
  (let ([make-byte-vector make-byte-vector])
    (lambda (s)
      (##sys#check-string s 'string->byte-vector)
      (let* ([n (##sys#size s)]
	     [bv (make-byte-vector n)] )
	(##core#inline "C_copy_memory" bv s n) 
	bv) ) ) )

(define byte-vector->string
  (let ([make-string make-string])
    (lambda (bv)
      (##sys#check-byte-vector bv 'byte-vector->string)
      (let* ([n (##sys#size bv)]
	     [s (make-string n)] )
	(##core#inline "C_copy_memory" s bv n) 
	s) ) ) )

(define (byte-vector-length bv)
  (##sys#check-byte-vector bv 'byte-vector-length)
  (##sys#size bv) )

(define-foreign-variable _c_header_size_mask int "C_HEADER_SIZE_MASK")

(let ([byte-vector-fill! byte-vector-fill!]
      [malloc
       (foreign-lambda* scheme-object ((int size))
	 "char *bv;
           if((bv = (char *)C_malloc(size + 3 + sizeof(C_header))) == NULL) return(C_SCHEME_FALSE);
           bv = (char *)C_align((C_word)bv);
           ((C_SCHEME_BLOCK *)bv)->header = C_BYTEVECTOR_TYPE | size;
           return((C_word)bv);") ] )
  (define (make size init alloc loc)
    (##sys#check-exact size loc)
    (if (fx> size _c_header_size_mask)
	(##sys#signal-hook #:bounds-error loc "out of range" size _c_header_size_mask)
	(let ([bv (alloc size)])
	  (cond [bv
		 (when (pair? init) (byte-vector-fill! bv (##sys#slot init 0)))
		 bv]
		[else (##sys#signal-hook #:runtime-error "can not allocate statically allocated bytevector" size)] ) ) ) )
  (set! make-static-byte-vector (lambda (size . init) (make size init malloc 'make-static-byte-vector))))

(define static-byte-vector->pointer 
  (lambda (bv)
    (##sys#check-byte-vector bv 'static-byte-vector->pointer)
    (if (##core#inline "C_permanentp" bv)
	(let ([p (##sys#make-pointer)])
	  (##core#inline "C_pointer_to_block" p bv)
	  p)
	(##sys#error 'static-byte-vector->pointer "can not coerce non-static bytevector" bv) ) ) )


;;; Accessors for arbitrary block objects:

(define block-set! ##sys#block-set!)
(define block-ref (getter-with-setter ##sys#block-ref ##sys#block-set!))

(define number-of-slots 
  (lambda (x)
    (when (or (not (##core#inline "C_blockp" x)) 
	      (##core#inline "C_specialp" x)
	      (##core#inline "C_byteblockp" x) )
      (##sys#signal-hook #:type-error 'number-of-slots "slots not accessible" x) )
    (##sys#size x) ) )

(define (number-of-bytes x)
  (cond [(not (##core#inline "C_blockp" x))
	 (##sys#signal-hook #:type-error 'number-of-bytes "can not compute number of bytes of immediate object" x) ]
	[(##core#inline "C_byteblockp" x) (##sys#size x)]
	[else (##core#inline "C_w2b" (##sys#size x))] ) )


;;; Record objects:

(define (make-record-instance type . args)
  (##sys#check-symbol type 'make-record-instance)
  (apply ##sys#make-structure type args) )

(define (record-instance? x)
  (and (##core#inline "C_blockp" x)
       (##core#inline "C_structurep" x) ) )

(define (record->vector x)
  (if (and (not (##sys#immediate? x)) (##sys#generic-structure? x))
      (let* ([n (##sys#size x)]
	     [v (##sys#make-vector n)] )
	(do ([i 0 (fx+ i 1)])
	    ((fx>= i n) v)
	  (##sys#setslot v i (##sys#slot x i)) ) )
      (##sys#signal-hook #:type-error 'record->vector "bad argument type - not a record structure" x) ) )


;;; Copy arbitrary object:

(define object-copy
  (let ([make-vector make-vector])
    (lambda (x)
      (let copy ([x x])
	(cond [(not (##core#inline "C_blockp" x)) x]
	      [(symbol? x) (##sys#intern-symbol (##sys#slot x 1))]
	      [else
	       (let* ([n (##sys#size x)]
		      [words (if (##core#inline "C_byteblockp" x) (##core#inline "C_words" n) n)]
		      [y (##core#inline "C_copy_block" x (make-vector words))] ) ; shamelessly mutating vector into something else
		 (unless (or (##core#inline "C_byteblockp" x) (symbol? x))
		   (do ([i (if (##core#inline "C_specialp" x) 1 0) (fx+ i 1)])
		       ((fx>= i n))
		     (##sys#setslot y i (copy (##sys#slot y i))) ) )
		 y) ] ) ) ) ) )


;;; Evict objects into static memory:

(define (object-evicted? x) (##core#inline "C_permanentp" x))

(define object-evict
  (let ([make-hash-table make-hash-table]
	[hash-table-ref/default hash-table-ref/default]
	[hash-table-set! hash-table-set!] )
    (lambda (x . allocator)
      (let ([allocator 
	     (if (pair? allocator) 
		 (car allocator)
		 (foreign-lambda c-pointer "C_malloc" int) ) ] 
	    [tab (make-hash-table eq?)] )
	(let evict ([x x])
	  (cond [(not (##core#inline "C_blockp" x)) x]
		[(hash-table-ref/default tab x #f)]
		[else
		 (let* ([n (##sys#size x)]
			[bytes (if (##core#inline "C_byteblockp" x) (align-to-word n) (##core#inline "C_bytes" n))]
			[y (##core#inline "C_evict_block" x (allocator (fx+ bytes (##core#inline "C_bytes" 1))))] )
		   (when (symbol? x) (##sys#setislot y 0 (##core#undefined)))
		   (hash-table-set! tab x y)
		   (unless (##core#inline "C_byteblockp" x)
		     (do ([i (if (or (##core#inline "C_specialp" x) (symbol? x)) 1 0) (fx+ i 1)])
			 ((fx>= i n))
		       ;; Note the use of `##sys#setislot' to avoid an entry in the mutations-table:
		       (##sys#setislot y i (evict (##sys#slot x i))) ) )
		   y) ] ) ) ) ) ) )

(define object-release
  (lambda (x . releaser)
    (let ([free (if (pair? releaser) 
		    (car releaser) 
		    (foreign-lambda void "C_free" c-pointer) ) ] )
      (let release ([x x])
	(cond [(not (##core#inline "C_blockp" x)) x]
	      [(not (##core#inline "C_permanentp" x)) x]
	      [else
	       (let ([n (##sys#size x)])
		 (unless (##core#inline "C_byteblockp" x)
		   (do ([i (if (##core#inline "C_specialp" x) 1 0) (fx+ i 1)])
		       ((fx>= i n))
		     (release (##sys#slot x i))) )
		 (free (##sys#address->pointer (##core#inline_allocate ("C_block_address" 4) x))) ) ] ) ) ) ) )

(define object-evict-to-location
  (let ([make-hash-table make-hash-table]
	[hash-table-ref/default hash-table-ref/default]
	[align-to-word align-to-word]
	[hash-table-set! hash-table-set!] )
    (lambda (x ptr . limit)
      (cond-expand
       [(not unsafe)
	(when (not (and (##core#inline "C_blockp" ptr) (##core#inline "C_specialp" ptr)))
	  (##sys#signal-hook #:type-error 'object-evict-to-location "bad argument type - not a pointer" ptr) ) ]
       [else] )
      (let* ([limit
	      (if (pair? limit)
		  (let ([limit (car limit)])
		    (##sys#check-exact limit 'object-evict-to-location)
		    limit)
		  #f) ]
	     [ptr2 (##sys#address->pointer (##sys#pointer->address ptr))]
	     [tab (make-hash-table eq?)]
	     [x2
	      (let evict ([x x])
		(cond [(not (##core#inline "C_blockp" x)) x]
		      [(hash-table-ref/default tab x #f)]
		      [else
		       (let* ([n (##sys#size x)]
			      [bytes 
			       (fx+ (if (##core#inline "C_byteblockp" x) (align-to-word n) (##core#inline "C_bytes" n))
				    (##core#inline "C_bytes" 1) ) ] )
			 (when limit
			   (set! limit (fx- limit bytes))
			   (when (fx< limit 0) 
			     (signal
			      (make-composite-condition
			       (make-property-condition
				'exn 'location 'object-evict-to-location
				'message "can not evict object - limit exceeded" 
				'arguments (list x limit))
			       (make-property-condition 'evict 'limit limit) ) ) ) )
			 (let ([y (##core#inline "C_evict_block" x ptr2)])
			   (when (symbol? x) (##sys#setislot y 0 (##core#undefined)))
			   (##sys#set-pointer-address! ptr2 (+ (##sys#pointer->address ptr2) bytes))
			   (hash-table-set! tab x y)
			   (unless (##core#inline "C_byteblockp" x)
			     (do ([i (if (or (##core#inline "C_specialp" x) (symbol? x))
					 1
					 0)
				     (fx+ i 1) ] )
				 ((fx>= i n))
			       (##sys#setislot y i (evict (##sys#slot x i))) ) ) ; see above
			   y) ) ] ) ) ] )
	(values x2 ptr2) ) ) ) )

(define object-size
  (let ([make-hash-table make-hash-table]
	[hash-table-ref/default hash-table-ref/default]
	[align-to-word align-to-word]
	[hash-table-set! hash-table-set!] )
    (lambda (x)
      (let ([tab (make-hash-table eq?)])
	(let evict ([x x])
	  (cond [(not (##core#inline "C_blockp" x)) 0]
		[(hash-table-ref/default tab x #f) 0]
		[else
		 (let* ([n (##sys#size x)]
			[bytes
			 (fx+ (if (##core#inline "C_byteblockp" x) (align-to-word n) (##core#inline "C_bytes" n))
			      (##core#inline "C_bytes" 1) ) ] )
		   (hash-table-set! tab x #t)
		   (unless (##core#inline "C_byteblockp" x)
		     (do ([i (if (or (##core#inline "C_specialp" x) (symbol? x))
				 1 
				 0)
			     (fx+ i 1) ] )
			 ((fx>= i n))
		       (set! bytes (fx+ (evict (##sys#slot x i)) bytes)) ) )
		   bytes) ] ) ) ) ) ) )

(define object-unevict
  (let ([make-vector make-vector]
	[make-hash-table make-hash-table]
	[hash-table-set! hash-table-set!]
	[hash-table-ref/default hash-table-ref/default] )
    (lambda (x #!optional (full #f))
      (define (err x)
	(##sys#signal-hook #:type-error 'object-unevict "can not copy object" x) )
      (let ([tab (make-hash-table eq?)])
	(let copy ([x x])
	  (cond [(not (##core#inline "C_blockp" x)) x]
		[(not (##core#inline "C_permanentp" x)) x]
		[(hash-table-ref/default tab x #f)]
		[(##core#inline "C_byteblockp" x) 
		 (if full
		     (let ([y (##core#inline "C_copy_block" x (##sys#make-string (##sys#size x)))])
		       (hash-table-set! tab x y)
		       y) 
		     x) ]
		[(symbol? x) 
		 (let ([y (##sys#intern-symbol (##sys#slot x 1))])
		   (hash-table-set! tab x y)
		   y) ]
		[else
		 (let* ([words (##sys#size x)]
			[y (##core#inline "C_copy_block" x (make-vector words))] )
		   (hash-table-set! tab x y)
		   (do ([i (if (##core#inline "C_specialp" x) 1 0) (fx+ i 1)])
		       ((fx>= i words))
		     (##sys#setslot y i (copy (##sys#slot y i))) )
		   y) ] ) ) ) ) ) )


;;; `become':

(define object-become! 
  (cond-expand
   [unsafe ##sys#become!]
   [else
    (lambda (lst)
      (##sys#check-list lst 'object-become!)
      (let loop ([lst lst])
	(cond [(null? lst)]
	      [(pair? lst)
	       (let ([a (##sys#slot lst 0)])
		 (##sys#check-pair a 'object-become!)
		 (unless (##core#inline "C_blockp" (##sys#slot a 0))
		   (##sys#signal-hook #:type-error 'object-become! "bad argument type - old item is immediate" a) )
		 (unless (##core#inline "C_blockp" (##sys#slot a 1))
		   (##sys#signal-hook #:type-error 'object-become! "bad argument type - new item is immediate" a) )
		 (loop (##sys#slot lst 1)) ) ]
	      [else (##sys#signal-hook #:type-error 'object-become! "bad argument type - not an a-list")] ) )
      (##sys#become! lst) ) ] ) )


;;; locatives:

(define (make-locative obj . index)
  (##sys#make-locative obj (:optional index 0) #f 'make-locative) )

(define (make-weak-locative obj . index)
  (##sys#make-locative obj (:optional index 0) #t 'make-weak-locative) )

(define (locative-set! x y) (##core#inline "C_i_locative_set" x y))
(define locative-ref (getter-with-setter (##core#primitive "C_locative_ref") locative-set!))
(define (locative->object x) (##core#inline "C_i_locative_to_object" x))
(define (locative? x) (and (##core#inline "C_blockp" x) (##core#inline "C_locativep" x)))


;;; Hooks:

(define ipc-hook-0 #f)			; we need this because `##sys#invalid-procedure-call-hook' may not have free variables.

(define (set-invalid-procedure-call-handler! proc)
  (unless (procedure? proc)
    (##sys#signal-hook #:type-error 'set-invalid-procedure-call-handler! "bad argument type - not a procedure" proc) )
  (set! ipc-hook-0 proc)
  (set! ##sys#invalid-procedure-call-hook 
    (lambda args
      (ipc-hook-0 ##sys#last-invalid-procedure args) ) ) )

(define invalid-procedure-call-handler set-invalid-procedure-call-handler!) ; DEPRECATED

(define (unbound-variable-value . val)
  (set! ##sys#unbound-variable-value-hook 
    (and (pair? val)
	 (vector (car val)) ) ) )


;;; Access computed globals:

(define (global-ref sym)
  (##sys#check-symbol sym 'global-ref)
  (##core#inline "C_retrieve" sym) )

(define (global-set! sym x)
  (##sys#check-symbol sym 'global-set!)
  (##sys#setslot sym 0 x) )

(define (global-bound? sym)
  (##sys#check-symbol sym 'global-bound?)
  (##sys#symbol-has-toplevel-binding? sym) )

(define (global-make-unbound! sym)
  (##sys#check-symbol sym 'global-make-unbound!)
  (##sys#setslot sym 0 (##sys#slot '##sys#arbitrary-unbound-symbol 0))
  sym)
