;;;; lolevel.scm - Low-level routines for CHICKEN
;
; Copyright (c) 2000-2007, Felix L. Winkelmann
; Copyright (c) 2008, The Chicken Team
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
  (unit lolevel)
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
#define C_memmove_o(to, from, n, toff, foff) C_memmove((char *)(to) + (toff), (char *)(from) + (foff), (n))
EOF
) )

(cond-expand
 [paranoia]
 [else
  (declare
    (no-bound-checks)
    (no-procedure-checks-for-usual-bindings)
    (bound-to-procedure
     ##sys#hash-table-ref ##sys#hash-table-set!
     ##sys#make-locative ##sys#become!
     ##sys#make-string
     make-property-condition make-composite-condition signal ##sys#set-pointer-address! ##sys#make-vector
     ##sys#make-pointer make-string make-byte-vector ##sys#not-a-proper-list-error ##sys#check-pointer
     ##sys#locative? ##sys#bytevector?
     extend-procedure ##sys#lambda-decoration ##sys#decorate-lambda ##sys#make-tagged-pointer ##sys#check-special
     ##sys#vector->closure! ##sys#error ##sys#signal-hook ##sys#address->pointer ##sys#pointer->address) ) ] )

(include "unsafe-declarations.scm")

(register-feature! 'lolevel)


;;; Move arbitrary blocks of memory around:

(define move-memory!
  (let ([memmove1 (foreign-lambda void "C_memmove_o" c-pointer c-pointer int int int)]
	[memmove2 (foreign-lambda void "C_memmove_o" c-pointer scheme-pointer int int int)]
	[memmove3 (foreign-lambda void "C_memmove_o" scheme-pointer c-pointer int int int)]
	[memmove4 (foreign-lambda void "C_memmove_o" scheme-pointer scheme-pointer int int int)]
	[slot1structs '(mmap u8vector u16vector u32vector s8vector s16vector s32vector f32vector f64vector)] )
    (lambda (from to #!optional n (foffset 0) (toffset 0))
      (define (err) (##sys#error 'move-memory! "need number of bytes to move" from to))
      (define (xerr x) (##sys#signal-hook #:type-error 'move-memory! "invalid argument type" x))
      (define (checkn n nmax off)
	(if (cond-expand [unsafe #t] [else (fx<= n (fx- nmax off))])
	    n
	    (##sys#error 'move-memory! "number of bytes to move too large" from to n nmax) ) )
      (define (checkn2 n nmax nmax2 off1 off2)
	(if (cond-expand [unsafe #t] [else (and (fx<= n (fx- nmax off1)) (fx<= n (fx- nmax2 off2)))])
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
	       (cond [(or (##sys#pointer? to) (##sys#locative? to))
		      (memmove1 to from (or n (err)) toffset foffset)]
		     [(or (##sys#bytevector? to) (string? to))
		      (memmove3 to from (checkn (or n (err)) (##sys#size to) toffset) toffset foffset) ]
		     [else (xerr to)] ) ]
	      [(or (##sys#bytevector? from) (string? from))
	       (let ([nfrom (##sys#size from)])
		 (cond [(or (##sys#pointer? to) (##sys#locative? to))
			(memmove2 to from (checkn (or n nfrom) nfrom foffset) toffset foffset)]
		       [(or (##sys#bytevector? to) (string? to))
			(memmove4 to from (checkn2 (or n nfrom) nfrom (##sys#size to) foffset toffset)
				  toffset foffset) ]
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
      (##sys#signal-hook #:type-error 'pointer-tag "bad argument type - not a pointer" x) ) )


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
	 (##sys#signal-hook #:type-error 'number-of-bytes "cannot compute number of bytes of immediate object" x) ]
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

(define (object-copy x)
  (let copy ([x x])
    (cond [(not (##core#inline "C_blockp" x)) x]
	  [(symbol? x) (##sys#intern-symbol (##sys#slot x 1))]
	  [else
	    (let* ([n (##sys#size x)]
		   [words (if (##core#inline "C_byteblockp" x) (##core#inline "C_words" n) n)]
		   [y (##core#inline "C_copy_block" x (##sys#make-vector words))] )
              (unless (or (##core#inline "C_byteblockp" x) (symbol? x))
		(do ([i (if (##core#inline "C_specialp" x) 1 0) (fx+ i 1)])
		    ((fx>= i n))
		  (##sys#setslot y i (copy (##sys#slot y i))) ) )
	      y) ] ) ) )


;;; Evict objects into static memory:

(define-constant evict-table-size 301)

(define (object-evicted? x) (##core#inline "C_permanentp" x))

(define object-evict
    (lambda (x . allocator)
      (let ([allocator 
	     (if (pair? allocator) 
		 (car allocator)
		 (foreign-lambda c-pointer "C_malloc" int) ) ] 
	    [tab (##sys#make-vector evict-table-size '())] )
	(let evict ([x x])
	  (cond [(not (##core#inline "C_blockp" x)) x]
		[(##sys#hash-table-ref tab x)]
		[else
		 (let* ([n (##sys#size x)]
			[bytes (if (##core#inline "C_byteblockp" x) (align-to-word n) (##core#inline "C_bytes" n))]
			[y (##core#inline "C_evict_block" x (allocator (fx+ bytes (##core#inline "C_bytes" 1))))] )
		   (when (symbol? x) (##sys#setislot y 0 (##core#undefined)))
		   (##sys#hash-table-set! tab x y)
		   (unless (##core#inline "C_byteblockp" x)
		     (do ([i (if (or (##core#inline "C_specialp" x) (symbol? x)) 1 0) (fx+ i 1)])
			 ((fx>= i n))
		       ;; Note the use of `##sys#setislot' to avoid an entry in the mutations-table:
		       (##sys#setislot y i (evict (##sys#slot x i))) ) )
		   y) ] ) ) ) ) )

(define object-release
  (lambda (x . releaser)
    (let ((free (if (pair? releaser) 
		    (car releaser) 
		    (foreign-lambda void "C_free" c-pointer) ) ) 
	  (released '()))
      (let release ([x x])
	(cond [(not (##core#inline "C_blockp" x)) x]
	      [(not (##core#inline "C_permanentp" x)) x]
	      ((memq x released) x)
	      [else
	       (let ([n (##sys#size x)])
		 (set! released (cons x released))
		 (unless (##core#inline "C_byteblockp" x)
		   (do ([i (if (##core#inline "C_specialp" x) 1 0) (fx+ i 1)])
		       ((fx>= i n))
		     (release (##sys#slot x i))) )
		 (free (##sys#address->pointer (##core#inline_allocate ("C_block_address" 4) x))) ) ] ) ) ) ) )

(define object-evict-to-location
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
	     [tab (##sys#make-vector evict-table-size '())]
	     [x2
	      (let evict ([x x])
		(cond [(not (##core#inline "C_blockp" x)) x]
		      [(##sys#hash-table-ref tab x)]
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
				'message "cannot evict object - limit exceeded" 
				'arguments (list x limit))
			       (make-property-condition 'evict 'limit limit) ) ) ) )
			 (let ([y (##core#inline "C_evict_block" x ptr2)])
			   (when (symbol? x) (##sys#setislot y 0 (##core#undefined)))
			   (##sys#set-pointer-address! ptr2 (+ (##sys#pointer->address ptr2) bytes))
			   (##sys#hash-table-set! tab x y)
			   (unless (##core#inline "C_byteblockp" x)
			     (do ([i (if (or (##core#inline "C_specialp" x) (symbol? x))
					 1
					 0)
				     (fx+ i 1) ] )
				 ((fx>= i n))
			       (##sys#setislot y i (evict (##sys#slot x i))) ) ) ; see above
			   y) ) ] ) ) ] )
	(values x2 ptr2) ) ) )

(define object-size
    (lambda (x)
      (let ([tab (##sys#make-vector evict-table-size '())])
	(let evict ([x x])
	  (cond [(not (##core#inline "C_blockp" x)) 0]
		[(##sys#hash-table-ref tab x) 0]
		[else
		 (let* ([n (##sys#size x)]
			[bytes
			 (fx+ (if (##core#inline "C_byteblockp" x) (align-to-word n) (##core#inline "C_bytes" n))
			      (##core#inline "C_bytes" 1) ) ] )
		   (##sys#hash-table-set! tab x #t)
		   (unless (##core#inline "C_byteblockp" x)
		     (do ([i (if (or (##core#inline "C_specialp" x) (symbol? x))
				 1 
				 0)
			     (fx+ i 1) ] )
			 ((fx>= i n))
		       (set! bytes (fx+ (evict (##sys#slot x i)) bytes)) ) )
		   bytes) ] ) ) ) ) )

(define object-unevict
    (lambda (x #!optional (full #f))
      (define (err x)
	(##sys#signal-hook #:type-error 'object-unevict "cannot copy object" x) )
      (let ([tab (##sys#make-vector evict-table-size '())])
	(let copy ([x x])
	  (cond [(not (##core#inline "C_blockp" x)) x]
		[(not (##core#inline "C_permanentp" x)) x]
		[(##sys#hash-table-ref tab x)]
		[(##core#inline "C_byteblockp" x) 
		 (if full
		     (let ([y (##core#inline "C_copy_block" x (##sys#make-string (##sys#size x)))])
		       (##sys#hash-table-set! tab x y)
		       y) 
		     x) ]
		[(symbol? x) 
		 (let ([y (##sys#intern-symbol (##sys#slot x 1))])
		   (##sys#hash-table-set! tab x y)
		   y) ]
		[else
		 (let* ([words (##sys#size x)]
			[y (##core#inline "C_copy_block" x (##sys#make-vector words))] )
		   (##sys#hash-table-set! tab x y)
		   (do ([i (if (##core#inline "C_specialp" x) 1 0) (fx+ i 1)])
		       ((fx>= i words))
		     (##sys#setslot y i (copy (##sys#slot y i))) )
		   y) ] ) ) ) ) )


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

(define (mutate-procedure old proc)
  (unless (##core#check (procedure? old))
    (##sys#signal-hook #:type-error 'mutate-procedure "bad argument type - not a procedure" old))
  (let* ((n (##sys#size old))
	 (words (##core#inline "C_words" n))
	 (y (##core#inline "C_copy_block" old (##sys#make-vector words))) )
    (##sys#become! (list (cons old (proc y))))
    y) )


;;; locatives:

(define (make-locative obj . index)
  (##sys#make-locative obj (optional index 0) #f 'make-locative) )

(define (make-weak-locative obj . index)
  (##sys#make-locative obj (optional index 0) #t 'make-weak-locative) )

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
