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
  (uses srfi-69)
  (usual-integrations)
  (disable-warning var redef)
  (hide ipc-hook-0 xproc-tag
   ##sys#check-block
   ##sys#check-become-alist
   ##sys#check-generic-structure
   ##sys#check-generic-vector )
  (foreign-declare #<<EOF
#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)
# include <sys/types.h>
#endif
#ifndef C_NONUNIX
# include <sys/mman.h>
#endif

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
     ##sys#check-pointer ##sys#check-closure ##sys#check-integer ##sys#check-special
     ##sys#error ##sys#signal-hook 
     ##sys#error-not-a-proper-list
     make-hash-table hash-table-ref/default hash-table-set!
     ##sys#make-pointer ##sys#make-tagged-pointer ##sys#make-locative ##sys#locative?
     ##sys#become!
     ##sys#make-string ##sys#make-vector ##sys#vector->closure!
     make-property-condition make-composite-condition signal
     ##sys#generic-structure?
     ##sys#set-pointer-address! ##sys#address->pointer ##sys#pointer->address
     ##sys#lambda-decoration ##sys#decorate-lambda
     extend-procedure ) ) ] )

(include "unsafe-declarations.scm")

(register-feature! 'lolevel)


;;; Helpers:

(define-inline (%pointer? x)
  (and (##core#inline "C_blockp" x) (##core#inline "C_anypointerp" x)) )

(define-inline (%generic-pointer? x)
  (or (%pointer? x)
      (##core#inline "C_locativep" x) ) )

(define-inline (%special-block? x)
  ; generic-pointer, port, closure
  (and (##core#inline "C_blockp" x) (##core#inline "C_specialp" x)) )

(define-inline (%generic-vector? x)
  (and (##core#inline "C_blockp" x)
       (not (or (##core#inline "C_specialp" x)
	        (##core#inline "C_byteblockp" x)))) )

(define-inline (%record-structure? x)
  (and (##core#inline "C_blockp" x) (##core#inline "C_structurep" x)) )



;;; Argument checking:

(define (##sys#check-block x . loc)
  (unless (##core#inline "C_blockp" x)
    (##sys#error-hook
     (foreign-value "C_BAD_ARGUMENT_TYPE_NO_BLOCK_ERROR" int) (and (pair? loc) (car loc))
     x) ) )

(define (##sys#check-become-alist x loc)
  (##sys#check-list x loc)
  (let loop ([lst x])
    (cond [(null? lst) ]
	  [(pair? lst)
	   (let ([a (car lst)])
	     (##sys#check-pair a loc)
	     (##sys#check-block (car a) loc)
	     (##sys#check-block (cdr a) loc)
	     (loop (cdr lst)) ) ]
	  [else
	   (##sys#signal-hook
	    #:type-error loc
	    "bad argument type - not an a-list of non-immediate objects" x) ] ) ) )

(define (##sys#check-generic-structure x . loc)
  (unless (%record-structure? x)
    (##sys#signal-hook
     #:type-error (and (pair? loc) (car loc))
     "bad argument type - not a structure" x) ) )

;; Vector, Structure, Pair, and Symbol

(define (##sys#check-generic-vector x . loc)
  (unless (%generic-vector? x)
    (##sys#signal-hook
     #:type-error (and (pair? loc) (car loc))
     "bad argument type - not a vector-like object" x) ) )

(define (##sys#check-pointer x . loc)
  (unless (%pointer? x)
    (##sys#error-hook
     (foreign-value "C_BAD_ARGUMENT_TYPE_NO_POINTER_ERROR" int)
     (and (pair? loc) (car loc))
     "bad argument type - not a pointer" x) ) )

(cond-expand
  [unsafe
   (define-syntax ##sys#check-pointer
     (syntax-rules ()
       ((_ . _) (##core#undefined))))
   (define-syntax ##sys#check-block
     (syntax-rules ()
       ((_ . _) (##core#undefined))))
   (define-syntax ##sys#check-become-alist
     (syntax-rules ()
       ((_ . _) (##core#undefined))))
   (define-syntax ##sys#check-generic-structure
     (syntax-rules ()
       ((_ . _) (##core#undefined))))
   (define-syntax ##sys#check-generic-vector
     (syntax-rules ()
       ((_ . _) (##core#undefined)))) ]
  [else] )


;;; Move arbitrary blocks of memory around:

(define move-memory!
  (let ([memmove1 (foreign-lambda void "C_memmove_o" c-pointer c-pointer int int int)]
	[memmove2 (foreign-lambda void "C_memmove_o" c-pointer scheme-pointer int int int)]
	[memmove3 (foreign-lambda void "C_memmove_o" scheme-pointer c-pointer int int int)]
	[memmove4 (foreign-lambda void "C_memmove_o" scheme-pointer scheme-pointer int int int)]
	[typerr (lambda (x)
		  (##sys#error-hook
		   (foreign-value "C_BAD_ARGUMENT_TYPE_ERROR" int)
		   'move-memory! x))]
	[slot1structs '(mmap
			u8vector u16vector u32vector s8vector s16vector s32vector
			f32vector f64vector)] )
    (lambda (from to #!optional n (foffset 0) (toffset 0))
      ;
      (define (nosizerr)
	(##sys#error 'move-memory! "need number of bytes to move" from to))
      ;
      (define (sizerr . args)
	(apply ##sys#error 'move-memory! "number of bytes to move too large" from to args))
      ;
      (define (checkn1 n nmax off)
	(if (cond-expand [unsafe #t] [else (fx<= n (fx- nmax off))])
	    n
	    (sizerr n nmax) ) )
      ;
      (define (checkn2 n nmax nmax2 off1 off2)
	(if (cond-expand [unsafe #t] [else (and (fx<= n (fx- nmax off1)) (fx<= n (fx- nmax2 off2)))])
	    n
	    (sizerr n nmax nmax2) ) )
      ;
      (##sys#check-block from 'move-memory!)
      (##sys#check-block to 'move-memory!)
      (let move ([from from] [to to])
	(cond [(##sys#generic-structure? from)
	       (if (memq (##sys#slot from 0) slot1structs)
		   (move (##sys#slot from 1) to)
		   (typerr from) ) ]
	      [(##sys#generic-structure? to)
	       (if (memq (##sys#slot to 0) slot1structs)
		   (move from (##sys#slot to 1))
		   (typerr to) ) ]
	      [(%generic-pointer? from)
	       (cond [(%generic-pointer? to)
		      (memmove1 to from (or n (nosizerr)) toffset foffset)]
		     [(or (##sys#bytevector? to) (string? to))
		      (memmove3 to from (checkn1 (or n (nosizerr)) (##sys#size to) toffset) toffset foffset) ]
		     [else
		      (typerr to)] ) ]
	      [(or (##sys#bytevector? from) (string? from))
	       (let ([nfrom (##sys#size from)])
		 (cond [(%generic-pointer? to)
			(memmove2 to from (checkn1 (or n nfrom) nfrom foffset) toffset foffset)]
		       [(or (##sys#bytevector? to) (string? to))
			(memmove4 to from (checkn2 (or n nfrom) nfrom (##sys#size to) foffset toffset)
				  toffset foffset) ]
		       [else
			(typerr to)] ) ) ]
	      [else
	       (typerr from)] ) ) ) ) )


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
		    [(fx>= i n)]
		  (##sys#setslot y i (copy (##sys#slot y i))) ) )
	      y) ] ) ) )


;;; Pointer operations:

(define allocate (foreign-lambda c-pointer "C_malloc" int))
(define free (foreign-lambda void "C_free" c-pointer))

(define (pointer? x) (%pointer? x))

(define (pointer-like? x) (%special-block? x))

(define (address->pointer addr)
  (##sys#check-integer addr 'address->pointer)
  (##sys#address->pointer addr) )

(define (pointer->address ptr)
  (##sys#check-special ptr 'pointer->address)
  (##sys#pointer->address ptr) )

(define null-pointer ##sys#null-pointer)

(define (null-pointer? ptr)
  (##sys#check-special ptr 'null-pointer?)
  (eq? 0 (##sys#pointer->address ptr) ) )

(define (object->pointer x)
  (and (##core#inline "C_blockp" x)
       ((foreign-lambda* nonnull-c-pointer ((scheme-object x)) "return((void *)x);") x) ) )

(define (pointer->object ptr)
  (##sys#check-pointer ptr 'pointer->object)
  (##core#inline "C_pointer_to_object" ptr) )

(define (pointer=? p1 p2)
  (##sys#check-special p1 'pointer=?)
  (##sys#check-special p2 'pointer=?)
  (##core#inline "C_pointer_eqp" p1 p2) )

(define pointer-offset
  (foreign-lambda* nonnull-c-pointer ([c-pointer ptr] [integer off])
    "return((unsigned char *)ptr + off);") )

(define align-to-word
  (let ([align (foreign-lambda integer "C_align" integer)])
    (lambda (x)
      (cond [(integer? x)
	     (align x)]
	    [(%special-block? x)
	     (##sys#address->pointer (align (##sys#pointer->address x))) ]
	    [else
	     (##sys#signal-hook
	      #:type-error 'align-to-word
	      "bad argument type - not a pointer or integer" x)] ) ) ) )


;;; Tagged-pointers:

(define (tag-pointer ptr tag)
  (let ([tp (##sys#make-tagged-pointer tag)])
    (if (%special-block? ptr)
	(##core#inline "C_copy_pointer" ptr tp)
	(##sys#error-hook (foreign-value "C_BAD_ARGUMENT_TYPE_NO_POINTER_ERROR" int) 'tag-pointer ptr) )
    tp) )

(define (tagged-pointer? x #!optional tag)
  (and (##core#inline "C_blockp" x)  (##core#inline "C_taggedpointerp" x)
       (or (not tag)
	   (equal? tag (##sys#slot x 1)) ) ) )

(define (pointer-tag x)
  (if (%special-block? x)
      (and (##core#inline "C_taggedpointerp" x)
	   (##sys#slot x 1) )
      (##sys#error-hook (foreign-value "C_BAD_ARGUMENT_TYPE_NO_POINTER_ERROR" int) 'pointer-tag x) ) )


;;; locatives:

;; Locative layout:
;
; 0	Object-address + Byte-offset (address)
; 1	Byte-offset (fixnum)
; 2	Type (fixnum)
;	0	vector or pair		(C_SLOT_LOCATIVE)
;	1	string			(C_CHAR_LOCATIVE)
;	2	u8vector or blob        (C_U8_LOCATIVE)
;	3	s8vector	        (C_S8_LOCATIVE)
;	4	u16vector		(C_U16_LOCATIVE)
;	5	s16vector		(C_S16_LOCATIVE)
;	6	u32vector		(C_U32_LOCATIVE)
;	7	s32vector		(C_S32_LOCATIVE)
;	8	f32vector		(C_F32_LOCATIVE)
;	9	f64vector		(C_F64_LOCATIVE)
; 3	Object or #f, if weak (C_word)

(define (make-locative obj . index)
  (##sys#make-locative obj (optional index 0) #f 'make-locative) )

(define (make-weak-locative obj . index)
  (##sys#make-locative obj (optional index 0) #t 'make-weak-locative) )

(define (locative-set! x y) (##core#inline "C_i_locative_set" x y))
(define locative-ref (getter-with-setter (##core#primitive "C_locative_ref") locative-set!))
(define (locative->object x) (##core#inline "C_i_locative_to_object" x))
(define (locative? x) (and (##core#inline "C_blockp" x) (##core#inline "C_locativep" x)))


;;; SRFI-4 number-vector:

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
   (foreign-lambda* int ([c-pointer p]) "return(*((signed char *)p));")
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


;;; Procedures extended with data:

; Unique id for extended-procedures
(define xproc-tag (vector 'extended))

(define (extend-procedure proc data)
  (##sys#check-closure proc 'extend-procedure)
  (##sys#decorate-lambda
   proc
   (lambda (x) (and (pair? x) (eq? xproc-tag (##sys#slot x 0)))) 
   (lambda (x i) (##sys#setslot x i (cons xproc-tag data)) x) ) )

(define-inline (%procedure-data proc)
  (##sys#lambda-decoration proc (lambda (x) (and (pair? x) (eq? xproc-tag (##sys#slot x 0))))) )

(define (extended-procedure? x)
  (and (##core#inline "C_blockp" x) (##core#inline "C_closurep" x)
       (%procedure-data x)
       #t) )

(define (procedure-data x)
  (and (##core#inline "C_blockp" x) (##core#inline "C_closurep" x)
       (and-let* ([d (%procedure-data x)])
	 (##sys#slot d 1) ) ) )

(define set-procedure-data!
  (let ((extend-procedure extend-procedure))
    (lambda (proc x)
      (let ((p2 (extend-procedure proc x)))
	(if (eq? p2 proc)
	    proc
	    (##sys#signal-hook
	     #:type-error 'set-procedure-data!
	     "bad argument type - not an extended procedure" proc) ) ) ) ) )


;;; Accessors for arbitrary vector-like block objects:

(define block-set! ##sys#block-set!)
(define block-ref (getter-with-setter ##sys#block-ref ##sys#block-set!))

(define (vector-like? x)
  (%generic-vector? x) )

(define (number-of-slots x)
  (##sys#check-generic-vector x 'number-of-slots)
  (##sys#size x) )

(define (number-of-bytes x)
  (cond [(not (##core#inline "C_blockp" x))
	 (##sys#signal-hook
	  #:type-error 'number-of-bytes
	  "cannot compute number of bytes of immediate object" x) ]
	[(##core#inline "C_byteblockp" x)
	 (##sys#size x)]
	[else
	 (##core#inline "C_w2b" (##sys#size x))] ) )


;;; Record objects:

;; Record layout:
;
; 0	Tag (symbol)
; 1..N	Slot (object)

(define (make-record-instance type . args)
  (##sys#check-symbol type 'make-record-instance)
  (apply ##sys#make-structure type args) )

(define (record-instance? x #!optional type)
  (and (%record-structure? x)
       (or (not type)
	   (eq? type (##sys#slot x 0)))) )

(define (record-instance-type x)
  (##sys#check-generic-structure x 'record-instance-type)
  (##sys#slot x 0) )

(define (record-instance-length x)
  (##sys#check-generic-structure x 'record-instance-length)
  (fx- (##sys#size x) 1) )

(define (record-instance-slot-set! x i y)
  (##sys#check-generic-structure x 'record-instance-slot-set!)
  (##sys#check-range i 0 (fx- (##sys#size x) 1) 'record-instance-slot-set!)
  (##sys#setslot x (fx+ i 1) y) )

(define record-instance-slot
  (getter-with-setter
   (lambda (x i)
     (##sys#check-generic-structure x 'record-instance-slot)
     (##sys#check-range i 0 (fx- (##sys#size x) 1) 'record-instance-slot)
     (##sys#slot x (fx+ i 1)) )
   record-instance-slot-set!))

(define (record->vector x)
  (##sys#check-generic-structure x 'record->vector)
  (let* ([n (##sys#size x)]
	 [v (##sys#make-vector n)] )
    (do ([i 0 (fx+ i 1)])
	 [(fx>= i n) v]
      (##sys#setslot v i (##sys#slot x i)) ) ) )



;;; Evict objects into static memory:

(define-constant evict-table-size 301)

(define (object-evicted? x) (##core#inline "C_permanentp" x))

(define (object-evict x . allocator)
  (let ([allocator 
	 (if (pair? allocator) (car allocator) (foreign-lambda c-pointer "C_malloc" int) ) ] 
	[tab (make-hash-table eq?)] )
    (##sys#check-closure allocator 'object-evict)
    (let evict ([x x])
      (cond [(not (##core#inline "C_blockp" x)) x ]
	    [(hash-table-ref/default tab x #f) ]
	    [else
	     (let* ([n (##sys#size x)]
		    [bytes (if (##core#inline "C_byteblockp" x) (align-to-word n) (##core#inline "C_bytes" n))]
		    [y (##core#inline "C_evict_block" x (allocator (fx+ bytes (##core#inline "C_bytes" 1))))] )
	       (when (symbol? x) (##sys#setislot y 0 (void)))
	       (hash-table-set! tab x y)
	       (unless (##core#inline "C_byteblockp" x)
		 (do ([i (if (or (##core#inline "C_specialp" x) (symbol? x)) 1 0) (fx+ i 1)])
		     [(fx>= i n)]
		   ;; Note the use of `##sys#setislot' to avoid an entry in the mutations-table:
		   (##sys#setislot y i (evict (##sys#slot x i))) ) )
	       y ) ] ) ) ) )

(define (object-evict-to-location x ptr . limit)
  (cond-expand [(not unsafe) (##sys#check-special ptr 'object-evict-to-location)] [else])
  (let* ([limit (and (pair? limit)
		     (let ([limit (car limit)])
		       (##sys#check-exact limit 'object-evict-to-location)
		       limit)) ]
	 [ptr2 (##sys#address->pointer (##sys#pointer->address ptr))]
	 [tab (make-hash-table eq?)]
	 [x2
	  (let evict ([x x])
	    (cond [(not (##core#inline "C_blockp" x)) x ]
		  [(hash-table-ref/default tab x #f) ]
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
		     (when (symbol? x) (##sys#setislot y 0 (void)))
		     (##sys#set-pointer-address! ptr2 (+ (##sys#pointer->address ptr2) bytes))
		     (hash-table-set! tab x y)
		     (unless (##core#inline "C_byteblockp" x)
		       (do ([i (if (or (##core#inline "C_specialp" x) (symbol? x)) 1 0) (fx+ i 1)] )
			   [(fx>= i n)]
			 (##sys#setislot y i (evict (##sys#slot x i))) ) ) ; see above
		     y) ) ] ) ) ] )
    (values x2 ptr2) ) )

(define (object-release x . releaser)
  (let ([free (if (pair? releaser) 
		  (car releaser) 
		  (foreign-lambda void "C_free" c-pointer) ) ]
	[released '() ] )
    (let release ([x x])
      (cond [(not (##core#inline "C_blockp" x)) x ]
	    [(not (##core#inline "C_permanentp" x)) x ]
	    [(memq x released) x ]
	    [else
	     (let ([n (##sys#size x)])
	       (set! released (cons x released))
	       (unless (##core#inline "C_byteblockp" x)
		 (do ([i (if (##core#inline "C_specialp" x) 1 0) (fx+ i 1)])
		     [(fx>= i n)]
		   (release (##sys#slot x i))) )
	       (free (##sys#address->pointer (##core#inline_allocate ("C_block_address" 4) x))) ) ] ) ) ) )

(define (object-size x)
  (let ([tab (make-hash-table eq?)])
    (let evict ([x x])
      (cond [(not (##core#inline "C_blockp" x)) 0 ]
	    [(hash-table-ref/default tab x #f) 0 ]
	    [else
	     (let* ([n (##sys#size x)]
		    [bytes
		     (fx+ (if (##core#inline "C_byteblockp" x) (align-to-word n) (##core#inline "C_bytes" n))
			  (##core#inline "C_bytes" 1) ) ] )
	       (hash-table-set! tab x #t)
	       (unless (##core#inline "C_byteblockp" x)
		 (do ([i (if (or (##core#inline "C_specialp" x) (symbol? x)) 1 0) (fx+ i 1)])
		     [(fx>= i n)]
		   (set! bytes (fx+ (evict (##sys#slot x i)) bytes)) ) )
	       bytes) ] ) ) ) )

(define (object-unevict x #!optional full)
  (let ([tab (make-hash-table eq?)])
    (let copy ([x x])
    (cond [(not (##core#inline "C_blockp" x)) x ]
	  [(not (##core#inline "C_permanentp" x)) x ]
	  [(hash-table-ref/default tab x #f) ]
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
		  [y (##core#inline "C_copy_block" x (##sys#make-vector words))] )
	     (hash-table-set! tab x y)
	     (do ([i (if (##core#inline "C_specialp" x) 1 0) (fx+ i 1)])
		 ((fx>= i words))
	       (##sys#setslot y i (copy (##sys#slot y i))) )
	     y) ] ) ) ) )


;;; `become':

(define (object-become! alst)
  (cond-expand [(not unsafe) (##sys#check-become-alist alst 'object-become!)] [else])
  (##sys#become! alst) )

(define (mutate-procedure old proc)
  (##sys#check-closure old 'mutate-procedure)
  (##sys#check-closure proc 'mutate-procedure)
  (let* ([n (##sys#size old)]
	 [words (##core#inline "C_words" n)]
	 [new (##core#inline "C_copy_block" old (##sys#make-vector words))] )
    (##sys#become! (list (cons old (proc new))))
    new ) )


;;; Hooks:

(define ipc-hook-0 #f)			; we need this because `##sys#invalid-procedure-call-hook' may not have free variables.

(define (set-invalid-procedure-call-handler! proc)
  (##sys#check-closure proc 'set-invalid-procedure-call-handler!)
  (set! ipc-hook-0 proc)
  (set! ##sys#invalid-procedure-call-hook 
    (lambda args
      (ipc-hook-0 ##sys#last-invalid-procedure args) ) ) )

(define (unbound-variable-value . val)
  (set! ##sys#unbound-variable-value-hook 
    (and (pair? val)
	 (vector (car val)))) )


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
  sym )
