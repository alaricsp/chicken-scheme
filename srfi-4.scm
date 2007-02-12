;;;; srfi-4.scm - Homogeneous numeric vectors
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
 (unit srfi-4)
 (disable-interrupts)
 (disable-warning redef)
 (usual-integrations)
 (hide ##sys#u8vector-set! ##sys#s8vector-set! ##sys#u16vector-set! ##sys#s16vector-set!
       ##sys#u32vector-set! ##sys#s32vector-set! ##sys#f32vector-set! ##sys#f64vector-set!
       ##sys#u8vector-ref ##sys#s8vector-ref ##sys#u16vector-ref ##sys#s16vector-ref subvector
       ##sys#u32vector-ref ##sys#s32vector-ref ##sys#f32vector-ref ##sys#f64vector-ref)
 (foreign-declare #<<EOF
#define C_u8peek(b, i)         C_fix(((unsigned char *)C_data_pointer(b))[ C_unfix(i) ])
#define C_s8peek(b, i)         C_fix(((char *)C_data_pointer(b))[ C_unfix(i) ])
#define C_u16peek(b, i)        C_fix(((unsigned short *)C_data_pointer(b))[ C_unfix(i) ])
#define C_s16peek(b, i)        C_fix(((short *)C_data_pointer(b))[ C_unfix(i) ])
#ifdef C_SIXTY_FOUR
# define C_a_u32peek(ptr, d, b, i) C_fix(((C_u32 *)C_data_pointer(b))[ C_unfix(i) ])
# define C_a_s32peek(ptr, d, b, i) C_fix(((C_s32 *)C_data_pointer(b))[ C_unfix(i) ])
#else
# define C_a_u32peek(ptr, d, b, i) C_unsigned_int_to_num(ptr, ((C_u32 *)C_data_pointer(b))[ C_unfix(i) ])
# define C_a_s32peek(ptr, d, b, i) C_int_to_num(ptr, ((C_s32 *)C_data_pointer(b))[ C_unfix(i) ])
#endif
#define C_f32peek(b, i)        (C_temporary_flonum = ((float *)C_data_pointer(b))[ C_unfix(i) ], C_SCHEME_UNDEFINED)
#define C_f64peek(b, i)        (C_temporary_flonum = ((double *)C_data_pointer(b))[ C_unfix(i) ], C_SCHEME_UNDEFINED)
#define C_u8poke(b, i, x)      ((((unsigned char *)C_data_pointer(b))[ C_unfix(i) ] = C_unfix(x)), C_SCHEME_UNDEFINED)
#define C_s8poke(b, i, x)      ((((char *)C_data_pointer(b))[ C_unfix(i) ] = C_unfix(x)), C_SCHEME_UNDEFINED)
#define C_u16poke(b, i, x)     ((((unsigned short *)C_data_pointer(b))[ C_unfix(i) ] = C_unfix(x)), C_SCHEME_UNDEFINED)
#define C_s16poke(b, i, x)     ((((short *)C_data_pointer(b))[ C_unfix(i) ] = C_unfix(x)), C_SCHEME_UNDEFINED)
#define C_u32poke(b, i, x)     ((((C_u32 *)C_data_pointer(b))[ C_unfix(i) ] = C_num_to_unsigned_int(x)), C_SCHEME_UNDEFINED)
#define C_s32poke(b, i, x)     ((((C_s32 *)C_data_pointer(b))[ C_unfix(i) ] = C_num_to_int(x)), C_SCHEME_UNDEFINED)
#define C_f32poke(b, i, x)     ((((float *)C_data_pointer(b))[ C_unfix(i) ] = C_flonum_magnitude(x)), C_SCHEME_UNDEFINED)
#define C_f64poke(b, i, x)     ((((double *)C_data_pointer(b))[ C_unfix(i) ] = C_flonum_magnitude(x)), C_SCHEME_UNDEFINED)
#define C_copy_subvector(to, from, start_to, start_from, bytes)   \
  (C_memcpy((C_char *)C_data_pointer(to) + C_unfix(start_to), (C_char *)C_data_pointer(from) + C_unfix(start_from), C_unfix(bytes)), \
    C_SCHEME_UNDEFINED)
EOF
) )

(cond-expand
 [paranoia]
 [else
  (declare
    (no-bound-checks)
    (no-procedure-checks-for-usual-bindings)
    (bound-to-procedure
     ##sys#check-exact ##sys#u8vector-ref ##sys#u8vector-set! ##sys#s8vector-ref ##sys#s8vector-set! 
     ##sys#u16vector-ref ##sys#u16vector-set!
     ##sys#s16vector-ref ##sys#s16vector-set! ##sys#u32vector-ref ##sys#u32vector-set! ##sys#s32vector-ref 
     ##sys#s32vector-set! read list->f64vector list->s32vector list->u32vector list->u16vector list-s8vector
     list->u8vector set-finalizer!
     ##sys#f32vector-ref ##sys#f32vector-set! ##sys#f64vector-ref ##sys#f64vector-set! ##sys#check-exact-interval
     ##sys#check-inexact-interval ##sys#check-number ##sys#check-structure ##sys#cons-flonum ##sys#check-list 
     ##sys#check-range ##sys#error ##sys#signal-hook
     ##sys#not-a-proper-list-error ##sys#print ##sys#allocate-vector) ) ] )

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
    (define-macro (##sys#check-bytevector . _) '(##core#undefined)) ) ]
 [else
  (declare (emit-exports "srfi-4.exports"))] )


;;; Helper routines:

(define ##sys#check-exact-interval
  (lambda (n from to loc)
    (##sys#check-exact n loc)
    (if (or (##core#inline "C_fixnum_lessp" n from)
	    (##core#inline "C_fixnum_greaterp" n to) )
	(##sys#error loc "numeric value is not in expected range" n from to) ) ) )

(define ##sys#check-inexact-interval
  (lambda (n from to loc)
    (##sys#check-number n loc)
    (if (or (< n from) (> n to))
	(##sys#error "numeric value is not in expected range" n from to) ) ) )


;;; Primitive accessors:

(define (##sys#u8vector-ref v i) (##core#inline "C_u8peek" (##core#inline "C_slot" v 1) i))
(define (##sys#s8vector-ref v i) (##core#inline "C_s8peek" (##core#inline "C_slot" v 1) i))
(define (##sys#u16vector-ref v i) (##core#inline "C_u16peek" (##core#inline "C_slot" v 1) i))
(define (##sys#s16vector-ref v i) (##core#inline "C_s16peek" (##core#inline "C_slot" v 1) i))
(define (##sys#u32vector-ref v i) (##core#inline_allocate ("C_a_u32peek" 4) (##core#inline "C_slot" v 1) i))
(define (##sys#s32vector-ref v i) (##core#inline_allocate ("C_a_s32peek" 4) (##core#inline "C_slot" v 1) i))

(define (##sys#f32vector-ref v i)
  (##core#inline "C_f32peek" (##core#inline "C_slot" v 1) i)
  (##sys#cons-flonum) )

(define (##sys#f64vector-ref v i)
  (##core#inline "C_f64peek" (##core#inline "C_slot" v 1) i)
  (##sys#cons-flonum) )

(define (##sys#u8vector-set! v i x) (##core#inline "C_u8poke" (##core#inline "C_slot" v 1) i x))
(define (##sys#s8vector-set! v i x) (##core#inline "C_s8poke" (##core#inline "C_slot" v 1) i x))
(define (##sys#u16vector-set! v i x) (##core#inline "C_u16poke" (##core#inline "C_slot" v 1) i x))
(define (##sys#s16vector-set! v i x) (##core#inline "C_s16poke" (##core#inline "C_slot" v 1) i x))
(define (##sys#u32vector-set! v i x) (##core#inline "C_u32poke" (##core#inline "C_slot" v 1) i x))
(define (##sys#s32vector-set! v i x) (##core#inline "C_s32poke" (##core#inline "C_slot" v 1) i x))
(define (##sys#f32vector-set! v i x) (##core#inline "C_f32poke" (##core#inline "C_slot" v 1) i x))
(define (##sys#f64vector-set! v i x) (##core#inline "C_f64poke" (##core#inline "C_slot" v 1) i x))


;;; Get vector length:

(let ()

  (define (len tag shift loc)
    (lambda (v)
      (##sys#check-structure v tag loc)
      (let ((bytes (##core#inline "C_block_size" (##core#inline "C_slot" v 1))))
	(if shift
	    (##core#inline "C_fixnum_shift_right" bytes shift)
	    bytes) ) ) )

  (set! u8vector-length (len 'u8vector #f 'u8vector-length))
  (set! s8vector-length (len 's8vector #f 's8vector-length))
  (set! u16vector-length (len 'u16vector 1 'u16vector-length))
  (set! s16vector-length (len 's16vector 1 's16vector-length))
  (set! u32vector-length (len 'u32vector 2 'u32vector-length))
  (set! s32vector-length (len 's32vector 2 's32vector-length))
  (set! f32vector-length (len 'f32vector 2 'f32vector-length))
  (set! f64vector-length (len 'f64vector 3 'f64vector-length)) )


;;; Safe accessors:

(let ()

  (define (get length acc loc)
    (lambda (v i)
      (let ((len (length v)))
	(##sys#check-range i 0 len loc)
	(acc v i) ) ) )

  (define (set length upd loc)
    (lambda (v i x)
      (let ((len (length v)))
	(##sys#check-exact x loc)
	(##sys#check-range i 0 len loc)
	(upd v i x) ) ) )
 
  (define (setu length upd loc)
    (lambda (v i x)
      (let ((len (length v)))
	(##sys#check-exact x loc)
	(if (fx< x 0)
	    (##sys#error loc "argument may not be negative" x) )
	(##sys#check-range i 0 len loc)
	(upd v i x) ) ) )
 
  (define (setw length upd loc)
    (lambda (v i x)
      (let ((len (length v)))
	(if (not (##sys#fits-in-int? x))
	    (##sys#error loc "argument exceeds integer range" x) )
	(##sys#check-range i 0 len loc)
	(upd v i x) ) ) )
 
  (define (setuw length upd loc)
    (lambda (v i x)
      (let ((len (length v)))
	(cond ((negative? x)
	       (##sys#error loc "argument may not be negative" x) )
	      ((not (##sys#fits-in-unsigned-int? x))
	       (##sys#error loc "argument exceeds integer range" x) ) )
	(##sys#check-range i 0 len loc)
	(upd v i x) ) ) )
 
  (define (setf length upd loc)
    (lambda (v i x)
      (let ((len (length v)))
	(##sys#check-number x loc)
	(##sys#check-range i 0 len loc)
	(upd v i (if (##core#inline "C_blockp" x)
		     x
		     (exact->inexact x) ) ) ) ) )
 
  (set! u8vector-set! (setu u8vector-length ##sys#u8vector-set! 'u8vector-set!))
  (set! s8vector-set! (set s8vector-length ##sys#s8vector-set! 's8vector-set!))
  (set! u16vector-set! (setu u16vector-length ##sys#u16vector-set! 'u16vector-set!))
  (set! s16vector-set! (set s16vector-length ##sys#s16vector-set! 's16vector-set!))
  (set! u32vector-set! (setuw u32vector-length ##sys#u32vector-set! 'u32vector-set!))
  (set! s32vector-set! (setw s32vector-length ##sys#s32vector-set! 's32vector-set!))
  (set! f32vector-set! (setf f32vector-length ##sys#f32vector-set! 'f32vector-set!))
  (set! f64vector-set! (setf f64vector-length ##sys#f64vector-set! 'f64vector-set!))

  (set! u8vector-ref
	(getter-with-setter (get u8vector-length ##sys#u8vector-ref 'u8vector-ref)
			    u8vector-set!) )
  (set! s8vector-ref
	(getter-with-setter (get s8vector-length ##sys#s8vector-ref 's8vector-ref)
			    s8vector-set!) )
  (set! u16vector-ref
	(getter-with-setter (get u16vector-length ##sys#u16vector-ref 'u16vector-ref)
			    u16vector-set!) )
  (set! s16vector-ref
	(getter-with-setter (get s16vector-length ##sys#s16vector-ref 's16vector-ref)
			    s16vector-set!) )
  (set! u32vector-ref
	(getter-with-setter
	 (get u32vector-length ##sys#u32vector-ref 'u32vector-ref)
	 u32vector-set!) )
  (set! s32vector-ref
	(getter-with-setter 
	 (get s32vector-length ##sys#s32vector-ref 's32vector-ref)
	 s32vector-set!) )
  (set! f32vector-ref
	(getter-with-setter 
	 (get f32vector-length ##sys#f32vector-ref 'f32vector-ref)
	 f32vector-set!) )
  (set! f64vector-ref 
	(getter-with-setter
	 (get f64vector-length ##sys#f64vector-ref 'f64vector-ref)
	 f64vector-set!) ) )



;;; Basic constructors:

(let* ([ext-alloc
	(foreign-lambda* scheme-object ([int bytes])
	  "C_word *buf = (C_word *)C_malloc(bytes + sizeof(C_header));"
	  "if(buf == NULL) return(C_SCHEME_FALSE);"
	  "C_block_header(buf) = C_make_header(C_BYTEVECTOR_TYPE, bytes);"
	  "return(buf);") ]
       [ext-free
	(foreign-lambda* void ([scheme-object bv])
	  "C_free((void *)C_block_item(bv, 1));") ]
       [set-finalizer! set-finalizer!]
       [alloc
	(lambda (loc len ext?)
	  (if ext? 
	      (let ([bv (ext-alloc len)])
		(or bv
		    (##sys#error loc "not enough memory - can not allocate external number vector" len)) )
	      (let ([bv (##sys#allocate-vector len #t #f #t)]) ; this could be made better...
		(##core#inline "C_string_to_bytevector" bv) 
		bv) ) ) ] )

  (set! release-number-vector
    (lambda (v)
      (if (and (##sys#generic-structure? v)
	       (memq (##sys#slot v 0) '(u8vector u16vector s8vector s16vector u32vector s32vector f32vector f64vector)) )
	  (ext-free v)
	  (##sys#error 'release-number-vector "bad argument type - not a number vector" v)) ) )

  (set! make-u8vector
    (lambda (len #!optional (init #f)  (ext? #f) (fin? #t))
      (##sys#check-exact len 'make-u8vector)
      (let ((v (##sys#make-structure 'u8vector (alloc 'make-u8vector len ext?))))
	(when (and ext? fin?) (set-finalizer! v ext-free))
	(if (not init)
	    v
	    (begin
	      (##sys#check-exact-interval init 0 #xff 'make-u8vector)
	      (do ((i 0 (##core#inline "C_fixnum_plus" i 1)))
		  ((##core#inline "C_fixnum_greater_or_equal_p" i len) v)
		(##sys#u8vector-set! v i init) ) ) ) ) ) )

  (set! make-s8vector
    (lambda (len #!optional (init #f)  (ext? #f) (fin #t))
      (##sys#check-exact len 'make-s8vector)
      (let ((v (##sys#make-structure 's8vector (alloc 'make-s8vector len ext?))))
	(when (and ext? fin?) (set-finalizer! v ext-free))
	(if (not init)
	    v
	    (begin
	      (##sys#check-exact-interval init -128 127 'make-s8vector)
	      (do ((i 0 (##core#inline "C_fixnum_plus" i 1)))
		  ((##core#inline "C_fixnum_greater_or_equal_p" i len) v)
		(##sys#s8vector-set! v i init) ) ) ) ) ) )

  (set! make-u16vector
    (lambda (len #!optional (init #f)  (ext? #f) (fin #t))
      (##sys#check-exact len 'make-u16vector)
      (let ((v (##sys#make-structure 'u16vector (alloc 'make-u16vector (##core#inline "C_fixnum_shift_left" len 1) ext?))))
	(when (and ext? fin?) (set-finalizer! v ext-free))
	(if (not init)
	    v
	    (begin
	      (##sys#check-exact-interval init 0 #xffff 'make-u16vector)
	      (do ((i 0 (##core#inline "C_fixnum_plus" i 1)))
		  ((##core#inline "C_fixnum_greater_or_equal_p" i len) v)
		(##sys#u16vector-set! v i init) ) ) ) ) ) )

  (set! make-s16vector
    (lambda (len #!optional (init #f)  (ext? #f) (fin #t))
      (##sys#check-exact len 'make-s16vector)
      (let ((v (##sys#make-structure 's16vector (alloc 'make-s16vector (##core#inline "C_fixnum_shift_left" len 1) ext?))))
	(when (and ext? fin?) (set-finalizer! v ext-free))
	(if (not init)
	    v
	    (begin
	      (##sys#check-exact-interval init -32768 32767 'make-s16vector)
	      (do ((i 0 (##core#inline "C_fixnum_plus" i 1)))
		  ((##core#inline "C_fixnum_greater_or_equal_p" i len) v)
		(##sys#s16vector-set! v i init) ) ) ) ) ) )

  (set! make-u32vector
    (lambda (len #!optional (init #f)  (ext? #f) (fin #t))
      (##sys#check-exact len 'make-u32vector)
      (let ((v (##sys#make-structure 'u32vector (alloc 'make-u32vector (##core#inline "C_fixnum_shift_left" len 2) ext?))))
	(when (and ext? fin?) (set-finalizer! v ext-free))
	(if (not init)
	    v
	    (begin
	      (##sys#check-exact init 'make-u32vector)
	      (do ((i 0 (##core#inline "C_fixnum_plus" i 1)))
		  ((##core#inline "C_fixnum_greater_or_equal_p" i len) v)
		(##sys#u32vector-set! v i init) ) ) ) ) ) )

  (set! make-s32vector
    (lambda (len #!optional (init #f)  (ext? #f) (fin #t))
      (##sys#check-exact len 'make-s32vector)
      (let ((v (##sys#make-structure 's32vector (alloc 'make-s32vector (##core#inline "C_fixnum_shift_left" len 2) ext?))))
	(when (and ext? fin?) (set-finalizer! v ext-free))
	(if (not init)
	    v
	    (begin
	      (##sys#check-exact init 'make-s32vector)
	      (do ((i 0 (##core#inline "C_fixnum_plus" i 1)))
		  ((##core#inline "C_fixnum_greater_or_equal_p" i len) v)
		(##sys#s32vector-set! v i init) ) ) ) ) ) )

  (set! make-f32vector
    (lambda (len #!optional (init #f)  (ext? #f) (fin #t))
      (##sys#check-exact len 'make-f32vector)
      (let ((v (##sys#make-structure 'f32vector (alloc 'make-f32vector (##core#inline "C_fixnum_shift_left" len 2) ext?))))
	(when (and ext? fin?) (set-finalizer! v ext-free))
	(if (not init)
	    v
	    (begin
	      (##sys#check-number init 'make-f32vector)
	      (unless (##core#inline "C_blockp" init)
		(set! init (exact->inexact init)) )
	      (do ((i 0 (##core#inline "C_fixnum_plus" i 1)))
		  ((##core#inline "C_fixnum_greater_or_equal_p" i len) v)
		(##sys#f32vector-set! v i init) ) ) ) ) ) )

  (set! make-f64vector
    (lambda (len #!optional (init #f)  (ext? #f) (fin #t))
      (##sys#check-exact len 'make-f64vector)
      (let ((v (##sys#make-structure 'f64vector (alloc 'make-f64vector (##core#inline "C_fixnum_shift_left" len 3) ext?))))
	(when (and ext? fin?) (set-finalizer! v ext-free))
	(if (not init)
	    v
	    (begin
	      (##sys#check-number init 'make-f64vector)
	      (unless (##core#inline "C_blockp" init)
		(set! init (exact->inexact init)) )
	      (do ((i 0 (##core#inline "C_fixnum_plus" i 1)))
		  ((##core#inline "C_fixnum_greater_or_equal_p" i len) v)
		(##sys#f64vector-set! v i init) ) ) ) ) ) ) )


;;; Creating vectors from a list:

(let ()

  (define (init make set loc)
    (lambda (lst)
      (##sys#check-list lst loc)
      (let* ((n (length lst))
	     (v (make n)) )
	(do ((p lst (##core#inline "C_slot" p 1))
	     (i 0 (##core#inline "C_fixnum_plus" i 1)) )
	    ((##core#inline "C_eqp" p '()) v)
	  (if (and (##core#inline "C_blockp" p) (##core#inline "C_pairp" p))
	      (set v i (##core#inline "C_slot" p 0))
	      (##sys#not-a-proper-list-error lst) ) ) ) ) )

  (set! list->u8vector (init make-u8vector u8vector-set! 'list->u8vector))
  (set! list->s8vector (init make-s8vector s8vector-set! 'list->s8vector))
  (set! list->u16vector (init make-u16vector u16vector-set! 'list->u16vector))
  (set! list->s16vector (init make-s16vector s16vector-set! 'list->s16vector))
  (set! list->u32vector (init make-u32vector u32vector-set! 'list->u32vector))
  (set! list->s32vector (init make-s32vector s32vector-set! 'list->s32vector))
  (set! list->f32vector (init make-f32vector f32vector-set! 'list->f32vector))
  (set! list->f64vector (init make-f64vector f64vector-set! 'list->f64vector)) )


;;; More constructors:
      
(define u8vector
  (let ((list->u8vector list->u8vector))
    (lambda xs (list->u8vector xs)) ) )

(define s8vector
  (let ((list->s8vector list->s8vector))
    (lambda xs (list->s8vector xs)) ) )

(define u16vector
  (let ((list->u16vector list->u16vector))
    (lambda xs (list->u16vector xs)) ) )

(define s16vector
  (let ((list->s16vector list->s16vector))
    (lambda xs (list->s16vector xs)) ) )

(define u32vector
  (let ((list->u32vector list->u32vector))
    (lambda xs (list->u32vector xs)) ) )

(define s32vector
  (let ((list->s32vector list->s32vector))
    (lambda xs (list->s32vector xs)) ) )

(define f32vector
  (let ((list->f32vector list->f32vector))
    (lambda xs (list->f32vector xs)) ) )

(define f64vector
  (let ((list->f64vector list->f64vector))
    (lambda xs (list->f64vector xs)) ) )


;;; Creating lists from a vector:

(let ()
  
  (define (init tag length ref)
    (lambda (v)
      (let ((len (length v)))
	(let loop ((i 0))
	  (if (fx>= i len)
	      '()
	      (cons (ref v i) 
		    (loop (fx+ i 1)) ) ) ) ) ) )

  (set! u8vector->list (init 'u8vector u8vector-length ##sys#u8vector-ref))
  (set! s8vector->list (init 's8vector s8vector-length ##sys#s8vector-ref))
  (set! u16vector->list (init 'u16vector u16vector-length ##sys#u16vector-ref))
  (set! s16vector->list (init 's16vector s16vector-length ##sys#s16vector-ref))
  (set! u32vector->list (init 'u32vector u32vector-length ##sys#u32vector-ref))
  (set! s32vector->list (init 's32vector s32vector-length ##sys#s32vector-ref))
  (set! f32vector->list (init 'f32vector f32vector-length ##sys#f32vector-ref))
  (set! f64vector->list (init 'f64vector f64vector-length ##sys#f64vector-ref)) )


;;; Predicates:

(define (u8vector? x) (##sys#structure? x 'u8vector))
(define (s8vector? x) (##sys#structure? x 's8vector))
(define (u16vector? x) (##sys#structure? x 'u16vector))
(define (s16vector? x) (##sys#structure? x 's16vector))
(define (u32vector? x) (##sys#structure? x 'u32vector))
(define (s32vector? x) (##sys#structure? x 's32vector))
(define (f32vector? x) (##sys#structure? x 'f32vector))
(define (f64vector? x) (##sys#structure? x 'f64vector))


;;; Accessing the packed bytevector:

(let ()

  (define (pack tag loc)
    (lambda (v)
      (##sys#check-structure v tag loc)
      (##core#inline "C_slot" v 1) ) )

  (define (unpack tag sz loc)
    (lambda (str)
      (##sys#check-byte-vector str loc)
      (let ([len (##core#inline "C_block_size" str)])
	(if (or (eq? #t sz) 
		(##core#inline "C_eqp" 0 (##core#inline "C_fixnum_modulo" len sz)))
	    (##sys#make-structure tag str) 
	    (##sys#error loc "bytevector does not have correct size for packing" tag len sz) ) ) ) )

  (set! u8vector->byte-vector (pack 'u8vector 'u8vector->byte-vector))
  (set! s8vector->byte-vector (pack 's8vector 's8vector->byte-vector))
  (set! u16vector->byte-vector (pack 'u16vector 'u16vector->byte-vector))
  (set! s16vector->byte-vector (pack 's16vector 's16vector->byte-vector))
  (set! u32vector->byte-vector (pack 'u32vector 'u32vector->byte-vector))
  (set! s32vector->byte-vector (pack 's32vector 's32vector->byte-vector))
  (set! f32vector->byte-vector (pack 'f32vector 'f32vector->byte-vector))
  (set! f64vector->byte-vector (pack 'f64vector 'f64vector->byte-vector)) 

  (set! byte-vector->u8vector (unpack 'u8vector #t 'byte-vector->u8vector))
  (set! byte-vector->s8vector (unpack 's8vector #t 'byte-vector->s8vector))
  (set! byte-vector->u16vector (unpack 'u16vector 2 'byte-vector->u16vector))
  (set! byte-vector->s16vector (unpack 's16vector 2 'byte-vector->s16vector))
  (set! byte-vector->u32vector (unpack 'u32vector 4 'byte-vector->u32vector))
  (set! byte-vector->s32vector (unpack 's32vector 4 'byte-vector->s32vector))
  (set! byte-vector->f32vector (unpack 'f32vector 4 'byte-vector->f32vector))
  (set! byte-vector->f64vector (unpack 'f64vector 8 'byte-vector->f64vector)) )


;;; Read syntax:

(set! ##sys#user-read-hook
  (let ([old-hook ##sys#user-read-hook]
	[read read]
	[consers (list 'u8 list->u8vector
		       's8 list->s8vector
		       'u16 list->u16vector
		       's16 list->s16vector
		       'u32 list->u32vector
		       's32 list->s32vector
		       'f32 list->f32vector
		       'f64 list->f64vector) ] )
    (lambda (char port)
      (if (memq char '(#\u #\s #\f #\U #\S #\F))
	  (let* ([x (read port)]
		 [tag (and (symbol? x) x)] )
	    (cond [(or (eq? tag 'f) (eq? tag 'F)) #f]
		  [(memq tag consers) => (lambda (c) ((##sys#slot (##sys#slot c 1) 0) (read port)))]
		  [else (##sys#error "illegal bytevector syntax" tag)] ) )
	  (old-hook char port) ) ) ) )


;;; Printing:

(set! ##sys#user-print-hook
  (let ((old-hook ##sys#user-print-hook))
    (lambda (x readable port)
      (let ((tag (assq (##core#inline "C_slot" x 0)
		       `((u8vector u8 ,u8vector->list)
			 (s8vector s8 ,s8vector->list)
			 (u16vector u16 ,u16vector->list)
			 (s16vector s16 ,s16vector->list)
			 (u32vector u32 ,u32vector->list)
			 (s32vector s32 ,s32vector->list)
			 (f32vector f32 ,f32vector->list)
			 (f64vector f64 ,f64vector->list) ) ) ) )
	(cond (tag
	       (##sys#print #\# #f port)
	       (##sys#print (cadr tag) #f port)
	       (##sys#print ((caddr tag) x) #t port) )
	      (else (old-hook x readable port)) ) ) ) ) )


;;; Subvectors:

(define (subvector v t es from to loc)
  (##sys#check-structure v t loc)
  (let* ([bv (##sys#slot v 1)]
	 [len (##sys#size bv)]
	 [ilen (##core#inline "C_fixnum_divide" len es)] )
    (##sys#check-range from 0 (fx+ ilen 1) loc)
    (##sys#check-range to 0 (fx+ ilen 1) loc)
    (let* ([size2 (fx* es (fx- to from))]
	   [bv2 (##sys#allocate-vector size2 #t #f #t)] )
      (##core#inline "C_string_to_bytevector" bv2)
      (let ([v (##sys#make-structure t bv2)])
	(##core#inline "C_copy_subvector" bv2 bv 0 (fx* from es) size2)
	v) ) ) )

(define (subu8vector v from to) (subvector v 'u8vector 1 from to 'subu8vector))
(define (subu16vector v from to) (subvector v 'u16vector 2 from to 'subu16vector))
(define (subu32vector v from to) (subvector v 'u32vector 4 from to 'subu32vector))
(define (subs8vector v from to) (subvector v 's8vector 1 from to 'subs8vector))
(define (subs16vector v from to) (subvector v 's16vector 2 from to 'subs16vector))
(define (subs32vector v from to) (subvector v 's32vector 4 from to 'subs32vector))
(define (subf32vector v from to) (subvector v 'f32vector 4 from to 'subf32vector))
(define (subf64vector v from to) (subvector v 'f64vector 8 from to 'subf64vector))

(define (write-u8vector v #!optional (port ##sys#standard-output) (from 0) (to (u8vector-length v)))
  (##sys#check-structure v 'u8vector 'write-u8vector)
  (##sys#check-port port 'write-u8vector)
  (let ((buf (##sys#slot v 1)))
    (do ((i from (fx+ i 1)))
	((fx>= i to))
      (##sys#write-char-0 (integer->char (##core#inline "C_u8peek" buf i)) port) ) ) )

(define (read-u8vector! n dest #!optional (port ##sys#standard-input) (start 0))
  (##sys#check-port port 'read-u8vector!)
  (##sys#check-exact start 'read-u8vector!)
  (##sys#check-structure dest 'u8vector 'read-u8vector!)
  (let ((dest (##sys#slot dest 1)))
    (when n
      (##sys#check-exact n 'read-u8vector!)
      (when (fx> (fx+ start n) (##sys#size dest))
	(set! n (fx- (##sys#size dest) start))))
    (##sys#read-string! n dest port start) ) )

(define read-u8vector
  (let ((open-output-string open-output-string)
	(get-output-string get-output-string) )
    (lambda (#!optional n (p ##sys#standard-input))
      (##sys#check-port p 'read-u8vector)
      (cond (n (##sys#check-exact n 'read-u8vector)
	       (let* ((str (##sys#allocate-vector n #t #f #t))
		      (n2 (##sys#read-string! n str p 0)) )
		 (##sys#make-structure
		  'u8vector
		  (if (eq? n n2)
		      str
		      (let ((str2 (##sys#allocate-vector n2 #t #f #t)))
			(##core#inline "C_substring_copy" str str2 0 n2 0)
			str2) ) ) ) )
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

(register-feature! 'srfi-4)
