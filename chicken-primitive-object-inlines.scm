;;;; chicken-primitive-object-nlines.scm
;;;; Kon Lovett, Jan '09
;;;; (Was chicken-sys-macros.scm)

; ***** SHOULD RENAME SAFE ROUTINES AS '*foo', KEEPING '%foo' FOR UNSAFE *****

; Usage
;
; (include "chicken-primitive-object-inlines")

;; Notes
;;
;; Provides inlines for primitive procedures. Use of these procedures
;; by non-core is highly suspect. Many of these routines are unsafe.
;;
;; In fact, any use is suspect ;-)
;;
;; A ##core#Inline is just what it says - literal inclusion in the compiled C
;; code of the C macro/function and the arguments taken literally, i.e. as the
;; C_word value.
;;
;; These are much faster than a lambda, but very dangerous since the arguments and
;; the return value are not converted. The C code must perform any such conversions.
;;
;; ##core#inline cannot be used with a runtime C function which is coded in the
;; CPS style.
;;
;; A ##core#primitive creates a lambda for a C function which is coded in the
;; CPS style.
;;
;; These have a stereotypical argument list which begins the 3 arguments C_word
;; c, C_word closure, and C_word k. Any actual arguments follow.
;;
;; c       - number of arguments, not including 'c', but including 'closure' & 'k'
;; closure - caller
;; k       - continuation

;;; Unsafe Type Predicates

;; Fixnum

(define-inline (%fixnum-type? x) (##core#inline "C_fixnump" x))

;; Character

(define-inline (%char-type? x) (##core#inline "C_charp" x))

;; Boolean

(define-inline (%boolean-type? x) (##core#inline "C_booleanp" x))

;; EOF

(define-inline (%eof-object-type? x) (##core#inline "C_eofp" x))

;; Null (the end-of-list value)

(define-inline (%eol-object-type? x) (##core#inline "C_i_nullp" x))

;; Undefined (void)

(define-inline (%undefined-type? x) (##core#inline "C_undefinedp" x))

;; Unbound (the unbound value, not 'is a symbol unbound')

(define-inline (%unbound-type? x) (##core#inline "C_unboundvaluep" x))

;; Byteblock

(define-inline (%byteblock-type? x) (##core#inline "C_byteblockp" x))

;; Bytevector

(define-inline (%bytevector-type? x) (##core#inline "C_bytevectorp" x))

;; String

(define-inline (%string-type? x) (##core#inline "C_stringp" x))

;; Flonum

(define-inline (%flonum-type? x) (##core#inline "C_flonump" x))

;; Lambda-info

(define-inline (%lambda-info-type? x) (##core#inline "C_lambdainfop" x))

;; Vector

(define-inline (%vector-type? x) (##core#inline "C_vectorp" x))

;; Pair

(define-inline (%pair-type? x) (##core#inline "C_pairp" x))

;; Bucket

; A bucket is used by the runtime for the symbol-table. The bucket type is not
; "seen" by Scheme code.

;; Structure

(define-inline (%structure-type? x) (##core#inline "C_structurep" x))

;; Symbol

(define-inline (%symbol-type? x) (##core#inline "C_symbolp" x))

;; Closure

(define-inline (%closure-type? x) (##core#inline "C_closurep" x))

;; Port

(define-inline (%port-type? x) (##core#inline "C_portp" x))

;; Any-pointer

(define-inline (%any-pointer-type? x) (##core#inline "C_anypointerp" x))

;; Simple-pointer

(define-inline (%simple-pointer-type? x) (##core#inline "C_pointerp" x))

;; Tagged-Pointer

(define-inline (%tagged-pointer-type? x) (##core#inline "C_taggedpointerp" x))

;; Swig-Pointer

(define-inline (%swig-pointer-type? x) (##core#inline "C_swigpointerp" x))

;; Locative

(define-inline (%locative-type? x) (##core#inline "C_locativep" x))

;;; Safe Type Predicates

;; Immediate

(define-inline (%immediate? x) (##core#inline "C_immp" x))

;; Fixnum

(define-inline (%fixnum? x) (and (%immediate? x) (%fixnum-type? x)))

;; Character

(define-inline (%char? x) (and (%immediate? x) (%char-type? x)))

;; Boolean

(define-inline (%boolean? x) (and (%immediate? x) (%boolean-type? x)))

(define-inline (%true-value? x) (and (%boolean? x) (##core#inline "C_and" x #t)))
(define-inline (%false-value? x) (not (%true-value? x)))

;; EOF

(define-inline (%eof-object? x) (and (%immediate? x) (%eof-object-type? x)))

;; Null (the end-of-list value)

(define-inline (%eol-object? x) (and (%immediate? x) (%eol-object-type? x)))

;; Undefined (void)

(define-inline (%undefined-value? x) (and (%immediate? x) (%undefined-type? x)))

(define-inline (%undefined-value) (##core#undefined))

;; Unbound (the unbound value, not 'is a symbol unbound')

(define-inline (%unbound-value? x) (and (%immediate? x) (%unbound-type? x)))

;; Block (anything not immediate)

(define-inline (%block? x) (##core#inline "C_blockp" x))

;; Special

(define-inline (%special? x) (##core#inline "C_specialp" x))

;; Byteblock

(define-inline (%byteblock? x) (and (%block? x) (%byteblock-type? x)))

;; Bytevector

(define-inline (%bytevector? x) (and (%block? x) (%bytevector-type? x)))

;; String

(define-inline (%string? x) (and (%block? x) (%string-type? x)))

;; Flonum

(define-inline (%flonum? x) (and (%block? x) (%flonum-type? x)))

;; Lambda-info

(define-inline (%lambda-info? x) (and (%block? x) (%lambda-info-type? x)))

;; Wordblock (special block)

(define-inline (%wordblock? x) (and (%block? x) (%special? x)))

;; Vector

(define-inline (%vector? x) (and (%block? x) (%vector-type? x)))

;; Pair

(define-inline (%pair? x) (and (%block? x) (%pair-type? x)))

;; Bucket

; A bucket is used by the runtime for the symbol-table. The bucket type is not
; "seen" by Scheme code.

;; Structure

(define-inline (%structure? x) (and (%block? x) (%structure-type? x)))

;; Symbol

(define-inline (%symbol? x) (and (%block? x) (%symbol-type? x)))

;; Closure

(define-inline (%closure? x) (and (%block? x) (%closure-type? x)))

;; Port

(define-inline (%port? x) (and (%block? x) (%port-type? x)))

;; Any-pointer

(define-inline (%pointer? x) (and (%block? x) (%any-pointer-type? x)))

;; Simple-pointer

(define-inline (%simple-pointer? x) (and (%block? x) (%simple-pointer-type? x)))

;; Tagged-Pointer

(define-inline (%tagged-pointer? x) (and (%block? x) (%tagged-pointer-type? x)))

;; Swig-Pointer

(define-inline (%swig-pointer? x) (and (%block? x) (%swig-pointer-type? x)))

;; Locative

(define-inline (%locative? x) (and (%block? x) (%locative-type? x)))

;; Forwarded (block object moved to new address, forwarding pointer)

(define-inline (%forwarded? x) (##core#inline "C_forwardedp" x))

;;; Operations

;Safe

(define-inline (%eq? x y) (##core#inline "C_eqp" x y))

;; Fixnum

;Safe

(define-inline (%fxrandom x) (##core#inline "C_random_fixnum" x))

;Unsafe

(define-inline (%fx= x y) (%eq? x y))
(define-inline (%fx> x y) (##core#inline "C_fixnum_greaterp" x y))
(define-inline (%fx< x y) (##core#inline "C_fixnum_lessp" x y))
(define-inline (%fx>= x y) (##core#inline "C_fixnum_greater_or_equal_p" x y))
(define-inline (%fx<= x y) (##core#inline "C_fixnum_less_or_equal_p" x y))

(define-inline (%fxclosed-right? l x h) (and (fx%< l x) (%fx<= x h)))
(define-inline (%fxclosed? l x h) (and (%fx<= l x) (%fx<= x h)))
(define-inline (%fxclosed-left? l x h) (and (%fx<= l x) (%fx< x h)))

(define-inline (%fxzero? fx) (%fx= 0 fx))
(define-inline (%fxpositive? fx) (%fx< 0 fx))
(define-inline (%fxnegative? fx) (%fx< fx 0))
(define-inline (%fxcardinal? fx) (%fx<= 0 fx))
(define-inline (%fxodd? fx) (%fx= 1 (%fxand fx 1)))
(define-inline (%fxeven? fx) (%fx= 0 (%fxand fx 1)))

(define-inline (%fxmin x y) (if (%fx< x y) x y))
(define-inline (%fxmax x y) (if (%fx< x y) y x))

(define-inline (%fx+ x y) (##core#inline "C_fixnum_plus" x y))
(define-inline (%fx- x y) (##core#inline "C_fixnum_difference" x y))
(define-inline (%fx* x y) (##core#inline "C_fixnum_times" x y))
(define-inline (%fx/ x y) (##core#inline "C_fixnum_divide" x y))
(define-inline (%fxmod x y) (##core#inline "C_fixnum_modulo" x y))

(define-inline (%fxadd1 fx) (##core#inline "C_fixnum_increase" fx))
(define-inline (%fxsub1 fx) (##core#inline "C_fixnum_decrease" fx))

(define-inline (%fxshl x y) (##core#inline "C_fixnum_shift_left" x y))
(define-inline (%fxshr x y) (##core#inline "C_fixnum_shift_right" x y))

(define-inline (%fxneg x) (##core#inline "C_fixnum_negate" x))
(define-inline (%fxabs fx) (if (%fxnegative? fx) (%fxneg fx) fx))

(define-inline (%fxand x y) (##core#inline "C_fixnum_and" x y))
(define-inline (%fxior x y) (##core#inline "C_fixnum_or" x y))
(define-inline (%fxxor x y) (##core#inline "C_fixnum_xor" x y))
(define-inline (%fxnot x) (##core#inline "C_fixnum_not" x))

;; Block

(define-inline (%peek-signed-integer b i) ((##core#primitive "C_peek_signed_integer") b i))
(define-inline (%peek-unsigned-integer b i) ((##core#primitive "C_peek_unsigned_integer") b i))
(define-inline (%poke-integer b i n) (##core#inline "C_poke_integer" b i n))

;Safe

(define-inline (%block-address b) (##core#inline_allocate ("C_block_address" 4) b))

;; Size of object in units of sub-object.

; (%block-allocate size byteblock? fill aligned-8-byte-boundry?)
;
; byteblock? #t - size is # of bytes, fill is-a character  -> "string"
; byteblock? #f - size is # of words, fill is-a any        -> "vector"

(define-inline (%block-allocate n bb? f a?) ((##core#primitive "C_allocate_vector") n bb? f a?))

;Unsafe

; Byteblock -> # of bytes
; Wordblock -> # of words.

(define-inline (%block-size b) (##core#inline "C_block_size" b))

;;

;; Byteblock

;Safe

(define-inline (%make-byteblock n f a?) (%block-allocate n #t f a?))

;Unsafe

(define-inline (%byteblock-length bb) (%block-size bb))

(define-inline (%byteblock-ref bb i) (##core#inline "C_subbyte" bb i))

(define-inline (%byteblock-set! bb i v) (##core#inline "C_setsubbyte" bb i v))

;; Generic-byteblock

;Safe

; generic-byteblock isa bytevector, string, flonum, or lambda-info
(define-inline (%generic-byteblock? x)
  (or (%bytevector? x) (%string? x) (%flonum? x) (%lambda-info? x)) )

;; Bytevector (byteblock)

;Safe

(define-inline (%make-bytevector sz)
  (let ((bv (%make-byteblock sz #f #t)))
    (##core#inline "C_string_to_bytevector" bv)
    bv ) )

(define-inline (%string->bytevector s)
  (let* ((n (%byteblock-length s) #;(%string-size s))
	       (bv (%make-bytevector sz)) )
    (##core#inline "C_copy_memory" bv s n)
    bv ) )

;Unsafe

(define-inline (%bytevector-length bv) (%byteblock-length bv))

(define-inline (%bytevector=? bv1 bv2)
  (let ((n (%bytevector-length bv1)))
    (and (%fx= n (%bytevector-length bv2))
         (%fx= 0 (##core#inline "C_string_compare" bv1 bv2 n)) ) ) )

(define-inline (%bytevector-ref bv i) (%byteblock-ref bv i))

(define-inline (%bytevector-set! bv i x) (%byteblock-set! bv i x))

;; Blob (isa bytevector w/o accessors)

(define-inline (%make-blob sz) (%make-bytevector sz))

(define-inline (%string->blob s) (%string->bytevector s))

(define-inline (%blob? x) (%bytevector? x))

(define-inline (%blob-size b) (%bytevector-length b))

(define-inline (%blob=? b1 b2) (%bytevector=? b1 b2))

;; String (byteblock)

;Safe

(define-inline (%make-string size fill) (%make-byteblock size fill #f))

;Unsafe

(define-inline (%bytevector->string bv)
  (let* ((n (%bytevector-length bv))
	       (s (%make-string n #\space)) )
    (##core#inline "C_copy_memory" s bv n)
    s ) )

(define-inline (%blob->string bv) (%bytevector->string bv))

(define-inline (%lambda-info->string li)
  (let* ((sz (%byteblock-length li) #;(%lambda-info-length li))
         (s (%make-string sz #\space)) )
    (##core#inline "C_copy_memory" s li sz)
    s ) )

(define-inline (%string-size s) (%byteblock-length s))
(define-inline (%string-length s) (%byteblock-length s))

(define-inline (%string-ref s i) (##core#inline "C_subchar" s i))

(define-inline (%string-set! s i c) (##core#inline "C_setsubchar" s i c))

(define-inline (%string-compare/length s1 s2 l) (##core#inline "C_string_compare" s1 s2 l))

(define-inline (%string-compare s1 s2)
  (let* ((l1 (%string-length s1))
         (l2 (%string-length s2))
         (d (%fx- l1 l2))
         (r (%string-compare/length s1 s2 (if (%fxpositive? d) l2 l1))) )
    (if (%fxzero? r) d
        r ) ) )

(define-inline (%string=? s1 s2) (%fxzero? (%string-compare s1 s2)))
(define-inline (%string<? s1 s2) (%fxnegative? (%string-compare s1 s2)))
(define-inline (%string>? s1 s2) (%fxpositive? (%string-compare s1 s2)))
(define-inline (%string<=? s1 s2) (%fx<= 0 (%string-compare s1 s2)))
(define-inline (%string>=? s1 s2) (%fx>= 0 (%string-compare s1 s2)))

(define-inline (%string-ci-compare/length s1 s2 l) (##core#inline "C_string_compare_case_insensitive" s1 s2 l))

(define-inline (%string-ci-compare s1 s2)
  (let* ((l1 (%string-length s1))
         (l2 (%string-length s2))
         (d (%fx- l1 l2))
         (r (%string-ci-compare/length s1 s2 (if (%fxpositive? d) l2 l1))) )
    (if (%fxzero? r) d
        r ) ) )

(define-inline (%string-ci=? s1 s2) (%fxzero? (%string-ci-compare s1 s2)))
(define-inline (%string-ci<? s1 s2) (%fxnegative? (%string-ci-compare s1 s2)))
(define-inline (%string-ci>? s1 s2) (%fxpositive? (%string-ci-compare s1 s2)))
(define-inline (%string-ci<=? s1 s2) (%fx<= 0 (%string-ci-compare s1 s2)))
(define-inline (%string-ci>=? s1 s2) (%fx>= 0 (%string-ci-compare s1 s2)))

;; Flonum (byteblock)

;Unsafe

(define-inline (%fp= x y) (##core#inline "C_flonum_equalp" x y))
(define-inline (%fp< x y) (##core#inline "C_flonum_lessp" x y))
(define-inline (%fp<= x y) (##core#inline "C_flonum_less_or_equal_p" x y))
(define-inline (%fp> x y) (##core#inline "C_flonum_greaterp" x y))
(define-inline (%fp>= x y) (##core#inline "C_flonum_greater_or_equal_p" x y))

(define-inline (%fpmax x y) (##core#inline "C_i_flonum_max" x y))
(define-inline (%fpmin x y) (##core#inline "C_i_flonum_min" x y))

(define-inline (%finite? x) (##core#inline "C_i_finitep" x))

(define-inline (%fp- x y) (##core#inline_allocate ("C_a_i_flonum_difference" 4) x y))
(define-inline (%fp* x y) (##core#inline_allocate ("C_a_i_flonum_times" 4) x y))
(define-inline (%fp/ x y) (##core#inline_allocate ("C_a_i_flonum_quotient" 4) x y))
(define-inline (%fp+ x y) (##core#inline_allocate ("C_a_i_flonum_plus" 4) x y))

(define-inline (%fpfraction x) ((##core#primitive "C_flonum_fraction") x))

(define-inline (%fpnegate x) (##core#inline_allocate ("C_a_i_flonum_negate" 4) x))

(define-inline (%fpfloor x) ((##core#primitive "C_flonum_floor") x))
(define-inline (%fpceiling x) ((##core#primitive "C_flonum_ceiling") x))
(define-inline (%fpround x) ((##core#primitive "C_flonum_round") x))
(define-inline (%fptruncate x) ((##core#primitive "C_flonum_truncate") x))

;Safe

(define-inline (%exact->inexact x) ((##core#primitive "C_exact_to_inexact") x))

; Actually 'number' operations
(define-inline (%fpabs x) (##core#inline_allocate ("C_a_i_abs" 4) x))
(define-inline (%fpacos x) (##core#inline_allocate ("C_a_i_acos" 4) x))
(define-inline (%fpasin x) (##core#inline_allocate ("C_a_i_asin" 4) x))
(define-inline (%fpatan x) (##core#inline_allocate ("C_a_i_atan" 4) x))
(define-inline (%fpatan2 x y) (##core#inline_allocate ("C_a_i_atan2" 4) x y))
(define-inline (%fpcos x) (##core#inline_allocate ("C_a_i_cos" 4) x))
(define-inline (%fpexp x) (##core#inline_allocate ("C_a_i_exp" 4) x))
(define-inline (%fplog x) (##core#inline_allocate ("C_a_i_log" 4) x))
(define-inline (%fpsin x) (##core#inline_allocate ("C_a_i_sin" 4) x))
(define-inline (%fpsqrt x) (##core#inline_allocate ("C_a_i_sqrt" 4) x))
(define-inline (%fptan x) (##core#inline_allocate ("C_a_i_tan" 4) x))

;; Lambda-info (byteblock)

;Unsafe

(define-inline (%string->lambda-info s)
  (let* ((n (%string-size s))
	       (li (%make-string n)) )
    (##core#inline "C_copy_memory" li s n)
    (##core#inline "C_string_to_lambdainfo" li)
    li ) )

(define-inline (%lambda-info-length li) (%byteblock-length s))

;; Wordblock

;Safe

(define-inline (%make-wordblock n f a?) (%block-allocate n #f f a?))

;Unsafe

(define-inline (%wordblock-length wb) (%block-size wb))

(define-inline (%wordblock-ref wb i) (##core#inline "C_slot" wb i))

(define-inline (%wordblock-set!/mutate wb i v) (##core#inline "C_i_setslot" wb i v))
(define-inline (%wordblock-set!/immediate wb i v) (##core#inline "C_i_set_i_slot" wb i v))
(define-inline (%wordblock-set! wb i v)
  (if (%immediate? v) (%wordblock-set!/immediate wb i v)
      (%wordblock-set!/mutate wb i v) ) )

;; Generic-vector (wordblock)

; generic-vector isa vector, pair, structure, symbol, or keyword
(define-inline (%generic-vector? x) (and (%block? x) (not (or (%special? x) (%byteblock? x)))))

;; Vector (wordblock)

;Safe

(define-inline (%make-vector size fill) (%make-wordblock size fill #f))

;Unsafe

(define-inline (%vector-length v) (%wordblock-length v))

(define-inline (%vector-ref v i) (%wordblock-ref v i))

(define-inline (%vector-set!/mutate v i x) (%wordblock-set!/mutate v i x))
(define-inline (%vector-set!/immediate v i x) (%wordblock-set!/immediate v i x))
(define-inline (%vector-set! v i x) (%wordblock-set! v i x))

;; Pair (wordblock)

;Safe

(define-inline (%null? x) (%eol-object? x))

(define-inline (%list? x) (or (%null? x) (%pair? x)))

(define-inline (%cons x y) (##core#inline_allocate ("C_a_i_cons" 3) x y) )

(define-inline (%length ls) (##core#inline "C_i_length" ls))

;Unsafe

(define-inline (%car pr) (%wordblock-ref pr 0))

(define-inline (%set-car!/mutate pr x) (%wordblock-set!/mutate pr 0 x))
(define-inline (%set-car!/immediate pr x) (%wordblock-set!/immediate pr 0 x))
(define-inline (%set-car! pr x) (%wordblock-set! pr 0 x))

(define-inline (%cdr pr) (%wordblock-ref pr 1))

(define-inline (%set-cdr!/mutate pr x) (%wordblock-set!/mutate pr 1 x))
(define-inline (%set-cdr!/immediate pr x) (%wordblock-set!/immediate pr 1 x))
(define-inline (%set-cdr! pr x) (%wordblock-set! pr 1 x))

(define-inline (%caar pr) (%car (%car pr)))
(define-inline (%cadr pr) (%car (%cdr pr)))
(define-inline (%cdar pr) (%cdr (%car pr)))
(define-inline (%cddr pr) (%cdr (%cdr pr)))

(define-inline (%caaar pr) (%car (%caar pr)))
(define-inline (%caadr pr) (%car (%cadr pr)))
(define-inline (%cadar pr) (%car (%cdar pr)))
(define-inline (%caddr pr) (%car (%cddr pr)))
(define-inline (%cdaar pr) (%cdr (%caar pr)))
(define-inline (%cdadr pr) (%cdr (%cadr pr)))
(define-inline (%cddar pr) (%cdr (%cdar pr)))
(define-inline (%cdddr pr) (%cdr (%cddr pr)))

;Safe

(define-inline (%memq x ls) (##core#inline "C_i_memq" x ls))
(define-inline (%memv x ls) (##core#inline "C_i_memv" x ls))
(define-inline (%member x ls) (##core#inline "C_i_member" x ls))

(define-inline (%assq x ls) (##core#inline "C_i_assq" x ls))
(define-inline (%assv x ls) (##core#inline "C_i_assv" x ls))
(define-inline (%assoc x ls) (##core#inline "C_i_assoc" x ls))

;Unsafe

(define-inline (%list-ref ls0 i0)
  ;(assert (and (proper-list? ls0) (exact? i0) (<= 0 i0 (sub1 (length ls0)))))
  (let loop ((ls ls0) (i i0))
    (cond ((%null? ls)  '() )
	        ((%fx= 0 i)   (%car ls) )
	        (else         (loop (%cdr ls) (%fx- i 1)) ) ) ) )

(define-inline (%list-pair-ref ls0 i0)
  ;(assert (and (proper-list? ls0) (exact? i0) (<= 0 i0 (sub1 (length ls0)))))
  (let loop ((ls ls0) (i i0))
    (cond ((%null? ls)  '() )
	        ((%fx= 0 i)   ls )
	        (else         (loop (%cdr ls) (%fx- i 1)) ) ) ) )

(define-inline (%last-pair ls0)
  ;(assert (and (proper-list? ls0) (pair? ls0)))
  (do ((ls ls0 (%cdr ls)))
      ((%null? (%cdr ls)) ls)) )

(define-inline (%list-copy ls0)
  ;(assert (proper-list? ls0))
  (let copy-rest ((ls ls0))
    (if (%null? ls) '()
        (%cons (%car ls) (copy-rest (%cdr ls))) ) ) )

(define-inline (%append! . lss)
  ;(assert (and (proper-list? lss) (for-each (cut proper-list? <>) lss)))
  (let ((lss (let position-at-first-pair ((lss lss))
               (cond ((%null? lss)        '() )
                     ((%null? (%car lss))  (position-at-first-pair (%cdr lss)) )
                     (else                 lss ) ) ) ) )
    (if (%null? lss) '()
        (let ((ls0 (%car lss)))
          ;(assert (pair? ls0))
          (let append!-rest ((lss (%cdr lss)) (pls ls0))
            (if (%null? lss) ls0
                (let ((ls (%car lss)))
                  (cond ((%null? ls)
                         (append!-rest (%cdr lss) pls) )
                        (else
                         (%set-cdr!/mutate (%last-pair pls) ls)
                         (append!-rest (%cdr lss) ls) ) ) ) ) ) ) ) ) )

(define-inline (%delq! x ls0)
  ;(assert (proper-list? ls0))
  (let find-elm ((ls ls0) (ppr #f))
    (cond ((%null? ls)
           ls0 )
	        ((%eq? x (%car ls))
	         (cond (ppr
	                (%set-cdr! ppr (%cdr ls))
	                ls0 )
	               (else
	                (%cdr ls) ) ) )
	        (else
	         (find-elm (%cdr ls) ls) ) ) ) )

(define-inline (%list-fold/1 func init ls0)
  ;(assert (and (proper-list? ls0) (procedure? func)))
  (let loop ((ls ls0) (acc init))
    (if (%null? ls) acc
        (loop (%cdr ls) (func (%car ls) acc)) ) ) )

(define-inline (%list-map/1 func ls0)
  ;(assert (and (proper-list? ls0) (procedure? func)))
  (let loop ((ls ls0))
    (if (%null? ls) '()
        (%cons (func (%car ls)) (loop (%cdr ls))) ) ) )

(define-inline (%list-for-each/1 proc ls0)
  ;(assert (and (proper-list? ls0) (procedure? proc)))
  (let loop ((ls ls0))
    (unless (%null? ls)
      (proc (%car ls))
      (loop (%cdr ls)) ) ) )

(define-inline (%list/1 obj) (%cons obj '()))

(define-inline (%list . objs)
  (let loop ((objs objs))
    (if (%null? objs) '()
        (%cons (%car objs) (loop (%cdr objs)) ) ) ) )

(define-inline (%make-list n e)
  (let loop ((n n) (ls '()))
    (if (%fxzero? n) ls
        (loop (%fxsub1 n) (%cons e ls)) ) ) )

(define-inline (%list-take ls0 n)
  (let loop ((ls ls0) (n n))
    (if (%fxzero? n) '()
        (%cons (%car ls) (loop (%cdr ls) (%fxsub1 n))) ) ) )

(define-inline (%list-drop ls0 n)
  (let loop ((ls ls0) (n n))
    (if (%fxzero? n) ls
        (loop (%cdr ls) (%fxsub1 n)) ) ) )

(define-inline (%any/1 pred? ls)
  (let loop ((ls ls))
    (and (not (%null? ls))
         (or (pred? (%car ls))
             (loop (%cdr ls)) ) ) ) )

(define-inline (%list-length ls0)
  (let loop ((ls ls0) (n 0))
    (if (%null? ls) n
        (loop (%cdr ls) (%fxadd1 n)) ) ) )

;; Structure (wordblock)

(define-inline (%make-structure t . s) (apply (##core#primitive "C_make_structure") t s))

(define-inline (%structure-instance? x s) (##core#inline "C_i_structurep" x s))

(define-inline (%structure-length r) (%wordblock-length r))

(define-inline (%structure-tag r) (%wordblock-ref r 0))

(define-inline (%structure-ref r i) (%wordblock-ref r i))

(define-inline (%structure-set!/mutate r i x) (%wordblock-set!/mutate r i x))
(define-inline (%structure-set!/immediate r i x) (%wordblock-set!/immediate r i x))
(define-inline (%structure-set! r i x) (%wordblock-set! r i x))

;; Port (wordblock)

; Port layout:
;
; 0	  FP (special - FILE *)
; 1	  input/output (bool)
; 2	  class (vector, see Port-class)
; 3	  name (string)
; 4	  row (fixnum)
; 5	  col (fixnum)
; 6	  EOF (bool)
; 7	  type (symbol)
; 8	  closed (bool)
; 9	  data
; 10-15	 reserved, port class specific

(define-inline (%port-filep port) (%peek-unsigned-integer port 0))
(define-inline (%port-input-mode? port) (%wordblock-ref port 1))
(define-inline (%port-class port) (%wordblock-ref port 2))
(define-inline (%port-name port) (%wordblock-ref port 3))
(define-inline (%port-row port) (%wordblock-ref port 4))
(define-inline (%port-column port) (%wordblock-ref port 5))
(define-inline (%port-eof? port) (%wordblock-ref port 6))
(define-inline (%port-type port) (%wordblock-ref port 7))
(define-inline (%port-closed? port) (%wordblock-ref port 8))
(define-inline (%port-data port) (%wordblock-ref port 9))

(define-inline (%input-port? x) (and (%port? x) (%port-input-mode? x)))
(define-inline (%output-port? x) (and (%port? x) (not (%port-input-mode? x))))

(define-inline (%port-filep-set! port fp) (%poke-integer port 0 fp))
(define-inline (%port-input-mode-set! port f) (%wordblock-set!/immediate port 1 f))
(define-inline (%port-class-set! port v) (%wordblock-set!/mutate port 2 v))
(define-inline (%port-name-set! port s) (%wordblock-set!/mutate port 3 s))
(define-inline (%port-row-set! port n) (%wordblock-set!/immediate port 4 n))
(define-inline (%port-column-set! port n) (%wordblock-set!/immediate port 5 n))
(define-inline (%port-eof-set! port f) (%wordblock-set!/immediate port 6 f))
(define-inline (%port-type-set! port s) (%wordblock-set!/mutate port 7 s))
(define-inline (%port-closed-set! port f) (%wordblock-set!/immediate port 8 f))
(define-inline (%port-data-set! port x) (%wordblock-set!/mutate port 9 x))

(define-inline (%make-port i/o class name type)
  ; port is 16 slots + a block-header word
  (let ((port (##core#inline_allocate ("C_a_i_port" 17))))
    (%port-input-mode-set! port i/o)
    (%port-class-set! port class)
    (%port-name-set! port name)
    (%port-row-set! port 1)
    (%port-column-set! port 0)
    (%port-type-set! port type)
    port ) )

; Port-class layout
;
; 0	  (read-char PORT) -> CHAR | EOF
; 1	  (peek-char PORT) -> CHAR | EOF
; 2	  (write-char PORT CHAR)
; 3	  (write-string PORT STRING)
; 4	  (close PORT)
; 5	  (flush-output PORT)
; 6	  (char-ready? PORT) -> BOOL
; 7	  (read-string! PORT COUNT STRING START) -> COUNT'
; 8	  (read-line PORT LIMIT) -> STRING | EOF

(define-inline (%make-port-class rc pc wc ws cl fl cr rs rl)
  (let ((class (%make-vector 9 #f)))
    (%vector-set! class 0 rc)
    (%vector-set! class 1 pc)
    (%vector-set! class 2 wc)
    (%vector-set! class 3 ws)
    (%vector-set! class 4 cl)
    (%vector-set! class 5 fl)
    (%vector-set! class 6 cr)
    (%vector-set! class 7 rs)
    (%vector-set! class 8 rl)
    class ) )

(define-inline (%port-class-read-char-ref c) (%vector-ref c 0))
(define-inline (%port-class-peek-char-ref c) (%vector-ref c 1))
(define-inline (%port-class-write-char-ref c) (%vector-ref c 2))
(define-inline (%port-class-write-string-ref c) (%vector-ref c 3))
(define-inline (%port-class-close-ref c) (%vector-ref c 4))
(define-inline (%port-class-flush-output-ref c) (%vector-ref c 5))
(define-inline (%port-class-char-ready-ref c) (%vector-ref c 6))
(define-inline (%port-class-read-string-ref c) (%vector-ref c 7))
(define-inline (%port-class-read-line-ref c) (%vector-ref c 8))

(define-inline (%port-class-read-char c p) ((%port-class-read-char-ref c) p) )
(define-inline (%port-class-peek-char c p) ((%port-class-peek-char-ref c) p))
(define-inline (%port-class-write-char c p c) ((%port-class-write-char-ref c) p c))
(define-inline (%port-class-write-string c p s) ((%port-class-write-string-ref c) p s))
(define-inline (%port-class-close c p) ((%port-class-close-ref c) p))
(define-inline (%port-class-flush-output c p) ((%port-class-flush-output-ref c) p))
(define-inline (%port-class-char-ready? c p) ((%port-class-char-ready-ref c) p))
(define-inline (%port-class-read-string! c p n d s) ((%port-class-read-string-ref c) p n d s))
(define-inline (%port-class-read-line c p l) ((%port-class-read-line-ref c) p l))

(define-inline (%port-read-char p) ((%port-class-read-char-ref (%port-class p)) p) )
(define-inline (%port-peek-char p) ((%port-class-peek-char-ref (%port-class p)) p))
(define-inline (%port-write-char p c) ((%port-class-write-char-ref (%port-class p)) p c))
(define-inline (%port-write-string p s) ((%port-class-write-string-ref (%port-class p)) p s))
(define-inline (%port-close p) ((%port-class-close-ref (%port-class p)) p))
(define-inline (%port-flush-output p) ((%port-class-flush-output-ref (%port-class p)) p))
(define-inline (%port-char-ready? p) ((%port-class-char-ready-ref (%port-class p)) p))
(define-inline (%port-read-string! p n d s) ((%port-class-read-string-ref (%port-class p)) p n d s))
(define-inline (%port-read-line p l) ((%port-class-read-line-ref (%port-class p)) p l))

;; Closure (wordblock)

;Unsafe

(define-inline (%make-closure! n)
  (let ((v (%make-vector n)))
    (##core#inline "C_vector_to_closure" v)
    v ) )

(define-inline (%procedure? x) (%closure? x))

(define-inline (%vector->closure! v a)
  (##core#inline "C_vector_to_closure" v)
  (##core#inline "C_update_pointer" a v) )

(define-inline (%closure-length c) (%wordblock-length? c))

(define-inline (%closure-ref c i) (%wordblock-ref c i))

(define-inline (%closure-set! c i v) (%wordblock-set! c i v))

(define-inline (%closure-copy tc fc l)
  (do ((i 1 (%fxadd1 i)))
      ((%fx>= i l))
    (%closure-set! tc i (%closure-ref fc i)) ) )

(define-inline (%closure-decoration c test)
  (let find-decor ((i (%fxsub1 (%closure-length c))))
    (and (%fxpositive? i)
         (let ((x (%closure-ref c i)))
           (if (test x) x
               (find-decor (%fxsub1 i)) ) ) ) ) )

(define-inline (%closure-decorate! c test dcor)
  (let ((l (%closure-length c)))
    (let find-decor ((i (%fxsub l)))
      (cond ((%fxzero? i)
             (let ((nc (%make-closure (%fxadd1 l))))
               (%closure-copy nc c l)
               (##core#inline "C_copy_pointer" c nc)
               (dcor nc i) ) )
            (else
             (let ((x (%closure-ref c i)))
               (if (test x) (dcor c i)
                   (find-decor (%fxsub i)) ) ) ) ) ) ) )

(define-inline (%closure-lambda-info c)
  (%closure-decoration c (lambda (x) (%lambda-info? x))) )

;; Symbol (wordblock)

;Unsafe

(define-inline (%symbol-binding s) (%wordblock-ref s 0))
(define-inline (%symbol-string s) (%wordblock-ref s 1))
(define-inline (%symbol-bucket s) (%wordblock-ref s 2))

(define-constant NAMESPACE-MAX-ID-LEN 31)

(define-inline (%qualified-symbol? s)
  (let ((str (%symbol-string s)))
    (and (%fxpositive? (%string-size str))
         (%fx<= (%byteblock-ref str 0) NAMESPACE-MAX-ID-LEN) ) ) )

;Safe

(define-inline (%string->symbol-interned s) ((##core#primitive "C_string_to_symbol") s))

(define-inline (%symbol-interned? x) (##core#inline "C_lookup_symbol" x))

(define-inline (%symbol-bound? s) (##core#inline "C_boundp" s))

;; Keyword (wordblock)

(define-inline (%keyword? x) (and (%symbol? x) (%fxzero? (%byteblock-ref (%symbol-string x) 0))))

;; Pointer (wordblock)

; simple-pointer, tagged-pointer, swig-pointer, locative
(define-inline (%generic-pointer? x) (or (%pointer? x) (%locative? x)))

; simple-pointer, tagged-pointer, swig-pointer, locative, closure, port, symbol, keyword
(define-inline (%pointer-like? x) (%wordblock? x))

; These operate on pointer-like objects

(define-inline (%pointer-null? ptr) (##core#inline "C_null_pointerp" ptr))

(define-inline (%pointer-ref ptr) (%wordblock-ref ptr 0))
(define-inline (%pointer-set! ptr y) (%wordblock-set!/mutate ptr 0 y))

(define-inline (%peek-byte ptr i) (##core#inline "C_peek_byte" ptr i))

(define-inline (%pointer->address ptr)
  ; Pack pointer address value into Chicken words; '4' is platform dependent!
  (##core#inline_allocate ("C_block_address" 4) (%generic-pointer-ref ptr)) )

;; Simple-pointer (wordblock)

(define-inline (%make-simple-pointer) ((##core#primitive "C_make_pointer")))

(define-inline (%make-pointer-null)
  (let ((ptr (%make-simple-pointer)))
    (##core#inline "C_update_pointer" 0 ptr)
    ptr ) )

(define-inline (%address->pointer a)
  (let ((ptr (%make-simple-pointer)))
    (##core#inline "C_update_pointer" a ptr)
    ptr ) )

(define-inline (%make-block-pointer b)
  (let ((ptr (%make-simple-pointer)))
    (##core#inline "C_pointer_to_block" ptr b)
    ptr ) )

;; Tagged-pointer (wordblock)

(define-inline (%make-tagged-pointer t) ((##core#primitive "C_make_tagged_pointer") t))

;; Swig-pointer (wordblock)

;; Locative (wordblock)

(define-inline (%make-locative typ obj idx weak?)
  (##core#inline_allocate ("C_a_i_make_locative" 5) typ obj idx weak?))

; Locative layout:
;
; 0	Object-address + byte-offset (address)
; 1	Byte-offset (fixnum)
; 2	Type (fixnum)
;	  0	vector or pair          (C_SLOT_LOCATIVE)
;	  1	string                  (C_CHAR_LOCATIVE)
;	  2	u8vector                (C_U8_LOCATIVE)
;	  3	s8vector or bytevector  (C_U8_LOCATIVE)
;	  4	u16vector		            (C_U16_LOCATIVE)
;	  5	s16vector		            (C_S16_LOCATIVE)
;	  6	u32vector		            (C_U32_LOCATIVE)
;	  7	s32vector		            (C_S32_LOCATIVE)
;	  8	f32vector		            (C_F32_LOCATIVE)
;	  9	f64vector		            (C_F64_LOCATIVE)
; 3	Object or #f, if weak (C_word)

(define-inline (%locative-address lv) (%pointer->address lv))

(define-inline (%locative-offset lv) (%wordblock-ref lv 1))
(define-inline (%locative-type lv) (%wordblock-ref lv 2))
(define-inline (%locative-weak? lv) (not (%wordblock-ref lv 3)))
(define-inline (%locative-object lv) (%wordblock-ref lv 3))

;; Numbers

;Safe

(define-inline (%number? x) (or (%fixnum? x) (%flonum? x)))
(define-inline (%integer? x) (##core#inline "C_i_integerp" x))
(define-inline (%exact? x) (##core#inline "C_i_exactp" x))
(define-inline (%inexact? x) (##core#inline "C_i_inexactp" x))

(define-inline (%= x y) (##core#inline "C_i_eqvp" x y))
(define-inline (%< x y) (##core#inline "C_i_lessp" x y))
(define-inline (%<= x y) (##core#inline "C_i_less_or_equalp" x y))
(define-inline (%> x y) (##core#inline "C_i_greaterp" x y))
(define-inline (%>= x y) (##core#inline "C_i_greater_or_equalp" x y))

(define-inline (%zero? n) (##core#inline "C_i_zerop" n))
(define-inline (%positive? n) (##core#inline "C_i_positivep" n))
(define-inline (%negative? n) (##core#inline "C_i_negativep" n))
(define-inline (%cardinal? fx) (%<= 0 fx))

(define-inline (%odd? n) (##core#inline "C_i_oddp" n))
(define-inline (%even? n) (##core#inline "C_i_evenp" n))

(define-inline (%+ x y) ((##core#primitive "C_plus") x y))
(define-inline (%- x y) ((##core#primitive "C_minus") x y))
(define-inline (%* x y) ((##core#primitive "C_times") x y))
(define-inline (%/ x y) ((##core#primitive "C_divide") x y))

(define-inline (%add1 x) (%+ x 1))
(define-inline (%sub1 x) (%- x 1))

(define-inline (%quotient x y) ((##core#primitive "C_quotient") x y))
(define-inline (%remainder x y) (let ((quo (%quotient x y))) (%- x (%* quo y))))

(define-inline (%expt x y) ((##core#primitive "C_expt") x y))
(define-inline (%abs x) (##core#inline_allocate ("C_a_i_abs" 4) x))
(define-inline (%acos x) (##core#inline_allocate ("C_a_i_acos" 4) x))
(define-inline (%asin x) (##core#inline_allocate ("C_a_i_asin" 4) x))
(define-inline (%atan x) (##core#inline_allocate ("C_a_i_atan" 4) x))
(define-inline (%atan2 x y) (##core#inline_allocate ("C_a_i_atan2" 4) x y))
(define-inline (%cos x) (##core#inline_allocate ("C_a_i_cos" 4) x))
(define-inline (%exp x) (##core#inline_allocate ("C_a_i_exp" 4) x))
(define-inline (%log x) (##core#inline_allocate ("C_a_i_log" 4) x))
(define-inline (%sin x) (##core#inline_allocate ("C_a_i_sin" 4) x))
(define-inline (%sqrt x) (##core#inline_allocate ("C_a_i_sqrt" 4) x))
(define-inline (%tan x) (##core#inline_allocate ("C_a_i_tan" 4) x))

(define-inline (%bitwise-and x y) (##core#inline_allocate ("C_a_i_bitwise_and" 4) x y))
(define-inline (%bitwise-xor x y) (##core#inline_allocate ("C_a_i_bitwise_xor" 4) x y))
(define-inline (%bitwise-ior x y) (##core#inline_allocate ("C_a_i_bitwise_ior" 4) x y))
(define-inline (%bitwise-not x) (##core#inline_allocate ("C_a_i_bitwise_not" 4) x))

(define-inline (%arithmetic-shift x d) (##core#inline_allocate ("C_a_i_arithmetic_shift" 4) x d))

(define-inline (%bit-set? n i) (##core#inline "C_i_bit_setp" n i))

(define-inline (%randomize n) (##core#inline "C_randomize" n))

;;; Operations

;Safe

(define-inline (%->boolean obj) (and obj #t))

(define-inline (%make-unique-object #!optional id) (if id (%make-vector 1 id) '#()))
