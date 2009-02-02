;;;; chicken-primitive-inlines.scm
;;;; Kon Lovett, Oct '07

;;;; Provides inlines & macros for primitive procedures
;;;; MUST be included


;;; Type Predicates (these are not fool-proof)

;; Argument is a 'C_word'

;; Immediate

(define-inline (%immediate? ?x) (##core#inline "C_immp" x))

;; Fixnum

(define-inline (%fixnum? x) (##core#inline "C_fixnump" x))

;; Character

(define-inline (%char? x) (##core#inline "C_charp" x))

;; Boolean

(define-inline (%boolean? x) (##core#inline "C_booleanp" x))

;; True

;(define-inline (%true? x) (##core#inline "" x))

;; False

;(define-inline (%false? x) (##core#inline "" x))

;; EOF

(define-inline (%eof-object? x) (##core#inline "C_eofp" x))

;; Null (the end-of-list value)

(define-inline (%null? x) (##core#inline "C_i_nullp" x))

;; Undefined (void)

(define-inline (%undefined? x) (##core#inline "C_undefinedp" x))

;; Unbound (the unbound value, not is a symbol unbound)

(define-inline (%unbound? x) (##core#inline "C_unboundvaluep" x))

;; Block (anything not immediate)

(define-inline (%block? x) (##core#inline "C_blockp" x))

;; Forwarded (block object moved to new address, forwarding pointer)

(define-inline (%forwarded? x) (##core#inline "C_forwardedp" x))

;; Special

(define-inline (%special? x) (##core#inline "C_specialp" x))

;; Byteblock

(define-inline (%byteblock-type? x) (##core#inline "C_byteblockp" x))

;; String

(define-inline (%string-type? x) (##core#inline "C_stringp" x))

;; Flonum

(define-inline (%flonum-type? x) (##core#inline "C_flonump" x))

;; Lambda-info

(define-inline (%lambda-info-type? x) (##core#inline "C_lambdainfop" x))

;; Vector

(define-inline (%vector-type? x) (##core#inline "C_vectorp" x))

;; Bytevector

(define-inline (%bytevector-type? x) (##core#inline "C_bytevectorp" x))

;; Pair

(define-inline (%pair-type? x) (##core#inline "C_pairp" x))

;; Bucket

; A bucket is used by the runtime for the symbol-table. The bucket type is not
; "seen" by Scheme code.

;; Structure

(define-inline (%structure-type? x) (##core#inline "C_structurep" x))

;; Closure

(define-inline (%closure-type? x) (##core#inline "C_closurep" x))

;; Port

(define-inline (%port-type? x) (##core#inline "C_portp" x))

;; Symbol

(define-inline (%symbol-type? x) (##core#inline "C_symbolp" x))

;; Simple-pointer

(define-inline (%simple-pointer-type? x) (##core#inline "C_pointerp" x))

;; Tagged-Pointer

(define-inline (%tagged-pointer-type? x) (##core#inline "C_taggedpointerp" x))

;; Swig-Pointer

(define-inline (%swig-pointer-type? x) (##core#inline "C_swigpointerp" x))

;; Locative

(define-inline (%locative-type? x) (##core#inline "C_locativep" x))


;;; Values


;;; Primitive Operations

(define-inline (%eq? x y) (##core#inline "C_eqp" x y))

(define-inline %become (##core#primitive "C_become"))


;;; Complex Types

;; Size of object in units of sub-object.
;; Byteblock is # of bytes, other are # of words.

(define-inline (%size x) (##core#inline "C_block_size" x))

;; Generic-bytevector

(define-inline (%byte-ref x i) (##core#inline "C_subbyte" x i))
(define-inline (%byte-set! x i n) (##core#inline "C_setsubbyte" x i n))

;; Generic-vector

(define-inline (%generic-vector? x)
  (and (%block? x)
       (not (or (%special? x)
	              (%byteblock? x)))) )

(define-inline (%slot-ref x i) (##core#inline "C_slot" x i))

(define-inline (%slot-set! x i y) (##core#inline "C_i_setslot" x i y))
(define-inline (%slot-set-immediate! x i y) (##core#inline "C_i_set_i_slot" x i y))

(define-inline (%block-address x) (##core#inline_allocate ("C_block_address" 4) x))

(define-inline %allocate-vector (##core#primitive "C_allocate_vector"))

;; String (byteblock)

(define-inline (%string? x)
  (and (%block? x) (%string-type? x)) )

(define-inline (%make-string size fill) (%allocate-vector size #t fill #f))

(define-inline (%string-ref s i) (##core#inline "C_subchar" s i))

(define-inline (%string-set! s i c) (##core#inline "C_setsubchar" s i c))

(define-inline (%string-length s) (%size s))

;; Flonum (byteblock)

(define-inline (%flonum? x)
  (and (%block? x) (%flonum-type? x)) )

(define-inline (%flonum-magnitude f) (##core#inline "C_flonum_magnitude" x))

;; Lambda-info (byteblock)

(define-inline (%lambda-info? x)
  (and (%block? x) (%lambda-info-type? x)) )

;; Vector (wordblock)

(define-inline (%vector? x)
  (and (%block? x) (%vector-type? x)) )

(define-inline (%make-vector size fill) (%allocate-vector size #f fill #f))

(define-inline (%vector-ref v i) (%slot-ref v i))

(define-inline (%vector-set-slot! v i x) (%slot-set! v i x))
(define-inline (%vector-set-immediate! v i x) (%slot-set-immediate! v i x))

(define-inline (%vector-set! v i x)
  (if (%immediate? x)
      (%vector-set-immediate! v i x)
      (%vector-set-slot! v i x) ) )

(define-inline (%vector-length v) (%size v))

;; Bytevector (wordblock)

(define-inline (%bytevector? x)
  (and (%block? x) (%bytevector-type? x)) )

(define-inline (%bytevector-ref v i) (%byte-ref v i))

(define-inline (%bytevector-set! v i x) (%byte-set! v i x))

(define-inline (%bytevector-length v) (%size v))

(define-inline (%string->bytevector s) (##core#inline "C_string_to_pbytevector" s))

(define-inline (%bytevector=? v1 v2)
  (let ([ln (%bytevector-length v1)])
    (and (%eq? n %bytevector-length v2))
         (%eq? 0 (##core#inline "C_string_compare" v1 v2 n)) ) )

(define-inline (%blob? x) (%bytevector? x))

(define-inline (%blob-size? x) (%size? x))

;; Pair (wordblock)

(define-inline (%pair? x)
  (and (%block? x) (%pair-type? x)) )

(define-inline (%list? x)
  (or (%null? x)
      (%pair? x)) )

(define-inline (%cons x y) (##core#inline_allocate ("C_a_i_cons" 3) x y) )

(define-inline (%length x y) (##core#inline "C_i_length" lst))

(define-inline (%car p) (%slot-ref p 0))
(define-inline (%cdr p) (%slot-ref p 1))

(define-inline (%caar p) (%car (%car p)))
(define-inline (%cadr p) (%car (%cdr p)))
(define-inline (%cdar p) (%cdr (%car p)))
(define-inline (%cddr p) (%cdr (%cdr p)))

(define-inline (%caaar p) (%car (%caar p)))
(define-inline (%caadr p) (%car (%cadr p)))
(define-inline (%cadar p) (%car (%cdar p)))
(define-inline (%caddr p) (%car (%cddr p)))
(define-inline (%cdaar p) (%cdr (%caar p)))
(define-inline (%cdadr p) (%cdr (%cadr p)))
(define-inline (%cddar p) (%cdr (%cdar p)))
(define-inline (%cdddr p) (%cdr (%cddr p)))

(define-inline (%set-car-slot! p x) (%slot-set! p 0 x))
(define-inline (%set-cdr-slot! p x) (%slot-set! p 1 x))
(define-inline (%set-car-immediate! p x) (%slot-set-immediate! p 0 x))
(define-inline (%set-cdr-immediate! p x) (%slot-set-immediate! p 1 x))

(define-inline (%set-car! p x)
  (if (%immediate? x)
      (%set-car-immediate! p x)
      (%set-car-slot! p x) ) )

(define-inline (%set-cdr! p x)
  (if (%immediate? x)
      (%set-cdr-immediate! p x)
      (%set-cdr-slot! p x) ) )

(define-inline (%last-pair l0)
  (do ([l l0 (%cdr l)])
      [(%null? (%cdr l)) l]) )

(define-inline (%memq x l) (##core#inline "C_i_memq" x l))
(define-inline (%memv x l) (##core#inline "C_i_memv" x l))
(define-inline (%member x l) (##core#inline "C_i_member" x l))
(define-inline (%assq x l) (##core#inline "C_i_assq" x l))
(define-inline (%assv x l) (##core#inline "C_i_assv" x l))
(define-inline (%assoc x l) (##core#inline "C_i_assoc" x l))

;; Structure (wordblock)

(define-inline (%generic-structure? x)
  (and (%block? x) (%structure-type? x)) )

(define-inline (%structure-instance? x s) (##core#inline "C_i_structurep" x s))

(cond-expand
  [hygienic-macros
    (define-syntax %structure?
      (syntax-rules ()
        [(_ ?x)     (%generic-structure? ?x)]
        [(_ ?x ?t)  (%structure-instance? ?x ,?t)] ) ) ]
  [else
    (define-macro (%structure? ?x . ?t)
      (if (%null? ?t) `(%generic-structure? ,?x) `(%structure-type? ,?x ,(car ?t))) ) ] )

(define-inline %make-structure (##core#primitive "C_make_structure"))

(define-inline (%vector->structure! vec) (##core#inline "C_vector_to_structure" vec))

(define-inline (%structure-ref r i) (%slot-ref r i))

(define-inline (%structure-slot-set! r i x) (%slot-set! r i x))
(define-inline (%structure-immediate-set! r i x) (%slot-set-immediate! r i x))

(define-inline (%structure-length r) (%size r))

(define-inline (%structure-tag r) (%slot-ref r 0))

;; Port (wordblock)

; Port layout:
;
; 0:  FP (special)
; 1:  input/output (bool)
; 2:  class (vector of procedures)
; 3:  name (string)
; 4:  row (fixnum)
; 5:  col (fixnum)
; 6:  EOF (bool)
; 7:  type ('stream | 'custom | 'string | 'socket)
; 8:  closed (bool)
; 9:  data
; 10-15: reserved, port class specific

(define-inline (%port? x)
  (and (%block? x) (%port-type? x)) )

(define-inline (%port-mode p) (%slot-ref? x 1))

(define-inline (%input-port? x)
  (and (%port? x)
       (%port-mode x)) )

(define-inline (%output-port? x)
  (and (%port? x)
       (not (%port-mode x))) )

;; Closure (wordblock)

(define-inline (%closure? x)
  (and (%block? x) (%closure-type? x)) )

(define-inline (%procedure x) (%closure? x))

(define-inline (%closure-size x) (%size? x))

(define-inline (%vector->closure! v a)
  (##core#inline "C_vector_to_closure" v)
  (##core#inline "C_update_pointer" a v) )

;; Symbol (wordblock)

(define-inline (%symbol? x)
  (and (%block? x) (%symbol-type? x)) )

(define-inline %intern-symbol (##core#primitive "C_string_to_symbol"))
(define-inline (%interned-symbol? x) (##core#inline "C_lookup_symbol" x))

(define-inline (%string->symbol s) (%intern-symbol s)

;; Keyword (wordblock)

(define-inline (%keyword? x)
  (and (%symbol? x)
       (%eq? 0 (%byte-ref (%slot-ref x) 0)) ) )

;; Locative (wordblock)

(define-inline (%locative? x)
  (and (%block? x) (%locative-type? x)) )

;; Generic-pointer

(define-inline (%generic-pointer? x)
  (or (%pointer? x)
      (%locative? x) ) )

; generic-pointer, port, closure
(define-inline (%special-block? x)
  (and (%block? x) (%special? x)) )

(define-inline (%pointer? x)
  (and (%block? x) (##core#inline "C_anypointerp" x)) )

(define-inline (%pointer-like? x) (%special-block? x))

(define-inline (%generic-pointer-ref x) (%slot-ref x 0))
(define-inline (%generic-pointer-set! x y) (%slot-set! x 0 y))

(define-inline (%pointer->address ptr)
  ; Pack pointer address value into Chicken words; '4' is platform dependent!
  (##core#inline_allocate ("C_a_unsigned_int_to_num" 4) (%generic-pointer-ref x)) )

(define-inline (%null-pointer? p)
  (%eq? 0 (%pointer->address ptr)) )
  (and (%block? x) (%swig-pointer-type? x)) )

;; Simple-ointer (wordblock)

(define-inline (%simple-pointer? x)
  (and (%block? x) (%simple-pointer-type? x)) )

(define-inline %make-simple-pointer (##core#primitive "C_make_pointer"))

(define-inline (%address->pointer addr)
  (let ([ptr (%make-simple-pointer)])
    (##core#inline "C_update_pointer" addr ptr)
    ptr ) )

(define-inline (%block-pointer x)
  (let ([ptr (%make-simple-pointer)])
    (##core#inline "C_pointer_to_block" ptr x)
    ptr ) )

(define-inline (%null-pointer)
  (let ([ptr (%make-simple-pointer)])
    (##core#inline "C_update_pointer" 0 ptr)
    ptr ) )

;; Tagged-pointer (wordblock)

(define-inline (%tagged-pointer? x)
  (and (%block? x) (%tagged-pointer-type? x)) )

(define-inline %make-tagged-pointer (##core#primitive "C_make_tagged_pointer"))

;; Swig-pointer (wordblock)

(define-inline (%swig-pointer? x)
  (and (%block? x) (%swig-pointer-type? x)) )


;;; Values


;;; Operations

;; Random

(define-inline (%random-fixnum x) (##core#inline "C_random_fixnum" x))
