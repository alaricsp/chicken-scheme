;;;; chicken-primitive-object-nlines.scm
;;;; Kon Lovett, Jan '09
;;;; (Was chicken-sys-macros.scm)

; Usage
;
; (include "chicken-primitive-object-inlines")

;; Notes
;
; Provides inlines & macros for primitive procedures. Use of these procedures
; by non-core & non-core-extensions is highly suspect. Many of these routines
; are unsafe.
;
; In fact, any use is suspect ;-)
;
; A ##core#Inline is just what it says - literal inclusion in the compiled C
; code of the C macro/function and the arguments taken literally, i.e. as the
; C_word value.
;
; These are much faster than a lambda, but very dangerous since the arguments and
; the return value are not converted. The C code must perform any such conversions.
;
; ##core#inline cannot be used with a runtime C function which is coded in the
;CPS style.
;
; A ##core#primitive creates a lambda for a C function which is coded in the
; CPS style.
;
; These have a stereotypical argument list which begins the 3 arguments C_word
; c, C_word closure, and C_word k. Any actual arguments follow.
;
; c - number of arguments, not including 'c', but including 'closure' & 'k'
; closure - caller
; k - continuation


;;; Type Predicates

;; Argument is a 'C_word'


;; Immediate

(define-inline (%immediate? ?x) (##core#inline "C_immp" x))


;; Fixnum

(define-inline (%fixnum? x) (##core#inline "C_fixnump" x))


;; Character

(define-inline (%char? x) (##core#inline "C_charp" x))


;; Boolean

(define-inline (%boolean? x) (##core#inline "C_booleanp" x))


;; EOF

(define-inline (%eof-object? x) (##core#inline "C_eofp" x))


;; Null (the end-of-list value)

(define-inline (%null? x) (##core#inline "C_i_nullp" x))


;; Undefined (void)

(define-inline (%undefined? x) (##core#inline "C_undefinedp" x))


;; Unbound (the unbound value, not 'is a symbol unbound')

(define-inline (%unbound? x) (##core#inline "C_unboundvaluep" x))


;; Block (anything not immediate)

(define-inline (%block? x) (##core#inline "C_blockp" x))


;; Vector

(define-inline (%vector-type? x) (##core#inline "C_vectorp" x))


;; Bytevector (isa vector so be careful; refers to how allocated, not what stored)

(define-inline (%bytevector-type? x) (##core#inline "C_bytevectorp" x))


;; Pair

(define-inline (%pair-type? x) (##core#inline "C_pairp" x))


;; Bucket

; A bucket is used by the runtime for the symbol-table. The bucket type is not
; "seen" by Scheme code.


;; Structure

(define-inline (%structure-type? x) (##core#inline "C_structurep" x))


;; Symbol

(define-inline (%symbol-type? x) (##core#inline "C_symbolp" x))


;; Byteblock

(define-inline (%byteblock? x) (##core#inline "C_byteblockp" x))


;; String

(define-inline (%string-type? x) (##core#inline "C_stringp" x))


;; Flonum

(define-inline (%flonum-type? x) (##core#inline "C_flonump" x))


;; Lambda-info

(define-inline (%lambda-info-type? x) (##core#inline "C_lambdainfop" x))


;; Special

(define-inline (%special? x) (##core#inline "C_specialp" x))


;; Closure

(define-inline (%closure-type? x) (##core#inline "C_closurep" x))


;; Port

(define-inline (%port-type? x) (##core#inline "C_portp" x))


;; Simple-pointer

(define-inline (%simple-pointer-type? x) (##core#inline "C_pointerp" x))


;; Tagged-Pointer

(define-inline (%tagged-pointer-type? x) (##core#inline "C_taggedpointerp" x))


;; Swig-Pointer

(define-inline (%swig-pointer-type? x) (##core#inline "C_swigpointerp" x))


;; Any-pointer

(define-inline (%any-pointer-type? x) (##core#inline "C_anypointerp" x))


;; Locative

(define-inline (%locative-type? x) (##core#inline "C_locativep" x))


;; Forwarded (block object moved to new address, forwarding pointer)

(define-inline (%forwarded? x) (##core#inline "C_forwardedp" x))



;;; Values



;;; Operations

(define-inline (%eq? x y) (##core#inline "C_eqp" x y))

(define-inline (%peek-signed-integer b i) ((##core#primitive "C_peek_signed_integer") b i))

(define-inline (%peek-unsigned-integer b i) ((##core#primitive "C_peek_unsigned_integer") b i))

(define-inline (%poke-integer b i n) (##core#inline "C_poke_integer" b i n))


;; Fixnum

(define-inline (%fx+ x y) (##core#inline "C_fixnum_plus" x y))
(define-inline (%fx- x y) (##core#inline "C_fixnum_difference" x y))
(define-inline (%fx* x y) (##core#inline "C_fixnum_times" x y))
(define-inline (%fx= x y) (%eq? x y))
(define-inline (%fx> x y) (##core#inline "C_fixnum_greaterp" x y))
(define-inline (%fx< x y) (##core#inline "C_fixnum_lessp" x y))
(define-inline (%fx>= x y) (##core#inline "C_fixnum_greater_or_equal_p" x y))
(define-inline (%fx<= x y) (##core#inline "C_fixnum_less_or_equal_p" x y))
(define-inline (%fxmin x y) (##core#inline "C_i_fixnum_min" x y))
(define-inline (%fxmax x y) (##core#inline "C_i_fixnum_max" x y))
(define-inline (%fxneg x) (##core#inline "C_fixnum_negate" x))
(define-inline (%fxand x y) (##core#inline "C_fixnum_and" x y))
(define-inline (%fxior x y) (##core#inline "C_fixnum_or" x y))
(define-inline (%fxxor x y) (##core#inline "C_fixnum_xor" x y))
(define-inline (%fxnot x) (##core#inline "C_fixnum_not" x))
(define-inline (%fxshl x y) (##core#inline "C_fixnum_shift_left" x y))
(define-inline (%fxshr x y) (##core#inline "C_fixnum_shift_right" x y))

; These are very unsafe, no check for division-by-zero
(define-inline (%fx/ x y) (##core#inline "C_fixnum_divide" x y))
(define-inline (%fxmod x y) (##core#inline "C_fixnum_modulo" x y))


;;; Block


;; Size of object in units of sub-object.

; byteblock is # of bytes, otherwise # of words.
;
(define-inline (%block-size x) (##core#inline "C_block_size" x))


;; (%block-allocate size byteblock? fill aligned-8-byte-boundry?)
;
; Creates & returns a string when 'byteblock?', otherwise a vector.
;
; Size is # of bytes when 'byteblock?', otherwise # of words.
; Fill is a character when 'byteblock?', otherwise any.
;
(define-inline (%block-allocate n bb f a) ((##core#primitive "C_allocate_vector") n bb f a))

(define-inline (%block-address x) (##core#inline_allocate ("C_block_address" 4) x))


;; Byte access

(define-inline (%make-block-byte n f a?) (%block-allocate n #t f a?))

(define-inline (%block-byte-ref x i) (##core#inline "C_subbyte" x i))
(define-inline (%block-byte-set! x i n) (##core#inline "C_setsubbyte" x i n))


;; Word access

(define-inline (%make-block-word n f a?) (%block-allocate n #f f a?))

(define-inline (%block-word-ref x i) (##core#inline "C_slot" x i))

(define-inline (%block-word-set! x i y) (##core#inline "C_i_setslot" x i y))
(define-inline (%block-word-set!/immediate x i y) (##core#inline "C_i_set_i_slot" x i y))



;;;


;; Generic-byteblock

; generic-byteblock isa string, flonum, or lambda-info
;
(define-inline (%generic-byteblock? x) (and (%block? x) (%byteblock? x)))


;; String (byteblock)

(define-inline (%make-string size fill) (%make-block-byte size fill #f))

(define-inline (%string? x) (and (%block? x) (%string-type? x)))

(define-inline (%string-ref s i) (##core#inline "C_subchar" s i))

(define-inline (%string-set! s i c) (##core#inline "C_setsubchar" s i c))

(define-inline (%string-length s) (%block-size s))

;%bytevector->string - see Bytevector


;; Flonum (byteblock)

(define-inline (%flonum? x) (and (%block? x) (%flonum-type? x)))


;; Lambda-info (byteblock)

(define-inline (%lambda-info? x) (and (%block? x) (%lambda-info-type? x)))


;; Generic-vector

; generic-vector isa vector, pair, structure, symbol, or keyword
;
(define-inline (%generic-vector? x)
  (and (%block? x)
       (not (or (%special? x) (%byteblock? x)))) )


;; Vector (wordblock)

(define-inline (%make-vector size fill) (%make-word-block size fill #f))

(define-inline (%vector? x) (and (%block? x) (%vector-type? x)))

(define-inline (%vector-ref v i) (%block-word-ref v i))

(define-inline (%vector-set! v i x) (%block-word-set! v i x))
(define-inline (%vector-set!/immediate v i x) (%block-word-set!/immediate v i x))

(define-inline (%vector-length v) (%block-size v))


;; Bytevector (wordblock, but byte referenced)

(define-inline (%make-bytevector sz)
  (let ([bv (%make-string sz #f #t)])
    (##core#inline "C_string_to_bytevector" bv)
    bv ) )

(define-inline (%bytevector? x) (and (%block? x) (%bytevector-type? x)))

(define-inline (%bytevector-ref bv i) (%block-byte-ref bv i))

(define-inline (%bytevector-set! bv i x) (%block-byte-set! bv i x))

(define-inline (%bytevector-length bv) (%block-size bv))

(define-inline (%bytevector=? v1 v2)
  (let ([ln (%bytevector-length v1)])
    (and (%eq? n %bytevector-length v2))
         (fx=? 0 (##core#inline "C_string_compare" v1 v2 n)) ) )

(define-inline (%string->bytevector s)
  (let* ([n (%string-length s)]
	       [bv (%make-bytevector sz)] )
    (##core#inline "C_copy_memory" bv s n) 
    bv ) )

(define-inline (%bytevector->string bv)
  (let* ([n (%bytevector-length bv)]
	       [s (%make-string n #\space)] )
    (##core#inline "C_copy_memory" s bv n) 
    s ) )


;; Blob (isa bytevector w/o accessors)

(define-inline (%make-blob sz) (%make-bytevector sz))

(define-inline (%blob? x) (%bytevector? x))

(define-inline (%blob-size b) (%bytevector-length b))

(define-inline (%blob=? b1 b2) (%bytevector=? b1 b2))

(define-inline (%string->blob s) (%string->bytevector s))

(define-inline (%blob->string bv) (%bytevector->string bv))


;; Pair (wordblock)

(define-inline (%pair? x) (and (%block? x) (%pair-type? x)))

(define-inline (%list? x) (or (%null? x) (%pair? x)))

(define-inline (%cons x y) (##core#inline_allocate ("C_a_i_cons" 3) x y) )

(define-inline (%length l) (##core#inline "C_i_length" l))

(define-inline (%car p) (%block-word-ref p 0))
(define-inline (%cdr p) (%block-word-ref p 1))

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

(define-inline (%set-car! p x) (%block-word-set! p 0 x))
(define-inline (%set-cdr! p x) (%block-word-set! p 1 x))
(define-inline (%set-car/immediate! p x) (%block-word-set!/immediate p 0 x))
(define-inline (%set-cdr/immediate! p x) (%block-word-set!/immediate p 1 x))

;; These are safe

(define-inline (%memq x l) (##core#inline "C_i_memq" x l))
(define-inline (%memv x l) (##core#inline "C_i_memv" x l))
(define-inline (%member x l) (##core#inline "C_i_member" x l))

(define-inline (%assq x l) (##core#inline "C_i_assq" x l))
(define-inline (%assv x l) (##core#inline "C_i_assv" x l))
(define-inline (%assoc x l) (##core#inline "C_i_assoc" x l))

; l0 must be a proper-list
(define-inline (%list-ref l0 i0)
  (let loop ([l l0] [i i0])
    (cond [(null? l)  '() ]
	        [(%fx= 0 i) (%car l) ]
	        [else       (loop (%cdr l) (%fx- i 1)) ] ) ) )

; l0 cannot be null
(define-inline (%last-pair l0)
  (do ([l l0 (%cdr l)])
      [(%null? (%cdr l)) l]) )

; each elm of ls must be a proper-list
(define-inline (%append! . ls)
  (let ([ls (let loop ([ls ls])
              (cond [(%null? ls)        '() ]
                    [(%null? (%car ls)) (loop (%cdr ls)) ]
                    [else               ls ] ) ) ] )
    (if (%null? ls)
        '()
        (let ([l0 (%car ls)])
          ;(assert (not (null? l0)))
          (let loop ([ls (%cdr ls)] [pl l0])
            (if (%null? ls)
                l0
                (let ([l1 (%car ls)])
                  (if (%null? l1)
                      (loop (%cdr ls) pl)
                      (begin
                        (%set-cdr! (%last-pair pl) l1)
                        (loop (%cdr ls) l1) ) ) ) ) ) ) ) ) )
    
; l0 must be a proper-list
(define-inline (%delq! x l0)
  (let loop ([l l0] [pp #f])
    (cond [(null? l)
           l0 ]
	        [(%eq? x (%car l))
	         (cond [pp
	                (%set-cdr! pp (%cdr l))
	                l0 ]
	               [else
	                (%cdr l) ] ) ]
	        [else
	         (loop (%cdr l) l) ] ) ) )


;; Structure (wordblock)

(define-inline (%make-structure t . s) (apply (##core#primitive "C_make_structure") t s))

(define-inline (%generic-structure? x) (and (%block? x) (%structure-type? x)))

(define-inline (%structure-instance? x s) (##core#inline "C_i_structurep" x s))

(define-inline (%structure-ref r i) (%block-word-ref r i))

(define-inline (%structure-set! r i x) (%block-word-set! r i x))
(define-inline (%structure-set!/immediate r i x) (%block-word-set!/immediate r i x))

(define-inline (%structure-length r) (%block-size r))

(define-inline (%structure-tag r) (%block-word-ref r 0))


;; Special-block (wordblock)

(define-inline (%special-block? x) (and (%block? x) (%special? x)))


;; Port (wordblock)

; Port layout:
;
; 0	  FP (special - C FILE *)
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

; port is 16 slots + a block-header word
;
;(define-inline (%make-port n) (##core#inline_allocate ("C_a_i_port" 17)))

(define-inline (%port? x) (and (%block? x) (%port-type? x)))

(define-inline (%port-filep port) (%peek-unsigned-integer port 0))
(define-inline (%port-input-mode? port) (%block-word-ref? port 1))
(define-inline (%port-class port) (%block-word-ref? port 2))
(define-inline (%port-name port) (%block-word-ref? port 3))
(define-inline (%port-row port) (%block-word-ref? port 4))
(define-inline (%port-column port) (%block-word-ref? port 5))
(define-inline (%port-eof? port) (%block-word-ref? port 6))
(define-inline (%port-type port) (%block-word-ref? port 7))
(define-inline (%port-closed? port) (%block-word-ref? port 8))
(define-inline (%port-data port) (%block-word-ref? port 9))

(define-inline (%port-filep-set! port fp) (%poke-integer port 0 fp))
(define-inline (%port-input-mode-set! port f) (%block-word-set!/immediate port 1 f))
(define-inline (%port-class port v) (%block-word-set! port 2 v))
(define-inline (%port-name-set! port s) (%block-word-set! port 3 s))
(define-inline (%port-row-set! port n) (%block-word-set!/immediate port 4 n))
(define-inline (%port-column-set! port n) (%block-word-set!/immediate port 5 n))
(define-inline (%port-eof-set! port f) (%block-word-set!/immediate port 6 f))
(define-inline (%port-type-set! port s) (%block-word-set! port 7 s))
(define-inline (%port-closed-set! port f) (%block-word-set!/immediate port 8 f))
(define-inline (%port-data-set! port port) (%block-word-set! port 9 x))

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


;; Closure (wordblock)

(define-inline (%closure? x) (and (%block? x) (%closure-type? x)))

(define-inline (%closure-size c) (%block-size? c))

(define-inline (%vector->closure! v a)
  (##core#inline "C_vector_to_closure" v)
  (##core#inline "C_update_pointer" a v) )


;; Symbol (wordblock)

(define-inline (%symbol? x) (and (%block? x) (%symbol-type? x)))

(define-inline (%symbol-binding s) (%block-word-ref s 0))
(define-inline (%symbol-string s) (%block-word-ref s 1))
(define-inline (%symbol-bucket s) (%block-word-ref s 2))

(define-inline (%string->symbol-interned s) ((##core#primitive "C_string_to_symbol") s))

;(define-inline (%symbol-intern! s) (%string->symbol (%symbol-string s)))

(define-inline (%symbol-interned? x) (##core#inline "C_lookup_symbol" x))

(define-inline (%symbol-bound? s) (##core#inline "C_boundp" s))


;; Keyword (wordblock)

(define-inline (%keyword? x)
  (and (%symbol? x)
       (%eq? 0 (%block-byte-ref (%symbol-string x) 0)) ) )


;; Locative (wordblock)

(define-inline (%make-locative typ obj idx weak?)
  (##core#inline_allocate ("C_a_i_make_locative" 5) typ obj idx weak?))

(define-inline (%locative? x) (and (%block? x) (%locative-type? x)))

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

;%locative-address - see Pointer
(define-inline (%locative-offset lv) (%block-word-ref lv 1))
(define-inline (%locative-type lv) (%block-word-ref lv 2))
(define-inline (%locative-weak? lv) (not (%block-word-ref lv 3)))
(define-inline (%locative-object lv) (%block-word-ref lv 3))


;; Pointer (wordblock)

(define-inline (%pointer? x) (and (%block? x) (%any-pointer-type? x)))

; simple-pointer, tagged-pointer, swig-pointer, locative
(define-inline (%generic-pointer? x) (or (%pointer? x) (%locative? x)))

; simple-pointer, tagged-pointer, swig-pointer, locative, closure, port, symbol, keyword
(define-inline (%pointer-like? x) (%special-block? x))

; These operate on pointer-like objects

(define-inline (%pointer-ref ptr) (%block-word-ref ptr 0))
(define-inline (%pointer-set! ptr y) (%block-word-set! ptr 0 y))

(define-inline (%peek-byte ptr i) (##core#inline "C_peek_byte" ptr i))

(define-inline (%pointer-null? ptr) (##core#inline "C_null_pointerp" ptr))

(define-inline (%pointer->address ptr)
  ; Pack pointer address value into Chicken words; '4' is platform dependent!
  (##core#inline_allocate ("C_block_address" 4) (%generic-pointer-ref x)) )

(define-inline (%locative-address lv) (%pointer->address lv))


;; Simple-pointer (wordblock)

(define-inline (%make-simple-pointer) ((##core#primitive "C_make_pointer")))

(define-inline (%simple-pointer? x) (and (%block? x) (%simple-pointer-type? x)))

(define-inline (%make-pointer-null)
  (let ([ptr (%make-simple-pointer)])
    (##core#inline "C_update_pointer" 0 ptr)
    ptr ) )

(define-inline (%address->pointer a)
  (let ([ptr (%make-simple-pointer)])
    (##core#inline "C_update_pointer" a ptr)
    ptr ) )

(define-inline (%make-pointer-block b)
  (let ([ptr (%make-simple-pointer)])
    (##core#inline "C_pointer_to_block" ptr b)
    ptr ) )


;; Tagged-pointer (wordblock)

(define-inline (%make-tagged-pointer t) ((##core#primitive "C_make_tagged_pointer") t))

(define-inline (%tagged-pointer? x) (and (%block? x) (%tagged-pointer-type? x)))


;; Swig-pointer (wordblock)

(define-inline (%swig-pointer? x) (and (%block? x) (%swig-pointer-type? x)))



;;; Values



;;; Operations


;; Random

(define-inline (%random-fixnum x) (##core#inline "C_random_fixnum" x))
