;;;; sys-macros.scm
;;;; Kon Lovett, Oct '07

; Provides '%' forms for common '##sys#' forms
; MUST be included

;;; Immediate

(define-macro (%immediate-value? ?x) (##sys#immediate? ,?x))

;;; Undefined

(define-inline (%undefined? x) (##core#inline "C_undefinedp" x))

;;; Unbound

(define-inline (%unbound? x) (eq? x (##sys#slot '##sys#arbitrary-unbound-symbol 0)))

;;; List

(define-inline (%null? l)
  ((##core#inline " C_i_nullp") l) )

(define-inline (%car p) (##sys#slot p 0))

(define-inline (%cdr p) (##sys#slot p 1))

(define-inline (%cadr p) (%car (%cdr p)))

(define-inline (%cddr p) (%cdr (%cdr p)))

(define-inline (%caddr p) (%car (%cdr (%cdr p))))

(define-inline (%cdddr p) (%cdr (%cdr (%cdr p))))

(define-inline (%set-car! p y) (##sys#setslot p 0 y))

(define-inline (%set-cdr! p y) (##sys#setslot p 1 y))

(define-inline (%last-pair lst0)
  (do ([lst lst0 (%cdr lst)])
       ([%null? (%cdr lst)] lst)))

;;; String

(define-inline (%string-length s) (##sys#size s))

;;; Vector


(define-inline (%vector-ref v i) (##sys#slot v i))

(define-inline (%vector-set! v i x) (##sys#setslot v i x))

(define-inline (%vector-length v) (##sys#size v))

;;; Blob

(define-inline (%blob? x) (##sys#bytevector? x))

;; Structure

(define-macro (%structure? ?x . ?t)
  (if (%null? ?t) `(##sys#generic-structure? ,?x) `(##sys#structure? ,?x ,(car ?t))))

(define-inline (%structure-ref r i) (##sys#slot r i))

(define-inline (%structure-set! r i x) (##sys#setslot r i x))

(define-inline (%structure-length r) (##sys#size r))

(define-inline (%structure-tag r) (##sys#slot r 0))

;;; Closure

(define-inline (%closure-ref c i) (##sys#slot c i))

(define-inline (%closure-length c) (##sys#size c))

;;; Pointer

(define-inline (%pointer? x) (##core#inline "C_pointerp" x))

(define-inline (%null-pointer? p) (##sys#null-pointer? p))

;;; Tagged-Pointer

(define-inline (%tagged-pointer-data p) (##sys#slot p 1))

(define-inline (%tagged-pointer? x) (##core#inline "C_taggedpointerp" x))


;;; Swig-Pointer

(define-inline (%swig-pointer? x) (##core#inline "C_swigpointerp" x))

;;; Locative

(define-inline (%locative? x) (##core#inline "C_locativep" x))

;;; Random

(define-inline (%random-fixnum x) (##core#inline "C_random_fixnum" x))
