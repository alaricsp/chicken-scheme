;;;; module-tests-compiled.scm


(include "test.scm")

(test-begin "modules/compiled")


;; inlines where walked twice (once for extracting mutable constants)
;; and then when expanded, this caused inline function names to be
;; aliased/renamed twice - also, aliasing in syntax-defs could make
;; inline func unrecognizable for canonicalizer.

(module m1 (f1)
  (import scheme chicken)
  (define-inline (bar x) (cons x '(foo)))
  (define-syntax s1
    (syntax-rules ()
      ((_ x) (list (bar x)))))
  (define (f1 x) (s1 x)))

(import m1)
(test-equal "inline in syntax" (f1 'ok) '((ok foo)))


;; here, the identical names of alias/real id pairs in primitive
;; modules with prefix applied would cause the second to be marked
;; ##core#aliase'd. That would avoid renaming of the newly defined
;; vector-fill!.

(module m2 (vector-fill!)
  (import (except scheme vector-fill!)
          (prefix (only scheme vector-fill!) %))
  (define (vector-fill! x v)
    (%vector-fill! v x)
    v))

(import m2)
(define v (vector 1 2 3))
(test-equal "unmarked primitive exports" (vector-fill! 99 v) '#(99 99 99))


(test-end "modules")
