(define (foo) 1)

(assert (= 1 (foo)))

(define-compiler-syntax foo
  (syntax-rules ()
    ((_ x) 2) ) )

(assert (= 2 (foo 42)))
(assert (= 1 (foo)))

(let-compiler-syntax ((foo (syntax-rules () ((_ x) 3))))
  (assert (= 3 (foo 42))))

(assert (= 2 (foo 42)))

(module m1 (bar)
  (import (prefix scheme s:) chicken)
  (define-compiler-syntax s:+
    (syntax-rules ()
      ((_ x y) (s:- x y))))
  (define-compiler-syntax bar
    (syntax-rules ()
      ((_ x y) "oink!")))
  (s:define (bar x) (s:+ x 1)) )

(module m2 ()
  (import scheme chicken (prefix m1 m-))
  (print (m-bar 10))
  (print (m-bar 10 23))
  (print (+ 4 3)))
