;;;; compiler-tests.scm


(module foo (bar)
  (import scheme chicken)
  (declare (hide bar))
  (define (bar x) (+ x 1)))

(assert (not (##sys#symbol-has-toplevel-binding? 'foo#bar)))
