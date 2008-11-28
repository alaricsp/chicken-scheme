;;;; syntax-tests-2.scm - tests using extended syntax at runtime

(require-library chicken-syntax)

(eval '(define-record-type x (make x) x? (x get-x)))
(assert (eq? 'yes (get-x (make 'yes))))
