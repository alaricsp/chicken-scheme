(define n 0)

(define (foo k) 
  (set! n (+ n 1))
  (k 123))

(let ((count (string->number (:optional (command-line-arguments) "10000"))))
  (do ((i count (- i 1)))
      ((zero? i) (print n))
    (call/cc (lambda (k) (foo k))) ) )
