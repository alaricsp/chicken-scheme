(define n 0)

(define (foo k) 
  (set! n (+ n 1))
  (##sys#direct-return k 123))

(let ((count (string->number (:optional (command-line-arguments) "10000"))))
  (do ((i count (- i 1)))
      ((zero? i) (print n))
    (##sys#call-with-direct-continuation (lambda (k) (foo k))) ) )
