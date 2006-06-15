;;; FIBC -- FIB using first-class continuations, written by Kent Dybvig

;;; fib with peano arithmetic (using numbers) with call/cc

(define (add1 x) (+ x 1))
(define (sub1 x) (- x 1))

(define (addc x y k)
  (if (zero? y)
    (k x)
    (addc (add1 x) (sub1 y) k)))

(define (fibc x c)
  (if (zero? x)
    (c 0)
    (if (zero? (sub1 x))
      (c 1)
      (addc (call-with-current-continuation (lambda (c) (fibc (sub1 x) c)))
            (call-with-current-continuation (lambda (c) (fibc (sub1 (sub1 x)) c)))
            c))))

(let ((x (time (fibc 30 (lambda (n) n)))))
  (if (not (equal? x 832040))
      (error "wrong result" x) ) )
