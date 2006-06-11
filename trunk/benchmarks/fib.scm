;;; fib.scm

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2))) ) )

(time (fib 30))
