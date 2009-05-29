;;;; scrutiny-tests.scm


(pp (current-environment))

(define (a)
  (define (b)
    (define (c)
      (let ((x (+ 3 4)))
	(if x 1 2)))))

(define (foo x)
  (if x 
      (values 1 2)
      (values 1 2 (+ (+ (+ (+  3)))))))

(let ((bar +))
  (bar 3 'a))

(pp)

(print (cpu-time))
(print (values))

(let ((x 100))
  (x))

(print (+ 'a 'b))

(set! car 33)

((values 1 2))
