;;; ctak.scm

(define (ctak x y z)
  (call-with-current-continuation
   (lambda (k)
     (ctak-aux k x y z))))

(define (ctak-aux k x y z)
  (cond ((not (< y x))			;xy
         (k z))
        (else (call-with-current-continuation
	       (lambda (k)		; (was missing)
		 (ctak-aux
		  k
		  (call-with-current-continuation
		   (lambda (k)
		     (ctak-aux k
			       (- x 1)
			       y
			       z)))
		  (call-with-current-continuation
		   (lambda (k)
		     (ctak-aux k
			       (- y 1)
			       z
			       x)))
		  (call-with-current-continuation
		   (lambda (k)
		     (ctak-aux k
			       (- z 1)
			       x
			       y)))))))) )


(time (do ((i 10 (- i 1))) ((zero? i)) (ctak 18 12 6)))
