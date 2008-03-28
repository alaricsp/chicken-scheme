(define (fac n)
  (let-syntax ((m1 (lambda (n r c) 
		     (pp `(M1: ,n))
		     (list (r 'sub1) (cadr n)))))
    (define (sub1 . _)
      (error "argh.") )
    (print "fac: " n)
    (if (zero? n)
	1
	(* n (fac (m1 n))))))

(print "run...")

(pp (fac 10))
