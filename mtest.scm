;;;; mtest.scm - various macro tests


;; test syntax-rules

(define-syntax test
  (syntax-rules ()
    ((_ x form)
     (let ((tmp x))
       (if (number? tmp)
	   form
	   (error "not a number" tmp))))))

(test 2 (print "ok."))

;; some basic contrived testing

(define (fac n)
  (let-syntax ((m1 (lambda (n r c) 
		     (pp `(M1: ,n))
		     (list (r 'sub1) (cadr n)))))
    (define (sub1 . _)			; ref. transp.? (should not be used here)
      (error "argh.") )
    #;(print "fac: " n)		  
    (if (test n (zero? n))
	1
	(* n (fac (m1 n))))))

(pp (fac 10))
