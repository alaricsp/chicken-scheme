;;;; takl.scm

 
(define (listn n)
  (if (= 0 n)
      '()
      (cons n (listn (- n 1)))) )
 
(define 18l (listn 18))
(define 12l (listn 12))
(define  6l (listn 6))
 
(define (mas x y z)
  (if (not (shorterp y x))
      z
      (mas (mas (cdr x)
		y z)
	   (mas (cdr y)
		z x)
	   (mas (cdr z)
		x y))))
 
(define (shorterp x y)
  (and (pair? y)
       (or (null? x)
	   (shorterp (cdr x)
		     (cdr y)))) )
 
(time (do ((i 10 (- i 1))) ((zero? i)) (mas 18l 12l 6l)))

