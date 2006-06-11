;;; div-rec.scm


(define (create-n n)
  (do ((n n (- n 1))
       (a '() (cons '() a)))
      ((= n 0) a)))
 
(define *ll* (create-n 200))
 
(define (recursive-div2 l)
  (cond ((null? l) '())
        (else (cons (car l) (recursive-div2 (cddr l))))))
 
(define (test l)
  (do ((i 3000 (- i 1)))
      ((= i 0))
    (recursive-div2 l)
    (recursive-div2 l)
    (recursive-div2 l)
    (recursive-div2 l)))

(time (test *ll*))
