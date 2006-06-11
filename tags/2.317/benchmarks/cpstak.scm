;;; cpstak.scm 


(define (cpstak x y z)
  (define (tak x y z k)
    (if (not (< y x))
        (k z)
        (tak (- x 1)
             y
             z
             (lambda (v1)
               (tak (- y 1)
                    z
                    x
                    (lambda (v2)
                      (tak (- z 1)
                           x
                           y
                           (lambda (v3)
                             (tak v1 v2 v3 k)))))))))
  (tak x y z (lambda (a) a)))
 
(time (do ((i 100 (- i 1))) ((zero? i)) (cpstak 18 12 6)))

