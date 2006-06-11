;;;; hanoi.scm

(define hanoi 
  (lambda (n)
    (letrec ((move-them 
              (lambda (n from to helper)
                        (if (> n 1)
                            (begin
                              (move-them (- n 1) from helper to)
                              (move-them (- n 1) helper to from))))))
      (move-them n 0 1 2))))

(time (do ((i 10 (- i 1))) ((zero? i)) (hanoi 20)))
