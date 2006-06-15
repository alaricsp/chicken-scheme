;;;; tak.scm


(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
	   (tak (- y 1) z x)
	   (tak (- z 1) x y) ) ) )

(time (do ((i 100 (- i 1))) ((zero? i)) (tak 18 12 6)))
