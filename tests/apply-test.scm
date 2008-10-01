(require-extension srfi-1)

(define manyargs (feature? 'manyargs))

(when manyargs (print "many arguments supported."))

(define (foo . args)
  (when (pair? args)
    (assert (= (length args) (last args)))))

(let ((max (if manyargs 500 100)))
  (do ((i 0 (add1 i)))
      ((>= i max))
    (apply foo (iota i 1))))
