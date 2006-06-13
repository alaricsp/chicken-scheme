(use syntax-case)

(define-syntax mu
  (syntax-rules ()
    ((mu argument ...) (lambda (f) (f argument ...)))))

(define m (mu 1 2 3))
(define v (lambda () (values 1 2 3)))
(define mm (lambda (x)
             (if (< x 0)
                 (mu 1 2 3)
                 (if (= x 0)
                     (mu 4 5 6)
                     (mu 7 8 9)))))
(define vv (lambda (x)
             (if (< x 0)
                 (values 1 2 3)
                 (if (= x 0)
                     (values 4 5 6)
                     (values 7 8 9)))))

(define-syntax dotimes
  (syntax-rules ()
    ((dotimes (i count) body0 body1 ...)
     (let ((%COUNT count))
       (do ((i 0 (+ i 1)))
           ((= i %COUNT))
         body0
         body1
         ...)))))

(define (test-m)
  (dotimes (i 10000000)
    (m (lambda (x y z)
         (+ i x y z)))))
(define (test-v)
  (dotimes (i 10000000)
    (receive (x y z) (v)
      (+ i x y z))))
(define (test-mm)
  (dotimes (i 10000000)
    ((mm i) (lambda (x y z)
              (+ i x y z)))))
(define (test-vv)
  (dotimes (i 10000000)
    (receive (x y z) (vv i)
      (+ i x y z))))


(time (test-m))
(time (test-v))
(time (test-mm))
(time (test-vv))
