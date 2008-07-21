(module m1 ((s1 f1))
  (import scheme chicken)
  (define (f1) (print "f1") 'f1)
  (define-syntax s1
    (syntax-rules ()
      ((_) (f1)))))

(module m2 (s2)
  (import scheme (rename m1 (s1 s1:s1)))
  (define-syntax s2
    (syntax-rules ()
      ((_) (s1:s1)))))

(module m3 (s3)
  (import scheme m2)
  (define-syntax s3
    (syntax-rules ()
      ((_) (s2)))))

(import m3)
(s3)

