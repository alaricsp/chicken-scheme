;;;; test-finalizers.scm

(define x (list 1 2 3))
(define y (list 4 5 6))
(define x-f #f)
(define y-f #f)

(begin
  (set-finalizer! 
   x
   (lambda (o)
     (format #t "Delete: ~A~%" o)
     (set! x-f #t)))
  #t) 
(begin 
  (set-finalizer! 
   y 
   (let ((p x))
     (lambda (o)
       (format #t "Delete: ~A: ~A~%" o p)
       (set! y-f #t))))
  #t)
(gc #t)
(assert (not x-f))
(assert (not y-f))
(set! x #f)
(gc #t)
(assert x-f)
(assert (not y-f))
(set! y #f)
(gc #t)
(assert x-f)
