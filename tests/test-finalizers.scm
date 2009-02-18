;;;; test-finalizers.scm

(define x (list 1 2 3))
(define y (list 4 5 6))
(define x-f #f)
(define y-f #f)

(begin
  (set-finalizer! 
   x
   (lambda (o)
     (format #t "Delete: ~A (y: ~a)~%" o y-f)
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

#|

This ought to work, see patches/finalizer.closures.diff for
a fix that unfortunately disables finalizers in the interpreter
(probably due to the different closure representation).

(assert (not y-f))
(set! x #f)
(gc #t)
(assert (not x-f))
(assert (not y-f))
(set! y #f)
(gc #t)
(assert y-f)
(assert x-f)
|#

(define foo-f #f)

(let ((foo (vector 1 2 3)))
  (set-finalizer! foo (lambda _ (set! foo-f #t)))
  #t)

(gc #t)
(assert foo-f)
