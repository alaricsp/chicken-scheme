(print "foo")

(##core#environment 
 foo
 ()
 (a b c)
 values
 (define (a x y) (##sys#cons x y))
 (define d 100)
 (define-syntax c
   (syntax-rules ()
     ((_ x) (a d x))))
 (define b (c 3)))

(print "bar")

(##core#environment
 bar
 ((foo values))
 ()
 values
 (define xxx (a b 99))
 (##sys#print xxx #t ##sys#standard-output)
 (##sys#print (c xxx) #t ##sys#standard-output))
