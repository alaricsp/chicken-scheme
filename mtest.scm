(print "foo")

(##core#environment 
 foo
 ()
 (a b c)
 values
 (define (a x) x)
 (define-syntax c
   (syntax-rules ()
     ((_ x) (a x))))
 (define b (c 3)))

(print "bar")

(##core#environment
 bar
 ((foo values))
 ()
 values
 (define xxx (a 99))
 (##sys#print xxx #t ##sys#standard-output)
 (##sys#print (c xxx) #t ##sys#standard-output))
