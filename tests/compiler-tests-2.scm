;;; compiler-tests-2.scm - tests for compiler with -lambda-lift


;;; rev. 12113 - lambda-lifting breakage, because lambda-bound variables
;                were incorrectly marked as global (analysis didn't walk
;                "lambda" nodes correctly, due t incorrect assumption 
;                that "lambda" doesn't occur. Major stupidity.
;                Test case by Joerg Wittenberger

(define (plus1 a)
 (define (plus b)
   (+ a b))
 (plus 1))

(print (plus1 1))

(define (len lst)
 (define (len n)
   (if (pair? lst)
       (begin
         (set! lst (cdr lst))
         (len (+ n 1)))
       n))
 (len 0))

(print (len '(1 2 3)))
