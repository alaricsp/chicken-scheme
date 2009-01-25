;;; currently disabled, need to compile matchable properly

(module foo (bar)
  (import scheme chicken)
  (import-for-syntax matchable)
  (begin-for-syntax
   (define (baz x) 
     (match x
       ((_ y) (list y)))))
  (define-syntax (bar x r c)
    `(,(r 'print) (,(r 'list) (baz (list 1 ,(cadr x)))))))
