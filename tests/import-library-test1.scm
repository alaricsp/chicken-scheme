(module foo (foo)
  (import scheme)
  (define-syntax foo 
    (syntax-rules ()
      ((_) (bar))))
  (define-syntax bar
    (syntax-rules ()
      ((_) (list 123)))))

