(require-library import-library-test1)

(module bar ()
  (import scheme chicken extras foo)
  (assert (equal? '(123) (foo))))
    