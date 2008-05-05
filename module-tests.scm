;;;; module-tests.scm


(include "tests/test.scm")

(test-begin "modules")

(test-equal "internal/variable"
(module foo (abc def)
  (import scheme)
  (define (abc x) (+ x 33))
  (define-syntax def
    (syntax-rules ()
      ((_ x) (+ 99 (abc x)))))
  (abc 1))
34)

(test-error "external/unimported variable (fail)" (abc 2))
(test-error "external/unimported syntax (fail)" (def 3))

(import foo)

(test-equal "external/imported variable" (abc 4) 37)
(test-equal "external/imported syntax" (def 5) 137)

(test-end "modules")
