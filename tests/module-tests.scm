;;;; module-tests.scm


(cond-expand
 (compiling
  (include "test.scm") )
 (else
  (load-relative "test.scm")))

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

(module bar (x y)
  (import (prefix scheme s:))
  (s:define (x y) (s:* y 2))
  (s:define y 1))

(import (prefix (only (except (rename bar (x z)) y) z) "bar-"))
(test-equal "modified import" (bar-z 10) 20)
(test-error "hidden import" y)

(module baz ((x s:list))
  (import (prefix scheme s:))
  (define-syntax x
    (syntax-rules ()
      ((_ x) (s:list x)))))

(import baz)
(test-equal "prefixed import and reexport" (x 1) '(1))

(test-end "modules")
