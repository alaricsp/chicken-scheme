;;;; module-tests.scm


(load "tests/test.scm")

(test-begin "modules")

(test-equal "internal/variable"
(module foo (abc def)
  (import scheme)
  (define (abc x)
    (display x)
    (newline)
    x)
  (define-syntax def
    (syntax-rules ()
      ((_ x) 
       (begin 
	 (display "(def) ")
	 (abc x)))))
  (abc 1))
1)

(test-error "external/unimported variable (fail)" (abc 2))
(test-error "external/unimported syntax (fail)" (def 3))

(import foo)
(test-equal "external/imported variable" (abc 4) 4)

(test-equal "external/imported syntax" (def 5) 5)

(test-end "modules")
