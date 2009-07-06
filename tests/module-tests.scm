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

(module m1 ((bar gna))
  (import scheme)
  (define (gna x) (list 'gna x))
  (define-syntax bar
    (syntax-rules ()
      ((_ x) (baz x))))
  (define-syntax baz
    (syntax-rules ()
      ((_ x) (gna 'x)))))

(module m2 (run)
  (import scheme chicken m1)
  (define-syntax baz
    (syntax-rules ()
      ((_ x) (list 'goo 'x))))
  (define (gna x) (print "ok."))
  (define (run) (gna 9) (bar 99)))

(import (only m2 run))
(test-equal "indirect imports" (run) '(gna 99))

(module m1 ((s1 f1))
  (import scheme chicken)
  (define (f1) (print "f1") 'f1)
  (define-syntax s1
    (syntax-rules ()
      ((_) (f1)))))

(module m2 (s2)
  (import scheme m1)
  (define-syntax s2
    (syntax-rules ()
      ((_) (s1)))))

(module m3 (s3)
  (import scheme m2)
  (define-syntax s3
    (syntax-rules ()
      ((_) (s2)))))

(import m3)
(test-equal "chained indirect imports" (s3) 'f1)

(module literal-compare-test (s1)
  (import scheme)
  (define-syntax s1
    (syntax-rules (and)
      ((_ (and x)) (list x))))
)

(import literal-compare-test)
(test-equal "literal compare and export" (s1 (and 100)) '(100))

(module y (y1)
  (import scheme)
  (define y1 10))

(module x (magnitude)
  (import (except scheme magnitude) y)
  (define magnitude y1))

(test-equal "redefinition of indirect import" (procedure? magnitude) #t)

(import x)
(test-equal "redefinition of indirect import (II)" magnitude 10)

(module m10 (m10x m10y)
  (import scheme)
  (define m10x 99)
  (define-syntax m10y
    (syntax-rules ()
      ((_ x) (list 'x)))))

(module m11 (m10x m10y)
  (import m10))

(import m11)
(test-equal "value reexport" m10x 99)
(test-equal "syntax reexport" (m10y 3) '(3))

;; found by Jim Ursetto;

(module m12 (begin0)
  (import scheme)
  (define-syntax begin0
    (syntax-rules ()
      ((_ e0 e1 ...)
       (##sys#call-with-values
	(lambda () e0)
	(lambda var
	  (begin
	    e1 ...
	    (apply ##sys#values var))))))))

(test-equal "primitive indirect value-binding reexport"
	    (module m13 ()
	      (import m12)		; note absence of "scheme"
	      (begin0 1 2 3))
	    1)

(module m14 (test-extlambda)
  (import chicken scheme)
  (define (test-extlambda string #!optional whatever)
    string))

(import m14)

(test-equal "extended lambda list uses expansion environment"
            "some text"
            (test-extlambda "some text"))

(test-end "modules")
