;;;; module-tests-2.scm


(cond-expand
 (compiling
  (include "test.scm") )
 (else
  (load-relative "test.scm")))

(test-begin "modules (II)")

(require-extension ec)			; import into toplevel env

(test-error 
 "empty initial module SE"
 (module m1 () (import chicken) (print :-dispatch))) ; should not be seen

(test-end "modules (II)")
