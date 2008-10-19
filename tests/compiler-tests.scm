;;;; compiler-tests.scm


(module foo (bar)
  (import scheme chicken)
  (declare (hide bar))
  (define (bar x) (+ x 1)))

(assert (not (##sys#symbol-has-toplevel-binding? 'foo#bar)))


;;; rev. 12104 (reported by Joerg Wittenberger)
;
; - canonicalization of assignment to location didn't walk expansion recursively

(define test-location
 (let-location
  ((again bool #f))
  (lambda ()
     ((foreign-lambda*
       int
       (((c-pointer bool) again))
       "*again=1; return(1);")
      (location again))
     again)))

(print (test-location))


;;; rev. 12188 (reported by Jörg Wittenberger)
;
; - generated init-assignment refers to alias, but alias isn't seen later)

(module
 x
 (bar)
 (import scheme chicken foreign)

 (define (bar n)
  (let-location
   ((off integer 0))
   (lambda () ((foreign-lambda*
                void
                (((c-pointer integer) i))
                "(*i)++;")
               (location off)) off)))
)

(import x)
(bar 42)
