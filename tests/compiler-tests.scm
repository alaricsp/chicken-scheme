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

;;; rev. 14574 (reported by Peter Bex)
;
; - type specifiers in foreign-lambda in macros are incorrectly renamed
; - variable names and type specifiers in foreign-lambda* and
;    foreign-primitive in macros are incorrectly renamed

(let-syntax ((strlen-macro
              (syntax-rules ()
                ((strlen-macro arg)
                 (print ((foreign-lambda int strlen c-string) arg)))))
             (strlen-macro*
              (syntax-rules ()
                ((strlen-macro* arg)
                 (print ((foreign-lambda* int ((c-string str))
                                          "C_return(strlen(str));") arg)))))
             (strlen-safe-macro
              (syntax-rules ()
                ((strlen-safe-macro arg)
                 (print ((foreign-safe-lambda int strlen c-string) arg)))))
             (strlen-safe-macro*
              (syntax-rules ()
                ((strlen-safe-macro* arg)
                 (print ((foreign-safe-lambda* int ((c-string str))
                                               "C_return(strlen(str));") arg)))))
             (strlen-primitive-macro
              (syntax-rules ()
                ((strlen-primitive-macro* arg)
                 (print ((foreign-primitive int ((c-string str))
                                            "C_return(strlen(str));") arg))))))
  (strlen-macro "hello, world")
  (strlen-macro* "hello, world")
  (strlen-safe-macro "hello, world")
  (strlen-safe-macro* "hello, world")
  (strlen-primitive-macro "hello, world"))
