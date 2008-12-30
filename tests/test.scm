;;;; test.scm - minimal testing framework
;
; by Alex Shinn, lifted from match-test by felix


(define *pass* 0)
(define *fail* 0)
(define *start* 0)
(define *fail-token* (gensym))

(define (run-test name thunk expect eq pass-msg fail-msg)
  (let ((result (thunk)))
    (cond
      ((eq expect result)
       (set! *pass* (+ *pass* 1))
       (format-result pass-msg name expect result))
      (else
       (set! *fail* (+ *fail* 1))
       (format-result fail-msg name expect result)))))

(define (format-result ls name expect result)
  (let lp ((ls ls))
    (cond
      ((null? ls) (newline))
      ((eq? (car ls) 'expect) (display expect) (lp (cdr ls)))
      ((eq? (car ls) 'result) (display result) (lp (cdr ls)))
      ((eq? (car ls) 'name) (if name (begin (display #\space) (display name))) (lp (cdr ls)))
      (else (display (car ls)) (lp (cdr ls))))))

(define (test-begin . o)
  (set! *pass* 0)
  (set! *fail* 0)
  (set! *start* (current-milliseconds)))

(define (format-float n prec)
  (let* ((str (number->string n))
         (len (string-length str)))
    (let lp ((i (- len 1)))
      (cond
        ((negative? i)
         (string-append str "." (make-string prec #\0)))
        ((eqv? #\. (string-ref str i))
         (let ((diff (+ 1 (- prec (- len i)))))
           (cond
             ((positive? diff)
              (string-append str (make-string diff #\0)))
             ((negative? diff)
              (substring str 0 (+ i prec 1)))
             (else
              str))))
        (else
         (lp (- i 1)))))))

(define (format-percent num denom)
  (let ((x (if (zero? denom) num (exact->inexact (/ num denom)))))
    (format-float (* 100 x) 2)))

(define (test-end . o)
  (let ((end (current-milliseconds))
        (total (+ *pass* *fail*)))
    (print "  " total " tests completed in "
	   (format-float (exact->inexact (/ (- end *start*) 1000)) 3)
	   " seconds")
    (print "  " *pass* " ("
	   (format-percent *pass* total)
	   "%) tests passed")
    (print "  " *fail* " ("
	   (format-percent *fail* total)
	   "%) tests failed")
    (exit (if (zero? *fail*) 0 1))))

(define (run-equal name thunk expect eq)
  (run-test name thunk expect eq
            '("(PASS)" name)
            '("(FAIL)" name ": expected " expect " but got " result)))

(define-syntax test-equal
  (syntax-rules ()
    ((_ name expr value eq) (run-equal name (lambda () expr) value eq))
    ((_ name expr value) (run-equal name (lambda () expr) value equal?))))

(define-syntax test-error
  (syntax-rules ()
    ((_ name expr)
     (run-equal
      name
      (lambda () (handle-exceptions ex *fail-token* expr)) *fail-token* eq?) )
    ((_ expr) (test-error 'expr expr))))

(define-syntax test-assert
  (syntax-rules ()
    ((_ name expr) (run-equal name (lambda () (if expr #t #f)) #t eq?))))

(define-syntax test-group
  (syntax-rules ()
    ((_ name body ...)
     (begin
       (print "\n" name ":\n")
       body ...))))
