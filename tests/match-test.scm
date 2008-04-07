(define *pass* 0)
(define *fail* 0)
(define *start* 0)

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

(define-syntax test-assert
  (syntax-rules ()
    ((_ name expr) (run-assert name (lambda () expr)))
    ((_ expr) (run-assert 'expr (lambda () expr)))))

(define (run-equal name thunk expect eq)
  (run-test name thunk expect eq
            '("(PASS)" name)
            '("(FAIL)" name ": expected " expect " but got " result)))

(define-syntax test-equal
  (syntax-rules ()
    ((_ name expr value eq) (run-equal name (lambda () expr) value eq))
    ((_ name expr value) (run-equal name (lambda () expr) value equal?))
    ((_ expr value) (run-assert 'expr (lambda () expr) value equal?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run tests

(test-begin "match")

(test-equal "any" (match 'any (_ 'ok)) 'ok)
(test-equal "symbol" (match 'ok (x x)) 'ok)
(test-equal "number" (match 28 (28 'ok)) 'ok)
(test-equal "string" (match "good" ("bad" 'fail) ("good" 'ok)) 'ok)
(test-equal "literal symbol" (match 'good ('bad 'fail) ('good 'ok)) 'ok)
(test-equal "null" (match '() (() 'ok)) 'ok)
(test-equal "pair" (match '(ok) ((x) x)) 'ok)
(test-equal "vector" (match '#(ok) (#(x) x)) 'ok)
(test-equal "any doubled" (match '(1 2) ((_ _) 'ok)) 'ok)
(test-equal "and empty" (match '(o k) ((and) 'ok)) 'ok)
(test-equal "and single" (match 'ok ((and x) x)) 'ok)
(test-equal "and double" (match 'ok ((and (? symbol?) y) 'ok)) 'ok)
(test-equal "or empty" (match '(o k) ((or) 'fail) (else 'ok)) 'ok)
(test-equal "or single" (match 'ok ((or x) 'ok)) 'ok)
(test-equal "or double" (match 'ok ((or (? symbol? y) y) y)) 'ok)
(test-equal "not" (match 28 ((not (a . b)) 'ok)) 'ok)
(test-equal "pred" (match 28 ((? number?) 'ok)) 'ok)
(test-equal "named pred" (match 28 ((? number? x) (+ x 1))) 29)

(test-equal "duplicate symbols pass" (match '(ok . ok) ((x . x) x)) 'ok)
(test-equal "duplicate symbols fail" (match '(ok . bad) ((x . x) 'bad) (else 'ok)) 'ok)
(test-equal "duplicate symbols samth" (match '(ok . ok) ((x . 'bad) x) (('ok . x) x)) 'ok)

(test-equal "ellipses"
            (match '((a . 1) (b . 2) (c . 3))
              (((x . y) ___) (list x y)))
            '((a b c) (1 2 3)))

(test-equal "real ellipses"
            (match '((a . 1) (b . 2) (c . 3))
              (((x . y) ...) (list x y)))
            '((a b c) (1 2 3)))

(test-equal "vector ellipses"
            (match '#(1 2 3 (a . 1) (b . 2) (c . 3))
              (#(a b c (hd . tl) ...) (list a b c hd tl)))
            '(1 2 3 (a b c) (1 2 3)))

(test-equal "pred ellipses"
            (match '(1 2 3)
              (((? odd? n) ___) n)
              (((? number? n) ___) n))
            '(1 2 3))

(test-equal "failure continuation"
            (match '(1 2)
              ((a . b) (=> next) (if (even? a) 'fail (next)))
              ((a . b) 'ok))
            'ok)

(test-equal "let"
            (match-let ((x 'ok) (y '(o k)))
              y)
            '(o k))

(test-equal "let*"
            (match-let* ((x 'f) (y 'o) ((z w) (list y x)))
              (list x y z w))
            '(f o o f))

(test-equal "getter car"
            (match '(1 . 2) (((get! a) . b) (list (a) b)))
            '(1 2))

(test-equal "getter cdr"
            (match '(1 . 2) ((a . (get! b)) (list a (b))))
            '(1 2))

(test-equal "getter vector"
            (match '#(1 2 3) (#((get! a) b c) (list (a) b c)))
            '(1 2 3))

(test-equal "setter car"
            (let ((x '(1 . 2)))
              (match x (((set! a) . b) (a 3)))
              x)
            '(3 . 2))

(test-equal "setter cdr"
            (let ((x '(1 . 2)))
              (match x ((a . (set! b)) (b 3)))
              x)
            '(1 . 3))

(test-equal "setter vector"
            (let ((x '#(1 2 3)))
              (match x (#(a (set! b) c) (b 0)))
              x)
            '#(1 0 3))

(test-equal "single tail"
            (match '((a . 1) (b . 2) (c . 3))
              (((x . y) ... last) (list x y last)))
            '((a b) (1 2) (c . 3)))

(test-equal "single tail 2"
            (match '((a . 1) (b . 2) 3)
              (((x . y) ... last) (list x y last)))
            '((a b) (1 2) 3))

(test-equal "multiple tail"
            (match '((a . 1) (b . 2) (c . 3) (d . 4) (e . 5))
              (((x . y) ... u v w) (list x y u v w)))
            '((a b) (1 2) (c . 3) (d . 4) (e . 5)))

(test-equal "Riastradh quasiquote"
            (match '(1 2 3) (`(1 ,b ,c) (list b c)))
            '(2 3))

(test-end "match")

