(load-relative "loopy-loop.scm")
(load-relative "matchable.scm")

(require-extension srfi-69)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SRFI-64 subset + test-approx=

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
    (printf "  ~A tests completed in ~A seconds\n"
            total (format-float (exact->inexact (/ (- end *start*) 1000)) 3))
    (printf "  ~A (~A%) tests passed\n"
            *pass* (format-percent *pass* total))
    (printf "  ~A (~A%) tests failed\n"
            *fail* (format-percent *fail* total))))

(define-syntax test-assert
  (syntax-rules ()
    ((_ x opt)
     (run-assert x (lambda () opt)))
    ((_ x ) (run-assert 'x (lambda () x)))))

(define (run-equal name thunk expect eq)
  (run-test name thunk expect eq
            '("(PASS)" name)
            '("(FAIL)" name ": expected " expect " but got " result)))

(define-syntax test-equal
  (syntax-rules ()
    ((_ x y opt)
     (run-equal x (lambda () y) opt equal?))
    ((_ x y) (run-equal 'x (lambda () x) y equal?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run tests

(test-begin "loop")

(test-equal
 "stepping"
 (loop lp ((i 0 (+ i 1)) (res '() (cons i res)))
   (if (= i 3)
     (reverse res)
     (lp)))
 '(0 1 2))

(test-equal
 "basic in-list"
 (let ((res '()))
   (loop ((x <- in-list '(a b c)))
     (set! res (cons x res)))
   res)
 '(c b a))

(test-equal
 "in-list with result"
 (loop ((x <- in-list '(a b c))
        (res '() (cons x res)))
   => res)
 '(c b a))

(test-equal
 "in-list with collecting"
 (loop ((x <- in-list '(a b c)) (res <- collecting x)) => res)
 '(a b c))

(test-equal
 "uneven length in-list's"
 (loop ((x <- in-list '(a b c))
        (y <- in-list '(1 2 3 4))
        (res <- collecting (cons x y)))
    => res)
 '((a . 1) (b . 2) (c . 3)))

(test-equal
 "in-lists"
 (loop ((ls <- in-lists '((a b c) (1 2 3)))
        (res <- collecting ls))
   => res)
 '((a 1) (b 2) (c 3)))

(define (flatten ls)
  (reverse
   (loop lp ((x ls <- in-list ls) (res '()))
       => res
     (if (pair? x)
       (lp res <- (lp ls <- x))
       (lp res <- (cons x res))))))

(test-equal
 "flatten (recursion test)"
 (flatten '(1 (2) (3 (4 (5)) 6) 7))
 '(1 2 3 4 5 6 7))

(test-equal
 "in-string"
 (loop ((c <- in-string "hello") (res <- collecting c)) => res)
 '(#\h #\e #\l #\l #\o))

(test-equal
 "in-string with start"
 (loop ((c <- in-string "hello" 3) (res <- collecting c)) => res)
 '(#\l #\o))

(test-equal
 "in-string with start and end"
 (loop ((c <- in-string "hello" 0 4) (res <- collecting c)) => res)
 '(#\h #\e #\l #\l))

(test-equal
 "in-string with start, end and step"
 (loop ((c <- in-string "hello" 1 4 2) (res <- collecting c)) => res)
 '(#\e #\l))

(test-equal
 "in-string-reverse"
 (loop ((c <- in-string-reverse "hello") (res <- collecting c)) => res)
 '(#\o #\l #\l #\e #\h))

(test-equal
 "in-vector"
 (loop ((x <- in-vector '#(1 2 3)) (res <- collecting x)) => res)
 '(1 2 3))

(test-equal
 "in-permutations"
 (loop ((p <- in-permutations '(a b c)) (res <- collecting p)) => res)
 '((a b c) (a c b) (b a c) (b c a) (c a b) (c b a)))

(test-equal
 "in-permutations with length"
 (loop ((p <- in-permutations '(a b c) 2) (res <- collecting p)) => res)
 '((a b) (a c) (b a) (b c) (c a) (c b)))

(test-equal
 "in-combinations"
 (loop ((p <- in-combinations '(a b c) 2) (res <- collecting p)) => res)
 '((a b) (a c) (b c)))

(test-equal
 "in-hash-table"
 (loop ((k v <- in-hash-table (alist->hash-table '((a . 1))))
        (res <- collecting (cons k v)))
   => res)
 '((a . 1)))

(test-end "loop")

