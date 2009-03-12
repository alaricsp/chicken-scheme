;;;; hash-table-tests.scm

(require-extension srfi-69)

(print "SRFI 69 procedures")
(assert (eq? hash equal?-hash))
(assert (eq? hash-by-identity eq?-hash))

;; Re-use variable
(define ht)

(print "HT - No Parameters")
(set! ht (make-hash-table))
(assert (hash-table? ht))
(assert (eq? equal? (hash-table-equivalence-function ht)))
(assert (eq? equal?-hash (hash-table-hash-function ht)))
(assert (not (hash-table-has-initial? ht)))

(print "HT - Test Parameter")
(set! ht (make-hash-table eq?))
(assert (hash-table? ht))
(assert (eq? eq? (hash-table-equivalence-function ht)))
(assert (eq? eq?-hash (hash-table-hash-function ht)))
(assert (not (hash-table-has-initial? ht)))

(print "HT - Number Test Parameter")
(set! ht (make-hash-table =))
(assert (hash-table? ht))
(assert (eq? = (hash-table-equivalence-function ht)))
(assert (eq? number-hash (hash-table-hash-function ht)))
(assert (not (hash-table-has-initial? ht)))

(print "HT - All Optional Parameters")
(set! ht (make-hash-table eqv? eqv?-hash 23))
(assert (hash-table? ht))
(assert (not (hash-table-has-initial? ht)))

(print "HT - All Parameters")
(set! ht (make-hash-table eqv? eqv?-hash 23
                          #:test equal? #:hash equal?-hash
                          #:initial 'foo
                          #:size 500
                          #:min-load 0.45 #:max-load 0.85
                          #:weak-keys #t #:weak-values #t))
(assert (hash-table? ht))
(assert (not (hash-table-weak-keys ht)))
(assert (not (hash-table-weak-values ht)))
(assert (eq? equal? (hash-table-equivalence-function ht)))
(assert (eq? equal?-hash (hash-table-hash-function ht)))
(assert (hash-table-has-initial? ht))
(assert (eq? (hash-table-initial ht) 'foo))

(print "HT - Insert with setter")
(set! (hash-table-ref ht 23.0) 'bar)
(assert (eq? (hash-table-ref ht 23.0) 'bar))

(print "HT - Insert with update!")
(hash-table-update! ht 'baz)
(assert (eq? (hash-table-ref ht 'baz) 'foo))
(assert (= (hash-table-size ht) 2))

(print "HT - A-List")
(let ([alist (hash-table->alist ht)])
  (assert (list? alist))
  (assert (= (length alist) 2))
  (assert (eq? (alist-ref 23.0 alist) 'bar))
  (assert (eq? (alist-ref 'baz alist) 'foo)) )

(print "HT - set! overwrites")
(hash-table-set! ht 23.0 'foo-bar)
(assert (eq? (hash-table-ref ht 23.0) 'foo-bar))

(print "HT - Delete")
(assert (hash-table-delete! ht 23.0))
(assert (not (hash-table-exists? ht 23.0)))
(assert (= (hash-table-size ht) 1))

(print "HT - Remove")
(assert (hash-table-remove! ht (lambda (k v) (eq? k 'baz))))
(assert (not (hash-table-exists? ht 'baz)))
(assert (= (hash-table-size ht) 0))

(print "HT - Make from A-List")
(set! ht (alist->hash-table '(("abc" . #t) ("cbs" . #t) ("cnn" . #f))))
(assert (hash-table? ht))
(assert (= (hash-table-size ht) 3))

(print "HT - Merge!")
(let ([ht2 (make-hash-table)])
  (set! (hash-table-ref ht2 23.0) 'bar)
  (set! (hash-table-ref ht2 'baz) 'foo)
  (let ([ht3 (hash-table-merge! ht2 ht)])
    (assert (eq? ht3 ht2))
    (assert (not (eq? ht3 ht)))
    (let ([alist (hash-table->alist ht3)])
      (assert (list? alist))
      (assert (= (length alist) 5))
      (assert (eq? (alist-ref "abc" alist equal?) #t))
      (assert (eq? (alist-ref "cbs" alist equal?) #t))
      (assert (eq? (alist-ref "cnn" alist equal?) #f))
      (assert (eq? (alist-ref 23.0 alist) 'bar))
      (assert (eq? (alist-ref 'baz alist) 'foo)) ) ) )

(print "HT - Merge")
(let ([ht2 (make-hash-table)])
  (set! (hash-table-ref ht2 23.0) 'bar)
  (set! (hash-table-ref ht2 'baz) 'foo)
  (let ([ht3 (hash-table-merge ht2 ht)])
    (assert (not (eq? ht3 ht2)))
    (assert (not (eq? ht3 ht)))
    (let ([alist (hash-table->alist ht3)])
      (assert (list? alist))
      (assert (= (length alist) 5))
      (assert (eq? (alist-ref "abc" alist equal?) #t))
      (assert (eq? (alist-ref "cbs" alist equal?) #t))
      (assert (eq? (alist-ref "cnn" alist equal?) #f))
      (assert (eq? (alist-ref 23.0 alist) 'bar))
      (assert (eq? (alist-ref 'baz alist) 'foo)) ) ) )

(print "HT - Map")
(let ([alist (hash-table-map ht (lambda (k v) (cons k v)))])
  (assert (list? alist))
  (assert (= (length alist) 3)) )

(print "HT - Fold")
(let ([alist (hash-table-fold ht (lambda (k v a) (cons (cons k v) a)) '())])
  (assert (list? alist))
  (assert (= (length alist) 3)) )

;; Stress Test

(set! ht (make-hash-table))

(define-constant stress-size 100000)

(print "HT - Stress Insert " stress-size " Fixnum Key Items")
(time
  (do ([i 0 (fx+ i 1)])
      [(fx= i stress-size)]
    (set! (hash-table-ref ht i) i) ) )

(print "HT - Stress Retrieve " stress-size " Fixnum Key Items")
(time
  (do ([i 0 (fx+ i 1)])
      [(fx= i stress-size)]
    (assert (fx= i (hash-table-ref ht i))) ) )
