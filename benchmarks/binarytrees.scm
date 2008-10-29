;;; The Computer Language Benchmarks Game
;;; http://shootout.alioth.debian.org/
;;; contributed by Sven Hartrumpf

(define make (lambda (item d)
  (if (= d 0)
    (list 'empty item)
    (let ((item2 (* item 2))
          (d2 (- d 1)))
      (list 'node (make (- item2 1) d2) item (make item2 d2))))))

(define check (lambda (t)
  (if (eq? (car t) 'empty)
    (cadr t)
    (+ (caddr t) (- (check (cadr t)) (check (cadddr t)))))))

(define main (lambda (n)
  (let* ((min-depth 4)
         (max-depth (max (+ min-depth 2) n)))
    (let ((stretch-depth (+ max-depth 1)))
      (display "stretch tree of depth ") (display stretch-depth) (write-char #\tab) (display " check: ") (display (check (make 0 stretch-depth))) (newline))
    (let ((long-lived-tree (make 0 max-depth)))
      (do ((d 4 (+ d 2))
           (c 0 0))
        ((> d max-depth))
        (let ((iterations (arithmetic-shift 1 (+ (- max-depth d) min-depth)))) ; chicken-specific: arithmetic-shift
          (do ((i 0 (+ i 1)))
            ((>= i iterations))
            (set! c (+ c (check (make i d)) (check (make (- i) d)))))
          (display (* 2 iterations)) (write-char #\tab) (display " trees of depth ") (display d) (write-char #\tab) (display " check: ") (display c) (newline)))
      (display "long lived tree of depth ") (display max-depth) (write-char #\tab) (display " check: ") (display (check long-lived-tree)) (newline)))))

(time (main 10))
