;;; by Marc Feeley
;
; This fails. Currently to heavy stuff to debug

(use srfi-18)

(define (dw tag thunk)
 (dynamic-wind
   (lambda () (pp (list 'before tag (current-thread))))
   thunk
   (lambda () (pp (list 'after tag (current-thread))))))

(define c1 #f)
(define c2 #f)
(define c3 #f)
(define c4 #f)

(define (f)
 (call/cc
  (lambda (k1)
    (set! c1 k1)
    (dw 111
        (lambda ()
          (call/cc
           (lambda (k2)
             (set! c2 k2)
             (dw 222
                 (lambda ()
                   (call/cc
                    (lambda (k3)
                      (set! c3 k3)
                      (dw 333
                          (lambda ()
                            (call/cc
                             (lambda (k4)
                               (set! c4 k4)
;                                (xxx) ;; error
                               (pp 'inner)))))))))))))))
 (pp (list 'done (current-thread))))

(thread-join!
 (thread-start!
 (make-thread (lambda () (f)))))

(thread-join!
 (thread-start!
 (make-thread (lambda () (c4 'dummy)))))

(thread-join!
 (thread-start!
 (make-thread (lambda () (c1 'dummy)))))


;; expected result:

;; (before 111 #<thread #2>)
;; (before 222 #<thread #2>)
;; (before 333 #<thread #2>)
;; inner
;; (after 333 #<thread #2>)
;; (after 222 #<thread #2>)
;; (after 111 #<thread #2>)
;; (done #<thread #2>)
;; (before 111 #<thread #3>)
;; (before 222 #<thread #3>)
;; (before 333 #<thread #3>)
;; (after 333 #<thread #3>)
;; (after 222 #<thread #3>)
;; (after 111 #<thread #3>)
;; (done #<thread #3>)
;; (done #<thread #4>)
