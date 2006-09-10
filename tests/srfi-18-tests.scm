(use srfi-18)

(cond-expand (dribble
(define-for-syntax count 0)

(define-macro (trail loc expr)
  (set! count (add1 count))
  `(begin
     (print "(" ,count ") " ,loc ": " ',expr ": get: " (##sys#slot get-mutex 5) ", put: " (##sys#slot put-mutex 5))
     (let ((xxx ,expr))
       (print "  (" ,count ") " ,loc ": " ',expr ": get: " (##sys#slot get-mutex 5) ", put: " (##sys#slot put-mutex 5))
       xxx) ) )
)(else
(define-macro (trail loc expr) expr)
))

(define (tprint . x)
 (printf "~a " (current-milliseconds))
 (apply print x))

(define (make-empty-mailbox)
 (let ((put-mutex (make-mutex))        ; allow put! operation
       (get-mutex (make-mutex))
       (cell #f))

   (define (put! obj)
     (trail 'put! (mutex-lock! put-mutex #f #f))     ; prevent put! operation
     (set! cell obj)
     (trail 'put! (mutex-unlock! get-mutex)) )

   (define (get!)
     (trail 'get! (mutex-lock! get-mutex #f #f))     ; wait until object in mailbox
     (let ((result cell))
       (set! cell #f)                  ; prevent space leaks
       (trail 'get! (mutex-unlock! put-mutex))       ; allow put! operation
       result))

   (trail 'main (mutex-lock! get-mutex #f #f))       ; prevent get! operation

   (lambda (print)
     (case print
       ((put!) put!)
       ((get!) get!)
       (else (error "unknown message"))))))

(define (mailbox-put! m obj) ((m 'put!) obj))
(define (mailbox-get! m) ((m 'get!)))

;(tprint 'start)

(define mb (make-empty-mailbox))

(thread-start!
 (make-thread
 (lambda ()
   (let lp ()
     ;(print "1: get")
     (let ((x (mailbox-get! mb)))
       ;(tprint "read: " x)
       (assert x)
       (lp))))))

(thread-start!
 (make-thread
 (lambda ()
   (thread-sleep! 1)
   ;(tprint 'put)
   ;(print "2: put")
   (mailbox-put! mb 'test)
   #;(print "2: endput"))))

(thread-sleep! 3)
;(tprint 'exit)
