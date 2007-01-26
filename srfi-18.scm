;;;; srfi-18.scm - Simple thread unit - felix
;
; Copyright (c) 2000-2007, Felix L. Winkelmann
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following
; conditions are met:
;
;   Redistributions of source code must retain the above copyright notice, this list of conditions and the following
;     disclaimer. 
;   Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
;     disclaimer in the documentation and/or other materials provided with the distribution. 
;   Neither the name of the author nor the names of its contributors may be used to endorse or promote
;     products derived from this software without specific prior written permission. 
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
; AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.
;
; Send bugs, suggestions and ideas to: 
;
; felix@call-with-current-continuation.org
;
; Felix L. Winkelmann
; Unter den Gleichen 1
; 37130 Gleichen
; Germany


(declare
 (unit srfi-18)
 (uses scheduler)
 (disable-warning redef)
 (disable-interrupts)
 (usual-integrations)
 (hide ##sys#compute-time-limit) )

(cond-expand
 [paranoia]
 [else
  (declare
    (no-bound-checks)
    (no-procedure-checks-for-usual-bindings)
    (bound-to-procedure
     ##sys#thread-yield!
     condition-property-accessor ##sys#tty-port? ##sys#thread-block-for-i/o thread-yield! ##sys#thread-unblock!
     ##sys#thread-basic-unblock! gensym ##sys#thread-block-for-timeout! ##sys#thread-kill!
     ##sys#thread-block-for-termination! make-thread ##sys#exact->inexact ##sys#flonum-fraction truncate
     ##sys#add-to-ready-queue
     ##sys#schedule ##sys#make-thread
     ##sys#check-number ##sys#error ##sys#signal-hook ##sys#signal
     ##sys#current-exception-handler ##sys#abandon-mutexes ##sys#check-structure ##sys#structure? ##sys#make-mutex
     ##sys#delq ##sys#compute-time-limit ##sys#fudge) ) ] )

(cond-expand
 [unsafe
  (eval-when (compile)
    (define-macro (##sys#check-structure . _) '(##core#undefined))
    (define-macro (##sys#check-range . _) '(##core#undefined))
    (define-macro (##sys#check-pair . _) '(##core#undefined))
    (define-macro (##sys#check-list . _) '(##core#undefined))
    (define-macro (##sys#check-symbol . _) '(##core#undefined))
    (define-macro (##sys#check-string . _) '(##core#undefined))
    (define-macro (##sys#check-char . _) '(##core#undefined))
    (define-macro (##sys#check-exact . _) '(##core#undefined))
    (define-macro (##sys#check-port . _) '(##core#undefined))
    (define-macro (##sys#check-number . _) '(##core#undefined))
    (define-macro (##sys#check-byte-vector . _) '(##core#undefined)) ) ]
 [else
  (declare (emit-exports "srfi-18.exports"))] )

(register-feature! 'srfi-18)

(define-macro (dbg . args) #f)
#;(define-macro (dbg . args)
  `(print "DBG: " ,@args) )


;;; Helper routines:

(define ##sys#compute-time-limit
  (let ([truncate truncate])
    (lambda (tm)
      (and tm
	   (cond [(##sys#structure? tm 'time) (##sys#slot tm 1)]
		 [(number? tm) (fx+ (##sys#fudge 16) (inexact->exact (truncate (* tm 1000))))]
		 [else (##sys#signal-hook #:type-error "invalid timeout argument" tm)] ) ) ) ) )


;;; Time objects:

(declare
  (foreign-declare #<<EOF
static C_TLS long C_ms;
#define C_get_seconds   C_seconds(&C_ms)
EOF
) )

(define-foreign-variable C_get_seconds double)
(define-foreign-variable C_startup_time_seconds double)
(define-foreign-variable C_ms long)

(define (current-time)
  (let* ([s C_get_seconds]
	 [ss C_startup_time_seconds] 
	 [ms C_ms] )
    (##sys#make-structure
     'time
     (inexact->exact (truncate (+ (* (- s ss) 1000) C_ms)))
     s
     C_ms) ) )

(define srfi-18:current-time current-time)

(define (time->seconds tm)
  (##sys#check-structure tm 'time 'time->seconds)
  (+ (##sys#slot tm 2) (/ (##sys#slot tm 3) 1000)) )

(define (time->milliseconds tm)
  (##sys#check-structure tm 'time 'time->milliseconds)
  (+ (inexact->exact (* (- (##sys#slot tm 2) C_startup_time_seconds) 1000))
     (##sys#slot tm 3) ) )

(define (seconds->time n)
  (##sys#check-number n 'seconds->time)
  (let* ([n2 (max 0 (- n C_startup_time_seconds))] ; seconds since startup
	 [ms (truncate (* 1000 (##sys#flonum-fraction (##sys#exact->inexact n))))] ; milliseconds
	 [n3 (inexact->exact (truncate (+ (* n2 1000) ms)))] ) ; milliseconds since startup
    (##sys#make-structure 'time n3 (truncate n) (inexact->exact ms)) ) )

(define (milliseconds->time nms)
  (##sys#check-exact nms 'milliseconds->time)
  (let ((s (+ C_startup_time_seconds (/ nms 1000))))
    (##sys#make-structure 'time nms s 0) ) )

(define (time? x) (##sys#structure? x 'time))

(define srfi-18:time? time?)


;;; Exception handling:

(define raise ##sys#signal)

(define (join-timeout-exception? x) 
  (and (##sys#structure? x 'condition)
       (memq 'join-timeout-exception (##sys#slot x 1)) ) )

(define (abandoned-mutex-exception? x)
  (and (##sys#structure? x 'condition)
       (memq 'abandoned-mutex-exception (##sys#slot x 1)) ) )

(define (terminated-thread-exception? x)
  (and (##sys#structure? x 'condition)
       (memq 'terminated-thread-exception (##sys#slot x 1)) ) )

(define (uncaught-exception? x)
  (and (##sys#structure? x 'condition)
       (memq 'uncaught-exception (##sys#slot x 1)) ) )

(define uncaught-exception-reason
  (condition-property-accessor 'uncaught-exception 'reason) )


;;; Threads:

(define make-thread
  (let ((gensym gensym))
    (lambda (thunk . name)
      (let ((thread
	     (##sys#make-thread
	      #f
	      'created
	      (if (pair? name) (##sys#slot name 0) (gensym 'thread))
	      (##sys#slot ##sys#current-thread 9) ) ) )
	(##sys#setslot 
	 thread 1 
	 (lambda () 
	   (##sys#call-with-values
	    thunk
	    (lambda results
	      (##sys#setslot thread 2 results)
	      (##sys#thread-kill! thread 'dead)
	      (##sys#schedule) ) ) ) )
	thread) ) ) )

(define (thread? x) (##sys#structure? x 'thread))
(define (current-thread) ##sys#current-thread)

(define (thread-state thread)
  (##sys#check-structure thread 'thread 'thread-state)
  (##sys#slot thread 3) )

(define (thread-specific thread)
  (##sys#check-structure thread 'thread 'thread-specific)
  (##sys#slot thread 10) )

(define (thread-specific-set! thread x)
  (##sys#check-structure thread 'thread 'thread-specific-set!)
  (##sys#setslot thread 10 x) )

(define (thread-quantum thread)
  (##sys#check-structure thread 'thread 'thread-quantum)
  (##sys#slot thread 9) )

(define (thread-quantum-set! thread q)
  (##sys#check-structure thread 'thread 'thread-quantum-set!)
  (##sys#check-exact q 'thread-quantum-set!)
  (##sys#setislot thread 9 (fxmax q 10)) )

(define (thread-name x)
  (##sys#check-structure x 'thread 'thread-name)
  (##sys#slot x 6) )

(define thread-start!
  (let ([make-thread make-thread])
    (lambda (thread)
      (if (procedure? thread)
	  (set! thread (make-thread thread))
	  (##sys#check-structure thread 'thread 'thread-start!) )
      (unless (eq? 'created (##sys#slot thread 3))
	(##sys#error 'thread-start! "thread can not be started a second time" thread) )
      (##sys#setslot thread 3 'ready)
      (##sys#add-to-ready-queue thread) 
      thread) ) )

(define thread-yield! ##sys#thread-yield!) ;In library.scm

(define thread-join!
  (lambda (thread . timeout)
    (##sys#check-structure thread 'thread 'thread-join!)
    (let* ((limit (and (pair? timeout) (##sys#compute-time-limit (##sys#slot timeout 0))))
	   (rest (and (pair? timeout) (##sys#slot timeout 1)))
	   (tosupplied (and rest (pair? rest)))
	   (toval (and tosupplied (##sys#slot rest 0))) )
      (##sys#call-with-current-continuation
       (lambda (return)
	 (let ([ct ##sys#current-thread])
	   (when limit (##sys#thread-block-for-timeout! ct limit))
	   (##sys#setslot
	    ct 1
	    (lambda ()
	      (case (##sys#slot thread 3)
		[(dead) (apply return (##sys#slot thread 2))]
		[(terminated)
		 (return 
		  (##sys#signal
		   (##sys#make-structure 
		    'condition '(uncaught-exception)
		    (list '(uncaught-exception . reason) (##sys#slot thread 7)) ) ) ) ]
		[else
		 (return
		  (if tosupplied
		      toval
		      (##sys#signal
		       (##sys#make-structure 'condition '(join-timeout-exception) '())) ) ) ] ) ) )
	   (##sys#thread-block-for-termination! ct thread) 
	   (##sys#schedule) ) ) ) ) ) )
	   
(define (thread-terminate! thread)
  (##sys#check-structure thread 'thread 'thread-terminate!)
  (when (eq? thread ##sys#primordial-thread)
    ((##sys#exit-handler)) )
  (##sys#setislot thread 2 (list (##core#undefined)))
  (##sys#setslot thread 7 (##sys#make-structure 'condition '(terminated-thread-exception) '()))
  (##sys#thread-kill! thread 'terminated)
  (when (eq? thread ##sys#current-thread) (##sys#schedule)) )

(define (thread-suspend! thread)
  (##sys#check-structure thread 'thread 'thread-suspend!)
  (##sys#setslot thread 3 'suspended)
  (when (eq? thread ##sys#current-thread)
    (##sys#call-with-current-continuation
     (lambda (return)
       (##sys#setslot thread 1 (lambda () (return (##core#undefined))))
       (##sys#schedule) ) ) ) )

(define (thread-resume! thread)
  (##sys#check-structure thread 'thread 'thread-resume!)
  (when (eq? (##sys#slot thread 3) 'suspended)
    (##sys#setslot thread 3 'ready)
    (##sys#add-to-ready-queue thread) ) )

(define (thread-sleep! tm)
  (define (sleep limit loc)
    (##sys#call-with-current-continuation
     (lambda (return)
       (let ((ct ##sys#current-thread))
	 (##sys#setslot ct 1 (lambda () (return (##core#undefined))))
	 (##sys#thread-block-for-timeout! ct limit)
	 (##sys#schedule) ) ) ) )
  (unless tm (##sys#signal-hook #:type-error 'thread-sleep! "invalid timeout argument" tm))
  (sleep (##sys#compute-time-limit tm) 'thread-sleep!) )


;;; Mutexes:

(define (mutex? x) (##sys#structure? x 'mutex))

(define make-mutex
  (let ((gensym gensym))
    (lambda id
      (let* ((id (if (pair? id) (car id) (gensym 'mutex)))
	     (m (##sys#make-mutex id ##sys#current-thread)) )
	m) ) ) )

(define (mutex-name x)
  (##sys#check-structure x 'mutex 'mutex-name) 
  (##sys#slot x 1) )

(define (mutex-specific mutex)
  (##sys#check-structure mutex 'mutex 'mutex-specific)
  (##sys#slot mutex 6) )

(define (mutex-specific-set! mutex x)
  (##sys#check-structure mutex 'mutex 'mutex-specific-set!)
  (##sys#setslot mutex 6 x) )

(define (mutex-state mutex)
  (##sys#check-structure mutex 'mutex 'mutex-state)
  (cond [(##sys#slot mutex 5) (or (##sys#slot mutex 2) 'not-owned)]
	[(##sys#slot mutex 4) 'abandoned]
	[else 'not-abandoned] ) )

(define mutex-lock! 
  (lambda (mutex . ms-and-t)
    (##sys#check-structure mutex 'mutex 'mutex-lock!)
    (let* ([limitsup (pair? ms-and-t)]
	   [limit (and limitsup (##sys#compute-time-limit (car ms-and-t)))]
	   [threadsup (fx> (length ms-and-t) 1)]
	   [thread (and threadsup (cadr ms-and-t))] 
	   [abd (##sys#slot mutex 4)] )
      (when thread (##sys#check-structure thread 'thread 'mutex-lock!))
      (##sys#call-with-current-continuation
       (lambda (return)
	 (let ([ct ##sys#current-thread])
	   (define (switch)
	     (##sys#setslot mutex 3 (##sys#append (##sys#slot mutex 3) (list ct)))
	     (##sys#schedule) )
	   (define (check)
	     (when abd
	       (return (##sys#signal (##sys#make-structure 'condition '(abandoned-mutex-exception) '()))) ) )
	   (dbg ct ": locking " mutex)
	   (cond [(not (##sys#slot mutex 5))
		  (if (and threadsup (not thread))
		      (begin
			(##sys#setislot mutex 2 #f)
			(##sys#setislot mutex 5 #t) )
		      (let* ([t (or thread ct)]
			     [ts (##sys#slot t 3)] )
			(if (or (eq? 'terminated ts) (eq? 'dead ts))
			    (##sys#setislot mutex 4 #t)
			    (begin
			      (##sys#setislot mutex 5 #t)
			      (##sys#setslot t 8 (cons mutex (##sys#slot t 8)))
			      (##sys#setslot mutex 2 t) ) ) ) )
		  (check)
		  (return #t) ]
		 [limit
		  (check)
		  (##sys#setslot
		   ct 1 
		   (lambda ()
		     (##sys#setslot mutex 3 (##sys#delq ct (##sys#slot mutex 3)))
		     (##sys#setslot ##sys#current-thread 8 (cons mutex (##sys#slot ##sys#current-thread 8)))
		     (##sys#setslot mutex 2 thread)
		     #f) )
		  (##sys#thread-block-for-timeout! ct limit)
		  (switch) ]
		 [else
		  (##sys#setslot ct 3 'sleeping)
		  (##sys#setslot ct 1 (lambda () (return #t)))
		  (switch) ] ) ) ) ) ) ) )

(define mutex-unlock!
  (lambda (mutex . cvar-and-to)
    (##sys#check-structure mutex 'mutex 'mutex-unlock!)
    (let ([ct ##sys#current-thread]
	  [cvar (and (pair? cvar-and-to) (car cvar-and-to))]
	  [timeout (and (fx> (length cvar-and-to) 1) (cadr cvar-and-to))] )
      (dbg ct ": unlocking " mutex)
      (when cvar (##sys#check-structure cvar 'condition-variable 'mutex-unlock!))
      (##sys#call-with-current-continuation
       (lambda (return)
	 (let ([waiting (##sys#slot mutex 3)]
	       [limit (and timeout (##sys#compute-time-limit timeout))] 
	       [result #t] )
	   (##sys#setislot mutex 4 #f)
	   (##sys#setislot mutex 5 #f)
	   (##sys#setslot ct 8 (##sys#delq mutex (##sys#slot ct 8)))
	   (##sys#setslot ct 1 (lambda () (return result)))
	   (when cvar
	     (##sys#setslot cvar 2 (##sys#append (##sys#slot cvar 2) (##sys#list ct)))
	     (cond [limit
		    (##sys#setslot 
		     ct 1
		     (lambda () 
		       (##sys#setslot cvar 2 (##sys#delq ct (##sys#slot cvar 2)))
		       (return #f) ) )
		    (##sys#thread-block-for-timeout! ct limit) ]
		   [else 
		    (##sys#setslot ct 3 'sleeping)] ) )
	   (unless (null? waiting)
	     (let* ([wt (##sys#slot waiting 0)]
		    [wts (##sys#slot wt 3)] )
	       (##sys#setslot mutex 3 (##sys#slot waiting 1))
	       (##sys#setislot mutex 5 #t)
	       (when (or (eq? wts 'blocked) (eq? wts 'sleeping))
		 (##sys#setslot mutex 2 wt)
		 (##sys#setslot wt 8 (cons mutex (##sys#slot wt 8)))
		 (when (eq? wts 'sleeping) (##sys#add-to-ready-queue wt) ) ) ) )
	   (##sys#schedule) ) ) ) ) ) )


;;; Condition variables:

(define make-condition-variable
  (let ([gensym gensym])
    (lambda name
      (##sys#make-structure
       'condition-variable 
       (if (pair? name)			; #1 name
	   (car name)
	   (gensym 'condition-variable) )
       '()				; #2 list of waiting threads
       (##core#undefined) ) ) ) )	; #3 specific

(define (condition-variable? x)
  (##sys#structure? x 'condition-variable) )

(define (condition-variable-specific cv)
  (##sys#check-structure cv 'condition-variable 'condition-variable-specific)
  (##sys#slot cv 3) )

(define (condition-variable-specific-set! cv x)
  (##sys#check-structure cv 'condition-variable 'condition-variable-specific-set!)
  (##sys#setslot cv 3 x) )

(define (condition-variable-signal! cvar)
  (##sys#check-structure cvar 'condition-variable 'condition-variable-signal!)
  (dbg "signalling " cvar)
  (let ([ts (##sys#slot cvar 2)])
    (unless (null? ts)
      (let* ([t0 (##sys#slot ts 0)]
	     [t0s (##sys#slot t0 3)] )
	(##sys#setslot cvar 2 (##sys#slot ts 1))
	(when (or (eq? t0s 'blocked) (eq? t0s 'sleeping))
	  (##sys#thread-basic-unblock! t0) ) ) ) ) )

(define (condition-variable-broadcast! cvar)
  (##sys#check-structure cvar 'condition-variable 'condition-variable-broadcast!)
  (dbg "broadcasting " cvar)
  (##sys#for-each
   (lambda (ti)
     (let ([tis (##sys#slot ti 3)])
       (when (or (eq? tis 'blocked) (eq? tis 'sleeping))
	 (##sys#thread-basic-unblock! ti) ) ) )
   (##sys#slot cvar 2) )
  (##sys#setislot cvar 2 '()) )


;;; Change continuation of thread to signal an exception:

(define (thread-signal! thread exn)
  (##sys#check-structure thread 'thread 'thread-signal!)
  (if (eq? thread ##sys#current-thread)
      (##sys#signal exn)
      (let ([old (##sys#slot thread 1)])
	(##sys#setslot
	 thread 1
	 (lambda ()
	   (##sys#signal exn)
	   (old) ) )
	(##sys#thread-unblock! thread) ) ) )

(define thread-deliver-signal! thread-signal!)


;;; Don't block in the repl: (by Chris Double)

(unless (eq? (build-platform) 'msvc)
  (set! ##sys#read-prompt-hook
    (let ([old ##sys#read-prompt-hook]
	  [thread-yield! thread-yield!] )
      (lambda ()
	(when (or (##sys#fudge 12) (##sys#tty-port? ##sys#standard-input))
	  (old)
	  (##sys#thread-block-for-i/o! ##sys#current-thread 0 #t)
	  (thread-yield!)))) ) )
