; scheduler.scm - Basic scheduler for multithreading
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
  (fixnum)
  (unit scheduler)
  (disable-interrupts)
  (usual-integrations)
  (emit-exports "scheduler.exports")
  (disable-warning var)
  (hide ##sys#ready-queue-head ##sys#ready-queue-tail ##sys#timeout-list
	##sys#update-thread-state-buffer ##sys#restore-thread-state-buffer
	##sys#remove-from-ready-queue ##sys#unblock-threads-for-i/o ##sys#force-primordial
	##sys#fdset-input-set ##sys#fdset-output-set ##sys#fdset-clear
	##sys#fdset-select-timeout ##sys#fdset-restore) 
  (foreign-declare #<<EOF
#ifdef HAVE_ERRNO_H
# include <errno.h>
# define C_signal_interrupted_p     C_mk_bool(errno == EINTR)
#else
# define C_signal_interrupted_p     C_SCHEME_FALSE
#endif

#ifdef _WIN32
# if _MSC_VER > 1300
# include <winsock2.h>
# include <ws2tcpip.h>
# else
# include <winsock.h>
# endif
/* Beware: winsock2.h must come BEFORE windows.h */
# define C_msleep(n)     (Sleep(C_unfix(n)), C_SCHEME_TRUE)
#else
# include <unistd.h>
# include <sys/types.h>
# include <sys/time.h>
# include <time.h>
static C_word C_msleep(C_word ms);
C_word C_msleep(C_word ms) {
#ifdef __CYGWIN__
  if(usleep(C_unfix(ms) * 1000) == -1) return C_SCHEME_FALSE;
#else
  struct timespec ts;
  unsigned long mss = C_unfix(ms);
  ts.tv_sec = mss / 1000;
  ts.tv_nsec = (mss % 1000) * 1000000;
  
  if(nanosleep(&ts, NULL) == -1) return C_SCHEME_FALSE;
#endif
  return C_SCHEME_TRUE;
}
#endif
static fd_set C_fdset_input, C_fdset_output, C_fdset_input_2, C_fdset_output_2;
#define C_fd_test_input(fd)  C_mk_bool(FD_ISSET(C_unfix(fd), &C_fdset_input))
#define C_fd_test_output(fd)  C_mk_bool(FD_ISSET(C_unfix(fd), &C_fdset_output))
EOF
) )

(cond-expand
 [paranoia]
 [else
  (declare (unsafe)) ] )


(define-macro (dbg . args) #f)
#;(define-macro (dbg . args)
  `(print "DBG: " ,@args) )


(define (##sys#schedule)
  (define (switch thread)
    (dbg "switching to " thread)
    (set! ##sys#current-thread thread)
    (##sys#setslot thread 3 'running)
    (##sys#restore-thread-state-buffer thread)
    (##core#inline "C_set_initial_timer_interrupt_period" (##sys#slot thread 9))
    ((##sys#slot thread 1)) )
  (let* ([ct ##sys#current-thread]
	 [eintr #f]
	 [cts (##sys#slot ct 3)] )
    (dbg "scheduling, current: " ct ", ready: " ##sys#ready-queue-head)
    (##sys#update-thread-state-buffer ct)
    ;; Put current thread on ready-queue:
    (when (or (eq? cts 'running) (eq? cts 'ready)) ; should ct really be 'ready? - normally not.
      (##sys#add-to-ready-queue ct) )
    (let loop1 ()
      ;; Unblock threads waiting for timeout:
      (unless (null? ##sys#timeout-list)
	(let ([now (##sys#fudge 16)])
	  (dbg "timeout (" now ") list: " ##sys#timeout-list)
	  (let loop ([lst ##sys#timeout-list])
	    (if (null? lst)
		(set! ##sys#timeout-list '())
		(let* ([tmo1 (caar lst)]
		       [tto (cdar lst)]
		       [tmo2 (##sys#slot tto 4)] )
		  (dbg "  " tto " -> " tmo2)
		  (if (eq? tmo1 tmo2)
		      (if (>= now tmo1)
			  (begin
			    (##sys#thread-basic-unblock! tto)
			    (loop (cdr lst)) )
			  (begin
			    (set! ##sys#timeout-list lst) 
			    ;; If there are no threads blocking on a select call (fd-list)
			    ;; but there are threads in the timeout list then sleep for
			    ;; the number of milliseconds of next thread to wake up.
			    (when (and (null? ##sys#ready-queue-head)
				       (null? ##sys#fd-list) 
				       (pair? ##sys#timeout-list))
			      (let ([tmo1 (caar ##sys#timeout-list)])
				(set! eintr
				  (and (not (##core#inline "C_msleep" (fxmax 0 (- tmo1 now))))
				       (foreign-value "C_signal_interrupted_p" bool) ) ) ) ) ) )
		      (loop (cdr lst)) ) ) ) ) ) )
      ;; Unblock threads blocked by I/O:
      (if eintr
	  (##sys#force-primordial)
	  (begin
	    (unless (null? ##sys#fd-list)
	      (##sys#unblock-threads-for-i/o) ) ) )
      ;; Fetch and activate next ready thread:
      (let loop2 ()
	(let ([nt (##sys#remove-from-ready-queue)])
	  (cond [(not nt) 
		 (if (and (null? ##sys#timeout-list) (null? ##sys#fd-list))
		     (##sys#signal-hook #:runtime-error "deadlock")
		     (loop1) ) ]
		[(eq? (##sys#slot nt 3) 'ready) (switch nt)]
		[else (loop2)] ) ) ) ) ) )

(define (##sys#force-primordial)
  (dbg "primordial thread forced due to interrupt")
  (##sys#thread-unblock! ##sys#primordial-thread) )

(define ##sys#ready-queue-head '())
(define ##sys#ready-queue-tail '())

(define (##sys#ready-queue) ##sys#ready-queue-head)

(define (##sys#add-to-ready-queue thread)
  (##sys#setslot thread 3 'ready)
  (let ((new-pair (cons thread '())))
    (cond ((eq? '() ##sys#ready-queue-head) 
	   (set! ##sys#ready-queue-head new-pair))
	  (else (set-cdr! ##sys#ready-queue-tail new-pair)) )
    (set! ##sys#ready-queue-tail new-pair) ) )

(define (##sys#remove-from-ready-queue)
  (let ((first-pair ##sys#ready-queue-head))
    (and (not (null? first-pair))
	 (let ((first-cdr (cdr first-pair)))
	   (set! ##sys#ready-queue-head first-cdr)
	   (when (eq? '() first-cdr) (set! ##sys#ready-queue-tail '()))
	   (car first-pair) ) ) ) )

(define (##sys#update-thread-state-buffer thread)
  (let ([buf (##sys#slot thread 5)])
    (##sys#setslot buf 0 ##sys#dynamic-winds)
    (##sys#setslot buf 1 ##sys#standard-input)
    (##sys#setslot buf 2 ##sys#standard-output)
    (##sys#setslot buf 3 ##sys#standard-error)
    (##sys#setslot buf 4 ##sys#current-exception-handler)
    (##sys#setslot buf 5 ##sys#current-parameter-vector) ) )

(define (##sys#restore-thread-state-buffer thread)
  (let ([buf (##sys#slot thread 5)])
    (set! ##sys#dynamic-winds (##sys#slot buf 0))
    (set! ##sys#standard-input (##sys#slot buf 1))
    (set! ##sys#standard-output (##sys#slot buf 2))
    (set! ##sys#standard-error (##sys#slot buf 3)) 
    (set! ##sys#current-exception-handler (##sys#slot buf 4))
    (set! ##sys#current-parameter-vector (##sys#slot buf 5)) ) )

(set! ##sys#interrupt-hook
  (let ([oldhook ##sys#interrupt-hook])
    (lambda (reason state)
      (when (fx= reason 255)		; C_TIMER_INTERRUPT_NUMBER
	(let ([ct ##sys#current-thread])
	  (##sys#setslot ct 1 (lambda () (oldhook reason state))) 
	  (##sys#schedule) ) )		; expected not to return!
      (oldhook reason state) ) ) )

(define ##sys#timeout-list '())

(define (##sys#thread-block-for-timeout! t tm)
  (dbg t " blocks for " tm)
  ;; This should really use a balanced tree:
  (let loop ([tl ##sys#timeout-list] [prev #f])
    (if (or (null? tl) (< tm (caar tl)))
	(if prev
	    (set-cdr! prev (cons (cons tm t) tl))
	    (set! ##sys#timeout-list (cons (cons tm t) tl)) )
	(loop (cdr tl) tl) ) ) 
  (##sys#setslot t 3 'blocked)
  (##sys#setislot t 4 tm) )

(define (##sys#thread-block-for-termination! t t2)
  (dbg t " blocks for " t2)
  (let ([state (##sys#slot t2 3)])
    (unless (or (eq? state 'dead) (eq? state 'terminated))
      (##sys#setslot t2 12 (cons t (##sys#slot t2 12)))
      (##sys#setslot t 3 'blocked) 
      (##sys#setslot t 11 t2) ) ) )

(define (##sys#thread-kill! t s)
  (dbg "killing: " t " -> " s ", recipients: " (##sys#slot t 12))
  (##sys#abandon-mutexes t)
  (##sys#setslot t 3 s)
  (##sys#setislot t 4 #f)
  (##sys#setislot t 11 #f)
  (##sys#setislot t 8 '())
  (let ([rs (##sys#slot t 12)])
    (unless (null? rs)
      (for-each
       (lambda (t2)
	 (dbg "  checking: " t2 " (" (##sys#slot t2 3) ") -> " (##sys#slot t2 11))
	 (when (eq? (##sys#slot t2 11) t)
	   (##sys#thread-basic-unblock! t2) ) )
       rs) ) )
  (##sys#setislot t 12 '()) )

(define (##sys#thread-basic-unblock! t)
  (dbg "unblocking: " t)
  (##sys#setislot t 11 #f)
  (##sys#setislot t 4 #f)
  (##sys#add-to-ready-queue t) )

(define ##sys#default-exception-handler
  (let ([print-error-message print-error-message]
	[display display]
	[print-call-chain print-call-chain]
	[open-output-string open-output-string]
	[get-output-string get-output-string] )
    (lambda (arg)
      (let ([ct ##sys#current-thread])
	(dbg "exception: " ct " -> " (if (##sys#structure? arg 'condition) (##sys#slot arg 2) arg))
	(cond [(foreign-value "C_abort_on_thread_exceptions" bool)
	       (let* ([pt ##sys#primordial-thread]
		      [ptx (##sys#slot pt 1)] )
		 (##sys#setslot 
		  pt 1 
		  (lambda ()
		    (##sys#signal arg)
		    (ptx) ) )
		 (##sys#thread-unblock! pt) ) ]
	      [##sys#warnings-enabled
	       (let ([o (open-output-string)])
		 (display "Warning (" o)
		 (display ct o)
		 (display "): " o)
		 (print-error-message arg ##sys#standard-error (get-output-string o))
		 (print-call-chain ##sys#standard-error 0 ct) ) ] )
	(##sys#setslot ct 7 arg)
	(##sys#thread-kill! ct 'terminated)
	(##sys#schedule) ) ) ) )


;;; `select()'-based blocking:

(define ##sys#fd-list '())

(define ##sys#fdset-select-timeout
  (foreign-lambda* int ([bool to] [unsigned-long tm])
    "struct timeval timeout;"
    "timeout.tv_sec = tm / 1000;"
    "timeout.tv_usec = tm % 1000;"
    "C_fdset_input_2 = C_fdset_input;"
    "C_fdset_output_2 = C_fdset_output;"
    "return(select(FD_SETSIZE, &C_fdset_input, &C_fdset_output, NULL, to ? &timeout : NULL));") )

(define ##sys#fdset-restore
  (foreign-lambda* void ()
    "C_fdset_input = C_fdset_input_2;"
    "C_fdset_output = C_fdset_output_2;") )

((foreign-lambda* void ()
   "FD_ZERO(&C_fdset_input);"
   "FD_ZERO(&C_fdset_output);") )

(define ##sys#fdset-input-set
  (foreign-lambda* void ([int fd])
    "FD_SET(fd, &C_fdset_input);" ) )

(define ##sys#fdset-output-set
  (foreign-lambda* void ([int fd])
    "FD_SET(fd, &C_fdset_output);" ) )

(define ##sys#fdset-clear
  (foreign-lambda* void ([int fd])
    "FD_CLR(fd, &C_fdset_input_2);"
    "FD_CLR(fd, &C_fdset_output_2);") )

(define (##sys#thread-block-for-i/o! t fd i/o)
  (dbg t " blocks for I/O " fd)
  (let loop ([lst ##sys#fd-list])
    (if (null? lst) 
	(set! ##sys#fd-list (cons (list fd t) ##sys#fd-list)) 
	(let ([a (car lst)])
	  (if (fx= fd (car a)) 
	      (##sys#setslot a 1 (cons t (cdr a)))
	      (loop (cdr lst)) ) ) ) )
  (if i/o
      (##sys#fdset-input-set fd)
      (##sys#fdset-output-set fd) )
  (##sys#setslot t 3 'blocked)
  (##sys#setslot t 11 (cons fd i/o)) )

(define (##sys#unblock-threads-for-i/o)
  (dbg "fd-list: " ##sys#fd-list)
  (let* ([to? (pair? ##sys#timeout-list)]
	 [rq? (pair? ##sys#ready-queue-head)]
	 [n (##sys#fdset-select-timeout	; we use FD_SETSIZE, but really should use max fd
	     (or rq? to?)
	     (if (and to? (not rq?))
		 (let* ([tmo1 (caar ##sys#timeout-list)]
			[now (##sys#fudge 16)])
		   (fxmax 0 (- tmo1 now)) )
		 0) ) ] )		; otherwise immediate timeout.
    (dbg n " fds ready")
    (cond [(eq? -1 n) (##sys#force-primordial)]
	  [(fx> n 0)
	   (set! ##sys#fd-list
	     (let loop ([n n] [lst ##sys#fd-list])
	       (if (zero? n)
		   lst
		   (let* ([a (car lst)]
			  [fd (car a)]
			  [inf (##core#inline "C_fd_test_input" fd)]
			  [outf (##core#inline "C_fd_test_output" fd)] )
		     (dbg "fd " fd " ready: input=" inf ", output=" outf)
		     (if (or inf outf)
			 (let loop2 ([threads (cdr a)])
			   (if (null? threads) 
			       (begin
				 (##sys#fdset-clear fd)
				 (loop (sub1 n) (cdr lst)) )
			       (let* ([t (car threads)]
				      [p (##sys#slot t 11)] )
				 (when (and (pair? p) (eq? fd (car p)))
				   (##sys#thread-basic-unblock! t) )
				 (loop2 (cdr threads)) ) ) )
			 (cons a (loop n (cdr lst))) ) ) ) ) ) ] )
    (##sys#fdset-restore) ) )


;;; Get list of all threads that are ready or waiting for timeout or waiting for I/O:

(define (##sys#all-threads)
  (append ##sys#ready-queue-head
          (apply append (map cdr ##sys#fd-list))
          (map cdr ##sys#timeout-list)))


;;; Remove all waiting threads from the relevant queues with the exception of the current thread:

(define (##sys#fetch-and-clear-threads)
  (let ([all (vector ##sys#ready-queue-head ##sys#ready-queue-tail ##sys#fd-list ##sys#timeout-list)])
    (set! ##sys#ready-queue-head '())
    (set! ##sys#ready-queue-tail '())
    (set! ##sys#fd-list '())
    (set! ##sys#timeout-list '()) 
    all) )


;;; Restore list of waiting threads:

(define (##sys#restore-threads vec)
  (set! ##sys#ready-queue-head (##sys#slot vec 0))
  (set! ##sys#ready-queue-tail (##sys#slot vec 1))
  (set! ##sys#fd-list (##sys#slot vec 2))
  (set! ##sys#timeout-list (##sys#slot vec 3)) )


;;; Unblock thread cleanly:

(define (##sys#thread-unblock! t)
  (when (eq? 'blocked (##sys#slot t 3))
    (set! ##sys#timeout-list (##sys#delq t ##sys#timeout-list))
    (set! ##sys#fd-list 
      (let loop ([fdl ##sys#fd-list])
	(if (null? fdl)
	    '()
	    (let ([a (##sys#slot fdl 0)])
	      (cons
	       (cons (##sys#slot a 0)
		     (##sys#delq t (##sys#slot a 1)) )
	       (loop (##sys#slot fdl 1)) ) ) ) ) )
    (##sys#setislot t 12 '())
    (##sys#thread-basic-unblock! t) ) )


;;; Multithreaded breakpoints

(define (##sys#break-entry name args)
  (when (or (not ##sys#break-in-thread) (eq? ##sys#break-in-thread ##sys#current-thread))
    (##sys#call-with-current-continuation
     (lambda (k)
       (let* ((pk (if (eq? ##sys#current-thread ##sys#primordial-thread)
		      '()
		      (list '(exn . thread) ##sys#current-thread
			    '(exn . primordial-continuation) 
			    (lambda _ ((##sys#slot ##sys#primordial-thread 1))))))
	      (exn (##sys#make-structure
		    'condition
		    '(exn breakpoint)
		    (append 
		     (list '(exn . message) "*** breakpoint ***"
			   '(exn . arguments) (cons name args)
			   '(exn . location) name
			   '(exn . continuation) k)
		     pk) ) ) )
	 (set! ##sys#last-breakpoint exn)
	 (cond ((eq? ##sys#current-thread ##sys#primordial-thread)
		(##sys#signal exn) )
	       (else
		(##sys#setslot ##sys#current-thread 3 'suspended)
		(##sys#setslot ##sys#current-thread 1 (lambda () (k (##core#undefined))))
		(let ([old (##sys#slot ##sys#primordial-thread 1)])
		  (##sys#setslot
		   ##sys#primordial-thread 1
		   (lambda ()
		     (##sys#signal exn)
		     (old) ) )
		  (##sys#thread-unblock! ##sys#primordial-thread)
		  (##sys#schedule) ) ) ) ) ) ) ) )
		  
(define (##sys#break-resume exn)
  ;; assumes current-thread is primordial
  (let* ((props (##sys#slot exn 2))
	 (a (member '(exn . continuation) props))
	 (t (member '(exn . thread) props))
	 (pk (or (member '(exn . primordial-continuation) props) a)))
    (when t
      (let ((t (cadr t)))
	(if a
	    (##sys#setslot t 1 (lambda () ((cadr a) (##core#undefined))))
	    (##sys#signal-hook #:type-error "condition has no continuation" exn) )
	(##sys#add-to-ready-queue t) ) )
    (if pk
	((cadr pk) (##core#undefined))
	(##sys#signal-hook #:type-error "condition has no continuation" exn) ) ) )
