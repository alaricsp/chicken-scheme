;;;; chicken-thread-object-primitive-inlines.scm
;;;; Kon Lovett, Jan '09

; Usage
;
; (include "chicken-primitive-object-inlines")
; (include "chicken-thread-object-inlines")

;; Notes
;
; Provides inlines & macros for thread objects. Use of these procedures
; by non-core & non-core-extensions is highly suspect. Many of these routines
; are unsafe.
;
; In fact, any use is suspect ;-)


;;; Mutex object helpers:

;; Mutex layout:
;
; 0     Tag - 'mutex
; 1     Name (object)
; 2     Thread (thread or #f)
; 3     Waiting threads (FIFO list)
; 4     Abandoned? (boolean)
; 5     Locked? (boolean)
; 6     Specific (object)

(define-inline (%mutex? x)
  (%structure-instance? x 'mutex) )

(define-inline (%mutex-name mx)
  (%structure-ref mx 1) )

(define-inline (%mutex-thread mx)
  (%structure-ref mx 2) )

(define-inline (%mutex-thread-set! mx th)
  (%structure-set!/mutate mx 2 th) )

(define-inline (%mutex-thread-clear! mx)
  (%structure-set!/immediate mx 2 #f) )

(define-inline (%mutex-waiters mx)
  (%structure-ref mx 3) )

(define-inline (%mutex-waiters-set! mx wt)
  (%structure-set!/mutate mx 3 wt) )

(define-inline (%mutex-waiters-empty? mx)
  (%null? (%mutex-waiters mx)) )

(define-inline (%mutex-waiters-empty! mx)
  (%structure-set!/immediate mx 3 '()) )

(define-inline (%mutex-waiters-add! mx th)
  (%mutex-waiters-set! mx (%append! (%mutex-waiters mx) (%cons th '()))) )

(define-inline (%mutex-waiters-delete! mx th)
  (%mutex-waiters-set! mx (%delq! th (%mutex-waiters mx))) )

(define-inline (%mutex-waiters-pop! mx)
  (let* ([wt (%mutex-waiters mx)]
         [top (%car wt)])
    (%mutex-waiters-set! mx (%cdr wt))
    top ) )

(define-inline (%mutex-abandoned? mx)
  (%structure-ref mx 4) )

(define-inline (%mutex-abandoned-set! mx f)
  (%structure-set!/immediate mx 4 f) )

(define-inline (%mutex-locked? mx)
  (%structure-ref mx 5) )

(define-inline (%mutex-locked-set! mx f)
  (%structure-set!/immediate mx 5 f) )

(define-inline (%mutex-specific mx)
  (%structure-ref mx 6) )

(define-inline (%mutex-specific-set! mx x)
  (%structure-set!/mutate mx 6 x) )


;;; Thread object helpers:

;; Thread layout:
;
; 0     Tag - 'thread
; 1     Thunk (procedure)
; 2     Results (list-of object)
; 3     State (symbol)
; 4     Block-timeout (fixnum or #f)
; 5     State buffer (vector)
;       0       Dynamic winds (list)
;       1       Standard input (port)
;       2       Standard output (port)
;       3       Standard error (port)
;       4       Exception handler (procedure)
;       5       Parameters (vector)
; 6     Name (object)
; 7     Reason (condition of #f)
; 8     Mutexes (list-of mutex)
; 9     Quantum (fixnum)
; 10    Specific (object)
; 11    Block object (thread or (pair-of fd io-mode))
; 12    Recipients (list-of thread)
; 13    Unblocked by timeout? (boolean)

(define-inline (%thread? x)
  (%structure-instance? x 'thread) )

(define-inline (%thread-thunk th)
  (%structure-ref th 1) )

(define-inline (%thread-thunk-set! th tk)
  (%structure-set!/mutate th 1 tk) )

(define-inline (%thread-results th)
  (%structure-ref th 2) )

(define-inline (%thread-results-set! th rs)
  (%structure-set!/mutate th 2 rs) )

(define-inline (%thread-state th)
  (%structure-ref th 3) )

(define-inline (%thread-state-set! th st)
  (%structure-set!/mutate th 3 st) )

(define-inline (%thread-block-timeout th)
  (%structure-ref th 4) )

(define-inline (%thread-block-timeout-set! th to)
  (%structure-set!/immediate th 4 to) )

(define-inline (%thread-block-timeout-clear! th)
  (%thread-block-timeout-set! th #f) )

(define-inline (%thread-state-buffer th)
  (%structure-ref th 5) )

(define-inline (%thread-state-buffer-set! th v)
  (%structure-set!/mutate th 5 v) )

(define-inline (%thread-name th)
  (%structure-ref th 6) )

(define-inline (%thread-reason th)
  (%structure-ref th 7) )

(define-inline (%thread-reason-set! th cd)
  (%structure-set!/mutate th 7 cd) )

(define-inline (%thread-mutexes th)
  (%structure-ref th 8) )

(define-inline (%thread-mutexes-set! th wt)
  (%structure-set!/mutate th 8 wx) )

(define-inline (%thread-mutexes-empty? th)
  (%null? (%thread-mutexes th)) )

(define-inline (%thread-mutexes-empty! th)
  (%structure-set!/immediate th 8 '()) )

(define-inline (%thread-mutexes-add! th mx)
  (%thread-mutexes-set! th (%cons mx (%thread-mutexes th))) )

(define-inline (%thread-mutexes-delete! th mx)
  (%thread-mutexes-set! th (%delq! mx (%thread-mutexes th))) )

(define-inline (%thread-quantum th)
  (%structure-ref th 9) )

(define-inline (%thread-quantum-set! th qt)
  (%structure-set!/immediate th 9 qt) )

(define-inline (%thread-specific th)
  (%structure-ref th 10) )

(define-inline (%thread-specific-set! th x)
  (%structure-set!/mutate th 10 x) )

(define-inline (%thread-block-object th)
  (%structure-ref th 11) )

(define-inline (%thread-block-object-set! th x)
  (%structure-set!/mutate th 11 x) )

(define-inline (%thread-block-object-clear! th)
  (%structure-set!/immediate th 11 #f) )

(define-inline (%thread-recipients th)
  (%structure-ref th 12) )

(define-inline (%thread-recipients-set! th x)
  (%structure-set!/mutate th 12 x) )

(define-inline (%thread-recipients-empty? th)
  (%null? (%condition-variable-waiters th)) )

(define-inline (%thread-recipients-empty! th)
  (%structure-set!/immediate th 12 '()) )

(define-inline (%thread-recipients-add! th rth)
  (%thread-recipients-set! t (%cons rth (%thread-recipients t))) )

(define-inline (%thread-recipients-process! th tk)
  (let ([rs (%thread-recipients t)])
    (unless (%null? rs) (for-each tk rs) ) )
  (%thread-recipients-empty! t) )

(define-inline (%thread-unblocked-by-timeout? th)
  (%structure-ref th 13) )

(define-inline (%thread-unblocked-by-timeout-set! th f)
  (%structure-set!/immediate th 13 f) )

(define-inline (%thread-blocked-for-timeout? th)
  (and (%thread-block-timeout th)
       (not (%thread-block-object th))) )

(define-inline (%thread-blocked? th)
  (%eq? 'blocked (%thread-state th)) )

(define-inline (%thread-created? th)
  (%eq? 'created (%thread-state th)) )

(define-inline (%thread-ready? th)
  (%eq? 'ready (%thread-state th)) )

(define-inline (%thread-sleeping? th)
  (%eq? 'sleeping (%thread-state th)) )

(define-inline (%thread-suspended? th)
  (%eq? 'suspended (%thread-state th)) )

(define-inline (%thread-terminated? th)
  (%eq? 'terminated (%thread-state th)) )

(define-inline (%thread-dead? th)
  (%eq? 'dead (%thread-state th)) )

;; Synonyms

(define-inline (%current-thread)
  ##sys#current-thread )


;;; Condition-variable object:

;; Condition-variable layout:
;
; 0     Tag - 'condition-variable
; 1     Name (object)
; 2     Waiting threads (FIFO list)
; 3     Specific (object)

(define-inline (%condition-variable? x)
  (%structure-instance? x 'condition-variable) )

(define-inline (%condition-variable-name cv)
  (%structure-ref cv 1) )

(define-inline (%condition-variable-waiters cv)
  (%structure-ref cv 2) )

(define-inline (%condition-variable-waiters-set! cv x)
  (%structure-set!/mutate cv 2 x) )

(define-inline (%condition-variable-waiters-empty? cv)
  (%null? (%condition-variable-waiters cv)) )

(define-inline (%condition-variable-waiters-empty! cv)
  (%structure-set!/immediate cv 2 '()) )

(define-inline (%condition-variable-waiters-add! cv th)
  (%condition-variable-waiters-set! cv (%append! (%condition-variable-waiters cv) (%cons th '()))) )

(define-inline (%condition-variable-waiters-delete! cv th)
  (%condition-variable-waiters-set! cv (%delq! th (%condition-variable-waiters cv))) )

(define-inline (%condition-variable-waiters-pop! mx)
  (let* ([wt (%condition-variable-waiters mx)]
         [top (%car wt)])
    (%condition-variable-waiters-set! mx (%cdr wt))
    top ) )

(define-inline (%condition-variable-specific cv)
  (%structure-ref cv 3) )

(define-inline (%condition-variable-specific-set! cv x)
  (%structure-set!/mutate cv 3 x) )
