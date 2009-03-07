;;; ports.scm - Optional non-standard ports
;
; Copyright (c) 2000-2007, Felix L. Winkelmann
; Copyright (c) 2008-2009, The Chicken Team
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
;
;   Redistributions of source code must retain the above copyright
;   notice, this list of conditions and the following disclaimer.

;   Redistributions in binary form must reproduce the above copyright
;   notice, this list of conditions and the following disclaimer in
;   the documentation and/or other materials provided with the
;   distribution.

;   Neither the name of the author nor the names of its contributors
;   may be used to endorse or promote products derived from this
;   software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
; COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGE.


(declare
 (unit ports)
; (uses data-structures)
 (usual-integrations)
 (disable-warning redef) )

(cond-expand
 [paranoia]
 [else
  (declare
    (no-bound-checks)
    (no-procedure-checks-for-usual-bindings)
    (bound-to-procedure
      ##sys#check-char ##sys#check-exact ##sys#check-port ##sys#check-string
      ##sys#substring ##sys#for-each ##sys#map ##sys#setslot
      ##sys#allocate-vector ##sys#check-pair ##sys#error-not-a-proper-list
      ##sys#member ##sys#assoc ##sys#error ##sys#signal-hook ##sys#read-string!
      ##sys#check-symbol ##sys#check-vector ##sys#floor ##sys#ceiling
      ##sys#truncate ##sys#round ##sys#check-number ##sys#cons-flonum
      ##sys#flonum-fraction ##sys#make-port ##sys#fetch-and-check-port-arg
      ##sys#print ##sys#check-structure ##sys#make-structure make-parameter
      ##sys#flush-output ##sys#write-char-0 ##sys#number->string
      ##sys#fragments->string ##sys#symbol->qualified-string
      ##extras#reverse-string-append ##sys#number? ##sys#procedure->string
      ##sys#pointer->string ##sys#user-print-hook ##sys#peek-char-0
      ##sys#read-char-0 ##sys#write-char ##sys#string-append ##sys#gcd ##sys#lcm
      ##sys#fudge ##sys#check-list ##sys#user-read-hook ##sys#check-closure ##sys#check-inexact
      input-port? make-vector list->vector  open-output-string floor 
      get-output-string current-output-port display write port? list->string
      call-with-input-string with-input-from-string
      make-string string newline char-name read 
      open-input-string call-with-input-file reverse ) ) ] )

(include "unsafe-declarations.scm")

(register-feature! 'ports)


;;;; Port-mapping (found in Gauche):

(define (port-for-each fn thunk)
  (let loop ()
    (let ((x (thunk)))
      (unless (eof-object? x)
	(fn x)
	(loop) ) ) ) )

(define port-map
  (let ((reverse reverse))
    (lambda (fn thunk)
      (let loop ((xs '()))
	(let ((x (thunk)))
	  (if (eof-object? x)
	      (reverse xs)
	      (loop (cons (fn x) xs))))))))

(define (port-fold fn acc thunk)
  (let loop ([acc acc])
    (let ([x (thunk)])
      (if (eof-object? x)
          acc
          (loop (fn x acc))) ) ) )

;;;; funky-ports

(define (make-broadcast-port . ports)
  (make-output-port
   (lambda (s) (for-each (cut write-string s #f <>) ports))
   noop
   (lambda () (for-each flush-output ports)) ) )

(define (make-concatenated-port p1 . ports)
  (let ((ports (cons p1 ports)))
    (make-input-port
     (lambda ()
       (let loop ()
	 (if (null? ports)
	     #!eof
	     (let ((c (read-char (car ports))))
	       (cond ((eof-object? c)
		      (set! ports (cdr ports))
		      (loop) )
		     (else c) ) ) ) ) )
     (lambda ()
       (and (not (null? ports))
	    (char-ready? (car ports))))
     noop
     (lambda ()
       (let loop ()
	 (if (null? ports)
	     #!eof
	     (let ((c (peek-char (car ports))))
	       (cond ((eof-object? c)
		      (set! ports (cdr ports))
		      (loop) )
		     (else c))))))
     (lambda (p n dest start)
       (let loop ((n n) (c 0))
	 (cond ((null? ports) c)
	       ((fx<= n 0) c)
	       (else
		(let ((m (read-string! n dest (car ports) (fx+ start c))))
		  (when (fx< m n)
		    (set! ports (cdr ports)) )
		  (loop (fx- n m) (fx+ c m))))))))))


;;; Redirect standard ports:

(define (with-input-from-port port thunk)
  (##sys#check-port port 'with-input-from-port)
  (fluid-let ([##sys#standard-input port])
    (thunk) ) )

(define (with-output-to-port port thunk)
  (##sys#check-port port 'with-output-from-port)
  (fluid-let ([##sys#standard-output port])
    (thunk) ) )

(define (with-error-output-to-port port thunk)
  (##sys#check-port port 'with-error-output-from-port)
  (fluid-let ([##sys#standard-error port])
    (thunk) ) )

;;; Extended string-port operations:
  
(define call-with-input-string 
  (let ([open-input-string open-input-string])
    (lambda (str proc)
      (let ((in (open-input-string str)))
	(proc in) ) ) ) )

(define call-with-output-string
  (let ((open-output-string open-output-string)
	(get-output-string get-output-string) )
    (lambda (proc)
      (let ((out (open-output-string)))
	(proc out)
	(get-output-string out) ) ) ) )

(define with-input-from-string
  (let ((open-input-string open-input-string))
    (lambda (str thunk)
      (fluid-let ([##sys#standard-input (open-input-string str)])
	(thunk) ) ) ) )

(define with-output-to-string
  (let ([open-output-string open-output-string]
	[get-output-string get-output-string] )
    (lambda (thunk)
      (fluid-let ([##sys#standard-output (open-output-string)])
	(thunk) 
	(get-output-string ##sys#standard-output) ) ) ) )


;;; Custom ports:
;
; - Port-slots:
;
;   10: last

(define make-input-port
  (lambda (read ready? close #!optional peek read-string read-line)
    (let* ((class
	    (vector 
	     (lambda (p)		; read-char
	       (let ([last (##sys#slot p 10)])
		 (cond [peek (read)]
		       [last
			(##sys#setislot p 10 #f)
			last]
		       [else (read)] ) ) )
	     (lambda (p)		; peek-char
	       (let ([last (##sys#slot p 10)])
		 (cond [peek (peek)]
		       [last last]
		       [else
			(let ([last (read)])
			  (##sys#setslot p 10 last)
			  last) ] ) ) )
	     #f				; write-char
	     #f				; write-string
	     (lambda (p)		; close
	       (close)
	       (##sys#setislot p 8 #t) )
	     #f				; flush-output
	     (lambda (p)		; char-ready?
	       (ready?) )
	     read-string		; read-string!
	     read-line) )		; read-line
	   (data (vector #f))
	   (port (##sys#make-port #t class "(custom)" 'custom)) )
      (##sys#setslot port 9 data) 
      port) ) )

(define make-output-port
  (let ([string string])
    (lambda (write close #!optional flush)
      (let* ((class
	      (vector
	       #f			; read-char
	       #f			; peek-char
	       (lambda (p c)		; write-char
		 (write (string c)) )
	       (lambda (p s)		; write-string
		 (write s) )
	       (lambda (p)		; close
		 (close)
		 (##sys#setislot p 8 #t) )
	       (lambda (p)		; flush-output
		 (when flush (flush)) )
	       #f			; char-ready?
	       #f			; read-string!
	       #f) )			; read-line
	     (data (vector #f))
	     (port (##sys#make-port #f class "(custom)" 'custom)) )
	(##sys#setslot port 9 data) 
	port) ) ) )
