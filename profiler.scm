;;;; profiler.scm - Support code for profiling applications
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
  (unit profiler)
  (usual-integrations)
  (hide ##sys#profile-name ##sys#profile-vector-list)
  (disable-interrupts)
  (fixnum-arithmetic) )

(cond-expand
 [paranoia]
 [else
  (declare
    (bound-to-procedure
     write-char write make-vector)
    (no-bound-checks) ) ] )


(define-constant profile-info-entry-size 5)


;;; Globals:

(define ##sys#profile-vector-list '())
(define ##sys#profile-name #f)
(define ##sys#profile-append-mode #f)


;;; Initialize profile counter vector:

(define ##sys#register-profile-info
  (let ([make-vector make-vector])
    (lambda (size filename)
      (when filename
	(set! ##sys#profile-name filename)
	(let ([oldeh (##sys#exit-handler)]
	      [oldieh (##sys#implicit-exit-handler)] )
	  (##sys#exit-handler
	   (lambda args
	     (##sys#finish-profile)
	     (apply oldeh args) ) )
	  (##sys#implicit-exit-handler
	   (lambda ()
	     (##sys#finish-profile)
	     (oldieh) ) ) ) )
      ;; entry: [name, count, time0, total, pending]
      (let ([vec (make-vector (* size profile-info-entry-size) 0)])
	(set! ##sys#profile-vector-list (cons vec ##sys#profile-vector-list))
	vec) ) ) )

(define (##sys#set-profile-info-vector! vec i x)
  (##sys#setslot vec (* i profile-info-entry-size) x) )


;;; Entry and exit into/out of profiled lambda:

(define (##sys#profile-entry index vec)
  (let* ([i (* index profile-info-entry-size)]
	 [ic (add1 i)]
	 [it0 (+ i 2)] 
	 [ip (+ i 4)] 
	 [ipc (##sys#slot vec ip)] )
    (##sys#setislot vec ic (add1 (##sys#slot vec ic)))
    (when (zero? ipc)
      (##sys#setislot vec it0 (##sys#fudge 6)) )
    (##sys#setislot vec ip (add1 ipc)) ) )

(define (##sys#profile-exit index vec)
  (let* ([i (* index profile-info-entry-size)]
	 [it0 (+ i 2)] 
	 [it (+ i 3)] 
	 [ip (+ i 4)] 
	 [ipc (sub1 (##sys#slot vec ip))] )
    (##sys#setislot vec ip ipc)
    (when (zero? ipc)
      (##sys#setislot vec it (+ (##sys#slot vec it) (- (##sys#fudge 6) (##sys#slot vec it0))))
      (##sys#setislot vec it0 0) ) ) )


;;; Generate profile:

(define ##sys#finish-profile 
  (let ([with-output-to-file with-output-to-file]
	[write-char write-char]
	[write write] )
    (lambda ()
      (apply
       with-output-to-file ##sys#profile-name
       (lambda () 
	 (for-each
	  (lambda (vec)
	    (let ([len (##sys#size vec)])
	      (do ([i 0 (+ i profile-info-entry-size)])
		  ((>= i len))
		(write-char #\()
		(write (##sys#slot vec i))
		(write-char #\space)
		(write (##sys#slot vec (add1 i)))
		(write-char #\space)
		(write (##sys#slot vec (+ i 3)))
		(write-char #\))
		(write-char #\newline) ) ) ) 
	  ##sys#profile-vector-list) )
       (if ##sys#profile-append-mode
	   '(append:)
	   '() ) ) ) ) )
