;;;; chicken-update
;
; Copyright (c) 2008, The Chicken Team
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


(require-extension setup-download posix utils extras srfi-1)

(include "repository-config.scm")

(define (quit fstr . args)
  (apply fprintf (current-error-port) fstr args)
  ((abort-setup)))

(define (main args)
  (let loop ((hosts *repository-hosts*))
    (if (null? hosts)
	(quit "unable to download repository file~%")
	(let* ((host (car hosts))
	       (fname (make-pathname (third host) "REPOSITORY")))
	  (let-values (((ok loc) (download-data (first host) (second host) fname)))
	    (cond (ok (move-file loc (repository-path)))
		  (else
		   (fprintf 
		    (current-error-port) "response from ~a: ~a~%"
		    (first host) loc)
		   (loop (cdr hosts)))))))))

(handle-exceptions ex 
    (begin
      (print-error-message ex)
      (exit -1) )
  (main (command-line-arguments) ) )
