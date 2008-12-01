;;;; setup-utils.scm
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


(require-library regex utils ports tcp extras posix files
		 srfi-1 srfi-13 data-structures)


(module setup-utils (version>=?
		     create-temporary-directory
		     remove-directory
		     yes-or-no?
		     get-terminal-width
		     format-string
		     remove-file
		     remove-extension
		     read-info)
  
  (import scheme chicken)
  (import regex utils ports tcp extras posix srfi-1 srfi-13 
	  data-structures files)

  (define (version>=? v1 v2)
    (define (version->list v)
      (map (lambda (x) (or (string->number x) x))
	   (string-split-fields "[-\\._]" v #:infix)))
    (let loop ((p1 (version->list v1))
	       (p2 (version->list v2)))
      (cond ((null? p1) (null? p2))
	    ((null? p2))
	    ((number? (car p1))
	     (and (if (number? (car p2))
		      (>= (car p1) (car p2))
		      (string>=? (number->string (car p1)) (car p2)))
		  (loop (cdr p1) (cdr p2))))
	    ((number? (car p2))
	     (and (string>=? (car p1) (number->string (car p2)))
		  (loop (cdr p1) (cdr p2))))
	    ((string>=? (car p1) (car p2)) (loop (cdr p1) (cdr p2)))
	    (else #f))))

  (define (format-string str cols #!optional right (padc #\space))
    (let* ((len (string-length str))
	   (pad (make-string (fxmax 0 (fx- cols len)) padc)) )
      (if right
	  (string-append pad str)
	  (string-append str pad) ) ) )

  (define get-terminal-width
    (let ((default-width 80))	     ; Standard default terminal width
      (lambda ()
	(let ((cop (current-output-port)))
	  (if (terminal-port? cop)
	      (let ((w (nth-value 1 (terminal-size cop))))
		(if (zero? w) default-width w))
	      default-width)))))

  (define (read-info egg)
    (with-input-from-file 
     (make-pathname (repository-path) egg ".setup-info")
     read))

  (define (create-temporary-directory)
    (let ((dir (or (getenv "TMPDIR") (getenv "TEMP") (getenv "TMP") "/tmp")))
      (let loop ()
	(let* ((n (##sys#fudge 16))	; current milliseconds
	       (pn (make-pathname dir (string-append "setup-" (number->string n 16)) "tmp")))
	  (cond ((file-exists? pn) (loop))
		(else (create-directory pn) pn))))))

  (define (remove-directory dir #!optional sudo)
    (if sudo
	(system* "sudo rm -fr '~a'" dir)
	(let walk ((dir dir))
	  (let ((files (directory dir #t)))
	    (for-each
	     (lambda (f)
	       (unless (or (string=? "." f) (string=? ".." f))
		 (let ((p (make-pathname dir f)))
		   (if (directory? p)
		       (walk p) 
		       (delete-file p)))))
	     files)
	    (delete-directory dir)))) )

  (define (yes-or-no? str #!key default (abort (cut signal 'aborted)))
    (let loop ()
      (printf "~%~A (yes/no/abort) " str)
      (when default (printf "[~A] " default))
      (flush-output)
      (let ((ln (read-line)))
	(cond ((eof-object? ln) (set! ln "abort"))
	      ((and default (string=? "" ln)) (set! ln default)) )
	(cond ((string-ci=? "yes" ln) #t)
	      ((string-ci=? "no" ln) #f)
	      ((string-ci=? "abort" ln) (abort))
	      (else
	       (printf "~%Please enter \"yes\", \"no\" or \"abort\".~%")
	       (loop) ) ) ) ) )
  
  (define (remove-extension egg #!optional sudo)
    (and-let* ((files (assq 'files (read-info egg))))
      (for-each (cut remove-file <> sudo) (cdr files)))
    (remove-file (make-pathname (repository-path) egg "setup-info") sudo))

  (define (remove-file path #!optional sudo)
    (if sudo
	(system* "sudo rm -f '~a'" path)
	(delete-file* path)))

)
