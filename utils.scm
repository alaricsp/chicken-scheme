;;;; utils.scm - Utilities for scripting and file stuff
;
; Copyright (c) 2000-2007, Felix L. Winkelmann
; Copyright (c) 2008-2009, The Chicken Team
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


(declare
  (unit utils)
  (uses extras srfi-13 posix files)
  (usual-integrations)
  (fixnum)
  (hide chop-pds)
  (disable-interrupts) )

(cond-expand
 [paranoia]
 [else
  (declare
    (always-bound
      ##sys#windows-platform)
    (bound-to-procedure
      ##sys#check-port port? read-string for-each-line read-line with-input-from-file
      command-line-arguments
      string-append
      system)
    (no-procedure-checks-for-usual-bindings)
    (no-bound-checks))] )

(include "unsafe-declarations.scm")

(register-feature! 'utils)


;;; Like `system', but allows format-string and bombs on nonzero return code:

(define system*
  (let ([sprintf sprintf]
	[system system] )
    (lambda (fstr . args)
      (let* ([str (apply sprintf fstr args)]
	     [n (system str)] )
	(unless (zero? n)
	  (##sys#error "shell invocation failed with non-zero return status" str n) ) ) ) ) )


;;; Handy I/O procedures:

(define for-each-line
  (let ([read-line read-line])
    (lambda (proc . port)
      (let ([port (if (pair? port) (car port) ##sys#standard-input)])
	(##sys#check-port port 'for-each-line)
	(let loop ()
	  (let ([ln (read-line port)])
	    (unless (eof-object? ln)
	      (proc ln)
	      (loop) ) ) ) ) ) ) )


;; This one is from William Annis:

(define (for-each-argv-line thunk)
  (define (file-iterator file thunk)
    (if (string=? file "-")
        (for-each-line thunk)
        (with-input-from-file file (cut for-each-line thunk) ) ) )
  (let ((args (command-line-arguments)))
    (if (null? args)
        ;; If no arguments, take from stdin,
        (for-each-line thunk)
        ;; otherwise, hit each file named in argv.
        (for-each (lambda (arg) (file-iterator arg thunk)) args))))


;;; Read file as string from given filename or port:

(define (read-all . file)
  (let ([file (optional file ##sys#standard-input)])
    (if (port? file)
	(read-string #f file)
	(with-input-from-file file (cut read-string #f)) ) ) )


;;; Quote string for shell

(define (qs str #!optional (platform (build-platform)))
  (case platform
    ((mingw32 msvc)
     (string-append "\"" str "\""))
    (else
     (if (zero? (string-length str))
	 "''"
	 (string-concatenate
	  (map (lambda (c)
		 (if (or (char-whitespace? c)
			 (memq c '(#\# #\" #\' #\` #\´ #\~ #\& #\% #\$ #\! #\* #\; #\< #\> #\\
				   #\( #\) #\[ #\] #\{ #\})))
		     (string #\\ c)
		     (string c)))
	       (string->list str)))))))


;;; Compile and load file

(define compile-file-options (make-parameter '("-S" "-O2" "-d2")))

(define compile-file
  (let ((csc (foreign-value "C_CSC_PROGRAM" c-string))
	(path (foreign-value "C_INSTALL_BIN_HOME" c-string)) )
    (lambda (filename #!key (options '()) output-file)
      (let ((cscpath (or (file-exists? (make-pathname path csc)) "csc"))
	    (tmpfile (and (not output-file) (create-temporary-file "so"))))
	(print "; compiling " filename " ...")
	(system* 
	 "~a -s ~a ~a -o ~a" 
	 (qs cscpath)
	 (string-intersperse (append (compile-file-options) options) " ")
	 (qs filename)
	 (qs (or output-file tmpfile)))
	(unless output-file 
	  (on-exit (cut delete-file* tmpfile)))
	(let ((f (or output-file tmpfile)))
	  (handle-exceptions ex
	      (begin
		(delete-file* f)
		(abort ex))
	    (load f)
	    f))))))
