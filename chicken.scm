;;;; chicken.scm - The CHICKEN Scheme compiler (loader/main-module)
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
  (uses chicken-syntax srfi-1 srfi-4 utils files support compiler optimizer scrutinizer driver 
	platform backend srfi-69)
  (compile-syntax) )			


(include "compiler-namespace")
(include "tweaks")

(eval-when (load) 
  (include "chicken-ffi-syntax") )


;;; Prefix argument list with default options:

(define compiler-arguments
  (append
   (remove 
    (lambda (x) (string=? x ""))
    (string-split (or (get-environment-variable "CHICKEN_OPTIONS") "")))
   (cdr (argv))))


;;; Process command-line options:
; 
; - remove runtime-options ("-:...")
; - filter out source-filename
; - convert options into symbols (without the initial hyphens)

(define (process-command-line args)
  (let loop ([args args] [options '()] [filename #f])
    (if (null? args)
	(values filename (reverse options))
	(let* ([arg (car args)]
	       [len (string-length arg)]
	       [char0 (string-ref arg 0)] )
	  (if (and (char=? #\- char0) (> len 1))
	      (if (and (> len 1) (char=? #\: (string-ref arg 1)))
		  (loop (cdr args) options filename)
		  (loop (cdr args) (cons (string->symbol (substring arg 1 len)) options) filename) )
	      (if filename
		  (loop (cdr args) (cons arg options) filename)
		  (loop (cdr args) options arg) ) ) ) ) ) )


;;; Run compiler with command-line options:

(receive (filename options) ((or (user-options-pass) process-command-line) compiler-arguments)
  (let loop ([os options])
    (unless (null? os)
      (let ([o (car os)]
	    [rest (cdr os)] )
	(cond [(eq? 'optimize-level o)
	       (let ([level (string->number (car rest))])
		 (case level
		   [(0) #f]
		   [(1)
		    (set! options (cons 'optimize-leaf-routines options)) ]
		   [(2)
		    (set! options (cons 'optimize-leaf-routines options)) ] 
		   [(3)
		    (set! options
		      (cons* 'optimize-leaf-routines 'local 'inline options) ) ]
		   [(4)
		    (set! options
		      (cons* 'optimize-leaf-routines 'local 'inline 'unsafe options) ) ]
		   [else (compiler-warning 'usage "invalid optimization level ~S - ignored" (car rest))] )
		 (loop (cdr rest)) ) ]
	      [(eq? 'debug-level o)
	       (let ([level (string->number (car rest))])
		 (case level
		   [(0) (set! options (cons* 'no-lambda-info 'no-trace options))]
		   [(1) (set! options (cons 'no-trace options))]
		   [(2) #f]
		   [else (compiler-warning 'usage "invalid debug level ~S - ignored" (car rest))] )
		 (loop (cdr rest)) ) ]
	      [(eq? 'benchmark-mode o)
	       (set! options 
		 (cons* 'fixnum-arithmetic 'disable-interrupts 'no-trace 'unsafe
			'optimize-leaf-routines 'block 'lambda-lift 'no-lambda-info
			'inline
			options) )
	       (loop rest) ]
	      [(memq o valid-compiler-options) (loop rest)]
	      [(memq o valid-compiler-options-with-argument)
	       (if (pair? rest)
		   (loop (cdr rest))
		   (quit "missing argument to `-~s' option" o) ) ]
	      [else
	       (compiler-warning 
		'usage "invalid compiler option `~a' - ignored" 
		(if (string? o) o (conc "-" o)) )
	       (loop rest) ] ) ) ) )
  (apply compile-source-file filename options)
  (exit) )
