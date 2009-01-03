;;;; utils.scm - Utilities for scripting and file stuff
;
; Copyright (c) 2000-2007, Felix L. Winkelmann
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


(declare
  (unit utils)
  (uses regex data-structures extras files srfi-13)
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
      string-search string-match regexp regexp-escape
      ##sys#symbol-has-toplevel-binding? ##sys#environment-symbols
      ##sys#hash-table-for-each ##sys#macro-environment
      ##sys#string-append reverse port? read-string with-input-from-file command-line-arguments
      for-each-line ##sys#check-port read-line getenv make-pathname file-exists? call-with-output-file
      decompose-pathname absolute-pathname? string-append ##sys#substring
      delete-file system)
    (no-procedure-checks-for-usual-bindings)
    (no-bound-checks))] )

(include "unsafe-declarations.scm")

(register-feature! 'utils)


;;; Environment utilities

(define ##sys#apropos-interned)
(define ##sys#apropos-macros)
(let ([string-search string-search]
      [regexp regexp]
      [regexp-escape regexp-escape])
  (let ([makpat
         (lambda (patt)
           (when (symbol? patt)
             (set! patt (symbol->string patt)))
           (when (string? patt)
             (set! patt (regexp (regexp-escape patt))))
           patt)])

    (set! ##sys#apropos-interned
      (lambda (patt env)
        (set! patt (makpat patt))
        (##sys#environment-symbols env
          (lambda (sym)
            (and (string-search patt (symbol->string sym))
	         (##sys#symbol-has-toplevel-binding? sym) ) ) ) ) )

    (set! ##sys#apropos-macros
      (lambda (patt env) ; env is currently ignored
        (set! patt (makpat patt))
        (let ([ms '()])
          (for-each
	   (lambda (a)
	     (let ((key (car a)))
	       (when (string-search patt (symbol->string key))
		 (set! ms (cons key ms)) ) ) )
	   (##sys#macro-environment))
          ms ) ) ) ) )

(define (##sys#apropos patt env #!optional macf)
  (let ([ts (##sys#apropos-interned patt env)])
    (if macf
        (##sys#append ts (##sys#apropos-macros patt env))
        ts ) ) )

(define apropos-list)
(define apropos)
(let ([%apropos-list
        (lambda (loc patt args) ; #!optional (env (interaction-environment)) #!key macros?
          (let ([env (interaction-environment)]
                [macros? #f])
            ; Handle extended lambda list optional & rest w/ keywords
            (let loop ([args args])
              (when (pair? args)
                (let ([arg (car args)])
                  (if (eq? #:macros? arg)
                      (begin
                        (set! macros? (cadr args))
                        (loop (cddr args)) )
                      (begin
                        (set! env arg)
                        (loop (cdr args)) ) ) ) ) )
	    (##sys#check-structure env 'environment loc)
            (unless (or (string? patt) (symbol? patt) (regexp? patt))
              (##sys#signal-hook #:type-error loc "bad argument type - not a string, symbol, or regexp" patt))
            (##sys#apropos patt env macros?) ) )]
      [disp-proc
        (lambda (proc labl)
          (let ([info (procedure-information proc)])
            (cond [(pair? info) (display (cons labl (cdr info)))]
		  [info         (display labl)]
		  [else         (display labl) ] ) ) ) ]
      [symlen
        (lambda (sym)
          (let ([len (##sys#size (##sys#symbol->qualified-string sym))])
            (if (keyword? sym)
                (fx- len 2) ; compensate for leading '###' when only a ':' is printed
                len ) ) )])

  (set! apropos-list
    (lambda (patt . rest)
      (%apropos-list 'apropos-list patt rest)))

  (set! apropos
    (lambda (patt . rest)
      (let ([ss (%apropos-list 'apropos patt rest)]
            [maxlen 0])
        (for-each
          (lambda (sym)
            (set! maxlen (fxmax maxlen (symlen sym))))
          ss)
        (for-each
          (lambda (sym)
            (display sym)
            (do ([i (fx- maxlen (symlen sym)) (fx- i 1)])
                [(fx<= i 0)]
              (display #\space))
            (display #\space) (display #\:) (display #\space)
            (if (macro? sym)
                ;FIXME want to display macro lambda arguments
                (display 'macro)
                (let ([bnd (##core#inline "C_retrieve" sym)])
                  (cond [(procedure? bnd)
                          (disp-proc bnd 'procedure)]
                        [else
                          (display 'variable)]) ) )
            (newline) )
          ss)))) )


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
