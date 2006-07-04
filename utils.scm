;;;; utils.scm - Utilities for scripting and file stuff
;
; Copyright (c) 2000-2006, Felix L. Winkelmann
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
  (unit utils)
  (uses regex extras)
  (usual-integrations)
  (fixnum)
  (hide chop-pds)
  (disable-interrupts) )


(cond-expand
 [paranoia]
 [else
  (declare
    (no-procedure-checks-for-usual-bindings)
    (bound-to-procedure
      ##sys#string-append reverse port? read-string with-input-from-file command-line-arguments
      for-each-line ##sys#check-port read-line getenv make-pathname file-exists? call-with-output-file
      decompose-pathname string-search absolute-pathname? string-append ##sys#substring string-match
      delete-file system)
    (no-bound-checks))] )

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
  (declare (emit-exports "utils.exports"))] )

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


;;; Like `delete-file', but does nothing if the file doesn't exist:

(define delete-file*
  (let ([file-exists? file-exists?]
	[delete-file delete-file] )
    (lambda (file)
      (and (file-exists? file) (delete-file file) #t) ) ) )


;;; Pathname operations:

(define absolute-pathname?
  (let* ([dr
	  (let ([st (software-type)])
	    (if (and (eq? 'windows st)
		     ;; Still windows even if 'Linux-like'
		     (not (eq? 'cygwin (build-platform))) )
		"([A-Za-z]:)?"
		"") ) ]
	 [rx (string-append dr "[\\/\\" (string ##sys#pathname-directory-separator) "].*")]
	 [string-match string-match] )
    (lambda (pn)
      (##sys#check-string pn 'absolute-pathname?)
      (pair? (string-match rx pn)) ) ) )

(define (chop-pds str)
  (and str
       (let ([len (##sys#size str)])
	 (if (and (fx> len 0) 
		  (eq? ##sys#pathname-directory-separator (##core#inline "C_subchar" str (fx- len 1))) )
	     (##sys#substring str 0 (fx- len 1))
	     str) ) ) )

(let ([string-append string-append]
      [absolute-pathname? absolute-pathname?]
      [pds (string ##sys#pathname-directory-separator)] )
  (define (conc-dirs dirs)
    (##sys#check-list dirs 'make-pathname)
    (let loop ([strs dirs])
      (if (null? strs) 
	  ""
	  (let ((s1 (car strs)))
	    (if (zero? (string-length s1))
		(loop (cdr strs))
		(string-append (chop-pds (car strs)) pds (loop (cdr strs))) ) ) ) ) )
  (define (canonicalize dir)
    (cond [(or (not dir) (null? dir)) ""]
	  [(string? dir) (conc-dirs (list dir))]
	  [else (conc-dirs dir)] ) )
  (define (_make-pathname dir file . ext)
    (let ([dirs (canonicalize dir)]
	  [file (or file "")]
	  [ext (if (pair? ext) (or (car ext) "") "")] )
      (##sys#check-string file 'make-pathname)
      (##sys#check-string ext 'make-pathname)
      (string-append
       dirs
       (if (and dir
		(fx> (##sys#size file) 0) 
		(eq? ##sys#pathname-directory-separator (##core#inline "C_subchar" file 0)) )
	   (##sys#substring file 1 (##sys#size file))
	   file)
       (if (and (fx> (##sys#size ext) 0)
		(not (char=? (##core#inline "C_subchar" ext 0) #\.)) )
	   "."
	   "")
       ext) ) )
  (set! make-pathname _make-pathname)
  (set! make-absolute-pathname
    (lambda (dir file . ext)
      (apply
       _make-pathname
       (let* ([dirs (canonicalize dir)]
	      [dlen (##sys#size dirs)] )
	 (if (not (absolute-pathname? dirs))
	     (##sys#string-append pds dirs)
	     dirs) )
       file
       ext) ) ) )

(define decompose-pathname
  (let* ([set (##sys#string-append "\\/\\" (string ##sys#pathname-directory-separator))]
	 [rx1 (string-append "^(.*[" set "])?([^" set "]+)(\\.([^" set ".]+))$")]
	 [rx2 (string-append "^(.*[" set "])?((\\.)?[^" set "]+)$")] 
	 [string-match string-match] )
    (lambda (pn)
      (##sys#check-string pn 'decompose-pathname)
      (if (eq? (##sys#size pn) 0)
	  (values #f #f #f) 
	  (let ([m (string-search rx1 pn)])
	    (if m
		(values (chop-pds (cadr m)) (caddr m) (car (cddddr m)))
		(let ([m (string-search rx2 pn)])
		  (if m
		      (values (chop-pds (cadr m)) (caddr m) #f)
		      (values pn #f #f) ) ) ) ) ) ) ) )

(let ([decompose-pathname decompose-pathname])
  (set! pathname-directory 
    (lambda (pn)
      (let-values ([(dir file ext) (decompose-pathname pn)])
	dir) ) )
  (set! pathname-file
    (lambda (pn)
      (let-values ([(dir file ext) (decompose-pathname pn)])
	file) ) )
  (set! pathname-extension 
    (lambda (pn)
      (let-values ([(dir file ext) (decompose-pathname pn)])
	ext) ) )
  (set! pathname-strip-directory 
    (lambda (pn)
      (let-values ([(dir file ext) (decompose-pathname pn)])
	(make-pathname #f file ext) ) ) )
  (set! pathname-strip-extension 
    (lambda (pn)
      (let-values ([(dir file ext) (decompose-pathname pn)])
	(make-pathname dir file) ) ) )
  (set! pathname-replace-directory
    (lambda (pn dir)
      (let-values ([(_ file ext) (decompose-pathname pn)])
	(make-pathname dir file ext) ) ) )
  (set! pathname-replace-file
    (lambda (pn file)
      (let-values ([(dir _ ext) (decompose-pathname pn)])
	(make-pathname dir file ext) ) ) )
  (set! pathname-replace-extension
    (lambda (pn ext)
      (let-values ([(dir file _) (decompose-pathname pn)])
	(make-pathname dir file ext) ) ) ) )

(define create-temporary-file
  (let ([getenv getenv]
	[make-pathname make-pathname] 
	[file-exists? file-exists?] 
	[call-with-output-file call-with-output-file] )
    (lambda ext
      (let ([dir (or (getenv "TMPDIR") (getenv "TEMP") (getenv "TMP"))]
	    [ext (if (pair? ext) (car ext) "tmp")])
	(##sys#check-string ext 'create-temporary-file)
	(let loop ()
	  (let* ([n (##sys#fudge 16)]
		 [pn (make-pathname dir (##sys#string-append "t" (number->string n 16)) ext)] )
	    (if (file-exists? pn) 
		(loop)
		(call-with-output-file pn (lambda (p) pn)) ) ) ) ) ) ) )


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
  (let ([file (:optional file ##sys#standard-input)])
    (if (port? file)
	(read-string #f file)
	(with-input-from-file file (cut read-string #f)) ) ) )


;;; Handy little things:

(define (shift! lst #!optional default)
  (if (null? lst)
      default
      (begin
	(##sys#check-pair lst 'shift!)
	(let ([x (##sys#slot lst 0)]
	      [d (##sys#slot lst 1)] )
	  (##sys#check-pair d 'shift!)
	  (##sys#setslot lst 1 (##sys#slot d 1))
	  (##sys#setslot lst 0 (##sys#slot d 0))
	  x) ) ) )

(define (unshift! x lst)
  (##sys#check-pair lst 'unshift!)
  (##sys#setslot lst 1 (cons (##sys#slot lst 0) (##sys#slot lst 1)))
  (##sys#setslot lst 0 x)
  lst)


;;;; Port-mapping (found in Gauche):

(define (port-for-each fn thunk)
  (let loop ()
    (let ((x (thunk)))
      (unless (eq? x #!eof)
	(fn x)
	(loop) ) ) ) )

(define port-map
  (let ((reverse reverse))
    (lambda (fn thunk)
      (let loop ((xs '()))
	(let ((x (thunk)))
	  (if (eq? x #!eof)
	      (reverse xs)
	      (loop (cons (fn x) xs))))))))
