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
  (uses regex data-structures extras)
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


;;; Like `delete-file', but does nothing if the file doesn't exist:

(define delete-file*
  (let ([file-exists? file-exists?]
	[delete-file delete-file] )
    (lambda (file)
      (and (file-exists? file) (delete-file file) #t) ) ) )

;;; file-copy and file-move : they do what you'd think.
(define (file-copy origfile newfile #!optional (clobber #f) (blocksize 1024))
    (##sys#check-string origfile 'file-copy)
    (##sys#check-string newfile 'file-copy)
    (##sys#check-number blocksize 'file-copy)
    (or (and (integer? blocksize) (> blocksize 0))
        (##sys#error (string-append
                         "invalid blocksize given: not a positive integer - "
                         (number->string blocksize))))
    (or (file-exists? origfile)
        (##sys#error (string-append "origfile does not exist - " origfile)))
    (and (file-exists? newfile)
         (or clobber
             (##sys#error (string-append
                              "newfile exists but clobber is false - "
                              newfile))))
    (let* ((i   (condition-case (open-input-file origfile)
                    (val ()
                        (##sys#error (string-append
                                         "could not open origfile for read - "
                                         origfile)))))
           (o   (condition-case (open-output-file newfile)
                    (val ()
                        (##sys#error (string-append
                                         "could not open newfile for write - "
                                         newfile)))))
           (s   (make-string blocksize)))
        (let loop ((d   (read-string! blocksize s i))
                   (l   0))
            (if (= 0 d)
                (begin
                    (close-input-port i)
                    (close-output-port o)
                    l)
                (begin
                    (condition-case (write-string s d o)
                        (val ()
                            (close-input-port i)
                            (close-output-port o)
                            (##sys#error (string-append
                                             "error writing file starting at "
                                             (number->string l)))))
                    (loop (read-string! blocksize s i) (+ d l)))))))

(define (file-move origfile newfile #!optional (clobber #f) (blocksize 1024))
    (##sys#check-string origfile 'file-move)
    (##sys#check-string newfile 'file-move)
    (##sys#check-number blocksize 'file-move)
    (or (and (integer? blocksize) (> blocksize 0))
        (##sys#error (string-append
                         "invalid blocksize given: not a positive integer - "
                         (number->string blocksize))))
    (or (file-exists? origfile)
        (##sys#error (string-append "origfile does not exist - " origfile)))
    (and (file-exists? newfile)
         (or clobber
             (##sys#error (string-append
                              "newfile exists but clobber is false - "
                              newfile))))
    (let* ((i   (condition-case (open-input-file origfile)
                    (val ()
                        (##sys#error (string-append
                                         "could not open origfile for read - "
                                         origfile)))))
           (o   (condition-case (open-output-file newfile)
                    (val ()
                        (##sys#error (string-append
                                         "could not open newfile for write - "
                                         newfile)))))
           (s   (make-string blocksize)))
        (let loop ((d   (read-string! blocksize s i))
                   (l   0))
            (if (= 0 d)
                (begin
                    (close-input-port i)
                    (close-output-port o)
                    (condition-case (delete-file origfile)
                        (val ()
                            (##sys#error (string-append
                                             "could not remove origfile - "
                                             origfile))))
                    l)
                (begin
                    (condition-case (write-string s d o)
                        (val ()
                            (close-input-port i)
                            (close-output-port o)
                            (##sys#error (string-append
                                             "error writing file starting at "
                                             (number->string l)))))
                    (loop (read-string! blocksize s i) (+ d l)))))))

;;; Pathname operations:

(define absolute-pathname?
  (let ([string-match string-match]
        [regexp regexp]
        [string-append string-append])
    (let* ([drv (if ##sys#windows-platform "([A-Za-z]:)?" "")]
           [patt (make-anchored-pattern (string-append drv "[\\/\\\\].*"))]
	   [rx (regexp patt)] )
      (lambda (pn)
        (##sys#check-string pn 'absolute-pathname?)
        (pair? (string-match rx pn)) ) ) ) )

(define (chop-pds str pds)
  (and str
       (let ((len (##sys#size str))
	     (pdslen (if pds (##sys#size pds) 1)))
	 (if (and (fx>= len 1)
		  (if pds
		      (##core#inline "C_substring_compare" str pds (fx- len pdslen) 0 pdslen)
		      (memq (##core#inline "C_subchar" str (fx- len pdslen))
			    '(#\/ #\\) ) ) )
	     (##sys#substring str 0 (fx- len pdslen))
	     str) ) ) )

(define make-pathname)
(define make-absolute-pathname)
(let ([string-append string-append]
      [absolute-pathname? absolute-pathname?]
      [def-pds "/"] )

  (define (conc-dirs dirs pds)
    (##sys#check-list dirs 'make-pathname)
    (let loop ([strs dirs])
      (if (null? strs)
	  ""
	  (let ((s1 (car strs)))
	    (if (zero? (string-length s1))
		(loop (cdr strs))
		(string-append 
		 (chop-pds (car strs) pds)
		 (or pds def-pds)
		 (loop (cdr strs))) ) ) ) ) )

  (define (canonicalize-dirs dirs pds)
    (cond [(or (not dirs) (null? dirs)) ""]
	  [(string? dirs) (conc-dirs (list dirs) pds)]
	  [else           (conc-dirs dirs pds)] ) )

  (define (_make-pathname loc dir file ext pds)
    (let ([ext (or ext "")]
	  [file (or file "")]
	  [pdslen (if pds (##sys#size pds) 1)] )
      (##sys#check-string dir loc)
      (##sys#check-string file loc)
      (##sys#check-string ext loc)
      (when pds (##sys#check-string pds loc))
      (string-append
       dir
       (if (and (fx>= (##sys#size file) pdslen)
		(if pds
                    (##core#inline "C_substring_compare" pds file 0 0 pdslen)
                    (memq (##core#inline "C_subchar" file 0) '(#\\ #\/))))
	   (##sys#substring file pdslen (##sys#size file))
	   file)
       (if (and (fx> (##sys#size ext) 0)
		(not (char=? (##core#inline "C_subchar" ext 0) #\.)) )
	   "."
	   "")
       ext) ) )

  (set! make-pathname
    (lambda (dirs file #!optional ext pds)
      (_make-pathname 'make-pathname (canonicalize-dirs dirs pds) file ext pds)))

  (set! make-absolute-pathname
    (lambda (dirs file #!optional ext pds)
      (_make-pathname
       'make-absolute-pathname
       (let ([dir (canonicalize-dirs dirs pds)])
	 (if (absolute-pathname? dir)
	     dir
	     (##sys#string-append (or pds def-pds) dir)) )
       file ext pds) ) ) )

(define decompose-pathname
  (let ([string-match string-match]
        [regexp regexp]
        [string-append string-append])
    (let* ([patt1 "^(.*[\\/\\\\])?([^\\/\\\\]+)(\\.([^\\/\\\\.]+))$"]
	   [patt2 "^(.*[\\/\\\\])?((\\.)?[^\\/\\\\]+)$"]
	   [rx1 (regexp patt1)]
	   [rx2 (regexp patt2)]
	   [strip-pds
	     (lambda (dir)
	        (and dir
		     (if (member dir '("/" "\\"))
		         dir
		         (chop-pds dir #f) ) ) )] )
      (lambda (pn)
        (##sys#check-string pn 'decompose-pathname)
        (if (fx= 0 (##sys#size pn))
	    (values #f #f #f)
	    (let ([ms (string-match rx1 pn)])
	      (if ms
		  (values (strip-pds (cadr ms)) (caddr ms) (car (cddddr ms)))
		  (let ([ms (string-match rx2 pn)])
		    (if ms
		        (values (strip-pds (cadr ms)) (caddr ms) #f)
		        (values (strip-pds pn) #f #f) ) ) ) ) ) ) ) ) )

(define pathname-directory)
(define pathname-file)
(define pathname-extension)
(define pathname-strip-directory)
(define pathname-strip-extension)
(define pathname-replace-directory)
(define pathname-replace-file)
(define pathname-replace-extension)
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

;; Directory string or list only contains path-separators
;; and/or current-directory names.

(define (directory-null? dir)
  (let loop ([lst
              (if (list? dir)
                  dir ; Don't bother to check for strings here
                  (begin
                    (##sys#check-string dir 'directory-null?)
                    (string-split dir "/\\" #t)))])
    (or (null? lst)
        (and (member (car lst) '("" "."))
             (loop (cdr lst)) ) ) ) )

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

(define (port-fold fn acc thunk)
  (let loop ([acc acc])
    (let ([x (thunk)])
      (if (eq? x #!eof)
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
