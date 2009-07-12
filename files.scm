;;;; files.scm - File and pathname operations
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
;
;   Redistributions in binary form must reproduce the above copyright
;   notice, this list of conditions and the following disclaimer in
;   the documentation and/or other materials provided with the
;   distribution.
;
;   Neither the name of the author nor the names of its contributors
;     may be used to endorse or promote products derived from this
;     software without specific prior written permission.
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
  (unit files)
  (uses regex data-structures)
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
      string-match regexp
      ##sys#string-append ##sys#substring  string-append
      get-environment-variable
      file-exists? delete-file
      call-with-output-file read-string)
    (no-procedure-checks-for-usual-bindings)
    (no-bound-checks))] )

(include "unsafe-declarations.scm")

(register-feature! 'files)


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
           [patt (string-append drv "[\\/\\\\].*")]
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
  (let ([get-environment-variable get-environment-variable]
	[make-pathname make-pathname]
	[file-exists? file-exists?]
	[call-with-output-file call-with-output-file] )
    (lambda ext
      (let ((dir (or (get-environment-variable "TMPDIR") 
		     (get-environment-variable "TEMP")
		     (get-environment-variable "TMP")
		     (file-exists? "/tmp")))
	    (ext (if (pair? ext) (car ext) "tmp")))
	(##sys#check-string ext 'create-temporary-file)
	(let loop ()
	  (let* ([n (##sys#fudge 16)]
		 [pn (make-pathname dir (##sys#string-append "t" (number->string n 16)) ext)] )
	    (if (file-exists? pn)
		(loop)
		(call-with-output-file pn (lambda (p) pn)) ) ) ) ) ) ) )


;;; normalize pathname for a particular platform

(define normalize-pathname
  (let ((open-output-string open-output-string)
	(get-output-string get-output-string)
	(get-environment-variable get-environment-variable)
	(reverse reverse)
	(display display))
    (lambda (path #!optional (platform (if (memq (build-platform) '(msvc mingw32)) 'windows 'unix)))
      (let ((sep (if (eq? platform 'windows) #\\ #\/)))
	(define (addpart part parts)
	  (cond ((string=? "." part) parts)
		((string=? ".." part) 
		 (if (null? parts)
		     '("..")
		     (cdr parts)))
		(else (cons part parts))))
	(##sys#check-string path 'normalize-pathname)
	(let ((len (##sys#size path))
	      (abspath #f)
	      (drive #f))
	  (let loop ((i 0) (prev 0) (parts '()))
	    (cond ((fx>= i len)
		   (when (fx> i prev)
		     (set! parts (addpart (##sys#substring path prev i) parts)))
		   (if (null? parts)
		       (##sys#string-append "." (string sep))
		       (let ((out (open-output-string))
			     (parts (reverse parts)))
			 (display (car parts) out)
			 (for-each
			  (lambda (p)
			    (##sys#write-char-0 sep out)
			    (display p out) )
			  (cdr parts))
			 (when (fx= i prev) (##sys#write-char-0 sep out))
			 (let* ((r1 (get-output-string out))
				(r (##sys#expand-home-path r1)))
			   (when (string=? r1 r)
			     (when abspath 
			       (set! r (##sys#string-append (string sep) r)))
			     (when drive
			       (set! r (##sys#string-append drive r))))
			   r))))
		  ((memq (string-ref path i) '(#\\ #\/))
		   (when (and (null? parts) (fx= i prev))
		     (set! abspath #t))
		   (if (fx= i prev)
		       (loop (fx+ i 1) (fx+ i 1) parts)
		       (loop (fx+ i 1)
			     (fx+ i 1)
			     (addpart (##sys#substring path prev i) parts))))
		  ((and (null? parts) 
			(char=? (string-ref path i) #\:)
			(eq? 'windows platform))
		   (set! drive (##sys#substring path 0 (fx+ i 1)))
		   (loop (fx+ i 1) (fx+ i 1) '()))
		  (else (loop (fx+ i 1) prev parts)))))))))


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
