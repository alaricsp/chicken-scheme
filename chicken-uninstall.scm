;;;; chicken-uninstall.scm
;
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


(require-library
 setup-api
 srfi-1 posix data-structures utils ports regex srfi-13 files)


(module main ()
  
  (import scheme chicken)
  (import setup-api)
  (import srfi-1 posix data-structures utils ports regex srfi-13 files)

  (define *force* #f)

  (define (gather-eggs patterns)
    (let ((eggs (map pathname-file 
		     (glob (make-pathname (repository-path) "*" "setup-info")))))
      (delete-duplicates
       (concatenate (map (cut grep <> eggs) patterns))
       string=?)))

  (define (quit code)
    (print "aborted.")
    (exit code))

  (define (ask eggs)
    (handle-exceptions ex
	(if (eq? ex 'aborted)
	    (quit 1) 
	    (signal ex))
      (yes-or-no? 
       (string-concatenate
	(append
	 '("About to delete the following extensions:\n\n")
	 (map (cut string-append "  " <> "\n") eggs)
	 '("\nDo you want to proceed?")))
       default: "no")))

  (define (uninstall pats)
    (let ((eggs (gather-eggs pats)))
      (cond ((null? eggs)
	     (print "nothing to remove.") )
	    ((or *force* (equal? eggs pats) (ask eggs))
	     (for-each
	      (lambda (e)
		(print "removing " e)
		(remove-extension e) )
	      eggs)))))

  (define (usage code)
    (print #<<EOF
usage: chicken-uninstall [OPTION | PATTERN] ...

  -h   -help                    show this message and exit
  -v   -version                 show version and exit
       -force                   don't ask, delete whatever matches
  -s   -sudo                    use sudo(1) for deleting files
EOF
);|
    (exit code))

  (define *short-options* '(#\h #\s))

  (define (main args)
    (let loop ((args args) (pats '()))
      (if (null? args)
	  (uninstall (if (null? pats) (usage 1) (reverse pats)))
	  (let ((arg (car args)))
	    (cond ((or (string=? arg "-help") 
		       (string=? arg "-h")
		       (string=? arg "--help"))
		   (usage 0))
		  ((or (string=? arg "-v") (string=? arg "-version"))
		   (print (chicken-version))
		   (exit 0))
		  ((string=? arg "-force")
		   (set! *force* #t)
		   (loop (cdr args) pats))
		  ((or (string=? arg "-s") (string=? arg "-sudo"))
		   (sudo-install #t)
		   (loop (cdr args) pats))
		  ((and (positive? (string-length arg))
			(char=? #\- (string-ref arg 0)))
		   (if (> (string-length arg) 2)
		       (let ((sos (string->list (substring arg 1))))
			 (if (null? (lset-intersection eq? *short-options* sos))
			     (loop (append (map (cut string #\- <>) sos) (cdr args)) pats)
			     (usage 1)))
		       (usage 1)))
		  (else (loop (cdr args) (cons arg pats))))))))

  (main (command-line-arguments))
  
 )
