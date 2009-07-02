;;;; henrietta.scm - Server program (CGI) for serving eggs from a repository over HTTP
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

; used environment variables:
;
; QUERY_STRING
; REMOTE_ADDR (optional)

; URL arguments:
;
; version=<version>
; name=<name>
; tests
; list


(require-library setup-download regex extras utils ports srfi-1 posix)


(module main ()

  (import scheme chicken regex extras utils ports srfi-1 posix)
  (import setup-api setup-download)

  (define *default-transport* 'svn)
  (define *default-location* (current-directory))
  (define *username* #f)
  (define *password* #f)
  (define *tests* #f)

  (define (headers)
    (print "Connection: close\r\nContent-type: text/plain\r\n\r\n"))

  (define (fail msg . args)
    (pp `(error ,msg ,@args))
    (cleanup)
    (exit 0))

  (define (cleanup)
    (and-let* ((tmpdir (temporary-directory)))
      (fprintf (current-error-port) "removing temporary directory `~a'~%" tmpdir)
      (remove-directory tmpdir)))

  (define test-file?
    (let ((rx (regexp "(\\./)?tests(/.*)?")))
      (lambda (path) (string-match rx path))))

  (define (retrieve name version)
    (let ((dir (handle-exceptions ex 
		   (fail ((condition-property-accessor 'exn 'message) ex)
			 ((condition-property-accessor 'exn 'arguments) ex))
		 (retrieve-extension 
		  name *default-transport* *default-location*
		  version: version 
		  quiet: #t 
		  destination: #f
		  tests: *tests*
		  username: *username* 
		  password: *password*))))
      (unless dir 
	(fail "no such extension or version" name version))
      (let walk ((dir dir) (prefix "."))
	(let ((files (directory dir)))
	  (for-each
	   (lambda (f)
	     (when (or *tests* (not (test-file? f)))
	       (let ((ff (string-append dir "/" f))
		     (pf (string-append prefix "/" f)))
		 (cond ((directory? ff)
			(print "\n#|--------------------|# \"" pf "/\" 0")
			(walk ff pf))
		       (else
			(print "\n#|--------------------|# \"" pf "\" " (file-size ff))
			(display (read-all ff)))))))
	   files)))
      (print "\n#!eof") ) )

  (define (listing)
    (let ((dir (handle-exceptions ex 
		   (fail ((condition-property-accessor 'exn 'message) ex)
			 ((condition-property-accessor 'exn 'arguments) ex))
		 (list-extensions
		  *default-transport* *default-location*
		  quiet: #t 
		  username: *username* 
		  password: *password*))))
      (if dir 
	  (display dir)
	  (fail "unable to retrieve extension-list"))))

  (define query-string-rx (regexp "[^?]+\\?(.+)"))
  (define query-arg-rx (regexp "^&?(\\w+)=([^&]+)"))

  (define (service)
    (let ((qs (getenv "QUERY_STRING"))
	  (ra (getenv "REMOTE_ADDR")))
      (fprintf (current-error-port) "~%========== serving request from ~a: ~s~%"
	       (or ra "<unknown>") qs)
      (unless qs
	(error "no QUERY_STRING set"))
      (let ((m (string-match query-string-rx qs))
	    (egg #f)
	    (version #f))
	(let loop ((qs (if m (cadr m) qs)))
	  (let* ((m (string-search-positions query-arg-rx qs))
		 (ms (and m (apply substring qs (cadr m))))
		 (rest (and m (substring qs (cadar m)))))
	    (cond ((not m)
		   (headers)		; from here on use `fail'
		   (cond (egg
			  (retrieve egg version)
			  (cleanup) )
			 (else (fail "no extension name specified") ) ))
		  ((string=? ms "version")
		   (set! version (apply substring qs (caddr m)))
		   (loop rest))
		  ((string=? ms "name")
		   (set! egg (apply substring qs (caddr m)))
		   (loop rest))
		  ((string=? ms "tests")
		   (set! *tests* #t)
		   (loop rest))
		  ((string=? ms "list")
		   (headers)
		   (listing))
		  (else
		   (warning "unrecognized query option" ms)
		   (loop rest))))))))
  

  (define (usage code)
    (print #<#EOF
usage: henrietta [OPTION ...]

  -h   -help                    show this message
  -l   -location LOCATION       install from given location (default: current directory)
  -t   -transport TRANSPORT     use given transport instead of default (#{*default-transport*})
       -username USER           set username for transports that require this
       -password PASS           set password for transports that require this
EOF
);|
    (exit code))

  (define *short-options* '(#\h #\l #\t))

  (define (main args)
    (let loop ((args args))
      (if (null? args)
	  (service)
	  (let ((arg (car args)))
	    (cond ((or (string=? arg "-help") 
		       (string=? arg "-h")
		       (string=? arg "--help"))
		   (usage 0))
		  ((or (string=? arg "-l") (string=? arg "-location"))
		   (unless (pair? (cdr args)) (usage 1))
		   (set! *default-location* (cadr args))
		   (loop (cddr args)))
		  ((or (string=? arg "-t") (string=? arg "-transport"))
		   (unless (pair? (cdr args)) (usage 1))
		   (set! *default-transport* (string->symbol (cadr args)))
		   (loop (cddr args)))
		  ((string=? "-username" arg)
		   (unless (pair? (cdr args)) (usage 1))
		   (set! *username* (cadr args))
		   (loop (cddr args)))
		  ((string=? "-password" arg)
		   (unless (pair? (cdr args)) (usage 1))
		   (set! *password* (cadr args))
		   (loop (cddr args)))
		  ((and (positive? (string-length arg))
			(char=? #\- (string-ref arg 0)))
		   (if (> (string-length arg) 2)
		       (let ((sos (string->list (substring arg 1))))
			 (if (null? (lset-intersection eq? *short-options* sos))
			     (loop (append (map (cut string #\- <>) sos) (cdr args)))
			     (usage 1)))
		       (usage 1)))
		  (else (loop (cdr args))))))))

  (main (command-line-arguments))
  
)
