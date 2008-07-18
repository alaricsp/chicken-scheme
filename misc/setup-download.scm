;;;; setup-download.scm - download part of egg setup
;
; Copyright (c) 2008, The Chicken Team
; Copyright (c) 2000-2007, Felix L. Winkelmann
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


(require-library regex posix utils srfi-1 srfi-13 tcp)


(module setup-download 

    (download-data
     setup-download-directory)

  (import scheme chicken extras regex posix utils srfi-1 srfi-13 tcp
	  data-structures setup-api)


(define setup-build-prefix
  (make-parameter
   (or (getenv "CHICKEN_TMPDIR") (getenv "TMPDIR")
       (getenv "TMP") (getenv "TEMP")
       ((lambda (user) 
	  (and user  (file-write-access? "/tmp") 
	       (conc "/tmp/chicken-setup-" *major-version* "-" user))) 
	(getenv "USER"))
       ((lambda (home user) 
	  (and home user  (conc home "/tmp/chicken-setup-" *major-version* "-" user))) 
	(getenv "HOME") (getenv "USER"))
       (current-directory))))

(define setup-download-directory 
  (make-parameter (make-pathname (setup-build-prefix) "downloads")))

(define *proxy-host* #f)
(define *proxy-port* #f)

(and-let* ((proxy (getenv "HTTP_PROXY"))
	   (m (string-match "(http://)?([^:]+)(:(\\d+))?" proxy)))
  (set! *proxy-host* (cadr m))
  (set! *proxy-port* 
    (cond ((fourth m) => number->string)
	  (else
	   (warning "invalid port in HTTP_PROXY: " proxy)
	   80))))

(define (http-get-path-request path fname host)
  (sprintf "~A HTTP/1.0\r\nHost: ~A\r\nConnection: close\r\nContent-length: 0\r\n\r\n"
	   (let ((p (make-pathname path fname "" "/")))
	     (if (absolute-pathname? p)
		 p
		 (conc "/" p) ) )
	   host))

(define (http-get-request path fname host)
  (if *proxy-host*
      (sprintf "GET http://~A~A" host (http-get-path-request path fname host))
      (sprintf "GET ~A" (http-get-path-request path fname host))))

(define (setup-tcp-connect host port)
  (if *proxy-host*
      (tcp-connect *proxy-host* *proxy-port*)
      (tcp-connect host port)))

(define (download-data host port filename)
  (let-values (((i o) (setup-tcp-connect host port)))
    (let ((req (http-get-request 
		(if filename (pathname-directory filename) "/")
		(if filename (pathname-strip-directory filename) filename)
		host) ) )
      (display req o) )
    (let loop ((first #t))
      (let ((ln (read-line i)))
	(cond ((and first (string-match "HTTP/[.0-9]+\\s+(\\d+)\\s+(.+)" ln)) =>
	       (lambda (m)
		 ;; *** doesn't handle redirects
		 (cond ((not (string=? "200" (cadr m)))
			(close-input-port i)
			(close-output-port o)
			(values #f (string-append (cadr m) " " (caddr m))) )
		       (else (loop #f))))
	      ((string=? "" ln)
	       (let ((data (read-string #f i)))
		 (close-input-port i)
		 (close-output-port o)
		 (unless (file-exists? (setup-download-directory))
		   (create-directory/parents (setup-download-directory)))
		 (let ((fpath (make-pathname 
			       (setup-download-directory)
			       (pathname-strip-directory filename))))
		   (with-output-to-file fpath
		     (cut display data) 
		     binary:)
		   (values #t fpath))))
	      (else (loop) ) ) ) ) ))

)
