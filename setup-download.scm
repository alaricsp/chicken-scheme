;;;; setup-download.scm
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


(require-library extras regex posix utils setup-utils srfi-1 data-structures tcp srfi-13)


(module setup-download (retrieve-extension
			locate-egg/local
			locate-egg/svn
			locate-egg/http
			temporary-directory)

  (import scheme chicken)
  (import extras regex posix utils setup-utils srfi-1 data-structures tcp srfi-13)

  (define temporary-directory (make-parameter #f))

  (define (get-temporary-directory)
    (or (temporary-directory)
	(let ((dir (create-temporary-directory)))
	  (temporary-directory dir)
	  dir)))

  (define (locate-egg/local egg dir #!optional version quiet)
    (let* ((eggdir (make-pathname dir egg))
	   (files (directory eggdir))
	   (trunkdir (make-pathname eggdir "trunk"))
	   (tagdir (make-pathname eggdir "tags"))
	   (hastrunk (and (file-exists? trunkdir) (directory? trunkdir))))
      (or (and (file-exists? tagdir) (directory? tagdir)
	       (let ((vs (directory tagdir)))
		 (if version
		     (if (member version vs)
			 (make-pathname tagdir version)
			 (error "version not found" egg version))
		     (let ((vs (sort vs version>=?)))
		       (and (pair? vs)
			    (make-pathname tagdir (car vs)))))))
	  (begin
	    (when version
	      (warning "extension has no such version - using trunk" egg version))
	    (or (and hastrunk trunkdir)
		eggdir)))))
  
  (define (locate-egg/svn egg repo #!optional version quiet)
    (let ((cmd (sprintf "svn ls -R \"~a/~a\"" repo egg)))
      (fprintf (if quiet (current-error-port) (current-output-port)) 
	       "checking available versions ...\n  ~a~%" cmd)
      (let* ((files (with-input-from-pipe cmd read-lines))
	     (hastrunk (member "trunk/" files)) 
	     (filedir
	      (or (let ((vs (filter-map
			     (lambda (f)
			       (and-let* ((m (string-search "^tags/([^/]+)/" f))
					  (v (cadr m)))
				 (print v)
				 v))
			     files)))
		    (if version
			(if (member version vs)
			    (string-append "tags/" version)
			    (error "version not found" egg version))
			(let ((vs (sort vs version>=?)))
			  (and (pair? vs)
			       (string-append "tags/" (car vs))))))
		  (begin
		    (when version
		      (warning "extension has no such version - using trunk" egg version))
		    (and hastrunk "trunk") )
		  ""))
	     (tmpdir (get-temporary-directory))
	     (cmd (sprintf "svn co \"~a/~a/~a\" \"~a\" ~a" repo egg filedir tmpdir
			   (if quiet "1>&2" ""))))
	(fprintf (if quiet (current-error-port) (current-output-port)) "  ~a~%" cmd)
	(system* cmd)
	tmpdir)) )

  (define (locate-egg/http egg url #!optional version quiet)
    (let* ((tmpdir (get-temporary-directory))
	   (m (string-match "(http://)?([^/]+)(:([^:/]+))?(/.+)" url))
	   (host (if m (caddr m) url))
	   (port (if (and m (cadddr m)) 
		     (or (string->number (list-ref m 4)) 
			 (error "not a valid port" (list-ref m 4)))
		     80))
	   (loc (string-append
		 (if m (list-ref m 5) "/")
		 (if version
		     (string-append "?version=" version)
		     ""))))
      (http-fetch host port loc tmpdir quiet)
      tmpdir))

  (define (http-fetch host port loc dest #!optional quiet)
    (let-values (((in out) (tcp-connect host port)))
      (fprintf out "GET ~a HTTP/1.1\r\nConnection: close\r\nUser-Agent: chicken-install ~a\r\nAccept: */*\r\nContent-length: 0\r\n\r\n"
	       loc (chicken-version))
      (close-output-port out)
      (let* ((h1 (read-line in))
	     (m (string-match "HTTP/[0-9.]+\\s+([0-9]+)\\s+.*" h1)))
	(print h1)
	;;*** handle redirects
	(unless (string=? "200" (cadr m))
	  (error "invalid response from server" h1))
	(do () ((string=? "" (read-line in))))
      (let loop ((files '()))
	(let ((name (read in)))
	  (fprintf (if quiet (current-error-port) (current-output-port))
		   "  ~a~%" name)
	  (cond ((and (pair? name) (eq? 'error (car name)))
		 (apply error (cdr name)))
	        ((or (eof-object? name) (not name))
		 (close-input-port in)
		 (reverse files) )
		((not (string? name))
		 (error "invalid file name - possibly corrupt transmission" name))
		((string-suffix? "/" name)
		 (create-directory (make-pathname dest name))
		 (loop files))
		(else
		 (let* ((size (read in))
			(data (read-string size in)) )
		   (with-output-to-file (make-pathname dest name)
		     (lambda ()
		       (display data) ) ) )
		 (loop (cons name files)))))))))

  (define (retrieve-extension name transport location #!optional version quiet)
    (case transport
      ((local) 
       (locate-egg/local name location version quiet)) 
      ((svn)
       (locate-egg/svn name location version quiet))
      ((http)
       (locate-egg/http name location version quiet))
      (else (error "unsupported transport" transport))))

)
