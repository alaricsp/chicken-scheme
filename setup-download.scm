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


(require-library extras regex posix utils setup-utils srfi-1 data-structures)


(module setup-download (retrieve-extension
			locate-egg/local
			locate-egg/svn
			locate-egg/http)

  (import scheme chicken)
  (import extras regex posix utils setup-utils srfi-1 data-structures)

  (define (locate-egg/local egg dir #!optional version)
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
  
  (define (locate-egg/svn egg repo #!optional version)
    (let ((cmd (sprintf "svn ls -R \"~a/~a\"" repo egg)))
      (print "checking available versions ...\n  " cmd)
      (let* ((files (with-input-from-pipe cmd read-lines))
	     (hastrunk (member "trunk/" files)) 
	     (filedir
	      (or (let ((vs (filter-map
			     (lambda (f)
			       (and-let* ((m (string-search "^tags/([^/]+)/" f)))
				 (cadr m)))
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
	     (tmpdir (create-temporary-directory))
	     (cmd (sprintf "svn co \"~a/~a/~a\" \"~a\"" repo egg filedir tmpdir)))
	(print "  " cmd)
	(system* cmd)
	tmpdir)) )

  (define (locate-egg/http egg url #!optional version)
    (let* ((tmpdir (create-temporary-directory))
	   (m (string-match "([^/]+)(:([^:/]+))?(/.+)" url))
	   (host (if m (cadr m) url))
	   (port (if (and m (caddr m)) 
		     (or (string->number (cadddr m)) 
			 (error "not a valid port" (cadddr m)))
		     80))
	   (loc (string-append
		 (if m (list-ref m 4) "/")
		 (if version
		     (string-append "?version=" version)
		     ""))))
      (http-fetch host port loc tmpdir)
      tmpdir))

  (define (retrieve-extension name transport location #!optional version)
    (case transport
      ((local) 
       (values (locate-egg/local name location version) #f) )
      ((svn)
       (values (locate-egg/svn name location version) #t) )
      ((http)
       (values (locate-egg/http name location version) #t) )
      (else (error "unsupported transport" transport))))

)
