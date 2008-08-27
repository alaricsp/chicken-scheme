;;;; chicken-install.scm
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


(require-library setup-utils setup-download)
(require-library srfi-1 posix data-structures utils ports regex ports extras
		 srfi-13 files)


#+(not csi)
(foreign-declare #<<EOF
#ifndef C_INSTALL_BIN_HOME
# define C_INSTALL_BIN_HOME   NULL
#endif
EOF
)


(module main ()
  
  (import scheme chicken srfi-1 posix data-structures utils ports regex ports extras
	  srfi-13 files)
  (import setup-utils setup-download)
  
  #+(not csi) (import foreign)

  (define *program-path*
    (or (and-let* ((p (getenv "CHICKEN_PREFIX")))
	  (make-pathname p "bin") )
	(cond-expand
	 (csi (make-pathname (current-directory) "bin"))			; just for debugging
	 (else (foreign-value "C_INSTALL_BIN_HOME" c-string) ) )) )

  (define *default-transport* #f)
  (define *default-location* #f)
  (define *keep* #f)
  (define *force* #f)
  (define *sudo* #f)
  (define *prefix* #f)
  (define *host-extension* #f)
  (define *run-tests* #f)
  (define *retrieve-only* #f)
  (define *no-install* #f)
  (define *username* #f)
  (define *password* #f)

  (define (load-defaults)
    (let* ((deff (make-pathname (repository-path) "setup.defaults"))
	   (def (cond ((file-exists? deff)
		       (with-input-from-file deff read))
		      (else '())))
	   (loc (assq 'location def))
	   (tr (assq 'transport def)))
      (when loc (set! *default-location* (cadr loc)))
      (when tr (set! *default-transport* (cadr tr)))
      (pair? def)))

  (define (deps key meta)
    (or (and-let* ((d (assq key meta)))
	  (cdr d))
	'()))

  (define (outdated-dependencies meta)
    (let ((ds (append 
	      (deps 'depends meta) 
	      (deps 'needs meta)
	      (if *run-tests* (deps 'test-depends meta) '()))))
      (let loop ((deps ds) (missing '()) (upgrade '()))
	(if (null? deps)
	    (values (reverse missing) (reverse upgrade))
	    (let ((dep (car deps))
		  (rest (cdr deps)))
	      (cond ((or (symbol? dep) (string? dep))
		     (loop rest
			   (if (or (eq? 'chicken dep)
				   (equal? "chicken" dep)
				   (extension-information dep))
			       missing
			       (cons (->string dep) missing))
			   upgrade))
		    ((and (list? dep) (= 2 (length dep))
			  (or (string? (car dep)) (symbol? (car dep))))
		     (let ((info (extension-information (car dep))))
		       (if info
			   (let ((v (assq 'version info)))
			     (cond ((not v) 
				    (warning "installed extension has unknown version - assuming it is outdated" 
					     (car dep))
				    (loop rest missing 
					  (alist-cons 
					   (->string (car dep))
					   (->string (cadr dep))
					   upgrade)))
				   ((version>=? (->string (cadr dep)) (->string (cadr v)))
				    (loop rest missing
					  (alist-cons
					   (->string (car dep)) (->string (cadr dep))
					   upgrade)))
				   (else (loop rest missing upgrade)))))))
		    (else 
		     (warning "invalid dependency syntax in extension meta information" dep)
		     (loop rest missing upgrade))))))))

    (define *eggs+dirs* '())
    (define *checked* '())

    (define (retrieve eggs)
      (print "retrieving ...")
      (for-each
       (lambda (egg)
	 (unless (assoc egg *eggs+dirs*)
	   (let* ((name (if (pair? egg) (car egg) egg))
		  (version (and (pair? egg) (cdr egg)))
		  (dir (retrieve-extension 
			name *default-transport* *default-location*
			version #f 
			(and *retrieve-only* (current-directory))
			*username* *password*)))
	     (unless dir
	       (error "extension or version not found"))
	     (print " " name " located at " dir)
	     (set! *eggs+dirs* (alist-cons name dir *eggs+dirs*)))))
       eggs)
      (unless *retrieve-only*
	(for-each
	 (lambda (e+d)
	   (unless (member (car e+d) *checked*)
	     (set! *checked* (cons (car e+d) *checked*))
	     (let ((mfile (make-pathname (cdr e+d) (car e+d) "meta")))
	       (cond ((file-exists? mfile)
		      (let ((meta (with-input-from-file mfile read)))
			(print "checking dependencies for `" (car e+d) "' ...")
			(let-values (((missing upgrade) (outdated-dependencies meta)))
			  (when (pair? missing)
			    (print " missing: " (string-intersperse missing ", "))
			    (retrieve missing))
			  (when (and (pair? upgrade)
				     (or *force*
					 (yes-or-no? 
					  (string-concatenate
					   (append
					    (list "The following installed extensions are outdated, because `"
						  (car e+d) "' requires later versions:\n")
					    (map (lambda (e)
						   (sprintf 
						    "  ~a (~a -> ~a)~%"
						    (car e) 
						    (let ((v (assq 'version (extension-information (car e)))))
						      (if v (cadr v) "???"))
						    (cdr e)))
						 upgrade)
					    '("\nDo you want to replace the existing extensions?")))
					  "no") ) )
			    (let ((ueggs (unzip1 upgrade)))
			      (print " upgrade: " (string-intersperse ueggs ", "))
			      (for-each 
			       (lambda (e)
				 (print "removing previously installed extension `" e "' ...")
				 (remove-extension e *sudo*) )
			       ueggs)
			      (retrieve ueggs))))))
		     (else
		      (warning 
		       (string-append 
			"extension `" (car e+d) "' has no .meta file "
			"- assuming it has no dependencies")))))))
	 *eggs+dirs*)))

    (define (install eggs)
      (retrieve eggs)
      (unless *retrieve-only*
	(for-each ; we assume the order reflects the dependency tree...
	 (lambda (e+d)
	   (print "installing " (car e+d) " ...")
	   (print "changing current directory to " (cdr e+d))
	   (parameterize ((current-directory (cdr e+d)))
	     (let ((cmd (sprintf
			 "~a/csi -bnq -e \"(require-library setup-api)\" -e \"(import setup-api)\" ~a ~a ~a ~a ~a ~a"
			 *program-path*
			 (if *sudo* "-e \"(sudo-install #t)\"" "")
			 (if *keep* "-e \"(keep-intermediates #t)\"" "")
			 (if *no-install* "-e \"(setup-install-flag #f)\"" "")
			 (if *host-extension* "-e \"(host-extension #t)\"" "")
			 (if *prefix* 
			     (sprintf "-e \"(installation-prefix \\\"~a\\\")\"" *prefix*)
			     "")
			 (make-pathname (cdr e+d) (car e+d) "setup"))))
	       (system* cmd))
	     (when (and *run-tests*
			(file-exists? "tests")
			(directory? "tests")
			(file-exists? "tests/run.scm") )
	       (current-directory "tests")
	       (let ((cmd (sprintf "~a/csi -s run.scm ~a" *program-path* (car e+d))))
		 (print cmd)
		 (system* cmd)))))
	 *eggs+dirs*)))

  (define (cleanup)
    (unless *keep*
      (and-let* ((tmpdir (temporary-directory)))
	(remove-directory tmpdir))))

  (define (usage code)
    (print #<#EOF
usage: chicken-install [OPTION | EXTENSION[:VERSION]] ...

  -h   -help                    show this message and exit
  -v   -version                 show version and exit
       -force                   don't ask, install even if versions don't match
  -k   -keep                    keep temporary files
  -l   -location LOCATION       install from given location instead of default (#{*default-location*})
  -t   -transport TRANSPORT     use given transport instead of default (#{*default-transport*})
  -s   -sudo                    use sudo(1) for installing or removing files
  -r   -retrieve                only retrieve egg into current directory, don't install
  -n   -no-install              do not install, just build (implies `-keep')
  -p   -prefix PREFIX           change installation prefix to PREFIX
       -host-extension          when cross-compiling, compile extension for host
       -test                    run included test-cases, if available
       -username USER           set username for transports that require this
       -password PASS           set password for transports that require this
EOF
);|
    (exit code))

  (define *short-options* '(#\h #\k #\l #\t #\s #\p #\r #\n #\v))

  (define (main args)
    (let ((defaults (load-defaults)))
      (let loop ((args args) (eggs '()))
	(cond ((null? args)
	       (when (null? eggs)
		 (let ((setups (glob "*.setup")))
		   (cond ((pair? setups)
			  (set! *eggs+dirs*
			    (append
			     (map (lambda (s) (cons (pathname-file s) ".")) setups)
			     *eggs+dirs*)))
			 (else
			  (print "no setup-scripts to process")
			  (exit 1))) ) )
	       (unless defaults
		 (unless *default-transport* 
		   (error "no default transport defined - please use `-transport' option"))
		 (unless *default-location* 
		   (error "no default location defined - please use `-location' option")))
	       (install (reverse eggs)))
	      (else
	       (let ((arg (car args)))
		 (cond ((or (string=? arg "-help") 
			    (string=? arg "-h")
			    (string=? arg "--help"))
			(usage 0))
		       ((string=? arg "-force")
			(set! *force* #t)
			(loop (cdr args) eggs))
		       ((or (string=? arg "-k") (string=? arg "-keep"))
			(set! *keep* #t)
			(loop (cdr args) eggs))
		       ((or (string=? arg "-s") (string=? arg "-sudo"))
			(set! *sudo* #t)
			(loop (cdr args) eggs))
		       ((or (string=? arg "-r") (string=? arg "-retrieve"))
			(set! *retrieve-only* #t)
			(loop (cdr args) eggs))
		       ((or (string=? arg "-l") (string=? arg "-location"))
			(unless (pair? (cdr args)) (usage 1))
			(set! *default-location* (cadr args))
			(loop (cddr args) eggs))
		       ((or (string=? arg "-t") (string=? arg "-transport"))
			(unless (pair? (cdr args)) (usage 1))
			(set! *default-transport* (string->symbol (cadr args)))
			(loop (cddr args) eggs))
		       ((or (string=? arg "-p") (string=? arg "-prefix"))
			(unless (pair? (cdr args)) (usage 1))
			(set! *prefix* (cadr args))
			(loop (cddr args) eggs))
		       ((or (string=? arg "-n") (string=? arg "-no-install"))
			(set! *keep* #t)
			(set! *no-install* #t)
			(loop (cdr args) eggs))
		       ((or (string=? arg "-v") (string=? arg "-version"))
			(print (chicken-version))
			(exit 0))
		       ((string=? "-test" arg)
			(set! *run-tests* #t)
			(loop (cdr args) eggs))
		       ((string=? "-host-extension" arg)
			(set! *host-extension* #t)
			(loop (cdr args) eggs))
		       ((string=? "-username" arg)
			(unless (pair? (cdr args)) (usage 1))
			(set! *username* (cadr args))
			(loop (cddr args) eggs))
		       ((string=? "-password" arg)
			(unless (pair? (cdr args)) (usage 1))
			(set! *password* (cadr args))
			(loop (cddr args) eggs))
		       ((and (positive? (string-length arg))
			     (char=? #\- (string-ref arg 0)))
			(if (> (string-length arg) 2)
			    (let ((sos (string->list (substring arg 1))))
			      (if (null? (lset-intersection eq? *short-options* sos))
				  (loop (append (map (cut string #\- <>) sos) (cdr args)) eggs)
				  (usage 1)))
			    (usage 1)))
		       ((equal? "setup" (pathname-extension arg))
			(let ((egg (pathname-file arg)))
			  (set! *eggs+dirs*
			    (alist-cons
			     egg
			     (let ((dir (pathname-directory arg)))
			       (if dir
				   (if (absolute-pathname? dir)
				       dir
				       (make-pathname (current-directory) dir) )
				   (current-directory)))
			     *eggs+dirs*))
			  (loop (cdr args) (cons egg eggs))))
		       ((string-match "([^:]+):(.+)" arg) =>
			(lambda (m)
			  (loop (cdr args) (alist-cons (cadr m) (caddr m) eggs))))
		       (else (loop (cdr args) (cons arg eggs))))))))))

  (handle-exceptions ex
      (begin
	(print-error-message ex (current-error-port))
	(cleanup)
	(exit 1))
    (main (command-line-arguments))
    (cleanup))
  
)
