;;;; setup-download.scm


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
    (let* ((files
	    (with-input-from-pipe 
	     (sprintf "svn ls -R '~a/~a'" repo egg)
	     read-lines))
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
	   (cmd (sprintf "svn co '~a/~a/~a' '~a'" repo egg filedir tmpdir)))
      (print "  " cmd)
      (system* cmd)
      tmpdir))

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
