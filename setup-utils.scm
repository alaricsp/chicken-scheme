;;;; setup-utils.scm


(require-library regex utils ports tcp extras posix srfi-1)


(module setup-utils (version>=?
		     http-fetch
		     create-temporary-directory
		     outdated-dependencies)
  
  (import scheme chicken)
  (import regex utils ports tcp extras posix srfi-1)

  (define (version>=? v1 v2)
    (define (version->list v)
      (map (lambda (x) (or (string->number x) x))
	   (string-split-fields "[-\\._]" v #:infix)))
    (let loop ((p1 (version->list v1))
	       (p2 (version->list v2)))
      (cond ((null? p1) (null? p2))
	    ((null? p2))
	    ((number? (car p1))
	     (and (if (number? (car p2))
		      (>= (car p1) (car p2))
		      (string>=? (number->string (car p1)) (car p2)))
		  (loop (cdr p1) (cdr p2))))
	    ((number? (car p2))
	     (and (string>=? (car p1) (number->string (car p2)))
		  (loop (cdr p1) (cdr p2))))
	    ((string>=? (car p1) (car p2)) (loop (cdr p1) (cdr p2)))
	    (else #f))))

  (define (http-fetch host port loc dest)
    (let-values (((in out) (tcp-connect host port)))
      (fprintf out "GET ~a HTTP/1.1\r\nConnection: close\r\nAccept: */*\r\nContent-length: 0\r\n\r\n"
	       loc)
      (close-output-port out)
      (let loop ((files '()))
	(let ((name (read in)))
	  (cond ((and (pair? name) (eq? 'error (car name)))
		 (apply error (cdr name)))
	        ((or (eof-object? name) (not name))
		 (close-input-port in)
		 (reverse files) )
		(else
		 (let* ((size (read in))
			(data (read-string size in)) )
		   (with-output-to-file (make-pathname dest name)
		     (lambda ()
		       (display data) ) ) )
		 (loop (cons name files))))))))

  (define (create-temporary-directory)
    (let ((dir (or (getenv "TMPDIR") (getenv "TEMP") (getenv "TMP") "/tmp")))
      (let loop ()
	(let* ((n (##sys#fudge 16))	; current milliseconds
	       (pn (make-pathname dir (string-append "setup-" (number->string n 16) "tmp"))))
	  (cond ((file-exists? pn) (loop))
		(else (create-directory pn) pn))))))

  (define (outdated-dependencies meta)
    (filter
     (lambda (dep)
       (cond ((or (symbol? dep) (string? dep))
	      (not (extension-information dep)))
	     ((and (list? dep) (= 2 (length dep))
		   (or (string? (car dep)) (symbol? (car dep))))
	      (let ((info (extension-information (car dep))))
		(or (not info)
		    (let ((v (assq 'version info)))
		      (cond ((not v) 
			     (warning "installed extension has unknown version - assuming it is outdated" 
				      (car dep))
			     #t)
			    (else (version>=? (->string (cadr dep)) (->string (cadr v)))))))))))
     (or (assq 'depends meta) 
	 (assq 'needs meta)
	 '())))

)
