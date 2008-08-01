;;;; setup-utils.scm


(require-library regex utils ports tcp extras posix 
		 srfi-1 srfi-13 data-structures)


(module setup-utils (version>=?
		     http-fetch
		     create-temporary-directory
		     remove-directory
		     outdated-dependencies
		     yes-or-no?)
  
  (import scheme chicken)
  (import regex utils ports tcp extras posix srfi-1 srfi-13 
	  data-structures)

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
      (fprintf out "GET ~a HTTP/1.1\r\nConnection: close\r\nUser-Agent: chicken-install ~a\r\nAccept: */*\r\nContent-length: 0\r\n\r\n"
	       loc (chicken-version))
      (close-output-port out)
      (let loop ((files '()))
	(let ((name (read in)))
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
		 (loop (cons name files))))))))

  (define (create-temporary-directory)
    (let ((dir (or (getenv "TMPDIR") (getenv "TEMP") (getenv "TMP") "/tmp")))
      (let loop ()
	(let* ((n (##sys#fudge 16))	; current milliseconds
	       (pn (make-pathname dir (string-append "setup-" (number->string n 16) "tmp"))))
	  (cond ((file-exists? pn) (loop))
		(else (create-directory pn) pn))))))

  (define (remove-directory dir)
    (let walk ((dir dir))
      (let ((files (directory dir #t)))
	(for-each
	 (lambda (f)
	   (unless (or (string=? "." f) (string=? ".." f))
	     (let ((p (make-pathname dir f)))
	       (if (directory? p)
		   (walk p) 
		   (delete-file p)))))
	 files)
	(delete-directory dir))))

  (define (outdated-dependencies meta)
    (let ((a (or (assq 'depends meta) 
		 (assq 'needs meta)) ) )
      (let loop ((deps (if a (cdr a) '())) (missing '()) (upgrade '()))
	(if (null? deps)
	    (values (reverse missing) (reverse upgrade))
	    (let ((dep (car deps))
		  (rest (cdr deps)))
	      (cond ((or (symbol? dep) (string? dep))
		     (loop rest
			   (if (extension-information dep)
			       missing
			       (cons dep missing))
			   upgrade))
		    ((and (list? dep) (= 2 (length dep))
			  (or (string? (car dep)) (symbol? (car dep))))
		     (let ((info (extension-information (car dep))))
		       (if info
			   (let ((v (assq 'version info)))
			     (cond ((not v) 
				    (warning "installed extension has unknown version - assuming it is outdated" 
					     (car dep))
				    (loop rest missing (cons (car dep) upgrade)))
				   ((version>=? (->string (cadr dep)) (->string (cadr v)))
				    (loop rest missing (cons (car dep) upgrade)))
				   (else (loop rest missing upgrade)))))))
		    (else 
		     (warning "invalid dependency syntax in extension meta information" dep)
		     (loop rest missing upgrade))))))))

  (define (yes-or-no? str #!key default (abort (cut signal 'aborted)))
    (let loop ()
      (printf "~%~A (yes/no/abort) " str)
      (when default (printf "[~A] " default))
      (flush-output)
      (let ((ln (read-line)))
	(cond ((eof-object? ln) (set! ln "abort"))
	      ((and default (string=? "" ln)) (set! ln default)) )
	(cond ((string-ci=? "yes" ln) #t)
	      ((string-ci=? "no" ln) #f)
	      ((string-ci=? "abort" ln) (abort))
	      (else
	       (printf "~%Please enter \"yes\", \"no\" or \"abort\".~%")
	       (loop) ) ) ) ) )

)
