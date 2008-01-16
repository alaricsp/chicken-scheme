;;;; setversion.scm - Bump version-number

(use srfi-1 utils)

(define buildversion (->string (car (read-file "buildversion"))))
(define buildbinaryversion (car (read-file "buildbinaryversion")))

(define files '("README" "manual/The User's Manual"))

(define (patch which rx subst)
  (match which
    ((from to) 
     (print "patching " from " ...")
     (with-output-to-file to
       (lambda ()
	 (with-input-from-file from
	   (lambda ()
	     (let loop ()
	       (let ((ln (read-line)))
		 (unless (eof-object? ln)
		   (write-line (string-substitute rx subst ln #t)) 
		   (loop) ) ) ) ) ) ) ) )
    (both
     (let ((tmp (create-temporary-file)))
       (patch (list both tmp) rx subst)
       (system* "mv ~S ~S" tmp both ) ) ) ) )

(define (parse-version v)
  (string-match "(\\d+)\\.(\\d+)\\.(\\d+)(.*)" v) )

(define (main args)
  (cond ((member "-set" args) =>
	 (lambda (a) (set! buildversion (cadr a))) )
	((not (member "-noinc" args))
	 (match (parse-version buildversion)
	   ((_ maj min pl huh) 
	    (set! buildversion (conc maj "." min "." (add1 (string->number pl)) huh)) ) ) ) )
  (with-output-to-file "buildversion" (cut display buildversion))
  (with-output-to-file "version.scm" 
    (lambda ()
      (write `(define-constant +build-version+ ,buildversion))
      (newline) ) )
  (system* "cat version.scm")
  (let ([vstr (sprintf "version ~A" buildversion)])
    (for-each (cut patch <> "version [0-9][-.0-9a-zA-Z]+" vstr) files) )
  (with-output-to-file "DONE" (cut print "- version is " buildversion))
  0)

(main (command-line-arguments))
