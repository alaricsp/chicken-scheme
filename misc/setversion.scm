;;;; setversion.scm - Bump version-number

(use utils)

(define buildversion (->string (car (read-file "buildversion"))))
(define buildbinaryversion (car (read-file "buildbinaryversion")))

(define files '("README"))

(define (patch which rx subst)
  (match which
    ((from to) 
     (printf "patching ~a ...~%" from)
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
       (system* "mv ~A ~A" tmp both ) ) ) ) )

(define (main args)
  (cond ((member "-set" args) =>
	 (lambda (a) (set! buildversion (cadr a))) )
	((not (member "-noinc" args))
	 (set! buildversion (number->string (+ (string->number buildversion) 0.001))) ) )
  (with-output-to-file "buildversion" (cut display buildversion))
  (with-output-to-file "version.scm" 
    (lambda ()
      (write `(define-constant +build-version+ ,buildversion))
      (newline) ) )
  (system* "cat version.scm")
  (let ([vstr (sprintf "Version ~A" buildversion)])
    (for-each (cut patch <> "Version [-.0-9a-zA-Z]+" vstr) files) )
  (with-output-to-file "DONE" (cut print "- version is " buildversion))
  0)

(main (command-line-arguments))
