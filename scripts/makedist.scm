;;;; makedist.scm - Make distribution tarballs


(define *release* #f)

(load-relative "tools.scm")

(set! *verbose* #t)

(define BUILDVERSION (with-input-from-file "buildversion" read))

(define *platform* 
  (let ((sv (symbol->string (software-version))))
    (if (string-match ".*bsd" sv) "bsd" sv)))

(define (release full?)
  (let* ((files (read-lines "distribution/manifest"))
	 (distname (conc "chicken-" BUILDVERSION)) 
	 (distfiles (map (cut prefix distname <>) files)) 
	 (tgz (conc distname ".tar.gz")))
    (run (rm -fr ,distname ,tgz))
    (run (mkdir -p ,distname
		,@(map (cut path distname <>) 
		       (delete-duplicates (filter-map prefix files) string=?))))
    (let ((missing '()))
      (for-each
       (lambda (f)
	 (if (-e f)
	     (run (cp -p ,f ,(path distname f))) 
	     (set! f (cons f missing))))
       files)
      (unless (null? missing)
	(warning "files missing" missing) ) )
    (run (tar cfz ,(conc distname ".tar.gz") ,distname))
    (when full?
      (run (cp ,tgz site)) )
    (run (rm -fr ,distname)) ) )

(define *makeargs*
  (simple-args
   (command-line-arguments)
   (lambda _
     (print "usage: makedist [--release] [--test] MAKEOPTION ...")
     (exit 1))) )

(run (gmake -f ,(conc "Makefile." *platform*) distfiles ,@*makeargs*))
(release *release*)
