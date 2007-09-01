;;;; makedist.scm - Make distribution tarballs


(define *release* #f)
(define *test* #f)

(load-relative "build.scm")

(set! *verbose* #t)

(define BUILDVERSION (with-input-from-file "buildversion" read))

(define *platform* (symbol->string (software-version)))

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

(run (make -f ,(conc "Makefile." *platform*) distfiles ,@*makeargs*))
(release *release*)

(when *test*
  (let* ((bdir "/tmp/test-dist-build")
	 (sdir "/tmp/test-dist-build/chicken-*")
	 (bbdir "/tmp/test-dist-build/build")
	 (idir "/tmp/test-dist-build/inst")
	 (tgz (conc "chicken-" BUILDVERSION ".tar.gz")) )
    (run (mkdir -p ,bdir))
    (run (tar xfz ,(conc "site/" tgz) -C ,bdir))
    (run (cd ,sdir ";" make -f ,(conc "Makefile." *platform*) 
	     install (conc "PREFIX=" bdir "/inst")))
    (run (cd ,idir ";" bin/chicken-setup -dv bloom-filter))
    (run (cd ,idir ";" "CSI_OPTIONS= echo ,r |" bin/csi -n -R bloom-filter))
    (run (rm -fr ,sdir ,idir)) ) )
