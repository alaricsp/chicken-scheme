#!/bin/sh
#| release.scm - Pack files for full Chicken distribution - felix -*- Scheme -*-
exec chicken-setup -script $0 "$@"
|#

(define buildversion (car (read-file "buildversion")))
(define buildnumber (car (read-file "buildnumber")))

(define packdir (sprintf "chicken-~A.~A" buildversion buildnumber))
(define zipfile (string-append packdir ".zip"))
(define tarfile (string-append packdir ".tar.gz"))

(define scmfiles 
  (string-split
   "chicken.scm support.scm easyffi.scm compiler.scm optimizer.scm c-platform.scm c-backend.scm build.scm
	 eval.scm extras.scm library.scm tcp.scm 
	 srfi-18.scm srfi-1.scm match.scm lolevel.scm batch-driver.scm scheduler.scm stub.scm
	 csi.scm srfi-4.scm regex.scm pregexp.scm pcre.scm posix.scm parameters.scm tweaks.scm posixwin.scm
	 chicken-more-macros.scm banner.scm tinyclos.scm utils.scm partition.scm
         chicken-ffi-macros.scm
	 srfi-13.scm srfi-14.scm nsample.scm profiler.scm
	 chicken-profile.scm regex-common.scm
         silex.scm chicken-setup.scm") )

(define cfiles
  (string-split 
   "runtime.c eval.c extras.c library.c srfi-1.c match.c chicken.c support.c easyffi.c
       compiler.c optimizer.c c-platform.c c-backend.c srfi-4.c regex.c pcre.c srfi-18.c 
       batch-driver.c tinyclos.c utinyclos.c profiler.c partition.c
       srfi-13.c srfi-14.c posix.c lolevel.c nsample.c pregexp.c posixwin.c stub.c
       ueval.c uextras.c ulibrary.c usrfi-1.c usrfi-4.c uregex.c upregexp.c upcre.c
       usrfi-18.c usrfi-13.c usrfi-14.c uposix.c tcp.c utcp.c uposixwin.c
       utils.c uutils.c scheduler.c ulolevel.c") )

(define otherfiles
  (string-split
   "chicken.rc chicken.ico csibatch.bat easyffi.l easyffi.l.silex makefile.vc
      chicken.h chicken.spec.in hen.el win-install.bat *.exports CMakeLists.txt chicken-defaults.replace.cmake csc.replace.cmake") )

(define benchfiles
  (string-split
"benchmarks/cscbench.scm
benchmarks/plists
benchmarks/boyer.scm
benchmarks/browse.scm
benchmarks/conform.scm
benchmarks/cpstak.scm
benchmarks/ctak.scm
benchmarks/dderiv.scm
benchmarks/deriv.scm
benchmarks/destructive.scm
benchmarks/div-iter.scm
benchmarks/div-rec.scm
benchmarks/dynamic.scm
benchmarks/earley.scm
benchmarks/fft.scm
benchmarks/fib.scm
benchmarks/fibc.scm
benchmarks/fprint.scm
benchmarks/fread.scm
benchmarks/hanoi.scm
benchmarks/lattice.scm
benchmarks/maze.scm
benchmarks/nqueens.scm
benchmarks/puzzle.scm
benchmarks/scheme.scm
benchmarks/tak.scm
benchmarks/takl.scm
benchmarks/takr.scm
benchmarks/traverse.scm
benchmarks/travinit.scm
benchmarks/triangl.scm") )

(define manfiles '("*.1"))

(define docfiles
  (string-split
   "ChangeLog ChangeLog.0-20040412 chicken.texi chicken.pdf LICENSE README TASKS NEWS chicken.html
    chicken.info* vctk-install.txt") )

(define distfiles
  (string-split
   "INSTALL Makefile.am Makefile.in aclocal.m4 config.guess chicken-config.h.in config.sub 
    configure configure.in install-sh ltmain.sh missing mkinstalldirs csc.scm.in") )

(define files (append scmfiles cfiles otherfiles distfiles docfiles))

(define path make-pathname)

(define (main args)
  (when (member "-help" (cdr args))
    (print "usage: chicken-setup -s release.scm [-help] [-test] [-release] [-snapshot]")
    (exit 0) )
  (let* ([testonly (member "-test" (cdr args))]
	 [dest (if testonly "chicken-current.tgz" tarfile)] )
    (when testonly (set! packdir "chicken-current-dist"))
    (print "completing make...")
    (print "pdf -> " (system "make pdf"))
    (run (make all regex.c pregexp.c uregex.c upregexp.c posixwin.c uposixwin.c chicken.html))
    (print "creating archive directory...")
    (run (rm -fr ,packdir)
	 (mkdir ,packdir)
	 (mkdir ,(path packdir "benchmarks")))
    (print "copying distribution files...")
    (run (cp -a ,@files ,packdir)
	 (cp -a ,@benchfiles ,(path packdir "benchmarks"))
	 (cp -a ,@manfiles ,packdir) )
    (print "creating compressed archives...")
    (run (rm -f chicken-*.zip chicken-*.tar.gz))
    (unless testonly
      (run (zip -r -q ,zipfile ,packdir))
      (change-directory "testbuild")
      (run (makeinfo --css-include=../chicken.css --html ../chicken.texi))
      (run (zip -r -q chicken-manual-html.zip chicken))
      (change-directory "..") )
    (run (tar cfz ,dest ,packdir))
    (when (member "-snapshot" (cdr args))
      (run (mkdir -p site)
	   (rm -fr site/*) )
      (print "copying tarball and ChangeLog to site directory...")
      (run (cp ,dest site)
	   (darcs changes >ChangeLog)
	   (cp ChangeLog site/ChangeLog.txt) ) )
    (when (member "-release" (cdr args))
      (print "copying files to site directory...")
      (run (cp ,zipfile site)
	   (darcs changes >ChangeLog)
	   (cp ChangeLog site/ChangeLog.txt)
	   (cp ,dest site)
	   (cp testbuild/chicken-manual-html.zip site)
	   (cp chicken.pdf site/chicken.pdf)
	   (mkdir -p site/manual)
	   (cp -av testbuild/chicken/* site/manual) ) )
    (print "cleaning up...")
    (run (rm -fr ,packdir))
    0) )

(main (cons #f (command-line-arguments)))
