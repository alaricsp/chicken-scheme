;;;; chicken-setup
;
; Copyright (c) 2000-2007, Felix L. Winkelmann
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
;
; Send bugs, suggestions and ideas to: 
;
; felix@call-with-current-continuation.org
;
; Felix L. Winkelmann
; Unter den Gleichen 1
; 37130 Gleichen
; Germany


(declare
  (run-time-macros)
  (uses srfi-1 regex utils posix tcp match srfi-18 srfi-13)
  (export move-file run:execute make/proc uninstall-extension
	  install-extension install-program install-script
	  setup-verbose-flag setup-install-flag installation-prefix
	  find-library find-header program-path remove-file* patch
	  yes-or-no? setup-build-directory setup-root-directory
	  create-directory test-compile try-compile copy-file run-verbose
	  required-chicken-version required-extension-version
	  cross-chicken) )

#>
#ifndef C_USE_C_DEFAULTS
# define C_INSTALL_BIN_HOME   NULL
# ifdef _MSC_VER
#  define C_INSTALL_CC                "cl"
# else
#  ifdef __GNUC__
#   define C_INSTALL_CC                "gcc"
#  else
#   define C_INSTALL_CC                "cc"
#  endif
# endif
#endif

#ifdef _WIN32
/* It is an error to include <windows.h> prematurely.  For instance,
 * <winsock2.h> must be included before <windows.h> */ 
# include <windows.h>
static void create_directory(char *pathname)
{
  CreateDirectory(pathname, NULL);
}
#else
static void create_directory(char *pathname) {}
#endif

#ifndef C_TARGET_CC
# define C_TARGET_CC  C_INSTALL_CC
#endif

#ifndef C_TARGET_CXX
# define C_TARGET_CXX  C_INSTALL_CXX
#endif

#ifndef C_TARGET_CFLAGS
# define C_TARGET_CFLAGS  ""
#endif

#ifndef C_TARGET_LFLAGS
# define C_TARGET_LFLAGS  ""
#endif
<#


(define-constant setup-file-extension "setup-info")
(define-constant remote-repository-name "repository")
(define-constant installed-executables '("chicken" "csc" "csi" "chicken-setup" "chicken-profile"))


(include "chicken-more-macros.scm")


(define-constant long-options
  '("-help" "-uninstall" "-list" "-run" "-repository" "-program-path" "-version" "-script" "-check"
    "-fetch" "-host" "-proxy" "-keep" "-verbose" "-csc-option" "-dont-ask" "-no-install" "-docindex" "-eval"
    "-debug" "-ls" "-release" "-test" "-fetch-tree" "-tree" "-svn" "-local" "-destdir" "-revision") )

(define-constant short-options
  '(#\h #\u #\l #\r #\R #\P #\V #\s #\C #\f #\H #\p #\k #\v #\c #\d #\n #\i #\e #\D #f #f #\t #f #f #f #f #f #f) )


(define *install-bin-path* 
  (or (and-let* ((p (getenv "CHICKEN_PREFIX")))
	(make-pathname p "bin") )
      (getenv "CHICKEN_HOME")
      (foreign-value "C_INSTALL_BIN_HOME" c-string) ) )

(define *cc* (foreign-value "C_TARGET_CC" c-string))
(define *cxx* (foreign-value "C_TARGET_CXX" c-string))
(define *target-cflags* (foreign-value "C_TARGET_CFLAGS" c-string))
(define *target-lflags* (foreign-value "C_TARGET_LFLAGS" c-string))

(define *windows*
  (and (eq? (software-type) 'windows) 
       (build-platform) ) )

(define *windows-shell* (memq *windows* '(msvc mingw32)))
(define *msvc* (eq? *windows* 'msvc))
(define *debug* #f)

(register-feature! 'chicken-setup)

(define *copy-command* (if *windows-shell* 'copy "cp -r"))
(define *remove-command* (if *windows-shell* "del /Q /S" "rm -fr"))
(define *move-command* (if *windows-shell* 'move 'mv))

(define (cross-chicken) (##sys#fudge 39))

(define create-directory
  (let ()
    (define (verb dir)
      (when (setup-verbose-flag) (printf "  creating directory `~a'~%~!" dir)) )
    (if *windows-shell*
	(lambda (dir)
	  (verb dir)
	  ((foreign-lambda void "create_directory" c-string) dir) ) 
	(lambda (dir)
	  (verb dir)
	  (system* "mkdir -p ~a" (quotewrap dir) ) ) ) ) )

(define (quotewrap str)
  (if (string-any char-whitespace? str)
      (string-append "\"" str "\"") 
      str) )

(define setup-root-directory (make-parameter #f))
(define setup-build-directory (make-parameter (current-directory)))
(define setup-verbose-flag (make-parameter #f))
(define setup-install-flag (make-parameter #t))

(define *fetch-only* #f)
(define *temporary-directory* #f)
(define *tmpdir-created* #f)
(define *keep-stuff* #f)
(define *csc-options* '())
(define *abort-hook* #f)
(define *dont-ask* #f)
(define *rebuild-doc-index* #f)
(define *check-repository* #f)
(define *repository-tree* #f)
(define *last-decent-host* #f)
(define *proxy-host* #f)
(define *proxy-port* #f)
(define *example-directory* (make-pathname (chicken-home) "examples"))
(define *base-directory* (current-directory))
(define *fetch-tree-only* #f)
(define *svn-repository* #f)
(define *local-repository* #f)
(define *destdir* #f)
(define *repository-hosts* '(("www.call-with-current-continuation.org" "eggs" 80)))
(define *revision* #f)
(define *repository-tree-downloaded* #f)


(define (abort-setup)
  (*abort-hook* #f) )

(define (yes-or-no? str . default)
  (let ((def (:optional default #f)))
    (let loop ()
      (printf "~%~A (yes/no/abort) " str)
      (when def (printf "[~A] " def))
      (flush-output)
      (let ((ln (read-line)))
	(cond ((eof-object? ln) (set! ln "abort"))
	      ((and def (string=? "" ln)) (set! ln def)) )
	(cond ((string-ci=? "yes" ln) #t)
	      ((string-ci=? "no" ln) #f)
	      ((string-ci=? "abort" ln) (abort-setup))
	      (else
	       (printf "~%Please enter \"yes\", \"no\" or \"abort\".~%")
	       (loop) ) ) ) ) ) )

(define (patch which rx subst)
  (when (setup-verbose-flag) (printf "patching ~A ...~%" which))
  (match which
    ((from to) 
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
       (system* "~A ~A ~A" *move-command* (quotewrap tmp) (quotewrap both) ) ) ) ) )

(define run-verbose (make-parameter #t))

(define (fixpath prg)
  (cond ((string=? prg "csc")
	 (string-intersperse 
	  (cons* (quotewrap (make-pathname *install-bin-path* prg))
		 "-feature" "compiling-extension"
		 *csc-options*) 
	  " ") )
	((member prg installed-executables) (quotewrap (make-pathname *install-bin-path* prg)))
	(else prg) ) )

(define (fixmaketarget file)
  (if (and (equal? "so" (pathname-extension file))
	   (not (string=? "so" ##sys#load-dynamic-extension)) )
      (pathname-replace-extension file ##sys#load-dynamic-extension)
      file) )

(define (run:execute explist)
  (define (smooth lst)
    (let ((slst (map ->string lst)))
      (string-intersperse (cons (fixpath (car slst)) (cdr slst)) " ") ) )
  (for-each
   (lambda (cmd)
     (when (run-verbose) (printf "  ~A~%~!" cmd))
     (system* "~a" cmd) )
   (map smooth explist) ) )

(define-macro (run . explist)
  `(run:execute (list ,@(map (lambda (x) (list 'quasiquote x)) explist))) )

(define-macro (compile . explist)
  `(run (csc ,@explist) ) )

(define (make:find-matching-line str spec)
  (let ((match? (lambda (s) (string=? s str))))
    (let loop ((lines spec))
      (cond
       ((null? lines) #f)
       (else (let* ((line (car lines))
		    (names (if (string? (car line))
			       (list (car line))
			       (car line))))
	       (if (ormap match? names)
		   line
		   (loop (cdr lines)))))))))

(define (make:form-error s p) (error (sprintf "~a: ~s" s p)))
(define (make:line-error s p n) (error (sprintf "~a: ~s for line: ~a" s p n)))

(define (make:check-spec spec)
  (and (or (list? spec) (make:form-error "specification is not a list" spec))
       (or (pair? spec) (make:form-error "specification is an empty list" spec))
       (andmap
	(lambda (line)
	  (and (or (and (list? line) (<= 2 (length line) 3))
		   (make:form-error "list is not a list with 2 or 3 parts" line))
	       (or (or (string? (car line))
		       (and (list? (car line))
			    (andmap string? (car line))))
		   (make:form-error "line does not start with a string or list of strings" line))
	       (let ((name (car line)))
		 (or (list? (cadr line))
		     (make:line-error "second part of line is not a list" (cadr line) name)
		     (andmap (lambda (dep)
			       (or (string? dep)
				   (make:form-error "dependency item is not a string" dep)))
			     (cadr line)))
		 (or (null? (cddr line))
		     (procedure? (caddr line))
		     (make:line-error "command part of line is not a thunk" (caddr line) name)))))
	spec)))

(define (make:check-argv argv)
  (or (string? argv)
      (andmap string? argv)
      (error "argument is not a string or string list" argv)))

(define (make:make/proc/helper spec argv)
  (when (vector? argv) (set! argv (vector->list argv)))
  (make:check-spec spec)
  (make:check-argv argv)
  (letrec ((made '())
	   (exn? (condition-predicate 'exn))
	   (exn-message (condition-property-accessor 'exn 'message))
	   (make-file
	    (lambda (s indent)
	      (let* ((line (make:find-matching-line s spec))
		     (s2 (fixmaketarget s)) 
		     (date (and (file-exists? s2)
				(file-modification-time s2))))
		(when (setup-verbose-flag)
		  (printf "make: ~achecking ~a~%" indent s2))
		(if line
		    (let ((deps (cadr line)))
		      (for-each (let ((new-indent (string-append " " indent)))
				  (lambda (d) (make-file d new-indent)))
				deps)
		      (let ((reason
			     (or (not date)
				 (ormap (lambda (dep)
					  (let ((dep2 (fixmaketarget dep)))
					    (unless (file-exists? dep2)
					      (error (sprintf "dependancy ~a was not made~%" dep2)))
					    (and (> (file-modification-time dep2) date)
						 dep2)) )
					deps))))
			(when reason
			  (let ((l (cddr line)))
			    (unless (null? l)
			      (set! made (cons s made))
			      (when (setup-verbose-flag)
				(printf "make: ~amaking ~a~a~%"
					indent
					s2
					(cond
					 ((not date)
					  (string-append " because " s2 " does not exist"))
					 ((string? reason)
					  (string-append " because " reason " changed"))
					 (else
					  (string-append (sprintf " just because (reason: ~a date: ~a)" 
								  reason date)))) ) )
			      (handle-exceptions exn
				  (begin
				    (printf "make: Failed to make ~a: ~a~%"
					    (car line)
					    (if (exn? exn)
						(exn-message exn)
						exn))
				    (signal exn) )
				((car l))))))))
		    (unless date
		      (error (sprintf "don't know how to make ~a" s2))))))))
    (cond
     ((string? argv) (make-file argv ""))
     ((null? argv) (make-file (caar spec) ""))
     (else (for-each (lambda (f) (make-file f "")) argv)))
    (when (setup-verbose-flag)
      (for-each (lambda (item)
		  (printf "make: made ~a~%" item))
	(reverse made)))) )

(define make/proc
  (case-lambda
   ((spec) (make:make/proc/helper spec '()))
   ((spec argv) (make:make/proc/helper spec argv))))

(define-macro (make spec #!optional (argv '()))
  (let ((form-error (lambda (s . p) (apply error s spec p))))
    (and (or (list? spec) (form-error "illegal specification (not a sequence)"))
	 (or (pair? spec) (form-error "empty specification"))
	 (andmap
	  (lambda (line)
	    (and (or (and (list? line) (>= (length line) 2))
		     (form-error "clause does not have at least 2 parts" line))
		 (let ((name (car line)))
		   (or (list? (cadr line))
		       (make:line-error "second part of clause is not a sequence" (cadr line) name)))))
	  spec))
    `(make/proc (list ,@(map (lambda (line)
			       `(list ,(car line)
				      (list ,@(cadr line))
				      ,@(let ((l (cddr line)))
					  (if (null? l)
					      '()
					      `((lambda ()
						  ,@l))))))
			     spec))
		,argv)))

(define (usage)
  (display #<<EOF
usage: chicken-setup [OPTION ...] FILENAME

  -h  -help                      show this text and exit
  -V  -version                   show version of this program and exit
      -release                   show release number and exit
  -R  -repository [PATH]         prints the location of the extension repository
  -u  -uninstall                 remove the following extension from repository
  -H  -host HOSTNAME[:PORT]      specify alternative host for downloading
  -p  -proxy HOSTNAME[:PORT]     connect via proxy
  -l  -list [NAME ...]           list installed extensions or show extension information
  -r  -run FILENAME              load and execute given file
  -P  -program-path DIRECTORY    specify path for installing executables or scripts
  -s  -script FILENAME           execute script with remaining arguments and exit
  -f  -fetch                     only download, don't extract, build or install
  -v  -verbose                   be verbose
  -k  -keep                      don't delete intermediate files
  -c  -csc-option OPTION         pass extra option to csc (if run with `(run (csc ...))')
  -d  -dont-ask                  always download, if asked
  -n  -no-install                don't install generated binaries and support files
  -i  -docindex                  display path for documentation index
  -C  -check                     check for available upgrades
  -e  -eval EXPRESSION           evaluate expression
  -t  -test EXTENSION ...        return success if all given extensions are installed
      -ls EXTENSION              list installed files for extension
      -fetch-tree                download and show repository catalog
      -tree FILENAME             use repository catalog from given file
      -svn URL                   fetch extension from subversion repository
      -local PATH                fetch extension from local filesystem
      -destdir PATH              specify alternative installation prefix
      -revision REV              specify SVN revision for checkout
  --                             ignore all following arguments

  Builds and installs extension libraries.

EOF
  )
  (exit) )

(define (make-setup-info-pathname fn #!optional (rpath (repository-path)))
  (make-pathname rpath fn setup-file-extension) )

(define program-path (make-parameter *install-bin-path*))

(define installation-prefix
  (make-parameter
   (or (getenv "CHICKEN_PREFIX")
       (match (string-match "(.*)/bin/?" *install-bin-path*)
	 ((_ p) p)
	 (_ #f) ) ) ) )

(define (with-ext filename ext)
  (if (and (equal? (pathname-extension filename) ext)
	   (file-exists? filename) )
      filename
      (let ((f2 (pathname-replace-extension filename ext)))
	(and (file-exists? f2) f2) ) ) )

(define (run-setup-script filename)
  (when (setup-verbose-flag) (printf "executing ~A ...~%" filename))
  (load filename) )

(define (write-info id files info)
  (let-values (((exports info) (fix-exports id info)))
    (let ((info `((files ,@files) 
		,@exports
		,@(or (and-let* (*repository-tree*
				 (a (assq id *repository-tree*))
				 (a2 (assq 'date (second a))) )
			`((release ,(second a2))) )
		      '() ) 
		,@info)) )
      (when (setup-verbose-flag) (printf "writing info ~A -> ~S ...~%" id info))
      (let* ((sid (->string id))
	    (setup-file (make-setup-info-pathname sid (repo-path #t)))
	    (write-setup-info (with-output-to-file setup-file
				(cut pp info))))
	(unless *windows-shell* (run (chmod a+r ,setup-file)))
	write-setup-info))))

(define (fix-exports id info)
  (let-values (((einfo oinfo) (partition (lambda (item) (eq? 'exports (car item))) info)))
    (let ((exports
	   (if (pair? einfo)
	       (append-map
		(lambda (eitem)
		  (let loop ((exports (cdr eitem)))
		    (if (null? exports)
			'()
			(let ((x (car exports))
			      (rest (cdr exports)) )
			  (cond ((string? x) (append (read-file x) (loop rest)))
				((symbol? x) (cons x (loop rest)))
				(else (error "invalid export item" x)) ) ) ) ) )
		einfo) 
	       (and-let* ((f (file-exists? (make-pathname #f (->string id) "exports"))))
		 (read-file f) ) ) ) )
      (if exports 
	  (values `((exports ,@exports)) oinfo)
	  (values '() oinfo) ) ) ) )

(define (compute-tmpdir fname)
  (if (equal? "egg-dir" (pathname-extension fname))
      fname
      (string-append fname "-dir") ) )

(define (chdir dir)
  (when (setup-verbose-flag) (printf "changing working directory to `~A'~%" dir))
  (change-directory dir) )

(define (rmtmpdir)
  (unless (string=? (current-directory) *base-directory*)
    (chdir *base-directory*) )
  (when *tmpdir-created*
    (set! *tmpdir-created* #f)
    (unless *keep-stuff*
      (when (setup-verbose-flag) (printf "removing temporary directory `~A'~%" *temporary-directory*))
      (handle-exceptions ex
	  (warning "removal of temporary directory failed" *temporary-directory*)
	(run (,*remove-command* ,(quotewrap *temporary-directory*))) )) ) )

(define (unpack/enter filename)
  (define (testgz fn)
    (with-input-from-file fn
      (lambda () (string=? "\x1f\x8b" (read-string 2))) ) )
  (let ((tmpdir (compute-tmpdir filename)))
    (cond ((file-exists? tmpdir) 
	   (chdir tmpdir)
	   (setup-build-directory (current-directory)) )
	  (else
	   (create-directory tmpdir)
	   (set! *tmpdir-created* #t)
	   (chdir tmpdir)
	   (setup-build-directory (current-directory))
	   (let ((fn2 (string-append "../" filename))
		 (v (setup-verbose-flag)) )
	     (if (testgz fn2)
		 (run (gunzip -c ,fn2 |\|| tar ,(if v 'xvf 'xf) -))
		 (run (tar ,(if v 'xvf 'xf) ,fn2)) ) ) ) )
    (set! *temporary-directory* tmpdir) ) )

(define (copy-file from to)
  (let ((from (if (pair? from) (car from) from))
	(to (if (pair? from) (make-pathname to (cadr from)) to)) )
    (ensure-directory to)
    (run (,*copy-command* ,(quotewrap from) ,(quotewrap to)) ) ) )

(define (move-file from to)
  (let ((from (if (pair? from) (car from) from))
	(to (if (pair? from) (make-pathname to (cadr from)) to)) )
    (ensure-directory to)
    (run (,*move-command* ,(quotewrap from) ,(quotewrap to)) ) ) )

(define (remove-file* dir)
  (run (,*remove-command* ,(quotewrap dir)) ) )

(define (make-dest-pathname path file)
  (match file
    ((from to) (make-dest-pathname path to))
    (_ (if (absolute-pathname? file)
	   file
	   (make-pathname path file) ) ) ) )

(define (check-filelist flist)
  (map (lambda (f)
	 (match f
	   ((? string?) f)
	   (((? string?) (? string?)) f)
	   (((? string? h) . (? string? t)) (list h t))
	   (_ (error "invalid file-specification" f)) ) )
       flist) )

(define (translate-extension f #!optional default)
  (pathname-replace-extension f
   (match (pathname-extension f)
     (#f default)
     ("so" ##sys#load-dynamic-extension)
     ("o" (if *windows-shell* "obj" "o"))
     ("a" (if *windows-shell* "lib" "a"))
     (x x) ) ) )

(define (install-extension id files #!optional (info '()))
  (define (soify f) (translate-extension f))
  (when (setup-install-flag)
    (let* ((files (check-filelist (if (list? files) files (list files))))
	   (rpath (repo-path))
	   (rpathd (repo-path #t))
	   (files (if *windows*
		      (map (lambda (f) 
			     (if (pair? f)
				 (list (soify (car f)) (soify (cadr f)))
				 (soify f)))
			   files)
		      files) ) 
	   (dests (map (lambda (f)
			 (let ((from (if (pair? f) (car f) f))
			       (to (make-dest-pathname rpathd f)) )
			   (when (and (not *windows*) (equal? "so" (pathname-extension to)))
			     (run (,*remove-command* ,to)) )
			   (copy-file from to)
			   (unless *windows-shell*
				   (run (chmod a+r ,to)))
			   (and-let* ((static (assq 'static info)))
			     (when (and (eq? (software-version) 'macosx)
					(equal? (cadr static) from) )
			       (run (ranlib ,to)) ) )
			   (make-dest-pathname rpath f)))
		       files) ) )
      (and-let* ((docs (assq 'documentation info)))
	(let ((docpath (pathname-directory (doc-index #t))))
	  (print "\n* Installing documentation files in " docpath ":")
	  (for-each
	   (lambda (f)
	     (if (file-exists? f)
		 (copy-file f (make-pathname docpath f))
		 (print "Warning: file " f " doesn't seem to exist") )
	     (print "  * " <>) (cdr docs)) 
	   (newline)
	   (set! *rebuild-doc-index* #t)) ) )
      (and-let* ((exs (assq 'examples info)))
	(print "\n* Installing example files in " *example-directory* ":")
	(for-each 
	 (lambda (f)
	   (copy-file f (make-pathname *example-directory* f))
	   (unless *windows-shell*
		   (run (chmod a+rx ,*example-directory*)))

	   (print "  * " f) )
	 (cdr exs))
	(newline) )
      (write-info id dests info) ) ) )

(define (install-program id files #!optional (info '()))
  (define (exify f)
    (translate-extension
     f
     (if *windows-shell* "exe" #f) ) )
  (when (setup-install-flag)
    (let* ((files (check-filelist (if (list? files) files (list files))))
	   (ppath (if *destdir* (make-pathname *destdir* "bin") (program-path)))
	   (files (if *windows*
                      (map (lambda (f)
                             (if (list? f) 
                                 (list (exify (car f)) (exify (cadr f)))
                                 (exify f) ) )
                           files)
                      files) ) 
	   (dests (map (lambda (f)
			 (let ((from (if (pair? f) (car f) f))
			       (to (make-dest-pathname ppath f)) )
			   (copy-file from to) 
			   (unless *windows-shell*
				   (run (chmod a+r ,to)))
			   to) )
		       files) ) )
      (write-info id dests info) ) ) )

(define (install-script id files #!optional (info '()))
  (when (setup-install-flag)
    (let* ((files (check-filelist (if (list? files) files (list files))))
	   (ppath (program-path)) 
	   (pfiles (map (lambda (f)
			  (let ((from (if (pair? f) (car f) f))
				(to (make-dest-pathname ppath f)) )
			    (copy-file from to) 
			    (unless *windows-shell*
				    (run (chmod a+r ,to)))
			    to) )
			files) ) )
      (unless *windows-shell*
	(run (chmod a+rx ,(string-intersperse pfiles " "))) )
      (write-info id pfiles info) ) ) )

(define (uninstall-extension ext)
  (let* ((info (extension-information ext))
	 (files (and info (assq 'files info))) )
    (if files
	(begin
	  (printf "deleting ~A ...~%" ext)
	  (for-each 
	   (lambda (f)
	     (let ((f (if (pair? f) (cadr f) f)))
	       (when (setup-verbose-flag) (printf "  deleting ~A~%" f))
	       (run (,*remove-command* ,(quotewrap f)) ) ) )
	   (cdr files) ) )
	(print "no files to uninstall") )
    (when (assq 'documentation info) (set! *rebuild-doc-index* #t))
    (delete-file* (make-setup-info-pathname (->string ext)))))

(define (repo-path #!optional ddir?)
  (let ((p (if (and ddir? *destdir*)
	       (make-pathname 
		(list *destdir* "lib/chicken") 
		(pathname-file (repository-path))) ; we need the binary-compat. version
	       (repository-path))) )
    (ensure-directory p)
    p) )

(define (ensure-directory path)
  (and-let* ((dir (pathname-directory path)))
    (if (file-exists? dir)
	(unless (directory? dir)
	  (error "can not create directory: a file with the same name already exists") )
	(begin
	  (create-directory dir)
	  (unless *windows-shell*
		  (run (chmod a+x ,dir)))))))

(define (try-compile code #!key c++ (cc (if c++ *cxx* *cc*)) (cflags "") (ldflags "") (verb (setup-verbose-flag)) (compile-only #f))
  (let* ((fname (create-temporary-file "c"))
	 (oname (pathname-replace-extension fname "o"))
	 (r (begin
	      (with-output-to-file fname (cut display code))
	      (system 
	       (let ((cmd (conc
			   cc " "
			   (if compile-only "-c" "") " "
			   cflags " " *target-cflags* " "
			   fname " "
			   (if compile-only
			       "" 
			       (conc ldflags " " *target-lflags*) )
			   " "
			   (if *msvc* "" ">/dev/null") " "
			   (if verb "" "2>&1") ) ) )
		 (when verb (print cmd " ..."))
		 cmd) ) ) ) )
    (when verb (print (if (zero? r) "succeeded." "failed.")))
    (system (sprintf "~A ~A" *remove-command* (quotewrap fname)))
    (zero? r) ) )

(define (required-chicken-version v)
  (when (string-ci<? (chicken-version) (->string v))
    (error (sprintf "CHICKEN version ~a or higher is required" v)) ) )

(define (upgrade-message ext msg)
  (error
   (sprintf
    "the required extension `~s' ~a - please run~%~%  chicken-setup ~a~%~%and repeat the current installation operation."
    ext msg ext) ) )

(define (required-extension-version . args)
  (let loop ((args args))
    (match args
      (() #f)
      ((ext version . more)
       (let ((info (extension-information ext))
	     (version (->string version)) )
	 (if info
	     (let ((ver (and (assq 'version info) (cadr (assq 'version info)))))
	       (cond ((not ver) (upgrade-message ext "has no associated version information"))
		     ((string-ci<? (->string ver) version)
		      (upgrade-message 
		       ext
		       (sprintf "is older than ~a, which is what this extension requires"
				version) ) )
		     (else (loop more)) ) ) 
	     (upgrade-message ext "is not installed") ) ) )
      (_ (error 'required-extension-information "bad argument format" args)) ) ) )

(define test-compile try-compile)

(define (find-library name proc)
  (test-compile 
   (sprintf "#ifdef __cplusplus~%extern \"C\"~%#endif~%char ~a();~%int main() { ~a(); return 0; }~%" proc proc)
   ldflags: (if *msvc*
		(conc name ".lib")
		(conc "-l" name) ) ) )

(define (find-header name)
  (test-compile
   (sprintf "#include <~a>\nint main() { return 0; }\n" name)
   compile-only: #t) )

(define (http-get-path-request path fname host)
  (sprintf "~A HTTP/1.0\r\nHost: ~A\r\nConnection: close\r\nContent-length: 0\r\n\r\n"
	   (let ((p (make-pathname path fname "" "/")))
	     (if (absolute-pathname? p)
		 p
		 (conc "/" p) ) )
	   host))

(define (http-get-request path fname host)
  (if *proxy-host*
      (sprintf "GET http://~A~A" host (http-get-path-request path fname host))
      (sprintf "GET ~A" (http-get-path-request path fname host))))

(define (setup-tcp-connect host port)
  (if *proxy-host*
      (tcp-connect *proxy-host* *proxy-port*)
      (tcp-connect host port)))

(define (download-repository-tree)
  (unless *repository-tree*
    (when (setup-verbose-flag) (print "downloading catalog ..."))
    (let loop ((hosts *repository-hosts*))
      (if (null? hosts)
	  (error "unable to connect")
	  (match hosts
	    (((host path port) . more)
	     (call/cc
	      (lambda (return)
		(or (handle-exceptions ex
		      (begin (printf "could not connect to ~A.~%" host) #f)
		      (when (setup-verbose-flag)
			(printf "downloading catalog from ~A ...~%" host) )
		      (let-values (((i o) (setup-tcp-connect host port)))
			(set! *last-decent-host* (car hosts))
			(let ((req (http-get-request path remote-repository-name host)))
			  (when (setup-verbose-flag) (display req))
			  (display req o) )
			(let ((ln (read-line i)))
			  (when (setup-verbose-flag) (print ln))
			  (when (string-match "HTTP.+ 404 .+" ln)
			    (print "no remote repository available") 
			    (return #f) ) )
			(let loop ()
			  (let ((ln (read-line i)))
			    (when (setup-verbose-flag) (print ln))
			    (if (string=? "" ln)
				(begin
				  (set! *repository-tree* (read i))
				  (set! *repository-tree-downloaded* #t)
				  (when *debug*
				    (print "catalog:")
				    (pp *repository-tree*) )
				  (close-input-port i)
				  (close-output-port o)
				  #t)
				(loop) ) ) ) ) )
		    (loop more) ) ) ) )
	    ((x . _) (error "(internal) invalid host" x)) ) ) ) ) )

(define *progress-indicator*
  (thread-start!
   (rec (loop)
     (thread-sleep! 1)
     (print* ".")
     (loop) ) ) )

(thread-suspend! *progress-indicator*)

(define (with-progress-indicator thunk)
  (dynamic-wind
      (cut thread-resume! *progress-indicator*)
      thunk
      (lambda ()
	(newline)
	(thread-suspend! *progress-indicator*) ) ) )

(define (download-data hostdata item #!optional filename)
  (unless hostdata (set! hostdata (car *repository-hosts*)))
  (cond (*local-repository*
	 (when (setup-verbose-flag) (printf "fetching from local directory ~a ...~%" *local-repository*))
	 (let ((p (->string item)))
	   (copy-file (make-pathname *local-repository* p) (make-pathname #f p "egg-dir")) ) )
	(*svn-repository*
	 (when (setup-verbose-flag) (printf "fetching from svn repository ~a ...~%" *svn-repository*))
	 (let ((p (->string item)))
	   (run (svn co ,(if *revision* (conc "--revision " *revision*) "")
		     ,(make-pathname *svn-repository* p) ,(make-pathname #f p "egg-dir"))) ) )
	(else
	 (match hostdata
	   ((host path port)
	    (let ((fname (or filename (third (assq item *repository-tree*)))))
	      (printf "downloading ~A from ~A ~!" fname hostdata)
	      (let-values (((i o) (setup-tcp-connect host port)))
		(let ((req (http-get-request 
			    (if filename (pathname-directory filename) path)
			    (if filename (pathname-strip-directory fname) fname)
			    host) ) )
		  (when *debug* (display req))
		  (display req o) )
		(let loop ()
		  (let ((ln (read-line i)))
		    ;; check for 404 here...
		    (if (string=? "" ln)
			(let ((data (with-progress-indicator (cut read-string #f i))))
			  (close-input-port i)
			  (close-output-port o)
			  (with-output-to-file (pathname-strip-directory fname)
			    (cut display data) 
			    binary:) ) 
			(loop) ) ) ) ) ) )
	   (x (error "(internal) invalid host" x)) ) ) ) )

(define (fetch-file ext)
  (define (eval-req r)
    (when (setup-verbose-flag) 
      (print "Testing system:")
      (pp r) )
    (let ((f (eval r)))
      (when (setup-verbose-flag) 
	(print "\t-> " f) )
      f) )
  (define (requirements reqs)
    (fold 
     (lambda (r reqs)
       (cond ((symbol? r)
	      (let ((node (assq r *repository-tree*)))
		(cond (node (append (requirements (cdddr node)) (list (car node)) reqs))
		      ((memq r ##sys#core-library-modules) reqs)
		      (else (error "Broken dependencies: extension does not exist" r) ) ) ) )
	     ((and *repository-tree-downloaded*
		   (not *windows*)
		   (or (zero? (current-user-id))
		       (not *dont-ask*) ) )
	      (print "WARNING: executing system test retrieved through potentially insecure network:\n")
	      (pp r) 
	      (cond ((yes-or-no?
		      "Do you want to execute this code ?"
		      (if (zero? (current-user-id)) "no" "yes") )
		     (requirements (eval-req r) ) )
		    (else
		     (print "Test cancelled - aborting")
		     (abort-setup) ) ) )
	     (else (requirements (eval-req r) )) ) ) 
     '() 
     reqs) )
  (and (or *dont-ask*
	   (yes-or-no?
	    (sprintf "The extension ~A does not exist.~%Do you want to download it ?" ext)
	    "yes") )
       (cond ((pathname-directory ext)
	      (printf "Warning: no repository index available, trying direct download...~%" ext)
	      (set! *last-decent-host* (car *repository-hosts*))
	      (set! *dont-ask* #t)
	      (download-data
	       *last-decent-host*
	       (pathname-file ext)
	       (pathname-replace-extension ext "egg") )
	      #t)
	     (else
	      (download-repository-tree)
	      (set! *dont-ask* #t)
	      (let ((a (and *repository-tree* (assq (string->symbol ext) *repository-tree*))))
		(when *debug* (printf "catalog entry: ~s~%" a))
		(cond (a (let ((reqs (remove extension-info (delete-duplicates (requirements (cdddr a)) eq?))))
			   (when (pair? reqs)
			     (print "downloading required extensions " reqs " ...")
			     (for-each (cut download-data *last-decent-host* <>) reqs)
			     (print "installing required extensions ...")
			     (for-each (cut install <>) (map ->string reqs)) )
			   (begin (download-data *last-decent-host* (first a)) #t) ) )
		      (else
		       (error "Extension does not exist in the repository" ext)) ) ) ) ) ) )

(define (install filename)
  (let ((df (not *fetch-only*)))
    (let loop ((filename filename))
      (cond ((and df (with-ext filename "setup")) => run-setup-script)
	    ((or (with-ext filename "egg") (with-ext filename "egg-dir")) =>
	     (lambda (f)
	       (when df
		 (unpack/enter f)
		 (let ((sfile (pathname-replace-extension f "setup")))
		   (when (and (not (file-exists? sfile)) (file-exists? "tags") )
		     (let ((ds (sort (directory "tags") string>=?)))
		       (when (pair? ds) 
			 (let ((d (make-pathname "tags" (car ds))))
			   (chdir d)
			   (setup-build-directory d) ) ) ) )
		   (loop sfile)
		   (rmtmpdir) ) ) ) )
	    ((fetch-file filename) 
	     (when df (loop (pathname-file filename))) ) ) ) ) )

(define (doc-index #!optional ddir?)
  (make-pathname (repo-path ddir?) "index.html"))

(define (extension-documented? rpath fn)
  (let ([pn (make-setup-info-pathname fn rpath)])
    (and (file-exists? pn)
	 (with-input-from-file pn
	   (lambda ()
	     (not (not (alist-ref 'documentation (read) eq?))) ) ) ) ) )

(define (delete-undocumented-extensions rpath lst)
  (filter (cut extension-documented? rpath <>) lst) )

(define (build-doc-index)
  (let ((rpath (repository-path))
	(hn (get-host-name)))
    (with-output-to-file (doc-index)
      (lambda ()
	(printf "<html><head><title>Egg documentation index for ~a</title></head>~%" hn)
	(printf "<body><font size=\"-1\"><p style=\"text-align: right\"><i><a href=\"http://www.call-with-current-continuation.org/eggs/index.html\">Visit the official egg index</a></i></p></font>~%")
	(printf "<font face='Arial, Helvetica'><h1>Egg documentation index:</h1>~%")
	(printf "<p>CHICKEN: ~a<br>Host: ~a<br>Repository path: ~a<br><p>~%" 
		(chicken-version #t)
		(get-host-name)
		rpath)
	(display "<center><table border='0' width='70%' cellpadding='3' cellspacing='0'>\n")
	(let ((c 0))
	  (for-each
	   (lambda (f)
	     (and-let* ((info (extension-information f)))
	       (printf "<tr style='background-color: #~a'><td>" (if (odd? c) "ffffff" "c6eff7"))
	       (set! c (add1 c))
	       (let ((doc (assq 'documentation info)))
		 (if doc
		     (printf "<a href=\"~a\">~a</a>" (cadr doc) f) 
		     (display f) )
		 (display "</td><td align='right'>")
		 (and-let* ((v (assq 'version info)))
		   (printf "Version: ~A" (cadr v)) )
		 (and-let* ((r (assq 'release info)))
		   (printf " Release: ~a" (cadr r)))
		 (display "</td></tr>\n") ) ) )
	   (delete-undocumented-extensions 
	    rpath
	    (sort (delete-duplicates
		   (grep "^[^.].*\\.*$" (map pathname-file (directory rpath))) string=?)
		  string<?) ) )
	  (display "</table></center></body></font></html>\n") ) ) ) ) )

(define (format-string str cols #!optional right (padc #\space))
  (let* ((len (string-length str))
	 (pad (make-string (fxmax 0 (fx- cols len)) padc)) )
    (if right
	(string-append pad str)
	(string-append str pad) ) ) )

(define (list-installed)
  (for-each
   (lambda (f)
     (and-let* ((info (extension-information f)))
       (print (format-string (->string f) 32)
	      " "
	      (format-string 
	       (or (and-let* ((v (assq 'version info)))
		     (sprintf "Version: ~A" (cadr v)) )
		   "") 
	       32 #t)
	      (or (and-let* ((r (assq 'release info)))
		    (sprintf " (Release ~a)" (cadr r)) )
		  "") ) ) )
   (sort (delete-duplicates
	  (grep "^[^.].*\\.*$" (map pathname-file (directory (repository-path)))) string=?)
	 string<?) ) )

(define (check-for-upgrades)
  (download-repository-tree)
  (for-each
   (match-lambda
     ((name props . _)
      (and-let* ((a (assq 'date props))
		 (info (extension-information name)) )
	(let ((infoa (assq 'release info)))
	  (when (or (not infoa) (string>? (cadr a) (cadr infoa)))
	    (print 
	     (format-string (symbol->string name) 32) 
	     (if infoa (conc "installed: " (cadr infoa) ", ") "")
	     "available: " (cadr a) ) ) ) ) ) )
   *repository-tree*) )

(define (main args)
  (define (parse-host host eggdir)
    (set! *repository-hosts*
      (cons (match (string-match "(.+)\\:([0-9]+)" host)
	      ((_ host port) (list host (if eggdir "eggs" "") (string->number port)))
	      (_ (list host (if eggdir "eggs" "") 80)) )
	    *repository-hosts*) )  )
  (setup-root-directory *base-directory*)
  (let ((uinst #f)
	(anydone #f))
    (let loop ((args args))
      (match args
	(((or "-help" "--help") . _) (usage))
	(("-uninstall" . more)
	 (set! uinst #t)
	 (loop more) )
	(("-list" more ...)
	 (if (pair? more)
	     (for-each 
	      (lambda (e)
		(let ((info (extension-information e)))
		  (cond (info
			 (print e ":\n")
			 (pp info) 
			 (newline) )
			(else (print "Warning: No extension named `" e "' installed.\n")) ) ) )
	      more)
	     (list-installed) )
	 (exit) )
	(("-run" fname . more)
	 (load fname)
	 (loop more) )
	(("-repository")
	 (print (repository-path))
	 (exit) )
	(("-repository" dir . more)
	 (repository-path dir)
	 (loop more) )
	(("-tree" file . more)
	 (set! *repository-tree* (with-input-from-file file read))
	 (loop more) )
	(("--" . more)
	 (exit) )
	(("-program-path")
	 (print (program-path))
	 (exit) )
	(("-destdir" path . more)
	 (set! *example-directory* (make-pathname path "examples"))
	 (set! *destdir* path) 
	 (installation-prefix path)
	 (loop more) )
	(("-program-path" dir . more)
	 (program-path dir)
	 (loop more) )
	(("-version" . _)
	 (printf "chicken-setup - ~A~%" (chicken-version #t))
	 (exit) )
	(("-release" . _)
	 (print (chicken-version))
	 (exit) )
	(("-script" filename . args)
	 (command-line-arguments args)
	 (load filename) 
	 (exit) )
	(("-eval" expr . more)
	 (eval `(begin ,@(with-input-from-string expr read-file))) 
	 (set! anydone #t)
	 (loop more) )
	(("-fetch" . more)
	 (set! *fetch-only* #t)
	 (loop more) )
	(("-host" host . more)
	 (match (string-match "http://(.*)" host)
	   ((_ host) (parse-host host #t) )
	   (_ (parse-host host #t)) )
	 (loop more) )
	(("-proxy" proxy . more)
	 (match (string-match "(.+)\\:([0-9]+)" proxy)
	   ((_ proxy port) (set! *proxy-host* proxy) (set! *proxy-port* (string->number port)))
	   (_ (set! *proxy-host* proxy) (set! *proxy-port* 80)) )
	 (loop more) )
	(("-keep" . more)
	 (set! *keep-stuff* #t)
	 (set! *csc-options* (append *csc-options* (list "-k")))
	 (loop more) )
	(("-verbose" . more)
	 (setup-verbose-flag #t)
	 (set! *csc-options* (append *csc-options* (list "-v")))
	 (loop more) )
	(("-csc-option" opt . more)
	 (set! *csc-options* (append *csc-options* (list opt)))
	 (loop more) )
	(("-ls" ext . more)
	 (and-let* ((info (extension-information ext))
		    (files (assq 'files info)) )
	   (for-each print (cdr files) ) )
	 (exit) )
	(("-dont-ask" . more)
	 (set! *dont-ask* #t)
	 (loop more) )
	(("-no-install" . more)
	 (setup-install-flag #f)
	 (set! *keep-stuff* #t)
	 (loop more) )
	(("-docindex" . more)
	 (let ((di (doc-index)))
	   (unless (file-exists? di)
	     (build-doc-index) )
	   (print di) ) )
	(("-debug" . more)
	 (set! *debug* #t)
	 (loop more) )
	(("-revision" rev . more)
	 (set! *revision* rev)
	 (loop more) )
	(("-check" . more)
	 (set! *check-repository* #t)
	 (set! anydone #t)
	 (loop more) )
	(("-svn" url . more)
	 (set! *svn-repository* url)
	 (set! *dont-ask* #t)
	 (loop more) )
	(("-local" path . more)
	 (set! *local-repository* path)
	 (set! *dont-ask* #t)
	 (loop more) )
	(("-fetch-tree" . more)
	 (set! *fetch-tree-only* #t)
	 (set! anydone #t)
	 (loop more) )
	(("-test" . exts)
	 (let ((missing (remove extension-info exts)))
	   (cond ((pair? missing) 
		  (print "the following extensions are currently not installed: " missing)
		  1)
		 (else (exit 0)) ) ) )
	(((or "-run" "-script" "-proxy" "-host" "-csc-option" "-ls" "-destdir" "-tree" "-local" "-svn" "-eval"))
	 (error "missing option argument" (car args)) )
	((filename . more)
	 (cond ((and (> (string-length filename) 0) (char=? #\- (string-ref filename 0)))
		(let ((os (string->list (substring filename 1))))
		  (if (every (cut memq <> short-options) os)
		      (loop 
		       (append
			(map (lambda (s) (list-ref long-options (list-index (cut eq? <> s) short-options))) os)
			more) )
		      (error "invalid option" filename) ) ) )
	       (else
		(set! anydone #t)
		((if uinst uninstall-extension install)
		 (match (string-match "http://([^/]+)/(.+)" filename)
		   ((_ host path)
		    (parse-host host #f)
		    (set! *dont-ask* #t)
		    (conc "/" path) )
		   (_ filename)) )
		(loop more) ) ) )
	(()
	 (unless anydone
	   (let ((setups (glob "*.setup")))
	     (if (null? setups)
		 (printf "No setup scripts to process~%")
		 (for-each (if uinst uninstall-extension install) setups) ) ) )
	 (when *check-repository* (check-for-upgrades))
	 (when *fetch-tree-only*
	   (download-repository-tree)
	   (pp *repository-tree*) )
	 (when *rebuild-doc-index*
	   (when (setup-verbose-flag) (printf "Rebuilding documentation index...\n"))
	   (build-doc-index) )
	 #f) ) ) ) )

(handle-exceptions ex 
    (begin
      (print-error-message ex)
      (exit -1) )
  (call/cc
   (lambda (return)
     (set! *abort-hook* return)
     (main (append (string-split (or (getenv "CHICKEN_SETUP_OPTIONS") ""))
		   (command-line-arguments) ) ) ) )
  (rmtmpdir) )
