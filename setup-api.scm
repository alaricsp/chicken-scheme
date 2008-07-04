;;;; setup-api.scm - build + installation API for eggs
;
; Copyright (c) 2008, The Chicken Team
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


(require-library srfi-1 regex utils posix srfi-13)


(module setup-api

    (move-file 
     (run execute)
     compile
     (make make/proc)
     install-extension 
     setup-verbose-flag
     setup-install-flag installation-prefix chicken-prefix 
     find-library
     find-header program-path remove-file* 
     patch yes-or-no?
     setup-root-directory create-directory
     test-compile try-compile copy-file run-verbose
     required-chicken-version required-extension-version cross-chicken
     host-extension)

  (import scheme chicken extras srfi-1 regex utils posix srfi-13 ports 
	  data-structures foreign)


;;; Constants, variables and parameters

#>
#ifndef C_INSTALL_BIN_HOME
# define C_INSTALL_BIN_HOME   NULL
#endif

#ifndef C_INSTALL_CC
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

#ifndef C_TARGET_CC
# define C_TARGET_CC  C_INSTALL_CC
#endif

#ifndef C_TARGET_CXX
# define C_TARGET_CXX  C_INSTALL_CXX
#endif

#ifndef C_TARGET_CFLAGS
# define C_TARGET_CFLAGS  C_INSTALL_CFLAGS
#endif

#ifndef C_TARGET_MORE_LIBS
# define C_TARGET_MORE_LIBS  C_INSTALL_LIB_HOME
#endif

#ifndef C_TARGET_LIB_HOME
# define C_TARGET_LIB_HOME  C_INSTALL_LIB_HOME
#endif

#ifndef C_CHICKEN_PROGRAM
# define C_CHICKEN_PROGRAM   "chicken"
#endif

#ifndef C_CSC_PROGRAM
# define C_CSC_PROGRAM   "csc"
#endif

#ifndef C_CSI_PROGRAM
# define C_CSI_PROGRAM   "csi"
#endif

#ifndef C_CHICKEN_PROFILE_PROGRAM
# define C_CHICKEN_PROFILE_PROGRAM   "chicken-profile"
#endif

#ifndef C_CHICKEN_SETUP_PROGRAM
# define C_CHICKEN_SETUP_PROGRAM   "chicken-setup"
#endif

#ifndef C_CHICKEN_BUG_PROGRAM
# define C_CHICKEN_BUG_PROGRAM   "chicken-bug"
#endif
<#


(define-constant setup-file-extension "setup-info")
(define-constant remote-repository-name "repository")

(define *installed-executables* 
  `(("chicken" . ,(foreign-value "C_CHICKEN_PROGRAM" c-string))
    ("csc" . ,(foreign-value "C_CSC_PROGRAM" c-string))
    ("csi" . ,(foreign-value "C_CSI_PROGRAM" c-string))
    ("chicken-profile" . ,(foreign-value "C_CHICKEN_PROFILE_PROGRAM" c-string))
    ("chicken-setup" . ,(foreign-value "C_CHICKEN_SETUP_PROGRAM" c-string))
    ("chicken-bug" . ,(foreign-value "C_CHICKEN_BUG_PROGRAM" c-string))))

(define *cc* (foreign-value "C_TARGET_CC" c-string))
(define *cxx* (foreign-value "C_TARGET_CXX" c-string))
(define *target-cflags* (foreign-value "C_TARGET_CFLAGS" c-string))
(define *target-libs* (foreign-value "C_TARGET_MORE_LIBS" c-string))
(define *target-lib-home* (foreign-value "C_TARGET_LIB_HOME" c-string))

(define *major-version* (##sys#fudge 41))
(define *default-eggdir* (conc "eggs/" *major-version*))

(define *windows*
  (and (eq? (software-type) 'windows) 
       (build-platform) ) )

(define *windows-shell* (or (eq? *windows* 'mingw32)
                            (eq? *windows* 'msvc)))
(define *debug* #f)

(register-feature! 'chicken-setup)

(define chicken-bin-path
  (or (and-let* ((p (getenv "CHICKEN_PREFIX")))
	(make-pathname p "bin") )
      (foreign-value "C_INSTALL_BIN_HOME" c-string) ) )

(define chicken-prefix
  (or (getenv "CHICKEN_PREFIX")
      (let ((m (string-match "(.*)/bin/?" chicken-bin-path)))
	(if m
	    (cadr m)
	    "/usr/local") ) ) )

(define program-path (make-parameter chicken-bin-path))

(define (cross-chicken) (##sys#fudge 39))
(define host-extension (make-parameter #f))

(define *copy-command* (if *windows-shell* 'copy "cp -r"))
(define *remove-command* (if *windows-shell* "del /Q /S" "rm -fr"))
(define *move-command* (if *windows-shell* 'move 'mv))
(define *gzip-program* 'gzip)
(define *tar-program* 'tar)
(define *builddir-created* #f)
(define *keep-stuff* #f)
(define *csc-options* '())
(define *abort-hook* #f)
(define *dont-ask* #f)
(define *repository-tree* #f)
(define *base-directory* (current-directory))
(define *fetch-tree-only* #f)
(define *svn-repository* #f)
(define *svn-trunk* #f)
(define *local-repository* #f)
(define *repository-hosts* (list (list "www.call-with-current-continuation.org" *default-eggdir* 80)))
(define *revision* #f)

(define setup-root-directory      (make-parameter *base-directory*))
(define setup-verbose-flag        (make-parameter #f))
(define setup-install-flag        (make-parameter #t))


; Convert a string with a version (such as "1.22.0") to a list of the
; numbers (such as (1 22 0)). If one of the version components cannot
; be converted to a number, then it is kept as a string.

(define (version-string->numbers string)
  (map (lambda (x) (or (string->number x) (->string x))) 
       (string-split string ".")))

; Given two lists with numbers corresponding to a software version (as returned
; by version-string->numbers), check if the first is greater than the second.

(define (version-numbers> a b)
  (cond ((null? a) #f)
	((null? b)  #t)
	((and (pair? a) (pair? b))
	 (let ((a1 (car a))
	       (an (cdr a))
	       (b1 (car b))
	       (bn (cdr b)))
	  (cond ((and (number? a1) (number? b1))
		 (cond ((> a1 b1) #t) ((= a1 b1) (version-numbers> an bn)) (else #f)))
		((and (string? a1) (string? b1))  
		 (cond ((string> a1 b1) #t) ((string= a1 b1) (version-numbers> an bn)) (else #f)))
		(else (version-numbers> (cons (->string a1) an) (cons (->string b1) bn))))) )
	(else (error 'version-numbers> "invalid revisions: " a b))))

(define create-directory/parents
  (let ([create-directory create-directory])
    (lambda (dir)
      (let loop ([dir dir])
        (when (and dir (not (directory? dir)))
          (loop (pathname-directory dir))
          (create-directory dir))) ) ) )

(define create-directory
  (let ()
    (define (verb dir)
      (when (setup-verbose-flag) (printf "  creating directory `~a'~%~!" dir)) )
    (if *windows-shell*
	(lambda (dir)
	  (verb dir)
	  (create-directory/parents dir) ) 
	(lambda (dir)
	  (verb dir)
	  (system* "mkdir -p ~a" (quotewrap dir) ) ) ) ) )



;;; Helper stuff

(define (quotewrapped? str)
  (and (string? str) (string-prefix? "\"" str) (string-suffix? "\"" str) ))

(define (quotewrap str)
  (cond ((quotewrapped? str) str)
	((or (string-any char-whitespace? str)
	     (and *windows-shell* (string-any (lambda (c) (char=? c #\/)) str)))
	 (string-append "\"" str "\""))
	(else str)))

(define (abort-setup)
  (*abort-hook* #f) )

(define (yes-or-no? str . default)
  (let ((def (optional default #f)))
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
  (if (list? which)
      (with-output-to-file (cadr which)
       (lambda ()
	 (with-input-from-file (car which)
	   (lambda ()
	     (let loop ()
	       (let ((ln (read-line)))
		 (unless (eof-object? ln)
		   (write-line (string-substitute rx subst ln #t)) 
		   (loop) ) ) ) ) ) ) )
      (let ((tmp (create-temporary-file)))
	(patch (list tmp tmp) rx subst)
	(system* "~A ~A ~A" *move-command* (quotewrap tmp)
		 (quotewrap which)))))

(define run-verbose (make-parameter #t))

(define (fixpath prg)
  (cond ((string=? prg "csc")
	 (string-intersperse 
	  (cons* (quotewrap 
		  (make-pathname 
		   chicken-bin-path
		   (cdr (assoc prg *installed-executables*))))
		 "-feature" "compiling-extension"
		 *csc-options*) 
	  " ") )
	((assoc prg *installed-executables*) =>
	 (lambda (a) (quotewrap (make-pathname chicken-bin-path (cdr a)))))
	(else prg) ) )

(define (fixmaketarget file)
  (if (and (equal? "so" (pathname-extension file))
	   (not (string=? "so" ##sys#load-dynamic-extension)) )
      (pathname-replace-extension file ##sys#load-dynamic-extension)
      file) )

(define (execute explist)
  (define (smooth lst)
    (let ((slst (map ->string lst)))
      (string-intersperse (cons (fixpath (car slst)) (cdr slst)) " ") ) )
  (for-each
   (lambda (cmd)
     (when (run-verbose) (printf "  ~A~%~!" cmd))
     (system* "~a" cmd) )
   (map smooth explist) ) )

(define-syntax run
  (syntax-rules ()
    ((_ exp ...)
     (execute (list `exp ...)))))

(define-syntax compile
  (syntax-rules ()
    ((_ exp ...)
     (run (csc exp ...)))))


;;; "make" functionality

(define (make:find-matching-line str spec)
  (let ((match? (lambda (s) (string=? s str))))
    (let loop ((lines spec))
      (cond
       ((null? lines) #f)
       (else (let* ((line (car lines))
		    (names (if (string? (car line))
			       (list (car line))
			       (car line))))
	       (if (any match? names)
		   line
		   (loop (cdr lines)))))))))

(define (make:form-error s p) (error (sprintf "~a: ~s" s p)))
(define (make:line-error s p n) (error (sprintf "~a: ~s for line: ~a" s p n)))

(define (make:check-spec spec)
  (and (or (list? spec) (make:form-error "specification is not a list" spec))
       (or (pair? spec) (make:form-error "specification is an empty list" spec))
       (every
	(lambda (line)
	  (and (or (and (list? line) (<= 2 (length line) 3))
		   (make:form-error "list is not a list with 2 or 3 parts" line))
	       (or (or (string? (car line))
		       (and (list? (car line))
			    (every string? (car line))))
		   (make:form-error "line does not start with a string or list of strings" line))
	       (let ((name (car line)))
		 (or (list? (cadr line))
		     (make:line-error "second part of line is not a list" (cadr line) name)
		     (every (lambda (dep)
			       (or (string? dep)
				   (make:form-error "dependency item is not a string" dep)))
			     (cadr line)))
		 (or (null? (cddr line))
		     (procedure? (caddr line))
		     (make:line-error "command part of line is not a thunk" (caddr line) name)))))
	spec)))

(define (make:check-argv argv)
  (or (string? argv)
      (every string? argv)
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
				 (any (lambda (dep)
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
   ((spec argv)
    (make:make/proc/helper
     spec
     (if (vector? argv)
	 (vector->list argv)
	 argv) ) ) ) )

(define-syntax make
  (lambda (form r c)
    (##sys#check-syntax 'make form '(_ _ . #(_ 0 1)))
    (let ((spec (cadr form))
	  (%list (r 'list))
	  (%lambda (r 'lambda)))
      (let ((form-error (lambda (s . p) (apply error s spec p))))
	(and (or (list? spec) (form-error "illegal specification (not a sequence)"))
	     (or (pair? spec) (form-error "empty specification"))
	     (every
	      (lambda (line)
		(and (or (and (list? line) (>= (length line) 2))
			 (form-error "clause does not have at least 2 parts" line))
		     (let ((name (car line)))
		       (or (list? (cadr line))
			   (make:line-error "second part of clause is not a sequence" (cadr line) name)))))
	      spec))
	`(,(r 'make/proc)
	  (list ,@(map (lambda (line)
			 `(,%list ,(car line)
				  (,%list ,@(cadr line))
				  ,@(let ((l (cddr line)))
				      (if (null? l)
					  '()
					  `((,%lambda () ,@l))))))
		       spec))
	  ,@(if (null? (cddr form))
		'('())
		(cddr form)))))))


;;; Processing setup scripts

(define (make-setup-info-pathname fn #!optional (rpath (repository-path)))
  (make-pathname rpath fn setup-file-extension) )

(define installation-prefix
  (make-parameter (or (getenv "CHICKEN_INSTALL_PREFIX") #f)))

(define (with-ext filename ext)
  (if (and (equal? (pathname-extension filename) ext)
	   (file-exists? filename) )
      filename
      (let ((f2 (pathname-replace-extension filename ext)))
	(and (file-exists? f2) f2) ) ) )

(define (write-info id files info)
    (let ((info `((files ,@files) 
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
	(unless *windows-shell* (run (chmod a+r ,(quotewrap setup-file))))
	write-setup-info)))

(define (chdir dir)
  (when (setup-verbose-flag) (printf "changing working directory to `~A'~%" dir))
  (change-directory dir) )

(define (copy-file from to #!optional (err #t) (prefix (installation-prefix)))
  (let ((from (if (pair? from) (car from) from))
	(to (let ((to-path (if (pair? from) (make-pathname to (cadr from)) to)))
	      (if (and prefix (not (string-prefix? prefix to-path)))
		  (make-pathname prefix to-path) to-path))))
    (ensure-directory to)
    (cond ((or (glob? from) (file-exists? from))
	   (begin
	     (run (,*copy-command* ,(quotewrap from) ,(quotewrap to))) 
	     to))
	  (err (error "file does not exist" from))
	  (else (warning "file does not exist" from)))))

(define (move-file from to)
  (let ((from  (if (pair? from) (car from) from))
	(to    (if (pair? from) (make-pathname to (cadr from)) to)))
    (ensure-directory to)
    (run (,*move-command* ,(quotewrap from) ,(quotewrap to)) ) ) )

(define (remove-file* dir)
  (run (,*remove-command* ,(quotewrap dir)) ) )

(define (make-dest-pathname path file)
  (if (list? file)
      (make-dest-pathname path (cadr file))
      (if (absolute-pathname? file)
	  file
	  (make-pathname path file) ) ) )

(define (check-filelist flist)
  (map (lambda (f)
	 (cond ((string? f) f)
	       ((and (list? f) (every string? f)) f)
	       ((and (pair? f) (list (car f) (cdr f))))
	       (else (error "invalid file-specification" f)) ) )
       flist) )

(define (translate-extension f #!optional default)
  (pathname-replace-extension f
   (let ((ext (pathname-extension f)))
     (cond ((not ext) default)
	   ((equal? "so" ext) ##sys#load-dynamic-extension)
	   ((equal? "a" ext) (if *windows-shell* "lib" "a"))
	   (else ext)))))


;;; Installation

(define (install-extension id files #!optional (info '()))
  (when (setup-install-flag)
    (let* ((files (check-filelist (if (list? files) files (list files))))
	   (rpath (repo-path))
	   (rpathd (repo-path #t))
	   (dests (map (lambda (f)
			 (let ((from (if (pair? f) (car f) f))
			       (to (make-dest-pathname rpathd f)) )
			   (when (and (not *windows*) 
				      (equal? "so" (pathname-extension to)))
			     (run (,*remove-command* ,(quotewrap to)) ))
			   (copy-file from to)
			   (unless *windows-shell*
			     (run (chmod a+r ,(quotewrap to))))
			   (and-let* ((static (assq 'static info)))
			     (when (and (eq? (software-version) 'macosx)
					(equal? (cadr static) from) 
					(equal? (pathname-extension to) "a"))
			       (run (ranlib ,(quotewrap to)) ) ))
			   (make-dest-pathname rpath f)))
		       files) ) )
      (write-info id dests info) ) ) )


;;; More helper stuff

(define (repo-path #!optional ddir?)
  (let ((p (if (and ddir? (installation-prefix))
	       (make-pathname (installation-prefix) (repository-path))
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
		  (run (chmod a+x ,(quotewrap dir))))))))

(define (try-compile code #!key c++ (cc (if c++ *cxx* *cc*)) (cflags "") (ldflags "") 
		     (verb (setup-verbose-flag)) (compile-only #f))
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
			       (conc "-L" *target-lib-home* " " ldflags " " *target-libs*) )
			   " >/dev/null "
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
    (cond ((null? args) #f)
	  ((and (list? args) (>= (length args) 2))
	   (let* ((ext (car args))
		  (version (cadr args))
		  (more (cddr args))
		  (info (extension-information ext))
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
	  (else 
	   (error 'required-extension-information "bad argument format" args)) ) ) )

(define test-compile try-compile)

(define (find-library name proc)
  (test-compile 
   (sprintf "#ifdef __cplusplus~%extern \"C\"~%#endif~%char ~a();~%int main() { ~a(); return 0; }~%" proc proc)
   ldflags: (conc "-l" name) ) )

(define (find-header name)
  (test-compile
   (sprintf "#include <~a>\nint main() { return 0; }\n" name)
   compile-only: #t) )

)
