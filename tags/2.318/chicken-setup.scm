;;;; chicken-setup
;
; Copyright (c) 2000-2006, Felix L. Winkelmann
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
  (uses srfi-1 regex utils posix tcp match)
  (export move-file run:execute make/proc uninstall-extension install-extension install-program install-script
	  install-sources setup-verbose-flag setup-install-flag
	  program-path remove-file* patch yes-or-no? setup-build-directory setup-root-directory create-directory
	  test-compile copy-file run-verbose) )


(foreign-declare #<<EOF
#ifdef C_USE_C_DEFAULTS
# include "chicken-defaults.h"
#else
# define C_INSTALL_BIN_HOME   NULL
# ifdef _MSC_VER
#  define C_CC                "cl"
# else
#  ifdef __GNUC__
#   define C_CC                "gcc"
#  else
#   define C_CC                "cc"
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
EOF
)


(include "parameters.scm")
(include "build.scm")
(include "chicken-more-macros.scm")


(define-constant long-options
  '("-help" "-uninstall" "-list" "-run" "-repository" "-program-path" "-version" "-script" "-check"
    "-fetch" "-host" "-proxy" "-keep" "-verbose" "-csc-option" "-dont-ask" "-no-install" "-docindex" "-eval") )

(define-constant short-options
  '(#\h #\u #\l #\r #\R #\P #\V #\s #\C #\f #\H #\p #\k #\v #\c #\d #\n #\i #\e) )


(define *install-bin-path* 
  (or (foreign-value "C_INSTALL_BIN_HOME" c-string)
      (getenv "CHICKEN_HOME") ) )

(define *cc* (foreign-value "C_CC" c-string))

(define *windows*
  (and (eq? (software-type) 'windows) 
       (build-platform) ) )

(define *windows-shell* (memq *windows* '(msvc mingw32)))

(register-feature! 'chicken-setup)

(define *copy-command* (if *windows-shell* 'copy "cp -r"))
(define *remove-command* (if *windows-shell* "del /Q /S" "rm -fr"))
(define *move-command* (if *windows-shell* 'move 'mv))

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
	  (system* "mkdir -p ~a" dir) ) ) ) )

(define setup-root-directory (make-parameter #f))
(define setup-build-directory (make-parameter #f))
(define setup-verbose-flag (make-parameter #f))
(define setup-install-flag (make-parameter #t))

(define *fetch-only* #f)
(define *temporary-directory* #f)
(define *tmpdir-created* #f)
(define *keep-stuff* #f)
(define *csc-options* '())
(define *abort-yes-or-no* #f)
(define *dont-ask* #f)
(define *rebuild-doc-index* #f)
(define *check-repository* #f)
(define *repository-tree* #f)
(define *last-decent-host* #f)
(define *proxy-host* #f)
(define *proxy-port* #f)

; Repository-format:
;
; ((NAME FILENAME REQUIRED-NAME ...) ...)
(define repository-hosts '(("www.call-with-current-continuation.org" "eggs" 80)))


(define (yes-or-no? str . default)
  (let ([def (:optional default #f)])
    (let loop ()
      (printf "~%~A (yes/no/abort) " str)
      (when def (printf "[~A] " def))
      (flush-output)
      (let ([ln (read-line)])
	(cond [(eof-object? ln) (set! ln "abort")]
	      [(and def (string=? "" ln)) (set! ln def)] )
	(cond [(string-ci=? "yes" ln) #t]
	      [(string-ci=? "no" ln) #f]
	      [(string-ci=? "abort" ln) (*abort-yes-or-no* #f)]
	      [else
	       (printf "~%Please enter \"yes\", \"no\" or \"abort\".~%")
	       (loop) ] ) ) ) ) )

(define (patch which rx subst)
  (when (setup-verbose-flag) (printf "patching ~A ...~%" which))
  (match which
    [(from to) 
     (with-output-to-file to
       (lambda ()
	 (with-input-from-file from
	   (lambda ()
	     (let loop ()
	       (let ([ln (read-line)])
		 (unless (eof-object? ln)
		   (write-line (string-substitute rx subst ln #t)) 
		   (loop) ) ) ) ) ) ) ) ]
    [both
     (let ([tmp (create-temporary-file)])
       (patch (list both tmp) rx subst)
       (system* "~A ~A ~A" *move-command* tmp both) ) ] ) )

(define run-verbose (make-parameter #t))

(define (fixpath prg)
  (cond [(string=? prg "csc")
	 (string-intersperse 
	  (cons* (make-pathname *install-bin-path* prg)
		 "-feature" "compiling-extension"
		 *csc-options*) 
	  " ") ]
	[(member prg installed-executables) (make-pathname *install-bin-path* prg)]
	[else prg] ) )

(define (fixmaketarget file)
  (if (and (equal? "so" (pathname-extension file))
	   (not (string=? "so" ##sys#load-dynamic-extension)) )
      (pathname-replace-extension file ##sys#load-dynamic-extension)
      file) )

(define (unformatify str)
  (string-translate* str '(("~" . "~~"))) )

(define (run:execute explist)
  (define (smooth lst)
    (let ([slst (map ->string lst)])
      (string-intersperse (cons (fixpath (car slst)) (cdr slst)) " ") ) )
  (for-each
   (lambda (cmd)
     (when (run-verbose) (printf "  ~A~%~!" cmd))
     (system* (unformatify cmd) ))
   (map smooth explist) ) )

(define-macro (run . explist)
  `(run:execute (list ,@(map (lambda (x) (list 'quasiquote x)) explist))) )

(define-macro (compile . explist)
  `(run (csc ,@explist) ) )

(define (make:find-matching-line str spec)
  (let ([match? (lambda (s) (string=? s str))])
    (let loop ([lines spec])
      (cond
       [(null? lines) #f]
       [else (let* ([line (car lines)]
		    [names (if (string? (car line))
			       (list (car line))
			       (car line))])
	       (if (ormap match? names)
		   line
		   (loop (cdr lines))))]))))

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
	       (let ([name (car line)])
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
      (and (vector? argv)
	   (andmap string? (vector->list argv)))
      (error "argument is not a string or string vector" argv)))

(define (make:make/proc/helper spec argv)
  (make:check-spec spec)
  (make:check-argv argv)
  (letrec ([made '()]
	   [exn? (condition-predicate 'exn)]
	   [exn-message (condition-property-accessor 'exn 'message)]
	   [make-file
	    (lambda (s indent)
	      (let* ([line (make:find-matching-line s spec)]
		     [s2 (fixmaketarget s)] 
		     [date (and (file-exists? s2)
				(file-modification-time s2))])

		(when (setup-verbose-flag)
		  (printf "make: ~achecking ~a~%" indent s2))

		(if line
		    (let ([deps (cadr line)])
		      (for-each (let ([new-indent (string-append " " indent)])
				  (lambda (d) (make-file d new-indent)))
				deps)
		      (let ([reason
			     (or (not date)
				 (ormap (lambda (dep)
					  (let ([dep2 (fixmaketarget dep)])
					    (unless (file-exists? dep2)
					      (error (sprintf "dependancy ~a was not made~%" dep2)))
					    (and (> (file-modification-time dep2) date)
						 dep2)) )
					deps))])
			(when reason
			  (let ([l (cddr line)])
			    (unless (null? l)
			      (set! made (cons s made))
			      (when (setup-verbose-flag)
				(printf "make: ~amaking ~a~a~%"
					indent
					s2
					(cond
					 [(not date)
					  (string-append " because " s2 " does not exist")]
					 [(string? reason)
					  (string-append " because " reason " changed")]
					 [else
					  (string-append (sprintf " just because (reason: ~a date: ~a)" 
								  reason date))]) ) )
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
		      (error (sprintf "don't know how to make ~a" s2))))))])
    (cond
     [(string? argv) (make-file argv "")]
     [(equal? argv '#()) (make-file (caar spec) "")]
     [else (for-each (lambda (f) (make-file f "")) (vector->list argv))])
    (when (setup-verbose-flag)
      (for-each (lambda (item)
		  (printf "make: made ~a~%" item))
	(reverse made)))) )

(define make/proc
  (case-lambda
   [(spec) (make:make/proc/helper spec '#())]
   [(spec argv) (make:make/proc/helper spec argv)]))

(define-macro (make spec . argv)
  (let ([make (lambda (spec argv)
		(let ([form-error (lambda (s . p) (apply error s spec p))])
		  (and (or (list? spec) (form-error "illegal specification (not a sequence)"))
		       (or (pair? spec) (form-error "empty specification"))
		       (andmap
			(lambda (line)
			  (and (or (and (list? line) (>= (length line) 2))
				   (form-error "clause does not have at least 2 parts" line))
			       (let ([name (car line)])
				 (or (list? (cadr line))
				     (make:line-error "second part of clause is not a sequence" (cadr line) name)))))
			spec))
		  `(make/proc (list ,@(map (lambda (line)
					     `(list ,(car line)
						    (list ,@(cadr line))
						    ,@(let ([l (cddr line)])
							(if (null? l)
							    '()
							    `((lambda ()
								,@l))))))
					   spec))
			      ,(if (vector? argv) `',argv (car argv)))))])
    (if (pair? argv)
	(make spec argv)
	(make spec '#())) ) )

(define (usage)
  (display #<<EOF
usage: chicken-setup [OPTION ...] FILENAME

  -h  -help                      show this text
  -V  -version                   show version of this program
  -R  -repository                prints the location of the extension repository
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
  --                             ignore all following arguments

  Builds and installs extension libraries.

EOF
  )
  (exit) )

(define program-path (make-parameter *install-bin-path*))

(define (with-ext filename ext)
  (if (and (equal? (pathname-extension filename) ext)
	   (file-exists? filename) )
      filename
      (let ([f2 (pathname-replace-extension filename ext)])
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
      (let ((sid (->string id)))
	(with-output-to-file (make-pathname (repo-path) sid setup-file-extension)
	  (cut pp info) ) ) ) ) )

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

(define (simple-install filename)
  (let ([so (pathname-replace-extension filename ##sys#load-dynamic-extension)]
	[id (pathname-strip-extension filename)] )
    (run (csc -O2 -no-trace -s ,filename -emit-exports ,(make-pathname #f id "exports")))
    (when (setup-install-flag)
      (unless *windows* (run (,*remove-command* ,(make-pathname (repo-path) ,so))))
      (run (,*copy-command* ,so ,(repo-path)))
      (write-info id (list (make-pathname (repository-path) so)) '()) )
    (unless *keep-stuff* (run (rm ,so)) ) ) )

(define (compute-tmpdir fname)
  (string-append fname ".dir") )

(define (chdir dir)
  (when (setup-verbose-flag) (printf "changing working directory to `~A'~%" dir))
  (change-directory dir) )

(define (rmtmpdir)
  (when *tmpdir-created*
    (chdir "..")
    (unless *keep-stuff* (run (,*remove-command* ,*temporary-directory*)) ) ) )

(define (unpack filename)
  (define (testgz fn)
    (with-input-from-file fn
      (lambda () (string=? "\x1f\x8b" (read-string 2))) ) )
  (let ([tmpdir (compute-tmpdir filename)])
    (cond ((file-exists? tmpdir) (chdir tmpdir))
	  (else
	   (create-directory tmpdir)
	   (set! *tmpdir-created* #t)
	   (chdir tmpdir)
	   (setup-build-directory (current-directory))
	   (let ((fn2 (string-append "../" filename)))
	     (if (testgz fn2)
		 (run (gunzip -c ,fn2 |\|| tar xvf -))
		 (run (tar xvf ,fn2)) ) ) ) )
    (set! *temporary-directory* tmpdir) ) )

(define (copy-file from to)
  (let ([from (if (pair? from) (car from) from)]
	[to (if (pair? from) (make-pathname to (cadr from)) to)] )
    (ensure-directory to)
    (run (,*copy-command* ,from ,to)) ) )

(define (move-file from to)
  (let ([from (if (pair? from) (car from) from)]
	[to (if (pair? from) (make-pathname to (cadr from)) to)] )
    (ensure-directory to)
    (run (,*move-command* ,from ,to)) ) )

(define (remove-file* dir)
  (run (,*remove-command* ,dir)) )

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

(define (install-extension id files #!optional (info '()))
  (define (soify f)
    (if (string=? (pathname-extension f) "so")
	(pathname-replace-extension f ##sys#load-dynamic-extension)
	f) )
  (when (setup-install-flag)
    (let* ([files (check-filelist (if (list? files) files (list files)))]
	   [rpath (repo-path)] 
	   [files (if *windows*
		      (map (lambda (f) 
			     (if (pair? f)
				 (list (soify (car f)) (soify (cadr f)))
				 (soify f)))
			   files)
		      files) ] )
      (for-each
       (lambda (f)
	 (when (and (not *windows*) (string=? "so" (pathname-extension f)))
	   (run (,*remove-command* ,(make-pathname rpath f))) )
	 (copy-file f rpath) )
       files)
      (when (assq 'documentation info) (set! *rebuild-doc-index* #t))
      (write-info id (map (cut make-dest-pathname rpath <>) files) info) ) ) )

(define (install-program id files #!optional (info '()))
  (define (exify f)
    (if (not (pathname-extension f)) 
	(pathname-replace-extension f "exe")
	f) )
  (when (setup-install-flag)
    (let ([files (check-filelist (if (list? files) files (list files)))]
	  [ppath (program-path)] )
      (when *windows*
	(set! files (map (lambda (f)
			   (if (list? f) 
			       (list (exify (car f)) (exify (cadr f)))
			       (exify f) ) )
			 files) ) )
      (for-each (cut copy-file <> ppath) files)
      (write-info id (map (cut make-dest-pathname ppath <>) files) info) ) ) )

(define (install-script id files #!optional (info '()))
  (when (setup-install-flag)
    (let* ([files (check-filelist (if (list? files) files (list files)))]
	   [ppath (program-path)] 
	   [pfiles (map (cut make-dest-pathname ppath <>) files)] )
      (for-each (cut copy-file <> ppath) files)
      (unless *windows-shell*
	(run (chmod a+x ,(string-intersperse pfiles " "))) )
      (write-info id pfiles info) ) ) )

(define (install-sources id . files)
  (when (setup-install-flag)
    (let* ([files (flatten files)]
	   [rpath (repository-path)]
	   [spath (make-pathname (append (list rpath) '("source")) (->string id))] )
      (create-directory spath)
      (for-each (cut copy-file <> spath) files) ) ) )

(define (uninstall-extension ext)
  (let* ([info (extension-information ext)]
	 [files (and info (assq 'files info))] )
    (if files
	(begin
	  (printf "deleting ~A ...~%" ext)
	  (for-each 
	   (lambda (f)
	     (let ((f (if (pair? f) (cadr f) f)))
	       (when (setup-verbose-flag) (printf "  deleting ~A~%" f))
	       (run (,*remove-command* ,f)) ) )
	   (cdr files) ) )
	(print "no files to uninstall") )
    (when (assq 'documentation info) (set! *rebuild-doc-index* #t))
    (let ((sfile (make-pathname (repository-path) (->string ext) setup-file-extension)))
      (delete-file* sfile)
      (delete-file* (pathname-replace-extension sfile alternative-setup-file-extension)) ) ) )

(define (repo-path)
  (let ((p (repository-path)))
    (ensure-directory p)
    p) )

(define (ensure-directory path)
  (and-let* ((dir (pathname-directory path)))
    (if (file-exists? dir)
	(unless (directory? dir)
	  (error "can not create directory: a file with the same name already exists") )
	(create-directory dir) ) ) )

(define (test-compile code #!key (cflags "") (ldflags "") (verb (setup-verbose-flag)) (compile-only #f))
  (let* ([fname (create-temporary-file "c")]
	 [oname (pathname-replace-extension fname "o")]
	 [r (begin
	      (with-output-to-file fname (cut display code))
	      (system 
	       (let ([cmd (sprintf "~A ~A ~A ~A ~A >/dev/null 2>&1"
				   *cc*
				   (if compile-only "-c" "")
				   cflags
				   fname
				   (if compile-only "" ldflags) ) ] )
		 (when verb (print* cmd " ..."))
		 cmd) ) ) ] )
    (when verb (print (if (zero? r) "succeeded." "failed.")))
    (system (sprintf "~A ~A" *remove-command* fname))
    (zero? r) ) )

(define (http-get-path-request path fname host)
  (sprintf "~A HTTP/1.0\r\nHost: ~A\r\nConnection: close\r\nContent-length: 0\r\n\r\n"
	   (let ((p (make-pathname path fname)))
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
    (print "downloading catalog ...")
    (let loop ([hosts repository-hosts])
      (if (null? hosts)
	  (error "unable to connect")
	  (match hosts
	    [((host path port) . more)
	     (call/cc
	      (lambda (return)
		(or (handle-exceptions ex
		      (begin (printf "could not connect to ~A.~%" host) #f)
		      (printf "downloading catalog from ~A ...~%" host)
		      (let-values ([(i o) (setup-tcp-connect host port)])
			(set! *last-decent-host* (car hosts))
			(let ((req (http-get-request path remote-repository-name host)))
			  (when (setup-verbose-flag) (display req))
			  (display req o) )
			(let ([ln (read-line i)])
			  (when (setup-verbose-flag) (print ln))
			  (when (string-match "HTTP.+ 404 .+" ln)
			    (print "no remote repository available") 
			    (return #f) ) )
			(let loop ()
			  (let ([ln (read-line i)])
			    (when (setup-verbose-flag) (print ln))
			    (if (string=? "" ln)
				(begin
				  (set! *repository-tree* (read i))
				  (close-input-port i)
				  (close-output-port o)
				  #t)
				(loop) ) ) ) ) )
		    (loop more) ) ) ) ] ) ) ) ) ) 

(define (download-data hostdata item #!optional filename)
  (match hostdata
    [(host path port)
     (let ([fname (or filename (third (assq item *repository-tree*)))])
       (printf "downloading ~A from ~A ...~%" fname hostdata)
       (let-values ([(i o) (setup-tcp-connect host port)])
	 (let ((req (http-get-request 
		     (if filename (pathname-directory filename) path)
		     (if filename (pathname-strip-directory fname) fname)
		     host) ) )
	   (when (setup-verbose-flag) (display req))
	   (display req o) )
	 (let loop ()
	   (let ([ln (read-line i)])
	     ;; check for 404 here...
	     (if (string=? "" ln)
		 (let ([data (read-string #f i)])
		   (close-input-port i)
		   (close-output-port o)
		   (with-output-to-file (pathname-strip-directory fname)
		     (cut display data) binary:) ) 
		 (loop) ) ) ) ) ) ] ) )

(define (fetch-file-from-net ext)
  (define (requirements reqs)
    (fold 
     (lambda (r reqs)
       (let ((node (assq r *repository-tree*)))
	 (cond (node (append (requirements (cdddr node)) reqs))
	       ((memq r core-library-modules) reqs)
	       (else (error "Broken dependencies: extension does not exist" r) ) ) ) ) 
     '() 
     reqs) )
  (and (or *dont-ask*
	   (yes-or-no?
	    (sprintf "The extension ~A does not exist.~%Do you want to download it ?" ext)
	    "yes") )
       (cond ((pathname-directory ext)
	      (printf "Warning: no repository index available, trying direct download...~%" ext)
	      (set! *last-decent-host* (car repository-hosts))
	      (set! *dont-ask* #t)
	      (download-data
	       *last-decent-host*
	       (pathname-file ext)
	       (pathname-replace-extension ext "egg") )
	      #t)
	     (else
	      (download-repository-tree)
	      (set! *dont-ask* #t)
	      (let ([a (and *repository-tree* (assq (string->symbol ext) *repository-tree*))])
		(cond (a (let ([reqs (remove extension-info (delete-duplicates (requirements (cdddr a)) eq?))])
			   (when (pair? reqs)
			     (print "downloading required extensions ...")
			     (for-each (cut download-data *last-decent-host* <>) reqs)
			     (print "installing required extensions ...")
			     (for-each (cut install <>) (map ->string reqs)) )
			   (begin (download-data *last-decent-host* (first a)) #t) ) )
		      (else
		       (error "Extension does not exist in the repository" ext)) ) ) ) ) ) )

(define (install filename)
  (let ([df (not *fetch-only*)])
    (let loop ((filename filename))
      (cond [(and df (with-ext filename "setup")) => run-setup-script]
	    [(and df (with-ext filename "scm")) => simple-install]
	    [(and df (with-ext filename "egg")) => 
	     (lambda (f)
	       (unpack f)
               (loop (pathname-replace-extension f "setup")) ) ]
	    [(fetch-file-from-net filename) 
	     (when df (loop (pathname-file filename))) ] ) ) ) )

(define (doc-index)
  (make-pathname (repo-path) "index.html"))

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
	     (and-let* ([info (extension-information f)])
	       (printf "<tr style='background-color: #~a'><td>" (if (odd? c) "ffffff" "c6eff7"))
	       (set! c (add1 c))
	       (let ([doc (assq 'documentation info)])
		 (if doc
		     (printf "<a href=\"~a\">~a</a>" (cadr doc) f) 
		     (display f) )
		 (display "</td><td align='right'>")
		 (and-let* ([v (assq 'version info)])
		   (printf "Version: ~A" (cadr v)) )
		 (and-let* ((r (assq 'release info)))
		   (printf " Release: ~a" (cadr r)))
		 (display "</td></tr>\n") ) ) )
	   (sort (delete-duplicates
		  (grep "^[^.].*\\.*$" (map pathname-file (directory rpath))) string=?)
		 string<?) )
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
     (and-let* ([info (extension-information f)])
       (print (format-string (->string f) 32)
	      " "
	      (format-string 
	       (or (and-let* ([v (assq 'version info)])
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
    (set! repository-hosts
      (cons (match (string-match "(.+)\\:([0-9]+)" host)
	      [(_ host port) (list host (if eggdir "eggs" "") (string->number port))]
	      [_ (list host (if eggdir "eggs" "") 80)] )
	    repository-hosts) )  )
  (setup-root-directory (current-directory))
  (let ((uinst #f)
	(anydone #f))
    (let loop ([args args])
      (match args
	[((or "-help" "--help") . _) (usage)]
	[("-uninstall" . more)
	 (set! uinst #t)
	 (loop more) ]
	[("-list" more ...)
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
	 (exit) ]
	[("-run" fname . more)
	 (load fname)
	 (loop more) ]
	[("-repository")
	 (print (repository-path))
	 (exit) ]
	[("-repository" dir . more)
	 (repository-path dir)
	 (loop more) ]
	[("--" . more)
	 (exit) ]
	[("-program-path")
	 (print (program-path))
	 (exit) ]
	[("-program-path" dir . more)
	 (program-path dir)
	 (loop more) ]
	[("-version" . _)
	 (printf "chicken-setup - Version ~A, Build ~A~%" build-version build-number)
	 (exit) ]
	[("-script" filename . args)
	 (command-line-arguments args)
	 (load filename) 
	 (exit) ]
	[("-eval" expr . more)
	 (eval `(begin ,@(with-input-from-string expr read-file))) 
	 (set! anydone #t)
	 (loop more) ]
	[("-fetch" . more)
	 (set! *fetch-only* #t)
	 (loop more) ]
	[("-host" host . more)
	 (match (string-match "http://(.*)" host)
	   ((_ host)
	    (parse-host host #t) )
	   (_ (parse-host host #t)) )
	 (loop more) ]
	[("-proxy" proxy . more)
	 (match (string-match "(.+)\\:([0-9]+)" proxy)
	   [(_ proxy port) (set! *proxy-host* proxy) (set! *proxy-port* (string->number port))]
	   [_ (set! *proxy-host* proxy) (set! *proxy-port* 80)] )
	 (loop more) ]
	[("-keep" . more)
	 (set! *keep-stuff* #t)
	 (set! *csc-options* (append *csc-options* (list "-k")))
	 (loop more) ]
	[("-verbose" . more)
	 (setup-verbose-flag #t)
	 (set! *csc-options* (append *csc-options* (list "-v")))
	 (loop more) ]
	[("-csc-option" opt . more)
	 (set! *csc-options* (append *csc-options* (list opt)))
	 (loop more) ]
	[("-dont-ask" . more)
	 (set! *dont-ask* #t)
	 (loop more) ]
	[("-no-install" . more)
	 (setup-install-flag #f)
	 (loop more) ]
	[("-docindex" . more)
	 (let ((di (doc-index)))
	   (unless (file-exists? di)
	     (build-doc-index) )
	   (print di) ) ]
	[("-check" . more)
	 (set! *check-repository* #t)
	 (set! anydone #t)
	 (loop more) ]
	[((or "-run" "-script" "-proxy" "-host" "-csc-option"))
	 (error "missing option argument" (car args)) ]
	[(filename . more)
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
		    (conc "/" path) )
		   (_ filename)) )
		(loop more) ) ) ]
	[()
	 (unless anydone
	   (let ((setups (glob "*.setup")))
	     (if (null? setups)
		 (printf "No setup scripts to process~%")
		 (for-each (if uinst uninstall-extension install) setups) ) ) )
	 (when *check-repository* (check-for-upgrades))
	 (when *rebuild-doc-index*
	   (when (setup-verbose-flag) (printf "Rebuilding documentation index...\n"))
	   (build-doc-index) )
	 #f] ) ) ) )

(handle-exceptions ex 
    (begin
      (print-error-message ex)
      (exit -1) )
  (call/cc
   (lambda (return)
     (set! *abort-yes-or-no* return)
     (main (command-line-arguments)) ) )
  (rmtmpdir) )
