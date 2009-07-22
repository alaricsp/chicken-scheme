;;;; batch-driver.scm - Driver procedure for the compiler
;
; Copyright (c) 2000-2007, Felix L. Winkelmann
; Copyright (c) 2008-2009, The Chicken Team
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


(declare
  (unit driver)
  (disable-warning var))

(include "compiler-namespace")
(include "tweaks")

(define-constant default-profile-name "PROFILE")
(define-constant funny-message-timeout 60000)

(define user-options-pass (make-parameter #f))
(define user-read-pass (make-parameter #f))
(define user-preprocessor-pass (make-parameter #f))
(define user-pass (make-parameter #f))
(define user-post-analysis-pass (make-parameter #f))


;;; Compile a complete source file:

(define (compile-source-file filename . options)
  (define (option-arg p)
    (if (null? (cdr p))
	(quit "missing argument to `-~A' option" (car p))
	(let ([arg (cadr p)])
	  (if (symbol? arg)
	      (quit "invalid argument to `~A' option" arg)
	      arg) ) ) )
  (initialize-compiler)
  (set! explicit-use-flag (memq 'explicit-use options))
  (let ([initforms `((##core#declare
		      ,@(append 
			 default-declarations
			 (if explicit-use-flag
			     '()
			     `((uses ,@units-used-by-default)) ) ) ) ) ]
        [verbose (memq 'verbose options)]
	[outfile (cond [(memq 'output-file options) 
			=> (lambda (node)
			     (let ([oname (option-arg node)])
			       (if (symbol? oname)
				   (symbol->string oname)
				   oname) ) ) ]
		       [(memq 'to-stdout options) #f]
		       [else (make-pathname #f (if filename (pathname-file filename) "out") "c")] ) ]
	[ipath (map chop-separator (string-split (or (get-environment-variable "CHICKEN_INCLUDE_PATH") "") ";"))]
	[opasses default-optimization-passes]
	[time0 #f]
	[time-breakdown #f]
	[forms '()]
	[cleanup-forms '(((##sys#implicit-exit-handler)))]
	[profile (or (memq 'profile options) (memq 'accumulate-profile options) (memq 'profile-name options))]
	[profile-name (or (and-let* ((pn (memq 'profile-name options))) (cadr pn)) default-profile-name)]
	[hsize (memq 'heap-size options)]
	[hisize (memq 'heap-initial-size options)]
	[hgrowth (memq 'heap-growth options)]
	[hshrink (memq 'heap-shrinkage options)]
	[kwstyle (memq 'keyword-style options)]
	[uses-units '()]
	[uunit (memq 'unit options)]
	[a-only (memq 'analyze-only options)]
	[dynamic (memq 'dynamic options)]
	[dumpnodes #f]
	[start-time #f]
	(upap #f)
	[ssize (or (memq 'nursery options) (memq 'stack-size options))] )

    (define (cputime) (##sys#fudge 6))

    (define (dribble fstr . args)
      (when verbose (printf "~?~%~!" fstr args)))

    (define (print-header mode dbgmode)
      (dribble "pass: ~a" mode)
      (and (memq dbgmode debugging-chicken)
	   (begin
	     (printf "[~a]~%" mode)
	     #t) ) )

    (define (print-node mode dbgmode n)
      (when (print-header mode dbgmode)
	(if dumpnodes
	    (dump-nodes n)
	    (pretty-print (build-expression-tree n)) ) ) )

    (define (print-db mode dbgmode db pass)
      (when (print-header mode dbgmode)
	(printf "(iteration ~s)~%" pass)
	(display-analysis-database db) ) )

    (define (print-expr mode dbgmode xs)
      (when (print-header mode dbgmode)
	(for-each pretty-print xs) ) )

    (define (infohook class data val)
      (let ([data2 ((or ##sys#default-read-info-hook (lambda (a b c) b)) class data val)])
	(when (and (eq? 'list-info class) (symbol? (car data2)))
	  (##sys#hash-table-set!
	   ##sys#line-number-database
	   (car data2)
	   (alist-cons data2 val
		       (or (##sys#hash-table-ref ##sys#line-number-database (car data2))
			   '() ) ) ) )
	data2) )

    (define (arg-val str)
      (let* ((len (string-length str))
	     (len1 (- len 1)) )
	(or (if (< len 2)
		(string->number str)
		(case (string-ref str len1)
		  ((#\m #\M) (* (string->number (substring str 0 len1)) (* 1024 1024)))
		  ((#\k #\K) (* (string->number (substring str 0 len1)) 1024))
		  (else (string->number str)) ) )
	    (quit "invalid numeric argument ~S" str) ) ) )

    (define (collect-options opt)
      (let loop ([opts options])
	(cond [(memq opt opts) => (lambda (p) (cons (option-arg p) (loop (cddr p))))]
	      [else '()] ) ) )

    (define (begin-time)
      (when time-breakdown (set! time0 (cputime))) )

    (define (end-time pass)
      (when time-breakdown
	(printf "milliseconds needed for ~a: \t~s~%" pass (- (cputime) time0)) ) )

    (define (read-form in)
      (##sys#read in infohook) )

    (define (analyze pass node . args)
      (let-optionals args ((no 0) (contf #t))
        (let ((db (analyze-expression node)))
	  (when upap
	    (upap pass db node
		  (cut get db <> <>)
		  (cut put! db <> <> <>)
		  no contf) )
	  db) ) )

    (when uunit
      (set! unit-name (string->c-identifier (stringify (option-arg uunit)))) )
    (when (or unit-name dynamic)
      (set! standalone-executable #f))
    (when (memq 'ignore-repository options)
      (set! ##sys#dload-disabled #t)
      (repository-path #f))
    (set! debugging-chicken 
      (append-map
       (lambda (do)
	 (map (lambda (c) (string->symbol (string c)))
	      (string->list do) ) )
       (collect-options 'debug) ) )
    (set! dumpnodes (memq '|D| debugging-chicken))
    (set! import-libraries
      (map (lambda (il)
	     (cons (string->symbol il) 
		   (string-append il ".import.scm")))
	   (collect-options 'emit-import-library)))
    (when (memq 'lambda-lift options) (set! do-lambda-lifting #t))
    (when (memq 'scrutinize options) (set! do-scrutinize #t))
    (when (memq 't debugging-chicken) (##sys#start-timer))
    (when (memq 'b debugging-chicken) (set! time-breakdown #t))
    (when (memq 'emit-exports options)
      (warning "deprecated compiler option: emit-exports") )
    (when (memq 'raw options)
      (set! explicit-use-flag #t)
      (set! cleanup-forms '())
      (set! initforms '()) )
    (when (memq 'no-lambda-info options)
      (set! emit-closure-info #f) )
    (when (memq 'no-compiler-syntax options)
      (set! compiler-syntax-enabled #f))
    (when (memq 'local options)
      (set! local-definitions #t))
    (when (memq 'inline-global options)
      (set! enable-inline-files #t)
      (set! inline-locally #t)
      (set! inline-globally #t))
    (set! disabled-warnings (map string->symbol (collect-options 'disable-warning)))
    (when (memq 'no-warnings options) 
      (dribble "Warnings are disabled")
      (set! ##sys#warnings-enabled #f) )
    (when (memq 'optimize-leaf-routines options) (set! optimize-leaf-routines #t))
    (when (memq 'unsafe options) 
      (set! unsafe #t) )
    (when (and dynamic (memq 'unsafe-libraries options))
      (set! emit-unsafe-marker #t) )
    (when (memq 'disable-interrupts options) (set! insert-timer-checks #f))
    (when (memq 'fixnum-arithmetic options) (set! number-type 'fixnum))
    (when (memq 'block options) (set! block-compilation #t))
    (when (memq 'emit-external-prototypes-first options)
      (set! external-protos-first #t))
    (when (memq 'inline options) (set! inline-locally #t))
    (and-let* ((ifile (memq 'emit-inline-file options)))
      (set! inline-locally #t)		; otherwise this option makes no sense
      (set! local-definitions #t)
      (set! inline-output-file (option-arg ifile)))
    (and-let* ([inlimit (memq 'inline-limit options)])
      (set! inline-max-size 
	(let ([arg (option-arg inlimit)])
	  (or (string->number arg)
	      (quit "invalid argument to `-inline-limit' option: `~A'" arg) ) ) ) )
    (when (memq 'case-insensitive options) 
      (dribble "Identifiers and symbols are case insensitive")
      (register-feature! 'case-insensitive)
      (case-sensitive #f) )
    (when (memq 'compress-literals options)
      (compiler-warning 'usage "`the -compress-literals' option is obsolete") )
    (when kwstyle
      (let ([val (option-arg kwstyle)])
	(cond [(string=? "prefix" val) (keyword-style #:prefix)]
	      [(string=? "none" val) (keyword-style #:none)]
	      [(string=? "suffix" val) (keyword-style #:suffix)]
	      [else (quit "invalid argument to `-keyword-style' option")] ) ) )
    (when (memq 'no-parenthesis-synonyms options)
      (dribble "Disabled support for parenthesis synonyms")
      (parenthesis-synonyms #f) )
    (when (memq 'no-symbol-escape options) 
      (dribble "Disabled support for escaped symbols")
      (symbol-escape #f) )
    (when (memq '("-r5rs-syntax") options)
      (dribble "Disabled the Chicken extensions to R5RS syntax")
      (case-sensitive #f)
      (keyword-style #:none)
      (parentheses-synonyms #f)
      (symbol-escape #f) )
    (set! verbose-mode verbose)
    (set! ##sys#read-error-with-line-number #t)
    (set! ##sys#include-pathnames
      (append (map chop-separator (collect-options 'include-path))
	      ##sys#include-pathnames
	      ipath) )
    (when (and outfile filename (string=? outfile filename))
      (quit "source- and output-filename are the same") )
    (set! uses-units (map string->symbol (collect-options 'uses)))
    (when (memq 'keep-shadowed-macros options)
      (set! undefine-shadowed-macros #f) )
    (when (memq 'no-argc-checks options)
      (set! no-argc-checks #t) )
    (when (memq 'no-bound-checks options)
      (set! no-bound-checks #t) )
    (when (memq 'no-procedure-checks options)
      (set! no-procedure-checks #t) )
    (when (memq 'no-procedure-checks-for-usual-bindings options)
      (for-each 
       (lambda (v)
         (mark-variable v '##compiler#always-bound-to-procedure)
         (mark-variable v '##compiler#always-bound) )
       default-standard-bindings)
      (for-each 
       (lambda (v)
         (mark-variable v '##compiler#always-bound-to-procedure)
         (mark-variable v '##compiler#always-bound) )
       default-extended-bindings) )

    ;; Handle feature options:
    (for-each 
     register-feature!
     (append-map (cut string-split <> ",") (collect-options 'feature)))

    ;; Load extensions:
    (set! ##sys#features (cons #:compiler-extension ##sys#features))
    (let ([extends (collect-options 'extend)])
      (dribble "Loading compiler extensions...")
      (when verbose (load-verbose #t))
      (for-each
       (lambda (f) (load (##sys#resolve-include-filename f #f #t))) 
       extends) )
    (set! ##sys#features (delete #:compiler-extension ##sys#features eq?))

    (set! ##sys#features (cons '#:compiling ##sys#features))
    (set! upap (user-post-analysis-pass))

    ;; Insert postponed initforms:
    (set! initforms (append initforms postponed-initforms))

    (let ((se (map string->symbol (collect-options 'static-extension))))
      ;; Append required extensions to initforms:
      (set! initforms
	(append 
	 initforms 
	 (map (lambda (r) `(##core#require-extension (,r) #t)) 
	      (append se (map string->symbol (collect-options 'require-extension))))))

      ;; add static-extensions as used units:
      (set! ##sys#explicit-library-modules
	(append ##sys#explicit-library-modules se)))

    (when (memq 'compile-syntax options)
      (set! ##sys#enable-runtime-macros #t) )
    (set! target-heap-size
      (if hsize
	  (arg-val (option-arg hsize))
	  (and-let* ([hsize default-default-target-heap-size]
		     [(not (zero? hsize))] )
	    hsize) ) )
    (set! target-initial-heap-size (and hisize (arg-val (option-arg hisize))))
    (set! target-heap-growth (and hgrowth (arg-val (option-arg hgrowth))))
    (set! target-heap-shrinkage (and hshrink (arg-val (option-arg hshrink))))
    (set! target-stack-size
      (if ssize
	  (arg-val (option-arg ssize))
	  (and-let* ([ssize default-default-target-stack-size]
		     [(not (zero? ssize))] )
	    ssize) ) )
    (set! emit-trace-info (not (memq 'no-trace options)))
    (set! disable-stack-overflow-checking (memq 'disable-stack-overflow-checks options))
    (when (memq 'm debugging-chicken) (set-gc-report! #t))
    (unless (memq 'no-usual-integrations options)
      (set! standard-bindings default-standard-bindings)
      (set! extended-bindings default-extended-bindings) )
    (dribble "debugging info: ~A"
	     (if emit-trace-info
		 "calltrace"
		 "none") )
    (when profile
      (let ([acc (eq? 'accumulate-profile (car profile))])
	(set! emit-profile #t)
	(set! profiled-procedures 'all)
	(set! initforms
	  (append
	   initforms
	   default-profiling-declarations
	   (if acc
	       '((set! ##sys#profile-append-mode #t))
	       '() ) ) )
	(dribble "Generating ~aprofile" (if acc "accumulated " "")) ) )

    ;;*** hardcoded "modules.db" is bad (also used in chicken-install.scm)
    (load-identifier-database "modules.db")

    (cond ((memq 'version options)
	   (print-version #t)
	   (newline) )
	  ((or (memq 'help options) (memq '-help options) (memq 'h options) (memq '-h options))
	   (print-usage))
	  ((memq 'release options)
	   (display (chicken-version)) 
	   (newline) )
	  ((not filename)
	   (print-version #t)
	   (display "\nEnter \"chicken -help\" for information on how to use it.\n") )
	  (else

	   ;; Display header:
	   (dribble "compiling `~a' ..." filename)
	   (set! source-filename filename)
	   (debugging 'r "options" options)
	   (debugging 'r "debugging options" debugging-chicken)
	   (debugging 'r "target heap size" target-heap-size)
	   (debugging 'r "target stack size" target-stack-size)
	   (set! start-time (cputime))

	   ;; Read toplevel expressions:
	   (set! ##sys#line-number-database (make-vector line-number-database-size '()))
	   (let ([prelude (collect-options 'prelude)]
		 [postlude (collect-options 'postlude)] 
		 [files (append 
			 (collect-options 'prologue)
			 (list filename)
			 (collect-options 'epilogue) ) ]  )

	     (let ([proc (user-read-pass)])
	       (cond [proc
		      (dribble "User read pass...")
		      (set! forms (proc prelude files postlude)) ]
		     [else
		      (do ([files files (cdr files)])
			  ((null? files)
			   (set! forms
			     (append (map string->expr prelude)
				     (reverse forms)
				     (map string->expr postlude) ) ) )
			(let* ((f (car files))
			       (in (check-and-open-input-file f)) )
			  (fluid-let ((##sys#current-source-filename f))
			    (let ((x1 (read-form in)) )
			      (do ((x x1 (read-form in)))
				  ((eof-object? x) 
				   (close-checked-input-file in f) )
				(set! forms (cons x forms)) ) ) ) ) ) ] ) ) )

	   ;; Start compilation passes:
	   (let ([proc (user-preprocessor-pass)])
	     (when proc
	       (dribble "User preprocessing pass...")
	       (set! forms (map proc forms))))

	   (print-expr "source" '|1| forms)
	   (begin-time)
	   (unless (null? uses-units)
	     (set! ##sys#explicit-library-modules (append ##sys#explicit-library-modules uses-units))
	     (set! forms (cons `(declare (uses ,@uses-units)) forms)) )
	   (let* ([exps0 (map canonicalize-expression (append initforms forms))]
		  [pvec (gensym)]
		  [plen (length profile-lambda-list)]
		  [exps (append
			 (map (lambda (ic) `(set! ,(cdr ic) ',(car ic))) immutable-constants)
			 (map (lambda (n) `(##core#callunit ,n)) used-units)
			 (if emit-profile
			     `((set! ,profile-info-vector-name 
				 (##sys#register-profile-info
				  ',plen
				  ',(if unit-name #f profile-name))))
			     '() )
			 (map (lambda (pl)
				`(##sys#set-profile-info-vector!
				  ,profile-info-vector-name
				  ',(car pl)
				  ',(cdr pl) ) )
			      profile-lambda-list)
			 exps0
			 (if (and (not unit-name) (not dynamic))
			     cleanup-forms
			     '() )
			 '((##core#undefined))) ] )

	     (when (and (pair? compiler-syntax-statistics)
			(debugging 'x "applied compiler syntax:"))
	       (for-each 
		(lambda (cs) (printf "  ~a\t\t~a~%" (car cs) (cdr cs)))
		compiler-syntax-statistics))
   	     (when (debugging '|N| "real name table:")
	       (display-real-name-table) )
	     (when (debugging 'n "line number database:")
	       (display-line-number-database) )

	     (when (and unit-name dynamic)
	       (compiler-warning 'usage "library unit `~a' compiled in dynamic mode" unit-name) )

	     (when (and unsafe (feature? 'compiling-extension))
	       (compiler-warning 
		'style
		"compiling extensions in unsafe mode is bad practice and should be avoided") )

	     (set! ##sys#line-number-database line-number-database-2)
	     (set! line-number-database-2 #f)

	     (end-time "canonicalization")
	     (print-expr "canonicalized" '|2| exps)

	     (when (memq 'check-syntax options) (exit))

	     (let ([proc (user-pass)])
	       (when proc
		 (dribble "User pass...")
		 (begin-time)
		 (set! exps (map proc exps))
		 (end-time "user pass") ) )

	     (let ((node0 (make-node
			   'lambda '(())
			   (list (build-node-graph
				  (canonicalize-begin-body exps) ) ) ) ) 
		   (db #f))

	       (print-node "initial node tree" '|T| node0)
	       (initialize-analysis-database)

	       (when do-scrutinize
		 ;;;*** hardcoded database file name
		 (unless (memq 'ignore-repository options)
		   (load-type-database "types.db"))
		 (for-each (cut load-type-database <> #f) (collect-options 'types))
		 (begin-time)
		 (set! first-analysis #f)
		 (set! db (analyze 'scrutiny node0))
		 (print-db "analysis" '|0| db 0)
		 (end-time "pre-analysis")
		 (begin-time)
		 (debugging 'p "performing scrutiny")
		 (scrutinize node0 db)
		 (end-time "scrutiny")
		 (set! first-analysis #t) )

	       (when do-lambda-lifting
		 (begin-time)
		 (unless do-scrutinize	; no need to do analysis if already done above
		   (set! first-analysis #f)
		   (set! db (analyze 'lift node0))
		   (print-db "analysis" '|0| db 0)
		   (end-time "pre-analysis (lambda-lift)"))
		 (begin-time)
		 (perform-lambda-lifting! node0 db)
		 (end-time "lambda lifting")
		 (print-node "lambda lifted" '|L| node0) 
		 (set! first-analysis #t) )
	       
	       (let ((req (concatenate (vector->list file-requirements))))
		 (when (debugging 'M "; requirements:")
		   (pp req))
		 (when enable-inline-files
		   (for-each
		    (lambda (id)
		      (and-let* ((ifile (##sys#resolve-include-filename 
					 (make-pathname #f (symbol->string id) "inline")
					 #f #t))
				 ((file-exists? ifile)))
			(dribble "Loading inline file ~a ..." ifile)
			(load-inline-file ifile)))
		    (concatenate (map cdr req))) )
		 (let ((ifs (collect-options 'consult-inline-file)))
		   (unless (null? ifs)
		     (set! inline-globally #t)
		     (set! inline-locally #t)
		     (for-each 
		      (lambda (ilf)
			(dribble "Loading inline file ~a ..." ilf)
			(load-inline-file ilf) )
		      ifs))))

	       (set! ##sys#line-number-database #f)
	       (set! constant-table #f)
	       (set! inline-table #f)
	       (unless unsafe
		 (scan-toplevel-assignments (first (node-subexpressions node0))) )

	       (begin-time)
	       (let ([node1 (perform-cps-conversion node0)])
		 (end-time "cps conversion")
		 (print-node "cps" '|3| node1)

		 ;; Optimization loop:
		 (let loop ([i 1] [node2 node1] [progress #t])

		   (begin-time)
		   (let ([db (analyze 'opt node2 i progress)])
		     (when first-analysis
		       (when (memq 'u debugging-chicken)
			 (dump-undefined-globals db))
		       (when (memq 'd debugging-chicken)
			 (dump-defined-globals db))
		       (when (memq 'v debugging-chicken)
			 (dump-global-refs db)) )
		     (set! first-analysis #f)
		     (end-time "analysis")
		     (print-db "analysis" '|4| db i)

		     (when (memq 's debugging-chicken) (print-program-statistics db))

		     (cond [progress
			    (debugging 'p "optimization pass" i)

			    (begin-time)
			    (receive (node2 progress-flag)
				(perform-high-level-optimizations node2 db)
			      (end-time "optimization")
			      (print-node "optimized-iteration" '|5| node2)

			      (cond [progress-flag (loop (add1 i) node2 #t)]
				    [(not inline-substitutions-enabled)
				     (debugging 'p "rewritings enabled...")
				     (set! inline-substitutions-enabled #t)
				     (loop (add1 i) node2 #t) ]
				    [optimize-leaf-routines
				     (begin-time)
				     (let ([db (analyze 'leaf node2)])
				       (end-time "analysis")
				       (begin-time)
				       (let ([progress (transform-direct-lambdas! node2 db)])
					 (end-time "leaf routine optimization")
					 (loop (add1 i) node2 progress) ) ) ]
				    [else (loop (add1 i) node2 #f)] ) ) ]
			   
			   [else
			    (print-node "optimized" '|7| node2)

			    (when inline-output-file
			      (let ((f inline-output-file))
				(dribble "Generating global inline file `~a' ..." f)
				(emit-global-inline-file f db) ) )

			    (begin-time)
			    (let ([node3 (perform-closure-conversion node2 db)])
			      (end-time "closure conversion")
			      (print-db "final-analysis" '|8| db i)
			      (when (and ##sys#warnings-enabled (> (- (cputime) start-time) funny-message-timeout))
				(display "(don't worry - still compiling...)\n") )
			      (when a-only (exit 0))
			      (print-node "closure-converted" '|9| node3)

			      (begin-time)
			      (receive (node literals lliterals lambdas)
				  (prepare-for-code-generation node3 db)
				(end-time "preparation")

                                (begin-time)
				(let ((out (if outfile (open-output-file outfile) (current-output-port))) )
				  (dribble "generating `~A' ..." outfile)
				  (generate-code literals lliterals lambdas out filename dynamic db)
				  (when outfile (close-output-port out)))
                                (end-time "code generation")
                                (when (memq 't debugging-chicken) (##sys#display-times (##sys#stop-timer)))
                                (compiler-cleanup-hook)
                                (dribble "compilation finished.") ) ) ] ) ) ) ) ) ) ) ) ) )
