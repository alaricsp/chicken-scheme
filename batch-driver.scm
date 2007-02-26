;;;; batch-driver.scm - Driver procedure for the compiler
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
  (unit driver)
  (disable-warning var))


#{compiler
  compiler-arguments process-command-line dump-nodes dump-undefined-globals
  default-standard-bindings default-extended-bindings side-effecting-standard-bindings
  non-foldable-standard-bindings foldable-standard-bindings non-foldable-extended-bindings foldable-extended-bindings
  standard-bindings-that-never-return-false side-effect-free-standard-bindings-that-never-return-false
  compiler-cleanup-hook check-global-exports disabled-warnings check-global-imports
  file-io-only undefine-shadowed-macros
  unit-name insert-timer-checks used-units inline-max-size
  debugging perform-lambda-lifting! disable-stack-overflow-checking
  foreign-declarations emit-trace-info block-compilation line-number-database-size
  target-heap-size target-stack-size target-heap-growth target-heap-shrinkage
  default-default-target-heap-size default-default-target-stack-size verbose-mode original-program-size
  target-initial-heap-size postponed-initforms
  current-program-size line-number-database-2 foreign-lambda-stubs immutable-constants foreign-variables
  rest-parameters-promoted-to-vector inline-table inline-table-used constant-table constants-used mutable-constants
  broken-constant-nodes inline-substitutions-enabled
  emit-profile profile-lambda-list profile-lambda-index profile-info-vector-name
  direct-call-ids foreign-type-table first-analysis emit-closure-info emit-line-info
  initialize-compiler canonicalize-expression expand-foreign-lambda update-line-number-database scan-toplevel-assignments
  perform-cps-conversion analyze-expression simplifications perform-high-level-optimizations perform-pre-optimization!
  reorganize-recursive-bindings substitution-table simplify-named-call emit-unsafe-marker
  perform-closure-conversion prepare-for-code-generation compiler-source-file create-foreign-stub expand-foreign-lambda*
  transform-direct-lambdas! source-filename compressed-literals literal-compression-threshold
  debugging-chicken bomb check-signature posq stringify symbolify build-lambda-list
  string->c-identifier c-ify-string words check-and-open-input-file close-checked-input-file fold-inner constant?
  collapsable-literal? immediate? canonicalize-begin-body extract-mutable-constants string->expr get get-all
  put! collect! count! get-line get-line-2 find-lambda-container display-analysis-database varnode qnode 
  build-node-graph build-expression-tree fold-boolean inline-lambda-bindings match-node expression-has-side-effects?
  simple-lambda-node? compute-database-statistics print-program-statistics output gen gen-list external-protos-first
  pprint-expressions-to-file foreign-type-check estimate-foreign-result-size scan-used-variables scan-free-variables
  topological-sort print-version print-usage initialize-analysis-database dump-exported-globals
  default-declarations units-used-by-default words-per-flonum default-debugging-declarations
  default-profiling-declarations default-optimization-passes compressed-literals-initializer
  inline-max-size file-requirements use-import-table lookup-exports-file
  foreign-string-result-reserve parameter-limit eq-inline-operator optimizable-rest-argument-operators
  membership-test-operators membership-unfold-limit valid-compiler-options valid-compiler-options-with-argument
  chop-separator chop-extension display-real-name-table display-line-number-database explicit-use-flag
  generate-code make-variable-list make-argument-list generate-foreign-stubs foreign-type-declaration
  export-list do-lambda-lifting compiler-warning export-file-name
  foreign-argument-conversion foreign-result-conversion}


(include "tweaks")

(define-constant default-profile-name "PROFILE")
(define-constant default-inline-max-size 10)
(define-constant funny-message-timeout 60000)


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
		      ,@(map (lambda (x) `(quote ,x))
			     (append 
			      default-declarations
			      (if explicit-use-flag
				  '()
				  `((uses ,@units-used-by-default)) ) ) ) ) ) ]
        [verbose (memq 'verbose options)]
	[outfile (cond [(memq 'output-file options) 
			=> (lambda (node)
			     (let ([oname (option-arg node)])
			       (if (symbol? oname)
				   (symbol->string oname)
				   oname) ) ) ]
		       [(memq 'to-stdout options) #f]
		       [else (make-pathname #f (if filename (pathname-file filename) "out") "c")] ) ]
	[ipath (map chop-separator (string-split (or (getenv "CHICKEN_INCLUDE_PATH") "") ";"))]
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
	[lcthreshold (memq 'compress-literals options)]
	[uses-units '()]
	[uunit (memq 'unit options)]
	[a-only (memq 'analyze-only options)]
	[dynamic (memq 'dynamic options)]
	[dumpnodes #f]
	[quiet (memq 'quiet options)]
	[start-time #f]
	[ssize (or (memq 'nursery options) (memq 'stack-size options))] )

    (define (cputime) (##sys#fudge 6))

    (define (print-header mode dbgmode)
      (when verbose (printf "pass: ~a~%~!" mode))
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

    (define (print mode dbgmode xs)
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

    (when uunit
      (set! unit-name (string->c-identifier (stringify (option-arg uunit)))) )
    (set! debugging-chicken 
      (append-map
       (lambda (do)
	 (map (lambda (c) (string->symbol (string c)))
	      (string->list do) ) )
       (collect-options 'debug) ) )
    (set! dumpnodes (memq '|D| debugging-chicken))
    (when (memq 'lambda-lift options) (set! do-lambda-lifting #t))
    (when (memq 't debugging-chicken) (##sys#start-timer))
    (when (memq 'b debugging-chicken) (set! time-breakdown #t))
    (and-let* ((xfile (memq 'emit-exports options)))
      (set! export-file-name (cadr xfile)) )
    (when (memq 'raw options)
      (set! explicit-use-flag #t)
      (set! cleanup-forms '())
      (set! initforms '()) )
    (when (memq 'no-lambda-info options)
      (set! emit-closure-info #f) )
    (set! use-import-table (memq 'check-imports options))
    (let ((imps (collect-options 'import)))
      (when (pair? imps)
	(set! use-import-table #t)
	(for-each lookup-exports-file imps) ) )
    (set! disabled-warnings (map string->symbol (collect-options 'disable-warning)))
    (when (memq 'no-warnings options) 
      (when verbose (printf "Warnings are disabled~%~!"))
      (set! ##sys#warnings-enabled #f) )
    (when (memq 'optimize-leaf-routines options) (set! optimize-leaf-routines #t))
    (when (memq 'unsafe options) 
      (set! unsafe #t)
      (##match#set-error-control #:fail) )
    (when (and dynamic (memq 'unsafe-libraries options))
      (set! emit-unsafe-marker #t) )
    (when (memq 'disable-interrupts options) (set! insert-timer-checks #f))
    (when (memq 'fixnum-arithmetic options) (set! number-type 'fixnum))
    (when (memq 'block options) (set! block-compilation #t))
    (when (memq 'emit-external-prototypes-first options) (set! external-protos-first #t))
    (when (memq 'inline options) (set! inline-max-size default-inline-max-size))
    (when (memq 'track-scheme options) (set! emit-line-info #t))
    (and-let* ([inlimit (memq 'inline-limit options)])
      (set! inline-max-size 
	(let ([arg (option-arg inlimit)])
	  (or (string->number arg)
	      (quit "invalid argument to `-inline-limit' option: `~A'" arg) ) ) ) )
    (when (memq 'case-insensitive options) 
      (when verbose (printf "Identifiers and symbols are case insensitive~%~!"))
      (register-feature! 'case-insensitive)
      (case-sensitive #f) )
    (when kwstyle
      (let ([val (option-arg kwstyle)])
	(cond [(string=? "prefix" val) (keyword-style #:prefix)]
	      [(string=? "none" val) (keyword-style #:none)]
	      [(string=? "suffix" val) (keyword-style #:suffix)]
	      [else (quit "invalid argument to `-keyword-style' option")] ) ) )
    (when lcthreshold
      (let ([t (option-arg lcthreshold)])
	(set! literal-compression-threshold 
	  (or (string->number t)
	      (quit "invalid argument to `-compress-literals' option: ~A" t) ) ) ) )
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

    ;; Handle feature options:
    (for-each register-feature! (collect-options 'feature))

    ;; Load extensions:
    (set! ##sys#features (cons #:compiler-extension ##sys#features))
    (let ([extends (collect-options 'extend)])
      (when verbose
	(printf "Loading compiler extensions...~%~!")
	(load-verbose #t) )
      (for-each (lambda (f) (load (##sys#resolve-include-filename f #f #t))) extends) )
    (set! ##sys#features (delete #:compiler-extension ##sys#features eq?))

    (set! ##sys#features (cons '#:compiling ##sys#features))
    (set! ##sys#features (cons #:match ##sys#features))
    (##sys#provide 'match) 

    ;; Insert postponed initforms:
    (set! initforms (append initforms postponed-initforms))

    ;; Handle `-extension' options:
    (when (memq 'extension options)
      (set! initforms 
	(append 
	 initforms
	 `((define-extension 
	     ,(string->symbol
	       (cond (outfile (pathname-file outfile))
		     (filename (pathname-file filename))
		     (else (quit "no filename available for `-extension' option")) ) ) ) ) ) ) )

    ;; Append required extensions to initforms:
    (let ([ids (lset-difference eq? (map string->symbol (collect-options 'require-extension)) uses-units)])
      (set! initforms
	(append initforms (map (lambda (r) `(##core#require-extension ',r)) ids)) ) )

    (when (memq 'run-time-macros options)
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
    (when verbose
      (printf "debugging info: ~A~%~!"
	      (if emit-trace-info
		  "stacktrace"
		  "none") ) )
    (when profile
      (let ([acc (eq? 'accumulate-profile (car profile))])
	(set! emit-profile #t)
	(set! initforms
	  (append
	   initforms
	   default-profiling-declarations
	   (if acc
	       '((set! ##sys#profile-append-mode #t))
	       '() ) ) )
	(when verbose
	  (printf "Generating ~aprofile~%~!" (if acc "accumulated " "") emit-profile) ) ) )

    (cond ((memq 'version options)
	   (print-version #t)
	   (newline) )
	  ((or (memq 'help options) (memq '-help options) (memq 'h options) (memq '-h options))
	   (print-usage))
	  ((memq 'release options)
	   (display (chicken-version)) 
	   (newline) )
	  ((not filename)
	   (unless quiet
	     (print-version #t)
	     (display "\n\nEnter \"chicken -help\" for information on how to use it.\n") ) )
	  (else

	   ;; Display header:
	   (unless quiet
	     (printf "compiling `~a' ...~%" filename) )
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
		      (when verbose (printf "User read pass...~%~!"))
		      (set! forms (proc prelude files postlude)) ]
		     [else
		      (do ([files files (cdr files)])
			  ((null? files)
			   (set! forms
			     (append (map string->expr prelude)
				     (reverse forms)
				     (map string->expr postlude) ) ) )
			(let* ((f (car files))
			       (in (check-and-open-input-file f)) 
			       (x1 (read-form in)) )
			  (do ((x x1 (read-form in)))
			      ((eof-object? x) 
			       (close-checked-input-file in f) )
			    (set! forms (cons x forms)) ) ) ) ] ) ) )

	   ;; Start compilation passes:
	   (let ([proc (user-preprocessor-pass)])
	     (when proc
	       (when verbose (printf "User preprocessing pass...~%~!"))
	       (set! forms (map proc forms))))

	   (print "source" '|1| forms)
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
			 (let ([is (fold (lambda (clf r)
					   `(let ([,(gensym) 
						   (set! ,(car clf)
						     (##sys#read-from-string ',(cdr clf)))])
					      ,r) )
					 '(##core#undefined) 
					 compressed-literals) ] )
			   (if compressed-literals-initializer
			       `((##core#set! ,compressed-literals-initializer 
					      (lambda () ,is) ) )
			       (list is) ) )
			 exps0
			 (if (and (not unit-name) (not dynamic))
			     cleanup-forms
			     '() )
			 '((##core#undefined))) ] )

   	     (when (debugging '|N| "real name table:")
	       (display-real-name-table) )
	     (when (debugging 'n "line number database:")
	       (display-line-number-database) )

	     (when (and block-compilation unit-name)
	       (compiler-warning 
		'usage
		"compilation of library unit `~a' in block-mode - globals may not be accessible outside this unit"
		unit-name) )

	     (when (and unit-name dynamic)
	       (compiler-warning 'usage "library unit `~a' compiled in dynamic mode" unit-name) )

	     (set! ##sys#line-number-database line-number-database-2)
	     (set! line-number-database-2 #f)

	     (end-time "canonicalization")
	     (print "canonicalized" '|2| exps)

	     (when (memq 'check-syntax options) (exit))

	     (let ([proc (user-pass)])
	       (when proc
		 (when verbose (printf "User pass...~%~!"))
		 (begin-time)
		 (set! exps (map proc exps))
		 (end-time "user pass") ) )

	     (let* ([node0 (make-node
			    'lambda '(())
			    (list (build-node-graph
				   (canonicalize-begin-body exps) ) ) ) ] 
		    [proc (user-pass-2)] )
	       (when (debugging 'M "; requirements:")
		 (pretty-print (hash-table->alist file-requirements)))
	       (when proc
		 (when verbose (printf "Secondary user pass...~%"))
		 (begin-time)
		 (set! first-analysis #f)
		 (let ([db (analyze-expression node0)])
		   (print-db "analysis (u)" '|0| db 0)
		   (end-time "pre-analysis (u)")
		   (begin-time)
		   (proc node0)
		   (end-time "secondary user pass")
		   (print-node "secondary user pass" '|U| node0) )
		 (set! first-analysis #t) )

	       (when do-lambda-lifting
		 (begin-time)
		 (set! first-analysis #f)
		 (let ([db (analyze-expression node0)])
		   (print-db "analysis" '|0| db 0)
		   (end-time "pre-analysis")
		   (begin-time)
		   (perform-lambda-lifting! node0 db)
		   (end-time "lambda lifting")
		   (print-node "lambda lifted" '|L| node0) )
		 (set! first-analysis #t) )

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
		   (let ([db (analyze-expression node2)])
		     (when first-analysis
		       (when use-import-table (check-global-imports db))
		       (check-global-exports db)
		       (when (memq 'u debugging-chicken)
			 (dump-undefined-globals db)) )
		     (set! first-analysis #f)
		     (end-time "analysis")
		     (print-db "analysis" '|4| db i)

		     (when (memq 's debugging-chicken) (print-program-statistics db))

		     (cond [progress
			    (debugging 'p "optimization pass" i)

			    (begin-time)
			    (receive (node2 progress-flag) (perform-high-level-optimizations node2 db)
			      (end-time "optimization")
			      (print-node "optimized-iteration" '|5| node2)

			      (cond [progress-flag (loop (add1 i) node2 #t)]
				    [(not inline-substitutions-enabled)
				     (debugging 'p "rewritings enabled...")
				     (set! inline-substitutions-enabled #t)
				     (loop (add1 i) node2 #t) ]
				    [optimize-leaf-routines
				     (begin-time)
				     (let ([db (analyze-expression node2)])
				       (end-time "analysis")
				       (begin-time)
				       (let ([progress (transform-direct-lambdas! node2 db)])
					 (end-time "leaf routine optimization")
					 (loop (add1 i) node2 progress) ) ) ]
				    [else (loop (add1 i) node2 #f)] ) ) ]
			   
			   [else
			    (print-node "optimized" '|7| node2)

			    (begin-time)
			    (let ([node3 (perform-closure-conversion node2 db)])
			      (end-time "closure conversion")
			      (print-db "final-analysis" '|8| db i)
			      (when (and ##sys#warnings-enabled (> (- (cputime) start-time) funny-message-timeout))
				(display "(don't despair - still compiling...)\n") )
			      (when export-file-name
				(dump-exported-globals db export-file-name) )
			      (let ([upap (user-post-analysis-pass)])
				(when upap 
				  (upap db
					(lambda (k p) (cut get db <> <>))
					(lambda (k p x) (cut put! db <> <>)) ) ) )
			      (when a-only (exit 0))
			      (print-node "closure-converted" '|9| node3)

			      (begin-time)
			      (receive (node literals lambdas) (prepare-for-code-generation node3 db)
				(end-time "preparation")

                                (begin-time)
				(let ((out (if outfile (open-output-file outfile) (current-output-port))) )
				  (unless quiet
				    (printf "generating `~A' ...~%" outfile) )
				  (generate-code literals lambdas out filename dynamic db)
				  (when outfile (close-output-port out)))
                                (end-time "code generation")
                                (when (memq 't debugging-chicken) (##sys#display-times (##sys#stop-timer)))
                                (compiler-cleanup-hook)
                                (when verbose 
                                  (printf "compilation finished.~%~!") ) ) ) ] ) ) ) ) ) ) ) ) ) )
