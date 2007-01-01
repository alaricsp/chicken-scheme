;;;; optimizer.scm - The CHICKEN Scheme compiler (optimizations)
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


(declare (unit optimizer))

#{compiler
  compiler-arguments process-command-line perform-lambda-lifting!
  default-standard-bindings default-extended-bindings side-effecting-standard-bindings
  non-foldable-standard-bindings foldable-standard-bindings non-foldable-extended-bindings foldable-extended-bindings
  standard-bindings-that-never-return-false side-effect-free-standard-bindings-that-never-return-false
  installation-home decompose-lambda-list external-to-pointer
  copy-node! export-list inline-list not-inline-list
  unit-name insert-timer-checks used-units external-variables
  debug-info-index debug-info-vector-name profile-info-vector-name
  foreign-declarations emit-trace-info block-compilation line-number-database-size
  always-bound-to-procedure block-globals make-block-variable-literal block-variable-literal? block-variable-literal-name
  target-heap-size target-stack-size constant-declarations
  default-default-target-heap-size default-default-target-stack-size verbose-mode original-program-size
  current-program-size line-number-database-2 foreign-lambda-stubs immutable-constants foreign-variables
  rest-parameters-promoted-to-vector inline-table inline-table-used constant-table constants-used mutable-constants
  broken-constant-nodes inline-substitutions-enabled loop-lambda-names expand-profile-lambda
  profile-lambda-list profile-lambda-index emit-profile expand-profile-lambda
  direct-call-ids foreign-type-table first-analysis expand-debug-lambda expand-debug-assignment expand-debug-call
  initialize-compiler canonicalize-expression expand-foreign-lambda update-line-number-database! scan-toplevel-assignments
  perform-cps-conversion analyze-expression simplifications perform-high-level-optimizations perform-pre-optimization!
  reorganize-recursive-bindings substitution-table simplify-named-call compiler-warning
  perform-closure-conversion prepare-for-code-generation compiler-source-file create-foreign-stub expand-foreign-lambda*
  transform-direct-lambdas! expand-foreign-callback-lambda* debug-lambda-list debug-variable-list debugging
  debugging-chicken bomb check-signature posq stringify symbolify build-lambda-list
  string->c-identifier c-ify-string words check-and-open-input-file close-checked-input-file fold-inner constant?
  collapsable-literal? immediate? canonicalize-begin-body extract-mutable-constants string->expr get get-all
  put! collect! count! get-line get-line-2 find-lambda-container display-analysis-database varnode qnode 
  build-node-graph build-expression-tree fold-boolean inline-lambda-bindings match-node expression-has-side-effects?
  simple-lambda-node? compute-database-statistics print-program-statistics output gen gen-list 
  pprint-expressions-to-file foreign-type-check estimate-foreign-result-size scan-used-variables scan-free-variables
  topological-sort print-version print-usage initialize-analysis-database
  expand-foreign-callback-lambda default-optimization-passes default-optimization-passes-when-trying-harder
  units-used-by-default words-per-flonum rewrite
  parameter-limit eq-inline-operator optimizable-rest-argument-operators
  membership-test-operators membership-unfold-limit valid-compiler-options valid-compiler-options-with-argument
  make-random-name final-foreign-type inline-max-size simplified-ops
  generate-code make-variable-list make-argument-list generate-foreign-stubs foreign-type-declaration
  foreign-argument-conversion foreign-result-conversion foreign-type-convert-argument foreign-type-convert-result}


(eval-when (compile eval)
  (match-error-control #:fail) )

(include "tweaks")

(define-constant maximal-number-of-free-variables-for-liftable 16)


;;; Scan toplevel expressions for assignments:

(define (scan-toplevel-assignments node)
  (let ([safe '()]
	[unsafe '()] )

    (define (mark v)
      (if (not (memq v unsafe)) (set! safe (cons v safe))) )

    (debugging 'p "scanning toplevel assignments...")
    (call-with-current-continuation
     (lambda (return)

       (define (scan-each ns e)
	 (for-each (lambda (n) (scan n e)) ns) )

       (define (scan n e)
	 (let ([params (node-parameters n)]
	       [subs (node-subexpressions n)] )
	   (case (node-class n)

	     [(##core#variable)
	      (let ([var (first params)])
		(if (and (not (memq var e)) (not (memq var safe)))
		    (set! unsafe (cons var unsafe)) ) ) ]

	     [(if ##core#cond ##core#switch)
	      (scan (first subs) e)
	      (return #f) ]

	     [(let)
	      (scan (first subs) e)
	      (scan (second subs) (append params e)) ]

	     [(lambda ##core#callunit) #f]

	     [(##core#call) (return #f)]

	     [(set!)
	      (let ([var (first params)])
		(if (not (memq var e)) (mark var))
		(scan (first subs) e) ) ]

	     [else (scan-each subs e)] ) ) )

       (scan node '()) ) )
    (debugging 'o "safe globals" safe)
    (set! always-bound (append safe always-bound)) ) )


;;; Do some optimizations:
;
; - optimize tail recursion by replacing trivial continuations.
; - perform beta-contraction (inline procedures called only once).
; - remove empty 'let' nodes.
; - evaluate constant expressions.
; - substitute variables bound to constants with the value.
; - remove variable-bindings which are never used (and which are not bound to side-effecting expressions).
; - perform simple copy-propagation.
; - remove assignments to unused variables if the assigned value is free of side-effects and the variable is
;   not global.
; - remove unused formal parameters from functions and change all call-sites accordingly.
; - rewrite calls to standard bindings into more efficient forms.
; - rewrite calls to known non-escaping procedures with rest parameter to cons up rest-list at call-site,
;   also: change procedure's lambda-list.

(define simplifications (make-vector 301 '()))
(define simplified-ops '())

(define (perform-high-level-optimizations node db)
  (let ([removed-lets 0]
	[removed-ifs 0]
	[replaced-vars 0]
	[rest-consers '()]
	[simplified-classes '()]
	[dirty #f] )

    (define (test sym item) (get db sym item))
    (define (constant-node? n) (eq? 'quote (node-class n)))
    (define (node-value n) (first (node-parameters n)))
    (define (touch) (set! dirty #t))

    (define (simplify n)
      (or (and-let* ([entry (##sys#hash-table-ref simplifications (node-class n))])
	    (any (lambda (s)
		   (and-let* ([vars (second s)]
			      [env (match-node n (first s) vars)] 
			      [n2 (apply (third s) db
					 (map (lambda (v) (cdr (assq v env))) vars) ) ] )
		     (let* ([name (caar s)]
			    [counter (assq name simplified-classes)] )
		       (if counter
			   (set-cdr! counter (add1 (cdr counter)))
			   (set! simplified-classes (alist-cons name 1 simplified-classes)) )
		       (touch)
		       (simplify n2) ) ) )
		 entry) )
	  n) )

    (define (walk n)
      (if (memq n broken-constant-nodes)
	  n
	  (simplify
	   (let* ((odirty dirty)
		  (n1 (walk1 n))
		  (subs (node-subexpressions n1)) )
	     (case (node-class n1)

	       ((if)			; (This can be done by the simplificator...)
		(cond ((constant-node? (car subs))
		       (set! removed-ifs (+ removed-ifs 1))
		       (touch)
		       (walk (if (node-value (car subs))
				 (cadr subs)
				 (caddr subs) ) ) )
		      (else n1) ) )

	       ((##core#call)
		(if (eq? '##core#variable (node-class (car subs)))
		    (let ((var (first (node-parameters (car subs)))))
		      (if (and (or (test var 'standard-binding)
				   (test var 'extended-binding) )
			       (test var 'foldable)
			       (every constant-node? (cddr subs)) )
			  (let ((form (cons var (map (lambda (arg) `(quote ,(node-value arg)))
						     (cddr subs) ) ) ) )
			    (handle-exceptions ex
				(begin
				  (unless odirty (set! dirty #f))
				  (set! broken-constant-nodes (lset-adjoin eq? broken-constant-nodes n1))
				  n1)
			      (let ((x (eval form)))
				(debugging 'o "folding constant expression" form)
				(touch)
				(make-node ; Build call to continuation with new result...
				 '##core#call
				 '(#t)
				 (list (cadr subs) (qnode x)) ) ) ) )
			  n1) )
		    n1) )

	       (else n1) ) ) ) ) )

    (define (walk1 n)
      (let ((subs (node-subexpressions n))
	    (params (node-parameters n)) 
	    (class (node-class n)) )
	(case class

	  ((##core#variable)
	   (let replace ((var (first params)))
	     (cond ((test var 'replacable) => replace)
		   ((test var 'collapsable)
		    (touch)
		    (debugging 'o "substituted constant variable" var)
		    (qnode (car (node-parameters (test var 'value)))) )
		   (else
		    (if (not (eq? var (first params)))
			(begin
			  (touch)
			  (set! replaced-vars (+ replaced-vars 1)) ) )
		    (varnode var) ) ) ) )

	  ((let)
	   (let ([var (first params)])
	     (cond [(or (test var 'replacable)
			(test var 'removable)
			(and (test var 'contractable) (not (test var 'replacing))) )
		    (touch)
		    (set! removed-lets (add1 removed-lets))
		    (walk (second subs)) ]
		   [else (make-node 'let params (map walk subs))] ) ) )

	  ((##core#lambda)
	   (let ([llist (third params)])
	     (cond [(test (first params) 'has-unused-parameters)
		    (decompose-lambda-list
		     llist
		     (lambda (vars argc rest)
		       (receive (unused used) (partition (lambda (v) (test v 'unused)) vars)
			 (touch)
			 (debugging 'o "removed unused formal parameters" unused)
			 (make-node
			  '##core#lambda
			  (list (first params) (second params)
				(cond [(and rest (test (first params) 'explicit-rest))
				       (debugging 'o "merged explicitly consed rest parameter" rest)
				       (build-lambda-list used (add1 argc) #f) ]
				      [else (build-lambda-list used argc rest)] )
				(fourth params) )
			  (list (walk (first subs))) ) ) ) ) ]
		   [(test (first params) 'explicit-rest)
		    (decompose-lambda-list
		     llist
		     (lambda (vars argc rest)
		       (touch)
		       (debugging 'o "merged explicitly consed rest parameter" rest)
		       (make-node
			'##core#lambda
			(list (first params)
			      (second params)
			      (build-lambda-list vars (add1 argc) #f)
			      (fourth params) )
			(list (walk (first subs))) ) ) ) ]
		   [else (walk-generic n class params subs)] ) ) )

	  ((##core#call)
	   (let* ([fun (car subs)]
		  [funclass (node-class fun)] )
	     (case funclass
	       [(##core#variable)
		;; Call to named procedure:
		(let* ([var (first (node-parameters fun))]
		       [lval (and (not (test var 'unknown)) (test var 'value))]
		       [args (cdr subs)] )
		  (cond [(test var 'contractable)
			 (let* ([lparams (node-parameters lval)]
				[llist (third lparams)] )
			   (check-signature var args llist)
			   (debugging 'o "contracted procedure" var)
			   (touch)
			   (walk (inline-lambda-bindings llist args (first (node-subexpressions lval)) #f)) ) ]
			[(memq var constant-declarations)
			 (or (and-let* ((k (car args))
					((eq? '##core#variable (node-class k)))
					(kvar (first (node-parameters k)))
					(lval (and (not (test kvar 'unknown)) (test kvar 'value))) 
					(eq? '##core#lambda (node-class lval))
					(llist (third (node-parameters lval)))
					((or (test (car llist) 'unused)
					     (and (not (test (car llist) 'references))
						  (not (test (car llist) 'assigned)))))
					((not (any (cut expression-has-side-effects? <> db) (cdr args) ))))
			       (debugging 'x "removed call to constant procedure with unused result" var)
			       (make-node
				'##core#call '(#t)
				(list k (make-node '##core#undefined '() '())) ) ) 
			     (walk-generic n class params subs)) ]
			[(and lval (eq? '##core#lambda (node-class lval)))
			 (let* ([lparams (node-parameters lval)]
				[llist (third lparams)] )
			   (decompose-lambda-list
			    llist
			    (lambda (vars argc rest)
			      (let ([fid (first lparams)])
				(cond [(and (test fid 'simple)
					    (test var 'inlinable)
					    (not (memq var not-inline-list))
					    (or (memq var inline-list)
						(< (fourth lparams) inline-max-size) ) )
				       (debugging 'i "procedure inlinable" var fid (fourth lparams))
				       (check-signature var args llist)
				       (debugging 'o "inlining procedure" var)
				       (touch)
				       (walk (inline-lambda-bindings llist args (first (node-subexpressions lval)) #t)) ]
				      [(test fid 'has-unused-parameters)
				       (if (< (length args) argc) ; Expression was already optimized (should this happen?)
					   (walk-generic n class params subs)
					   (let loop ((vars vars) (argc argc) (args args) (used '()))
					     (cond [(or (null? vars) (zero? argc))
						    (touch)
						    (make-node
						     '##core#call
						     params
						     (map walk (cons fun (append-reverse used args))) ) ]
						   [(test (car vars) 'unused)
						    (touch)
						    (debugging 'o "removed unused parameter to known procedure" (car vars) var)
						    (if (expression-has-side-effects? (car args) db)
							(make-node
							 'let
							 (list (gensym 't))
							 (list (walk (car args))
							       (loop (cdr vars) (sub1 argc) (cdr args) used) ) )
							(loop (cdr vars) (sub1 argc) (cdr args) used) ) ]
						   [else (loop (cdr vars)
							       (sub1 argc)
							       (cdr args)
							       (cons (car args) used) ) ] ) ) ) ]
				      [(and (test fid 'explicit-rest)
					    (not (memq n rest-consers)) ) ; make sure we haven't inlined rest-list already
				       (let ([n (length llist)])
					 (if (< (length args) n)
					     (walk-generic n class params subs)
					     (begin
					       (debugging 'o "consed rest parameter at call site" var n)
					       (let-values ([(args rargs) (split-at args n)])
						 (let ([n2 (make-node
							    '##core#call
							    params
							    (map walk
								 (cons fun
								       (append 
									args
									(list
									 (if (null? rargs)
									     (qnode '())
									     (make-node
									      '##core#inline_allocate 
									      (list "C_a_i_list" (* 3 (length rargs)))
									      rargs) ) ) ) ) ) ) ] )
						   (set! rest-consers (cons n2 rest-consers))
						   n2) ) ) ) ) ]
				      [else (walk-generic n class params subs)] ) ) ) ) ) ]
			[else (walk-generic n class params subs)] ) ) ]
	       [(##core#lambda)
		(if (first params)
		    (walk-generic n class params subs)
		    (make-node '##core#call (cons #t (cdr params)) (map walk subs)) ) ]
	       [else (walk-generic n class params subs)] ) ) )

	  ((set!)
	   (let ([var (first params)])
	     (cond [(or (test var 'contractable) (test var 'replacable))
		    (touch)
		    (make-node '##core#undefined '() '()) ]
		   [(and (or (not (test var 'global))
			     block-compilation
			     (and export-list (not (memq var export-list))) )
			 (not (test var 'references)) 
			 (not (expression-has-side-effects? (first subs) db)) )
		    (touch)
		    (debugging 'o "removed side-effect free assignment to unused variable" var)
		    (make-node '##core#undefined '() '()) ]
		   [else (make-node 'set! params (list (walk (car subs))))] ) ) )

	  (else (walk-generic n class params subs)) ) ) )
    
    (define (walk-generic n class params subs)
      (let ((subs2 (map walk subs)))
	(if (every eq? subs subs2)
	    n
	    (make-node class params subs2) ) ) )

    (if (perform-pre-optimization! node db)
	(values node #t)
	(begin
	  (debugging 'p "traversal phase...")
	  (set! simplified-ops '())
	  (let ((node2 (walk node)))
	    (when (pair? simplified-classes) (debugging 'o "simplifications" simplified-classes))
	    (when (and (pair? simplified-ops) (debugging 'o "  call simplifications:"))
	      (for-each
	       (lambda (p)
		 (print* #\tab (car p))
		 (if (> (cdr p) 1)
		     (print #\tab (cdr p))
		     (newline) ) )
	       simplified-ops) )
	    (when (> replaced-vars 0) (debugging 'o "replaced variables" replaced-vars))
	    (when (> removed-lets 0) (debugging 'o "removed binding forms" removed-lets))
	    (when (> removed-ifs 0) (debugging 'o "removed conditional forms" removed-ifs))
	    (values node2 dirty) ) ) ) ) )


;;; Pre-optimization phase:
;
; - Transform expressions of the form '(if (not <x>) <y> <z>)' into '(if <x> <z> <y>)'.
; - Transform expressions of the form '(if (<x> <y> ...) <z> <q>)' into '<z>' if <x> names a
;   standard-binding that is never #f and if it's arguments are free of side-effects.

(define (perform-pre-optimization! node db)
  (let ((dirty #f)
	(removed-nots 0) )

    (define (touch) (set! dirty #t) #t)
    (define (test sym prop) (get db sym prop))

    (debugging 'p "pre-optimization phase...")

    ;; Handle '(if (not ...) ...)':
    (if (test 'not 'standard-binding)
	(for-each 
	 (lambda (site)
	   (let* ((n (cdr site))
		  (subs (node-subexpressions n))
		  (kont (first (node-parameters (second subs))))
		  (lnode (and (not (test kont 'unknown)) (test kont 'value)))
		  (krefs (test kont 'references)) )
	     ;; Call-site has one argument and a known continuation (which is a ##core#lambda)
	     ;;  that has only one use:
	     (if (and lnode krefs (= 1 (length krefs)) (= 3 (length subs))
		      (eq? '##core#lambda (node-class lnode)) )
		 (let* ((llist (third (node-parameters lnode)))
			(body (first (node-subexpressions lnode))) 
			(bodysubs (node-subexpressions body)) )
		   ;; Continuation has one parameter?
		   (if (and (proper-list? llist) (null? (cdr llist)))
		       (let* ((var (car llist))
			      (refs (test var 'references)) )
			 ;; Parameter is only used once?
			 (if (and refs (= 1 (length refs)) (eq? 'if (node-class body)))
			     ;; Continuation contains an 'if' node?
			     (let ((iftest (first (node-subexpressions body))))
			       ;; Parameter is used only once and is the test-argument?
			       (if (and (eq? '##core#variable (node-class iftest))
					(eq? var (first (node-parameters iftest))) )
				   ;; Modify call-site to call continuation directly and swap branches
				   ;;  in the conditional:
				   (begin
				     (set! removed-nots (+ removed-nots 1))
				     (node-parameters-set! n '(#t))
				     (node-subexpressions-set! n (cdr subs))
				     (node-subexpressions-set! 
				      body
				      (cons (car bodysubs) (reverse (cdr bodysubs))) )
				     (touch) ) ) ) ) ) ) ) ) ) )
	 (or (test 'not 'call-sites) '()) ) )
    
    ;; Handle '(if (<func> <a> ...) ...)', where <func> never returns false:
    (for-each
     (lambda (varname)
       (if (test varname 'standard-binding)
	   (for-each
	    (lambda (site)
	      (let* ((n (cdr site))
		     (subs (node-subexpressions n))
		     (kont (first (node-parameters (second subs)))) 
		     (krefs (test kont 'references)) 
		     (lnode (and (not (test kont 'unknown)) (test kont 'value))) )
		;; Call-site has side-effect-free arguments and a known continuation that has only one use?
		(if (and lnode
			 (eq? '##core#lambda (node-class lnode))
			 krefs (= 1 (length krefs))
			 (not (any (lambda (sn) (expression-has-side-effects? sn db)) (cddr subs))) )
		    (let* ((llist (third (node-parameters lnode)))
			   (body (first (node-subexpressions lnode))) )
		      ;; Continuation has one parameter and contains an 'if' node?
		      (if (and (proper-list? llist)
			       (null? (cdr llist))
			       (eq? 'if (node-class body)) )
			  (let* ((var (car llist))
				 (refs (test var 'references)) 
				 (iftest (first (node-subexpressions body))) )
			    ;; Parameter is used only once and is the test-argument?
			    (if (and refs (= 1 (length refs))
				     (eq? '##core#variable (node-class iftest))
				     (eq? var (first (node-parameters iftest))) )
				(let ((bodysubs (node-subexpressions body)))
				  ;; Modify call-site to call continuation directly and swap branches
				  ;;  in the conditional:
				  (debugging 'o "removed call in test-context" varname)
				  (node-parameters-set! n '(#t))
				  (node-subexpressions-set! n (list (second subs) (qnode #t)))
				  (touch) ) ) ) ) ) ) ) )
	    (or (test varname 'call-sites) '()) ) ) )
     side-effect-free-standard-bindings-that-never-return-false)

    (when (> removed-nots 0) (debugging 'o "Removed `not' forms" removed-nots))
    dirty) )


;;; Simplifications:

(define (register-simplifications class . ss)
  (##sys#hash-table-set! simplifications class ss) )


(register-simplifications
 '##core#call
 ;; (<named-call> ...) -> (<primitive-call/inline> ...)
 `((##core#call d (##core#variable (a)) b . c)
   (a b c d)
   ,(lambda (db a b c d)
      (let loop ((entries (or (##sys#hash-table-ref substitution-table a) '())))
	(cond ((null? entries) #f)
	      ((simplify-named-call db d a b (caar entries) (cdar entries) c)
	       => (lambda (r)
		    (let ((as (assq a simplified-ops)))
		      (if as 
			  (set-cdr! as (add1 (cdr as)))
			  (set! simplified-ops (alist-cons a 1 simplified-ops)) ) )
		    r) )
	      (else (loop (cdr entries))) ) ) ) ) )


(register-simplifications
 'let

 ;; (let ((<var1> (##core#inline <eq-inline-operator> <var0> <const1>)))
 ;;   (if <var1> <body1>
 ;;       (let ((<var2> (##core#inline <eq-inline-operator> <var0> <const2>)))
 ;;         (if <var2> <body2>
 ;;             <etc.>
 ;; -> (##core#switch (2) <var0> <const1> <body1> <const2> <body2> <etc.>)
 ;; - <var1> and <var2> have to be referenced once only.
 `((let (var1) (##core#inline (op) (##core#variable (var0)) (quote (const1)))
	(if d1 (##core#variable (var1))
	    body1
	    (let (var2) (##core#inline (op) (##core#variable (var0)) (quote (const2)))
		 (if d2 (##core#variable (var2))
		     body2
		     rest) ) ) )
   (var0 var1 var2 op const1 const2 body1 body2 d1 d2 rest)
   ,(lambda (db var0 var1 var2 op const1 const2 body1 body2 d1 d2 rest)
      (and (equal? op eq-inline-operator)
	   (immediate? const1)
	   (immediate? const2)
	   (= 1 (length (get db var1 'references)))
	   (= 1 (length (get db var2 'references)))
	   (make-node
	    '##core#switch
	    '(2)
	    (list (varnode var0)
		  (qnode const1)
		  body1
		  (qnode const2)
		  body2
		  rest) ) ) ) )

 ;; (let ((<var> (##core#inline <eq-inline-operator> <var0> <const>)))
 ;;   (if <var>
 ;;       <body>
 ;;       (##core#switch <n> <var0> <const1> <body1> ... <rest>) ) )
 ;; -> (##core#switch <n+1> <var0> <const> <body> <const1> <body1> ... <rest>)
 ;; - <var> has to be referenced once only.
 `((let (var) (##core#inline (op) (##core#variable (var0)) (quote (const)))
	(if d (##core#variable (var))
	    body
	    (##core#switch (n) (##core#variable (var0)) . clauses) ) )
   (var op var0 const d body n clauses)
   ,(lambda (db var op var0 const d body n clauses)
      (and (equal? op eq-inline-operator)
	   (immediate? const)
	   (= 1 (length (get db var 'references)))
	   (make-node
	    '##core#switch
	    (list (add1 n))
	    (cons* (varnode var0)
		   (qnode const)
		   body
		   clauses) ) ) ) )
	      
 ;; (let ((<var1> (##core#undefined)))
 ;;   (let ((<var2> (##core#undefined)))
 ;;     ...
 ;;     (let ((<tmp1> (set! <var1> <x1>))
 ;;       (let ((<tmp2> (set! <var2> <x2>)))
 ;;         ...
 ;;         <body>) ... )
 ;; -> <a simpler sequence of let's>
 ;; - <tmpI> may not be used.
 `((let (var1) (##core#undefined ())
	more)
   (var1 more)
   ,(lambda (db var1 more)
      (let loop1 ([vars (list var1)] 
		  [body more] )
	(let ([c (node-class body)]
	      [params (node-parameters body)] 
	      [subs (node-subexpressions body)] )
	  (and (eq? c 'let)
	       (null? (cdr params))
	       (let* ([val (first subs)]
		      [valparams (node-parameters val)]
		      [valsubs (node-subexpressions val)] )
		 (case (node-class val)
		   [(##core#undefined) (loop1 (cons (first params) vars) (second subs))]
		   [(set!)
		    (let ([allvars (reverse vars)])
		      (and (pair? allvars)
			   (eq? (first valparams) (first allvars))
			   (let loop2 ([vals (list (first valsubs))]
				       [vars (cdr allvars)] 
				       [body (second subs)] )
			     (let ([c (node-class body)]
				   [params (node-parameters body)]
				   [subs (node-subexpressions body)] )
			       (cond [(and (eq? c 'let)
					   (null? (cdr params))
					   (not (get db (first params) 'references))
					   (pair? vars)
					   (eq? 'set! (node-class (first subs)))
					   (eq? (car vars) (first (node-parameters (first subs)))) )
				      (loop2 (cons (first (node-subexpressions (first subs))) vals)
					     (cdr vars)
					     (second subs) ) ]
				     [(null? vars)
				      (receive (n progress) 
					  (reorganize-recursive-bindings allvars (reverse vals) body) 
					(and progress n) ) ]
				     [else #f] ) ) ) ) ) ]
		   [else #f] ) ) ) ) ) ) )

 ;; (let ((<var1> <var2>))
 ;;   (<var1> ...) )
 ;; -> (<var2> ...)
 ;; - <var1> used only once
 #| this doesn't seem to work (Sven Hartrumpf):
 `((let (var1) (##core#variable (var2))
	(##core#call p (##core#variable (var1)) . more) ) ; `p' was `#t', bombed also
   (var1 var2 p more)
   ,(lambda (db var1 var2 p more)
      (and (= 1 (length (get db var1 'references)))
	   (make-node
	    '##core#call p
	    (cons (varnode var2) more) ) ) ) )
 |#

 ;; (let ((<var> (##core#inline <op> ...)))
 ;;   (if <var> <x> <y>) )
 ;; -> (if (##core#inline <op> ...) <x> <y>)
 ;; - <op> may not be the eq-inline operator (so rewriting to "##core#switch" works).
 ;; - <var> has to be referenced only once.
 `((let (var) (##core#inline (op) . args)
	(if d (##core#variable (var))
	    x
	    y) ) 
   (var op args d x y)
   ,(lambda (db var op args d x y)
      (and (not (equal? op eq-inline-operator))
	   (= 1 (length (get db var 'references)))
	   (make-node
	    'if d
	    (list (make-node '##core#inline (list op) args)
		  x y) ) ) ) ) )


(register-simplifications
 'if

 ;; (if <x>
 ;;     (<var> <y>)
 ;;     (<var> <z>) )
 ;; -> (<var> (##core#cond <x> <y> <z>))
 ;; - inline-substitutions have to be enabled (so IF optimizations have already taken place).
 `((if d1 x
       (##core#call d2 (##core#variable (var)) y)
       (##core#call d3 (##core#variable (var)) z) )
   (d1 d2 d3 x y z var)
   ,(lambda (db d1 d2 d3 x y z var)
      (and inline-substitutions-enabled
	   (make-node
	    '##core#call d2
	    (list (varnode var)
		  (make-node '##core#cond '() (list x y z)) ) ) ) ) )

 ;; (if (##core#inline <memXXX> <x> '(<c1> ...)) ...)
 ;; -> (let ((<var> <x>))
 ;;      (if (##core#cond (##core#inline XXX? <var> '<c1>) #t ...) ...)
 ;; - there is a limit on the number of items in the list of constants.
 `((if d1 (##core#inline (op) x (quote (clist)))
       y
       z)
   (d1 op x clist y z)
   ,(lambda (db d1 op x clist y z)
      (and-let* ([opa (assoc op membership-test-operators)]
		 [(proper-list? clist)]
		 [(< (length clist) membership-unfold-limit)] )
	(let ([var (gensym)]
	      [eop (list (cdr opa))] )
	  (make-node
	   'let (list var)
	   (list 
	    x
	    (make-node
	     'if d1
	     (list
	      (fold-right
	       (lambda (c rest)
		 (make-node
		  '##core#cond '()
		  (list 
		   (make-node '##core#inline eop (list (varnode var) (qnode c)))
		   (qnode #t)
		   rest) ) )
	       (qnode #f)
	       clist)
	      y
	      z) ) ) ) ) ) ) ) )


;;; Perform dependency-analysis and transform letrec's into simpler constructs (if possible):

(define (reorganize-recursive-bindings vars vals body)
  (let ([graph '()]
	[valmap (map cons vars vals)] )

    (define (find-path var1 var2)
      (let find ([var var1] [traversed '()])
	(and (not (memq var traversed))
	     (let ([arcs (cdr (assq var graph))])
	       (or (memq var2 arcs)
		   (let ([t2 (cons var traversed)])
		     (any (lambda (v) (find v t2)) arcs) ) ) ) ) ) )

    ;; Build dependency graph:
    (for-each
     (lambda (var val) (set! graph (alist-cons var (scan-used-variables val vars) graph)))
     vars vals)

    ;; Compute recursive groups:
    (let ([groups '()]
	  [done '()] )
      (for-each
       (lambda (var)
	 (when (not (memq var done))
	   (let ([g (filter
		     (lambda (v) (and (not (eq? v var)) (find-path var v) (find-path v var)))
		     vars) ] )
	     (set! groups (alist-cons (gensym) (cons var g) groups))
	     (set! done (append (list var) g done)) ) ) )
       vars)

      ;; Coalesce groups into a new graph:
      (let ([cgraph '()])
	(for-each
	 (lambda (g)
	   (let ([id (car g)]
		 [deps
		  (append-map
		   (lambda (var) (filter (lambda (v) (find-path var v)) vars)) 
		   (cdr g) ) ] )
	     (set! cgraph
	       (alist-cons 
		id
		(filter-map
		 (lambda (g2) (and (not (eq? g2 g)) (lset<= eq? (cdr g2) deps) (car g2))) 
		 groups)
		cgraph) ) ) )
	 groups) 

	;; Topologically sort secondary dependency graph:
	(let ([sgraph (topological-sort cgraph eq?)]
	      [optimized '()] )

	  ;; Construct new bindings:
	  (let ([n2
		 (fold
		  (lambda (gn body)
		    (let* ([svars (cdr (assq gn groups))]
			   [svar (car svars)] )
		      (cond [(and (null? (cdr svars))
				  (not (memq svar (cdr (assq svar graph)))) )
			     (set! optimized (cons svar optimized))
			     (make-node 'let svars (list (cdr (assq svar valmap)) body)) ]
			    [else
			     (fold-right
			      (lambda (var rest)
				(make-node
				 'let (list var)
				 (list (make-node '##core#undefined '() '()) rest) ) )
			      (fold-right
			       (lambda (var rest)
				 (make-node
				  'let (list (gensym))
				  (list (make-node 'set! (list var) (list (cdr (assq var valmap))))
					rest) ) )
			       body
			       svars)
			      svars) ] ) ) )
		  body
		  sgraph) ] )
	    (cond [(pair? optimized)
		   (debugging 'o "eliminated assignments" optimized)
		   (values n2 #t) ]
		  [else (values n2 #f)] ) ) ) ) ) ) )


;;;; Rewrite named calls to more primitive forms:

(define substitution-table (make-vector 301 '()))

(define (rewrite name . class-and-args)
  (let ((old (or (##sys#hash-table-ref substitution-table name) '())))
    (##sys#hash-table-set! substitution-table name (append old (list class-and-args))) ) )

(define (simplify-named-call db params name cont class classargs callargs)
  (define (test sym prop) (get db sym prop))

  (case class

    ;; (eq?/eqv?/equal? <var> <var>) -> (quote #t)
    ;; (eq?/eqv?/equal? ...) -> (##core#inline <iop> ...)
    ((1) ; classargs = (<argc> <iop>)
     (and (test name 'standard-binding)
	  (or (and (= (length callargs) (first classargs))
		   (let ((arg1 (first callargs))
			 (arg2 (second callargs)) )
		     (and (eq? '##core#variable (node-class arg1))
			  (eq? '##core#variable (node-class arg2))
			  (equal? (node-parameters arg1) (node-parameters arg2))
			  (make-node '##core#call '(#t) (list cont (qnode #t))) ) ) )
	      (and inline-substitutions-enabled
		   (make-node
		    '##core#call '(#t) 
		    (list cont (make-node '##core#inline (list (second classargs)) callargs)) ) ) ) ) )

    ;; (<op> ...) -> (##core#inline <iop> ...)
    ;; (<op> <rest-vector>) -> (##core#inline <iopv> <rest-vector>)
    ((2) ; classargs = (<argc> <iop> <safe> <iopv>)
     (and inline-substitutions-enabled
	  (= (length callargs) (first classargs))
	  (or (test name 'extended-binding) (test name 'standard-binding))
	  (or (third classargs) unsafe)
	  (let ([arg1 (first callargs)]
		[iopv (fourth classargs)] )
	    (make-node
	     '##core#call '(#t)
	     (list 
	      cont
	      (cond [(and iopv
			  (eq? '##core#variable (node-class arg1))
			  (eq? 'vector (get db (first (node-parameters arg1)) 'rest-parameter)) )
		     (make-node '##core#inline (list iopv) callargs) ]
		    [else (make-node '##core#inline (list (second classargs)) callargs)] ) ) ) ) ) )

    ;; (<op>) -> <var>
    ((3) ; classargs = (<var>)
     (and inline-substitutions-enabled
	  (null? callargs)
	  (or (test name 'standard-binding) (test name 'extended-binding))
	  (make-node '##core#call '(#t) (list cont (varnode (first classargs)))) ) )

    ;; (<op> a b) -> (<primitiveop> a (quote <i>) b)
    ((4) ; classargs = (<primitiveop> <i>)
     (and inline-substitutions-enabled
	  unsafe
	  (= 2 (length callargs))
	  (test name 'standard-binding)
	  (make-node '##core#call (list #f (first classargs))
		     (list (varnode (first classargs))
			   cont
			   (first callargs)
			   (qnode (second classargs))
			   (second callargs) ) ) ) )

    ;; (<op> a) -> (##core#inline <iop> a (quote <x>))
    ((5) ; classargs = (<iop> <x> <numtype>)
     ;; - <numtype> may be #f
     (and inline-substitutions-enabled
	  (or (test name 'extended-binding)
	      (test name 'standard-binding) )
	  (= 1 (length callargs))
	  (let ((ntype (third classargs)))
	    (or (not ntype) (eq? ntype number-type)) )
	  (make-node '##core#call '(#t)
		     (list cont
			   (make-node '##core#inline (list (first classargs))
				      (list (first callargs)
					    (qnode (second classargs)) ) ) ) ) ) )

    ;; (<op> a) -> (##core#inline <iop1> (##core#inline <iop2> a))
    ((6) ; classargs = (<iop1> <iop2> <safe>)
      (and (or (third classargs) unsafe)
	   inline-substitutions-enabled
	   (= 1 (length callargs))
	   (test name 'standard-binding)
	   (make-node '##core#call '(#t)
		      (list cont
			    (make-node '##core#inline (list (first classargs))
				       (list (make-node '##core#inline (list (second classargs))
							callargs) ) ) ) ) ) )

    ;; (<op> ...) -> (##core#inline <iop> ... (quote <x>))
    ((7) ; classargs = (<argc> <iop> <x> <safe>)
     (and (or (fourth classargs) unsafe)
	  inline-substitutions-enabled
	  (= (length callargs) (first classargs))
	  (or (test name 'standard-binding) (test name 'extended-binding))
	  (make-node '##core#call '(#t)
		     (list cont
			   (make-node '##core#inline (list (second classargs))
				      (append callargs
					      (list (qnode (third classargs))) ) ) ) ) ) )

    ;; (<op> ...) -> <<call procedure <proc> with <classargs>, <cont> and <callargs> >>
    ((8) ; classargs = (<proc> ...)
     (and inline-substitutions-enabled
	  (or (test name 'standard-binding)
	      (test name 'extended-binding) )
	  ((first classargs) db classargs cont callargs) ) )

    ;; (<op> <x1> ...) -> (##core#inline "C_and" (##core#inline <iop> <x1> <x2>) ...)
    ;; (<op> [<x>]) -> (quote #t)
    ((9) ; classargs = (<iop-fixnum> <iop-flonum> <fixnum-safe> <flonum-safe>)
     (and inline-substitutions-enabled
	  (test name 'standard-binding)
	  (if (< (length callargs) 2)
	      (make-node '##core#call '(#t) (list cont (qnode #t)))
	      (and (or (and unsafe (not (eq? number-type 'generic)))
		       (and (eq? number-type 'fixnum) (third classargs))
		       (and (eq? number-type 'flonum) (fourth classargs)) )
		   (let* ([names (map (lambda (z) (gensym)) callargs)]
			  [vars (map varnode names)] )
		     (fold-right
		      (lambda (x n y) (make-node 'let (list n) (list x y)))
		      (make-node
		       '##core#call '(#t)
		       (list 
			cont
			(let ([op (list
				   (if (eq? number-type 'fixnum)
				       (first classargs)
				       (second classargs) ) ) ] )
			  (fold-boolean
			   (lambda (x y) (make-node '##core#inline op (list x y))) 
			   vars) ) ) )
		      callargs names) ) ) ) ) )

    ;; (<op> a [b]) -> (<primitiveop> a (quote <i>) b)
    ((10) ; classargs = (<primitiveop> <i> <bvar> <safe>)
     (and inline-substitutions-enabled
	  (or (fourth classargs) unsafe)
	  (test name 'standard-binding)
	  (let ((n (length callargs)))
	    (and (< 0 n 3)
		 (make-node '##core#call (list #f (first classargs))
			    (list (varnode (first classargs))
				  cont
				  (first callargs)
				  (qnode (second classargs))
				  (if (null? (cdr callargs))
				      (varnode (third classargs))
				      (second callargs) ) ) ) ) ) ) )

    ;; (<op> ...) -> (<primitiveop> ...)
    ((11) ; classargs = (<argc> <primitiveop> <safe>)
     ;; <argc> may be #f.
     (and inline-substitutions-enabled
	  (or (third classargs) unsafe)
	  (or (test name 'standard-binding) (test name 'extended-binding))
	  (let ([argc (first classargs)])
	    (and (or (not argc)
		     (= (length callargs) (first classargs)) )
		 (make-node '##core#call (list #t (second classargs))
			    (cons* (varnode (second classargs))
				   cont
				   callargs) ) ) ) ) )

    ;; (<op> a) -> a
    ;; (<op> ...) -> (<primitiveop> ...)
    ((12) ; classargs = (<primitiveop> <safe> <maxargc>)
     (and inline-substitutions-enabled
	  (or (test name 'standard-binding) (test name 'extended-binding))
	  (or (second classargs) unsafe)
	  (let ((n (length callargs)))
	    (and (<= n (third classargs))
		 (case n
		   ((1) (make-node '##core#call '(#t) (cons cont callargs)))
		   (else (make-node '##core#call (list #t (first classargs))
				    (cons* (varnode (first classargs))
					   cont callargs) ) ) ) ) ) ) )

    ;; (<op> ...) -> ((##core#proc <primitiveop>) ...)
    ((13) ; classargs = (<primitiveop> <safe>)
     (and inline-substitutions-enabled
	  (or (test name 'extended-binding) (test name 'standard-binding))
	  (or (second classargs) unsafe)
	  (let ((pname (first classargs)))
	    (make-node '##core#call (if (pair? params) (cons #t (cdr params)) params)
		       (cons* (make-node '##core#proc (list pname #t) '())
			      cont callargs) ) ) ) )

    ;; (<op> <x> ...) -> (##core#inline <iop-safe>/<iop-unsafe> <x> ...)
    ((14) ; classargs = (<numtype> <argc> <iop-safe> <iop-unsafe>)
     (and inline-substitutions-enabled
	  (= (second classargs) (length callargs))
	  (or (test name 'extended-binding)
	      (test name 'standard-binding) )
	  (eq? number-type (first classargs))
	  (or (fourth classargs) unsafe)
	  (make-node
	   '##core#call '(#t)
	   (list cont
		 (make-node
		  '##core#inline
		  (list (if unsafe (fourth classargs) (third classargs)))
		  callargs) ) ) ) )

    ;; (<op> <x>) -> (<primitiveop> <x>)   - if numtype1
    ;;             | <x>                   - if numtype2
    ((15) ; classargs = (<numtype1> <numtype2> <primitiveop> <safe>)
     (and inline-substitutions-enabled
	  (= 1 (length callargs))
	  (or unsafe (fourth classargs))
	  (or (test name 'extended-binding)
	      (test name 'standard-binding) )
	  (cond ((eq? number-type (first classargs))
		 (make-node '##core#call (list #t (third classargs))
			    (cons* (varnode (third classargs)) cont callargs) ) )
		((eq? number-type (second classargs))
		 (make-node '##core#call '(#t) (cons cont callargs)) )
		(else #f) ) ) )

    ;; (<alloc-op> ...) -> (##core#inline_allocate (<aiop> <words>) ...)
    ((16) ; classargs = (<argc> <aiop> <safe> <words>)
     ;; - <argc> may be #f, saying that any number of arguments is allowed,
     ;; - <words> may be a list of one element (the number of words), meaning that
     ;;   the words are to be multiplied with the number of arguments.
     ;; - <words> may also be #t, meaning that the number of words is the same as the
     ;;   number of arguments plus 1.
     (let ([argc (first classargs)]
	   [rargc (length callargs)]
	   [w (fourth classargs)] )
       (and inline-substitutions-enabled
	    (or (not argc) (= rargc argc))
	    (or (test name 'extended-binding) (test name 'standard-binding))
	    (or (third classargs) unsafe)
	    (make-node
	     '##core#call '(#t)
	     (list cont 
		   (make-node
		    '##core#inline_allocate
		    (list (second classargs) 
			  (cond [(eq? #t w) (add1 rargc)]
				[(pair? w) (* rargc (car w))]
				[else w] ) )
		    callargs) ) ) ) ) )

    ;; (<op> ...) -> (##core#inline <iop>/<unsafe-iop> ...)
    ((17) ; classargs = (<argc> <iop-safe> [<iop-unsafe>])
     (and inline-substitutions-enabled
	  (= (length callargs) (first classargs))
	  (or (test name 'extended-binding) (test name 'standard-binding))
	  (make-node
	   '##core#call '(#t)
	   (list cont
		 (make-node '##core#inline
			    (list (if (and unsafe (pair? (cddr classargs)))
				      (third classargs)
				      (second classargs) ) )
			    callargs)) ) ) )

    ;; (<op>) -> (quote <null>)
    ((18) ; classargs = (<null>)
     (and inline-substitutions-enabled
	  (null? callargs)
	  (or (test name 'extended-binding) (test name 'standard-binding))
	  (make-node '##core#call '(#t) (list cont (qnode (first classargs))) ) ) )

    ;; (<op>) -> <id>
    ;; (<op> <x>) -> <x>
    ;; (<op> <x1> ...) -> (##core#inline <fixop> <x1> (##core#inline <fixop> ...)) [fixnum-mode]
    ;; (<op> <x1> ...) -> (##core#inline <ufixop> <x1> (##core#inline <ufixop> ...)) [fixnum-mode + unsafe]
    ;; - Remove "<id>" from arguments.
    ((19) ; classargs = (<id> <fixop> <ufixop> <fixmode>)
     (and inline-substitutions-enabled
	  (or (test name 'standard-binding) (test name 'extended-binding))
	  (let* ([id (first classargs)]
		 [fixop (if unsafe (third classargs) (second classargs))]
		 [callargs 
		  (remove
		   (lambda (x)
		     (and (eq? 'quote (node-class x))
			  (eq? id (first (node-parameters x))) ) ) 
		   callargs) ] )
	    (cond [(null? callargs) (make-node '##core#call '(#t) (list cont (qnode id)))]
		  [(null? (cdr callargs))
		   (make-node '##core#call '(#t) (list cont (first callargs))) ]
		  [(or (fourth classargs) (eq? number-type 'fixnum))
		   (make-node
		    '##core#call '(#t)
		    (list
		     cont
		     (fold-inner
		      (lambda (x y)
			(make-node '##core#inline (list fixop) (list x y)) )
		      callargs) ) ) ]
		  [else #f] ) ) ) )

    ;; (<op> ...) -> (##core#inline <iop> <arg1> ... (quote <x>) <argN>)
    ((20) ; classargs = (<argc> <iop> <x> <safe>)
     (let ([n (length callargs)])
       (and (or (fourth classargs) unsafe)
	    inline-substitutions-enabled
	    (= n (first classargs))
	    (or (test name 'standard-binding) (test name 'extended-binding))
	    (make-node
	     '##core#call '(#t)
	     (list cont
		   (make-node 
		    '##core#inline (list (second classargs))
		    (let-values ([(head tail) (split-at callargs (sub1 n))])
		      (append head
			      (list (qnode (third classargs)))
			      tail) ) ) ) ) ) ) )

    ;; (<op>) -> <id>
    ;; (<op> <x>) -> <x>
    ;; (<op> <x1> ...) -> (##core#inline_allocate (<genop> <words>) <x1> (##core#inline_allocate (<genop> <words>) ...))
    ;; (<op> <x1> ...) -> (##core#inline <[u]fixop> <x1> (##core#inline <[u]fixop> ...)) [fixnum-mode (perhaps unsafe)]
    ;; - Remove "<id>" from arguments.
    ((21) ; classargs = (<id> <fixop> <ufixop> <genop> <words>)
     (and inline-substitutions-enabled
	  (or (test name 'standard-binding) (test name 'extended-binding))
	  (let* ([id (first classargs)]
		 [words (fifth classargs)]
		 [genop (fourth classargs)]
		 [fixop (if unsafe (third classargs) (second classargs))]
		 [callargs 
		  (remove
		   (lambda (x)
		     (and (eq? 'quote (node-class x))
			  (eq? id (first (node-parameters x))) ) ) 
		   callargs) ] )
	    (cond [(null? callargs) (make-node '##core#call '(#t) (list cont (qnode id)))]
		  [(null? (cdr callargs))
		   (make-node '##core#call '(#t) (list cont (first callargs))) ]
		  [else
		   (make-node
		    '##core#call '(#t)
		    (list
		     cont
		     (fold-inner
		      (lambda (x y)
			(if (eq? number-type 'fixnum)
			    (make-node '##core#inline (list fixop) (list x y))
			    (make-node '##core#inline_allocate (list genop words) (list x y)) ) )
		      callargs) ) ) ] ) ) ) )

    ;; (<alloc-op> ...) -> (##core#inline_allocate (<aiop> <words>) ...)
    ;; (<alloc-op> ...) -> (##core#inline <fxop> ...) [fixnum mode]
    ((22) ; classargs = (<argc> <aiop> <safe> <words> <fxop>)
     (let ([argc (first classargs)]
	   [rargc (length callargs)]
	   [w (fourth classargs)] )
       (and inline-substitutions-enabled
	    (= rargc argc)
	    (or (test name 'extended-binding) (test name 'standard-binding))
	    (or (third classargs) unsafe)
	    (make-node
	     '##core#call '(#t)
	     (list cont 
		   (if (eq? number-type 'fixnum)
		       (make-node
			'##core#inline
			(list (fifth classargs))
			callargs)
		       (make-node
			'##core#inline_allocate
			(list (second classargs) w)
			callargs) ) ) ) ) ) )

    (else (bomb "bad type (optimize)")) ) )


;;; Optimize direct leaf routines:

(define (transform-direct-lambdas! node db)
  (let ([dirty #f]
	[inner-ks '()] 
	[hoistable '()] 
	[allocated 0] )

    ;; Process node tree and walk lambdas that meet the following constraints:
    ;;  - Only external lambdas (no CPS redexes),
    ;;  - All calls are either to the direct continuation or (tail-) recursive calls.
    ;;  - No allocation, no rest parameter.
    ;;  - The lambda has a known container variable and all it's call-sites are known.

    (define (walk d n dn)
      (let ([params (node-parameters n)]
	    [subs (node-subexpressions n)] )
	(case (node-class n)
	  [(##core#lambda)
	   (let ([llist (third params)])
	     (if (and d
		      (second params)
		      (not (get db d 'unknown))
		      (proper-list? llist)
		      (and-let* ([val (get db d 'value)]
				 [refs (get db d 'references)]
				 [sites (get db d 'call-sites)] )
			(and (eq? n val)
			     (= (length refs) (length sites))
			     (scan (first subs) (first llist) d dn (cons d llist)) ) ) )
		 (transform n d inner-ks hoistable dn allocated) 
		 (walk #f (first subs) #f) ) ) ]
	  [(set!) (walk (first params) (first subs) #f)]
	  [(let)
	   (walk (first params) (first subs) n)
	   (walk #f (second subs) #f) ]
	  [else (for-each (lambda (x) (walk #f x #f)) subs)] ) ) )

    (define (scan n kvar fnvar destn env)
      (let ([closures '()]
	    [recursive #f] )
	(define (rec n v vn e)
	  (let ([params (node-parameters n)]
		[subs (node-subexpressions n)] )
	    (case (node-class n)
	      [(##core#variable)
	       (let ([v (first params)])
		 (or (not (get db v 'boxed))
		     (not (memq v env))
		     (and (not recursive)
			  (begin
			    (set! allocated (+ allocated 2))
			    #t) ) ) ) ]
	      [(##core#lambda)
	       (and v
		    (decompose-lambda-list
		     (third params)
		     (lambda (vars argc rest)
		       (set! closures (cons v closures))
		       (rec (first subs) #f #f (append vars e)) ) ) ) ]
	      [(##core#inline_allocate)
	       (and (not recursive)
		    (begin
		      (set! allocated (+ allocated (second params)))
		      (every (lambda (x) (rec x #f #f e)) subs) ) ) ]
	      [(##core#direct_lambda)
	       (and vn destn
		    (null? (scan-used-variables (first subs) e)) 
		    (begin
		      (set! hoistable (alist-cons v vn hoistable))
		      #t) ) ]
	      [(##core#inline_ref)
	       (and (let ([n (estimate-foreign-result-size (second params))])
		      (or (zero? n)
			  (and (not recursive)
			       (begin
				 (set! allocated (+ allocated n))
				 #t) ) ) )
		    (every (lambda (x) (rec x #f #f e)) subs) ) ]
	      [(##core#inline_loc_ref)
	       (and (let ([n (estimate-foreign-result-size (first params))])
		      (or (zero? n)
			  (and (not recursive)
			       (begin
				 (set! allocated (+ allocated n))
				 #t) ) ) )
		    (every (lambda (x) (rec x #f #f e)) subs) ) ]
	      [(##core#call)
	       (let ([fn (first subs)])
		 (and (eq? '##core#variable (node-class fn))
		      (let ([v (first (node-parameters fn))])
			(cond [(eq? v fnvar)
			       (and (zero? allocated)
				    (let ([k (second subs)])
				      (when (eq? '##core#variable (node-class k))
					(set! inner-ks (cons (first (node-parameters k)) inner-ks)) )
				      (set! recursive #t)
				      #t) ) ]
			      [else (eq? v kvar)] ) )
		      (every (lambda (x) (rec x #f #f e)) (cdr subs)) ) ) ]
	      [(##core#direct_call)
	       (let ([n (fourth params)])
		 (or (zero? n)
		     (and (not recursive)
			  (begin
			    (set! allocated (+ allocated n))
			    (every (lambda (x) (rec x #f #f e)) subs) ) ) ) ) ]
	      [(set!) (rec (first subs) (first params) #f e)]
	      [(let)
	       (and (rec (first subs) (first params) n e)
		    (rec (second subs) #f #f (append params e)) ) ]
	      [else (every (lambda (x) (rec x #f #f e)) subs)] ) ) )
	(set! inner-ks '())
	(set! hoistable '())
	(set! allocated 0)
	(and (rec n #f #f env)
	     (lset= eq? closures (delete kvar inner-ks eq?)) ) ) )

    (define (transform n fnvar ks hoistable destn allocated)
      (if (pair? hoistable)
	  (debugging 'o "direct leaf routine with hoistable closures/allocation" fnvar (delay (unzip1 hoistable)) allocated)
	  (debugging 'o "direct leaf routine/allocation" fnvar allocated) )
      (set! dirty #t)
      (let* ([params (node-parameters n)]
	     [argc (length (third params))]
	     [klambdas '()] 
	     [sites (get db fnvar 'call-sites)] 
	     [ksites '()] )
	(match params
	  [(id _ (kvar vars ...) _)
	   ;; Remove continuation argument:
	   (set-car! (cddr params) vars)
	   ;; Make "##core#direct_lambda":
	   (node-class-set! n '##core#direct_lambda)
	   ;; Transform recursive calls and remove unused continuations:

	   (let rec ([n (first (node-subexpressions n))])
	     (let ([params (node-parameters n)]
		   [subs (node-subexpressions n)] )
	       (case (node-class n)
		 [(##core#call)
		  (let* ([fn (first subs)]
			 [arg0 (second subs)]
			 [fnp (node-parameters fn)] 
			 [arg0p (node-parameters arg0)] )
		    (when (eq? '##core#variable (node-class fn))
		      (cond [(eq? fnvar (first fnp))
			     (set! ksites (alist-cons #f n ksites))
			     (cond [(eq? kvar (first arg0p))
				    (unless (= argc (length (cdr subs)))
				      (compiler-warning
				       'call
				       "known procedure called recursively with wrong number of arguments: `~A'" fnvar) )
				    (node-class-set! n '##core#recurse)
				    (node-parameters-set! n (list #t id))
				    (node-subexpressions-set! n (cddr subs)) ]
				   [(assq (first arg0p) klambdas)
				    => (lambda (a)
					 (let* ([klam (cdr a)]
						[kbody (first (node-subexpressions klam))] )
					   (unless (= argc (length (cdr subs)))
					     (compiler-warning
					      'call
					      "known procedure called recursively with wrong number of arguments: `~A'" fnvar) )
					   (node-class-set! n 'let)
					   (node-parameters-set! n (take (third (node-parameters klam)) 1))
					   (node-subexpressions-set!
					    n
					    (list (make-node '##core#recurse (list #f id) (cddr subs)) kbody) )
					   (rec kbody) ) ) ]
				   [else (bomb "missing kvar" arg0p)] ) ]
			    [(eq? kvar (first fnp))
			     (node-class-set! n '##core#return)
			     (node-parameters-set! n '())
			     (node-subexpressions-set! n (cdr subs)) ]
			    [else (bomb "bad call (leaf)")] ) ) ) ]
		 [(let)
		  (let ([var (first params)]
			[val (first subs)] )
		    (cond [(memq var ks)
			   (set! klambdas (alist-cons var val klambdas))
			   (copy-node! (second subs) n)
			   (rec n) ]
			  [else (for-each rec subs)] ) ) ]

		 [else (for-each rec subs)] ) ) )

	   ;; Transform call-sites:
	   (for-each
	    (lambda (site)
	      (let* ([n (cdr site)]
		     [nsubs (node-subexpressions n)] )
		(unless (= argc (length (cdr nsubs)))
		  (compiler-warning 
		   'call 
		   "known procedure called with wrong number of arguments: `~A'" fnvar) )
		(node-subexpressions-set!
		 n
		 (list (second nsubs)
		       (make-node
			'##core#direct_call
			(list #t #f id allocated)
			(cons (car nsubs) (cddr nsubs)) ) ) ) ) )
	    (lset-difference (lambda (s1 s2) (eq? (cdr s1) (cdr s2))) sites ksites) )

	   ;; Hoist direct lambdas out of container:
	   (when (and destn (pair? hoistable))
	     (let ([destn0 (make-node #f #f #f)])
	       (copy-node! destn destn0) ; get copy of container binding
	       (let ([hoisted
		      (fold-right	; build cascade of bindings for each hoistable direct lambda...
		       (lambda (h rest)
			 (make-node
			  'let (list (car h))
			  (let ([dlam (first (node-subexpressions (cdr h)))])
			    (list (make-node (node-class dlam) (node-parameters dlam) (node-subexpressions dlam))
				  rest) ) ) )
		       destn0
		       hoistable) ] )
		 (copy-node! hoisted destn) ; mutate container binding to hold hoistable bindings
		 (for-each 
		  (lambda (h)		; change old direct lambdas bindings to dummy ones...
		    (let ([vn (cdr h)])
		      (node-parameters-set! vn (list (gensym)))
		      (set-car! (node-subexpressions vn) (make-node '##core#undefined '() '())) ) )
		  hoistable) ) ) ) ]
	  [_ (bomb "invalid parameter list" params)] ) ) )

    (debugging 'p "direct leaf routine optimization pass...")
    (walk #f node #f)
    dirty) )


;;; Lambda lift:
;
; - Find lambda-liftable local procedures and lift them to toplevel.
; - Pass free variables as extra parameters, including the free variables of
;   other lifted procedures. This implies that lifted procedures that call each
;   other have to be in the same scope.
; - Declare the lifted procedures (effectively) as bound-to-procedure and block-global.

(define (perform-lambda-lifting! node db)
  (let ([lambda-values '()]
	[eliminated '()] )
    
    (define (find-lifting-candidates)
      ;; Collect potentially liftable procedures and return as a list of (<name> . <value>) pairs:
      ;; - Also build a-list that maps lambda-nodes to names.
      (let ([cs '()])
	(##sys#hash-table-for-each
	 (lambda (sym plist)
	   (and-let* ([val (assq 'value plist)]
		      [refs (assq 'references plist)]
		      [css (assq 'call-sites plist)] 
		      [nrefs (length (cdr refs))] )
	     (when (and (not (assq 'unknown plist))
			(eq? 'lambda (node-class (cdr val)))
			(not (assq 'global plist)) 
			#;(> nrefs 1)
			(= nrefs (length (cdr css))) )
	       (set! lambda-values (alist-cons (cdr val) sym lambda-values))
	       (set! cs (alist-cons sym (cdr val) cs)) ) ) )
	 db)
	cs) )

    (define (build-call-graph cs)
      ;; Build call-graph of the form ((<name> (<free1> ...) <called1> ...) ...):
      (let ([g '()]
	    [free '()]
	    [called '()] )

	(define (walk n env)
	  (let ([class (node-class n)]
		[params (node-parameters n)]
		[subs (node-subexpressions n)] )
	    (case class
	      [(##core#variable set!)
	       (let ([var (first params)])
		 (unless (or (memq var env) (get db var 'global))
		   (set! free (cons var free)) )
		 (when (assq var cs) (set! called (cons var called)))
		 (for-each (lambda (n) (walk n env)) subs) ) ]
	      [(let)
	       (let loop ([vars params] [vals subs])
		 (if (null? vars)
		     (walk (car vals) (append params env))
		     (let ([var (car vars)])
		       (walk (car vals) env)
		       (loop (cdr vars) (cdr vals)) ) ) ) ]
	      [(lambda)
	       (decompose-lambda-list
		(first params)
		(lambda (vars argc rest) (walk (first subs) (append vars env))) ) ]
	      [else (for-each (lambda (n) (walk n env)) subs)] ) ) )

	(for-each
	 (lambda (cs)
	   (let* ([here (car cs)]
		  [lval (cdr cs)] 
		  [llist (car (node-parameters lval))] )
	     (set! free '())
	     (set! called '())
	     (decompose-lambda-list
	      llist
	      (lambda (vars arg rest)
		(walk (car (node-subexpressions lval)) vars) ) )
	     (set! g (alist-cons here (cons free called) g)) ) )
	 cs)
	g) )

    (define (eliminate cs graph)
      ;; Eliminate all liftables that have free variables that are assigned to (and are not liftable),
      ;;  or that have more than N free variables (including free variables of called liftables):
      (remove
       (lambda (gn)
	 (or (> (count-free-variables (car gn) graph) maximal-number-of-free-variables-for-liftable)
	     (any (lambda (v) 
		    (and (get db v 'assigned) 
			 (not (assq v cs)) ) )
		  (second gn) ) ) )
       graph) )

    (define (count-free-variables name graph)
      (let ([gnames (unzip1 graph)])
	(let count ([n name] [walked '()])
	  (let* ([a (assq n graph)]
		 [cs (lset-difference eq? (cddr a) walked gnames)]
		 [f (length (delete-duplicates (second a) eq?))]
		 [w2 (cons n (append cs walked))] )
	    (fold + f (map (lambda (c) (count c w2)) cs)) ) ) ) )

    (define (collect-accessibles graph)
      ;; Collect accessible variables for each liftable into list of the form (<name> <accessible1> ...):
      (let ([al '()])
	(let walk ([n node] [vars '()])
	  (let ([class (node-class n)]
		[params (node-parameters n)]
		[subs (node-subexpressions n)] )
	    (case class
	      [(##core#variable quote ##core#undefined ##core#primitive ##core#proc) #f]
	      [(let)
	       (let loop ([vars2 params] [vals subs])
		 (if (null? vars2)
		     (walk (car vals) (append params vars))
		     (begin
		       (walk (car vals) vars)
		       (loop (cdr vars2) (cdr vals)) ) ) ) ]
	      [(lambda)
	       (let ([lval (assq n lambda-values)])
		 (when lval
		   (let ([name (cdr lval)])
		     (when (assq name graph)
		       (set! al (alist-cons (cdr lval) vars al))) ) ) )
	       (decompose-lambda-list
		(first params)
		(lambda (vars2 argc rest)
		  (walk (car subs) (append vars2 vars)) ) ) ]
	      [else
	       (for-each (lambda (n) (walk n vars)) subs) ] ) ) )
	al) )

    (define (eliminate2 graph al)
      ;; Eliminate liftables that have call-sites without access to all free variables;
      (remove
       (lambda (gn)
	 (let* ([name (first gn)]
		[free (second gn)] )
	   (any (lambda (gn2)
		  (and (memq name (cddr gn2)) ; callee?
		       (lset<= eq? (cdr (assq (car gn2) al)) free) ) )
		graph) ) ) 
       graph) )

    (define (eliminate3 graph)
      ;; Eliminate liftables that call other eliminated liftables:
      ;; - repeat until nothing changes.
      (let loop ([graph graph] [n (length graph)])
	(let* ([g2 (filter (lambda (gn) (every (lambda (n) (assq n graph)) (cddr gn))) graph)]
	       [n2 (length g2)] )
	  (if (= n n2)
	      g2
	      (loop g2 n2) ) ) ) )

    (define (eliminate4 graph)
      ;; Eliminate liftables that have unknown call-sites which do not have access to
      ;;  any of the free variables of all callees:
      (let walk ([n node] [vars '()])
	(let ([class (node-class n)]
	      [params (node-parameters n)]
	      [subs (node-subexpressions n)] )
	  (case class
	    [(##core#variable quote ##core#undefined ##core#primitive ##core#proc) #f]
	    [(let)
	     (let loop ([vars2 params] [vals subs])
	       (if (null? vars2)
		   (walk (car vals) (append params vars))
		   (begin
		     (walk (car vals) vars)
		     (loop (cdr vars2) (cdr vals)) ) ) ) ]
	    [(lambda)
	     (decompose-lambda-list
	      (first params)
	      (lambda (vars2 argc rest)
		(walk (car subs) (append vars2 vars)) ) ) ]
	    [(##core#call)
	     (let ([fn (first subs)])
	       (call-with-current-continuation
		(lambda (return)
		  (when (eq? '##core#variable (node-class fn))
		    (let ([done '()])
		      (let loop ([name (first (node-parameters fn))])
			(unless (memq name done)
			  (set! done (cons name done))
			  (let ([gn (assq name graph)])
			    (when gn
			      (unless (lset<= eq? (second gn) vars)
				#;(print "*** " (first (node-parameters fn)) " | " name ": " vars " / " (second gn)) 
				(set! graph (delete! gn graph eq?))
				(return #f) )
			      (for-each loop (cddr gn)) ) ) ) ) ) ) ) )
	       (for-each (lambda (n) (walk n vars)) subs) ) ]
	    [else (for-each (lambda (n) (walk n vars)) subs)] ) ) )
      graph)

    (define (compute-extra-variables graph)
      ;; Gather variables that have to be passed additionally:
      ;; - do not pass variables that are defined inside the body of a liftable.
      (define (defined n)
	(let ([defd '()])
	  (let walk ([n n])
	    (let ([class (node-class n)]
		  [params (node-parameters n)]
		  [subs (node-subexpressions n)] )
	      (case class
		[(let)
		 (set! defd (append params defd))
		 (for-each walk subs) ]
		[(lambda)
		 (decompose-lambda-list
		  (first params)
		  (lambda (vars argc rest)
		    (set! defd (append vars defd))
		    (walk (first subs)) ) ) ]
		[else (for-each walk subs)] ) ) )
	  defd) )
      (let ([extras (map (lambda (gn) (cons (first gn) (second gn))) graph)]
	    [walked '()] )
	(define (walk gn)
	  (let ([name (car gn)])
	    ;; Hm. To handle liftables that are called recursively (but indirect) I use this kludge. Is it safe?
	    (unless (> (count (cut eq? name <>) walked) 1)
	      (set! walked (cons name walked))
	      (let ([callees (cddr gn)])
		(for-each (lambda (c) (walk (assq c graph))) callees)
		(let ([f (assq name extras)])
		  (set-cdr! f (append (cdr f) (concatenate (map (lambda (n2) (cdr (assq n2 extras))) callees)))) ) ) ) ) )
	(for-each walk graph)
	(map (lambda (xt)
	       (let* ([name (car xt)]
		      [defd (defined (get db name 'value))] )
		 (cons name
		       (remove 
			(lambda (v)
			  (or (assq v graph)
			      (memq v defd) ) )
			(delete-duplicates (cdr xt) eq?)) ) ) )
	     extras) ) )

    (define (reconstruct! graph extra)
      ;; Reconstruct node tree by adding global definitions:
      (node-subexpressions-set!
       node
       (list
	(fold-right
	 (lambda (gn body)
	   (let* ([name (car gn)]
		  [lval (get db name 'value)] )
	     (set! block-globals (cons name block-globals))
	     (decompose-lambda-list
	      (first (node-parameters lval))
	      (lambda (vars argc rest)
		(let* ([xvars (cdr (assq name extra))]
		       [xaliases (map gensym xvars)]
		       [xmap (map cons xvars xaliases)] )
		  (rename-extra-variables! (first (node-subexpressions lval)) xmap)
		  (make-node
		   'let (list (gensym 't))
		   (list (make-node
			  'set! (list name)
			  (list
			   (make-node
			    'lambda
			    (list (build-lambda-list (append xaliases vars) (+ argc (length xvars)) rest))
			    (node-subexpressions lval) ) ) )
			 body) ) ) ) ) ) )
	 (first (node-subexpressions node))
	 graph) ) ) )

    (define (rename-extra-variables! node xmap)
      ;; Rename variables from a given map:
      (define (rename v)
	(let ([a (assq v xmap)])
	  (if a (cdr a) v) ) )
      (let walk ([n node])
	(let ([class (node-class n)]
	      [params (node-parameters n)]
	      [subs (node-subexpressions n)] )
	  (case class
	    [(let)
	     (node-parameters-set! n (map rename params))
	     (for-each walk subs) ]
	    [(##core#variable)
	     (node-parameters-set! n (list (rename (first params)))) ]
	    [(set!)
	     (node-parameters-set! n (list (rename (first params))))
	     (for-each walk subs) ]
	    [(lambda)
	     (decompose-lambda-list
	      (first params)
	      (lambda (vars argc rest)
		(set-car! params (build-lambda-list (map rename vars) argc rest)) 
		(walk (first subs)) ) ) ]
	    [else (for-each walk subs)] ) ) ) )

    (define (extend-call-sites! extra)
      ;; Change call sites by adding extra variables:
      (let walk ([n node])
	(let ([class (node-class n)]
	      [params (node-parameters n)]
	      [subs (node-subexpressions n)] )
	  (case class
	    [(##core#call)
	     (let ([fn (first subs)])
	       (when (eq? '##core#variable (node-class fn))
		 (let ([a (assq (first (node-parameters fn)) extra)])
		   (when a
		     (set-car! params #t)
		     (node-subexpressions-set! 
		      n
		      (cons fn (append (map varnode (cdr a)) (cdr subs))) ) ) ) )
	       (for-each walk (node-subexpressions n)) ) ]
	    [else (for-each walk subs)] ) ) ) )

    (define (remove-local-bindings! graph)
      ;; Remove local definitions of lifted procedures:
      (let walk ([n node])
	(let ([class (node-class n)]
	      [params (node-parameters n)]
	      [subs (node-subexpressions n)] )
	  (case class
	    [(let)
	     (for-each walk (node-subexpressions n))
	     (let ([vars2 '()]
		   [vals2 '()] )
	       (do ([vars params (cdr vars)]
		    [vals subs (cdr vals)] )
		   ((null? vars)
		    (cond [(null? vars2) (copy-node! (car vals) n)]
			  [else
			   (node-parameters-set! n (reverse vars2))
			   (node-subexpressions-set! n (append (reverse vals2) vals)) ] ) )
		 (unless (assq (car vars) graph)
		   (set! vars2 (cons (car vars) vars2))
		   (set! vals2 (cons (car vals) vals2)) ) ) ) ]
	    [(set!)
	     (for-each walk (node-subexpressions n))
	     (when (assq (first params) graph)
	       (node-class-set! n '##core#undefined)
	       (node-parameters-set! n '())
	       (node-subexpressions-set! n '()) ) ]
	    [else (for-each walk subs)] ) ) ) )

    (debugging 'p "gathering liftables...")
    (let ([cs (find-lifting-candidates)])
      (debugging 'p "building call graph...")
      (let ([g (build-call-graph cs)])
	(debugging 'p "eliminating non-liftables...")
	(let ([g2 (eliminate cs g)])
	  (when (debugging 'l "call-graph:") (pretty-print g2))
	  (debugging 'p "computing access-lists...")
	  (let ([al (collect-accessibles g2)])
	    (when (debugging 'l "accessibles:") (pretty-print al))
	    (debugging 'p "eliminating liftables by access-lists and non-liftable callees...")
	    (let ([ls (eliminate3 (eliminate4 g2))]) ;(eliminate2 g2 al)))])
	      (debugging 'o "liftable local procedures" (delay (unzip1 ls)))
	      (debugging 'p "gathering extra parameters...")
	      (let ([extra (compute-extra-variables ls)])
		(when (debugging 'l "additional parameters:") (pretty-print extra))
		(debugging 'p "changing call sites...")
		(extend-call-sites! extra)
		(debugging 'p "removing local bindings...")
		(remove-local-bindings! ls)
		(debugging 'p "moving liftables to toplevel...")
		(reconstruct! ls extra) ) ) ) ) ) ) ) )
