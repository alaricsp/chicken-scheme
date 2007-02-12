;;;; compiler.scm - The CHICKEN Scheme compiler
;
;
; "This is insane. What we clearly want to do is not exactly clear, and is rooted in NCOMPLR."
;
;
;-----------------------------------------------------------------------------------------------------------
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
;
;
; Supported syntax:
;
; - Declaration specifiers:
;
; (unit <unitname>)
; (uses {<unitname>})
; ([not] standard-bindings {<name>})
; ([not] usual-integrations {<name>})
; ([not] extended-bindings (<name>})
; ([number-type] <type>)
; (fixnum-arithmetic)
; (unsafe)
; ([not] safe)
; ([not] interrupts-enabled)
; (no-bound-checks)
; (no-argc-checks)
; (no-procedure-checks)
; (no-procedure-checks-for-usual-bindings)
; (block-global {<name>})
; (lambda-lift)
; (hide {<name>})
; (disable-interrupts)
; (disable-warning <class> ...)
; (always-bound {<name>})
; (foreign-declare {<string>})
; (foreign-parse {<string>})
; (block)
; (separate)
; (run-time-macros)
; (export {<name>})
; (compress-literals [<threshold>])
; (safe-globals)
; (namespace <name> {<symbol})
; (custom-declare (<tag> <name> <filename> <arg> ...) <string> ...)
; (data <tag1> <exp1> ...)
; (post-process <string> ...)
; (emit-exports <string>)
; (keep-shadowed-macros)
; (import <symbol-or-string> ...)
;
;   <type> = fixnum | generic
;
; - Source language:
;
; <variable>
; <constant>
; (##core#declare {(quote <spec>)})
; (##core#immutable <exp>)
; (##core#global-ref <variable>)
; (quote <exp>)
; (if <exp> <exp> [<exp>])
; (let ({(<variable> <exp>)}) <body>)
; (##core#let-location (quote <symbol>) (quote <type>) [<init>] <exp>)
; (lambda <variable> <body>)
; (lambda ({<variable>}+ [. <variable>]) <body>)
; (set! <variable> <exp>)
; (##core#set! <variable> <exp>)
; (##core#named-lambda <name> <llist> <body>)
; (##core#loop-lambda <llist> <body>)
; (##core#undefined)
; (##core#primitive <name>)
; (##core#inline <op> {<exp>})
; (##core#inline_allocate (<op> <words>) {<exp>})
; (##core#inline_ref (<name> <type>))
; (##core#inline_update (<name> <type>) <exp>)
; (##core#inline_loc_ref (<type>) <exp>)
; (##core#inline_loc_update (<type>) <exp> <exp>)
; (##core#compiletimetoo <exp>)
; (##core#compiletimeonly <exp>)
; (##core#elaborationtimetoo <exp>)
; (##core#elaborationtimeonly <exp>)
; (##core#define-foreign-variable (quote <symbol>) (quote <type>) [(quote <string>)])
; (##core#define-foreign-type (quote <symbol>) (quote <type>) [<proc1> [<proc2>]])
; (##core#foreign-lambda (quote <type>) (quote <string>) {(quote <type>)})
; (##core#foreign-lambda* (quote <type>) (quote ({(<type> <var>)})) {(quote <string>)})
; (##core#foreign-callback-lambda (quote <type>) (quote <string>) {(quote <type>)})
; (##core#foreign-callback-lambda* (quote <type>) (quote ({(<type> <var>)})) {(quote <string>)})
; (##core#foreign-primitive (quote <type>) (quote ({(<type> <var>)})) {(quote <string>)})
; (##core#define-inline (quote <name>) <exp>)
; (##core#define-constant (quote <name>) <exp>)
; (##core#foreign-callback-wrapper (quote <name>) (quote <qualifiers>) (quote <type>) (quote {<type>}) <exp>)
; (##core#define-external-variable (quote <name>) (quote <type>) (quote <bool>))
; (##core#check <exp>)
; (##core#require-for-syntax <exp> ...)
; (##core#require-extension '<id> ...)
; (##core#app <exp> {<exp>})
; (<exp> {<exp>})
;
; - Core language:
;
; [##core#variable {<variable>}]
; [if {} <exp> <exp> <exp>)]
; [quote {<exp>}]
; [let {<variable>} <exp-v> <exp>]
; [##core#lambda {<id> <mode> (<variable>... [. <variable>]) <size>} <exp>]
; [set! {<variable>} <exp>]
; [##core#undefined {}]
; [##core#global-ref {<variable>}]
; [##core#primitive {<name>}]
; [##core#inline {<op>} <exp>...]
; [##core#inline_allocate {<op> <words>} <exp>...]
; [##core#inline_ref {<name> <type>}]
; [##core#inline_update {<name> <type>} <exp>]
; [##core#inline_loc_ref {<type>} <exp>]
; [##core#inline_loc_update {<type>} <exp> <exp>]
; [##core#call {<safe-flag> [<debug-info>]} <exp-f> <exp>...]
; [##core#callunit {<unitname>} <exp>...]
; [##core#switch {<count>} <exp> <const1> <body1> ... <defaultbody>]
; [##core#cond <exp> <exp> <exp>]
; [##core#recurse {<tail-flag>} <exp1> ...]
; [##core#return <exp>]
; [##core#direct_call {<safe-flag> <debug-info> <call-id> <words>} <exp-f> <exp>...]
; [##core#direct_lambda {<id> <mode> (<variable>... [. <variable>]) <size>} <exp>]
;
; - Closure converted/prepared language:
;
; [if {} <exp> <exp> <exp>]
; [quote {<exp>}]
; [##core#bind {<count>} <exp-v>... <exp>]
; [##core#undefined {}]
; [##core#inline {<op>} <exp>...]
; [##core#inline_allocate {<op <words>} <exp>...]
; [##core#inline_ref {<name> <type>}]
; [##core#inline_update {<name> <type>} <exp>]
; [##core#inline_loc_ref {<type>} <exp>]
; [##core#inline_loc_update {<type>} <exp> <exp>]
; [##core#closure {<count>} <exp>...]
; [##core#box {} <exp>]
; [##core#unbox {} <exp>]
; [##core#ref {<index>} <exp>]
; [##core#update {<index>} <exp> <exp>]
; [##core#updatebox {} <exp> <exp>]
; [##core#update_i {<index>} <exp> <exp>]
; [##core#updatebox_i {} <exp> <exp>]
; [##core#call {<safe-flag> [<debug-info> [<call-id> <customizable-flag>]]} <exp-f> <exp>...]
; [##core#callunit {<unitname>} <exp>...]
; [##core#local {<index>}]
; [##core#setlocal {<index>} <exp>]
; [##core#global {<literal> <safe-flag> <block-mode> [<name>]}]
; [##core#setglobal {<literal> <block-mode>} <exp>]
; [##core#setglobal_i {<literal> <block-mode>} <exp>]
; [##core#literal {<literal>}]
; [##core#immediate {<type> [<immediate>]}]     - type: bool/fix/nil/char
; [##core#proc {<name> [<non-internal>]}]
; [##core#recurse {<tail-flag> <call-id>} <exp1> ...]
; [##core#return <exp>]
; [##core#direct_call {<safe-flag> <debug-info> <call-id> <words>} <exp-f> <exp>...]
;
;
; Analysis database entries:
;
; <variable>:
;
;   captured -> <boolean>                    If true: variable is used outside it's home-scope
;   global -> <boolean>                      If true: variable does not occur in any lambda-list
;   call-sites -> ((<lambda-id> <node>) ...) Known call-nodes of a named procedure
;   home -> <lambda-id>                      Procedure which introduces this variable
;   unknown -> <boolean>                     If true: variable can not have a known value
;   assigned -> <boolean>                    If true: variable is assigned somewhere
;   assigned-locally -> <boolean>            If true: variable has been assigned inside user lambda
;   undefined -> <boolean>                   If true: variable is unknown yet but can be known later
;   value -> <node>                          Variable has a known value
;   potential-value -> <node>                Global variable was assigned this value
;   references -> (<node> ...)               Nodes that are accesses of this variable (##core#variable nodes)
;   side-effecting -> <boolean>              If true: variable names side-effecting standard-binding
;   foldable -> <boolean>                    If true: variable names foldable standard-binding
;   boxed -> <boolean>                       If true: variable has to be boxed after closure-conversion
;   contractable -> <boolean>                If true: variable names contractable procedure
;   inlinable -> <boolean>                  If true: variable names potentially inlinable procedure
;   collapsable -> <boolean>                 If true: variable refers to collapsable constant
;   removable -> <boolean>                   If true: variable is not used
;   replacable -> <variable>                 Variable can be replaced by another variable
;   replacing -> <boolean>                   If true: variable can replace another variable (don't remove)
;   standard-binding -> <boolean>            If true: variable names a standard binding
;   extended-binding -> <boolean>            If true: variable names an extended binding
;   unused -> <boolean>                      If true: variable is a formal parameter that is never used
;   rest-parameter -> #f | 'vector | 'list   If true: variable holds rest-argument list mode
;   o-r/access-count -> <n>                  Contains number of references as arguments of optimizable rest operators
;   constant -> <boolean>                    If true: variable has fixed value
; 
; <lambda-id>:
;
;   contains -> (<lambda-id> ...)            Procedures contained in this lambda
;   contained-in -> <lambda-id>              Procedure containing this lambda
;   has-unused-parameters -> <boolean>       If true: procedure has unused formal parameters
;   use-expr -> (<lambda-id> ...)            Marks non-direct use-sites of common subexpression
;   closure-size -> <integer>                Number of free variables stored in a closure
;   customizable -> <boolean>                If true: all call sites are known, procedure does not escape
;   simple -> <boolean>                      If true: procedure only calls its continuation
;   explicit-rest -> <boolean>               If true: procedure is called with consed rest list
;   captured-variables -> (<var> ...)        List of closed over variables


(declare
 (unit compiler)
 (disable-warning var) )

#>
#ifndef C_USE_C_DEFAULTS
# define C_INSTALL_SHARE_HOME NULL
#endif

#ifndef C_DEFAULT_TARGET_STACK_SIZE
# define C_DEFAULT_TARGET_STACK_SIZE 0
#endif

#ifndef C_DEFAULT_TARGET_HEAP_SIZE
# define C_DEFAULT_TARGET_HEAP_SIZE 0
#endif
<#


#{compiler
  compiler-arguments process-command-line explicit-use-flag inline-list not-inline-list
  default-standard-bindings default-extended-bindings side-effecting-standard-bindings
  non-foldable-standard-bindings foldable-standard-bindings non-foldable-extended-bindings foldable-extended-bindings
  standard-bindings-that-never-return-false side-effect-free-standard-bindings-that-never-return-false
  installation-home decompose-lambda-list external-to-pointer defconstant-bindings constant-declarations
  copy-node! error-is-extended-binding toplevel-scope toplevel-lambda-id
  unit-name insert-timer-checks used-units external-variables require-imports-flag custom-declare-alist
  profile-info-vector-name finish-foreign-result pending-canonicalizations
  foreign-declarations emit-trace-info block-compilation line-number-database-size
  always-bound-to-procedure block-globals make-block-variable-literal block-variable-literal? block-variable-literal-name
  target-heap-size target-stack-size valid-c-identifier?
  target-initial-heap-size internal-bindings source-filename dump-nodes source-info->string
  default-default-target-heap-size default-default-target-stack-size verbose-mode original-program-size
  current-program-size line-number-database-2 foreign-lambda-stubs immutable-constants foreign-variables
  rest-parameters-promoted-to-vector inline-table inline-table-used constant-table constants-used mutable-constants
  broken-constant-nodes inline-substitutions-enabled loop-lambda-names expand-profile-lambda
  profile-lambda-list profile-lambda-index emit-profile expand-profile-lambda
  direct-call-ids foreign-type-table first-analysis callback-names namespace-table disabled-warnings
  initialize-compiler canonicalize-expression expand-foreign-lambda update-line-number-database! scan-toplevel-assignments
  compiler-warning import-table use-import-table
  perform-cps-conversion analyze-expression simplifications perform-high-level-optimizations perform-pre-optimization!
  reorganize-recursive-bindings substitution-table simplify-named-call inline-max-size
  perform-closure-conversion prepare-for-code-generation compiler-source-file create-foreign-stub 
  expand-foreign-lambda* data-declarations emit-control-file-item expand-foreign-primitive
  process-declaration external-protos-first basic-literal? emit-line-info
  transform-direct-lambdas! expand-foreign-callback-lambda* debugging emit-unsafe-marker
  debugging-chicken bomb check-signature posq stringify symbolify build-lambda-list
  string->c-identifier c-ify-string words check-and-open-input-file close-checked-input-file fold-inner constant?
  collapsable-literal? immediate? canonicalize-begin-body extract-mutable-constants string->expr get get-all
  put! collect! count! get-line get-line-2 find-lambda-container display-analysis-database varnode qnode 
  build-node-graph build-expression-tree fold-boolean inline-lambda-bindings match-node expression-has-side-effects?
  simple-lambda-node? compute-database-statistics print-program-statistics output gen gen-list 
  pprint-expressions-to-file foreign-type-check estimate-foreign-result-size scan-used-variables scan-free-variables
  topological-sort print-version print-usage initialize-analysis-database export-list csc-control-file
  estimate-foreign-result-location-size compressed-literals-initializer
  expand-foreign-callback-lambda default-optimization-passes default-optimization-passes-when-trying-harder
  units-used-by-default words-per-flonum disable-stack-overflow-checking
  parameter-limit eq-inline-operator optimizable-rest-argument-operators postponed-initforms
  membership-test-operators membership-unfold-limit valid-compiler-options valid-compiler-options-with-argument
  make-random-name final-foreign-type real-name-table real-name set-real-name! safe-globals-flag
  location-pointer-map literal-compression-threshold compressed-literals compressable-literal
  lookup-exports-file undefine-shadowed-macros process-lambda-documentation emit-syntax-trace-info
  generate-code make-variable-list make-argument-list generate-foreign-stubs foreign-type-declaration
  process-custom-declaration do-lambda-lifting file-requirements emit-closure-info export-file-name
  foreign-argument-conversion foreign-result-conversion foreign-type-convert-argument foreign-type-convert-result}

(eval-when (compile eval)
  (match-error-control #:fail) )


(include "tweaks")


(define-inline (gensym-f-id) (gensym 'f_))

(eval-when (eval)
  (define installation-home #f)
  (define default-target-heap-size #f)
  (define default-target-stack-size #f) )

(eval-when (load)
  (define-foreign-variable installation-home c-string "C_INSTALL_SHARE_HOME")
  (define-foreign-variable default-target-heap-size int "C_DEFAULT_TARGET_HEAP_SIZE")
  (define-foreign-variable default-target-stack-size int "C_DEFAULT_TARGET_STACK_SIZE") )

(define user-options-pass (make-parameter #f))
(define user-read-pass (make-parameter #f))
(define user-preprocessor-pass (make-parameter #f))
(define user-pass (make-parameter #f))
(define user-pass-2 (make-parameter #f))
(define user-post-analysis-pass (make-parameter #f))

(define-constant foreign-type-table-size 301)
(define-constant analysis-database-size 3001)
(define-constant default-line-number-database-size 997)
(define-constant inline-table-size 301)
(define-constant constant-table-size 301)
(define-constant real-name-table-size 997)
(define-constant import-table-size 997)
(define-constant default-literal-compression-threshold 50)
(define-constant default-inline-max-size 10)


;;; Global variables containing compilation parameters:

(define unit-name #f)
(define number-type 'generic)
(define standard-bindings '())
(define extended-bindings '())
(define insert-timer-checks #t)
(define used-units '())
(define unsafe #f)
(define always-bound '())
(define always-bound-to-procedure '())
(define foreign-declarations '())
(define emit-trace-info #f)
(define block-compilation #f)
(define line-number-database-size default-line-number-database-size)
(define target-heap-size #f)
(define target-initial-heap-size #f)
(define target-stack-size #f)
(define optimize-leaf-routines #f)
(define emit-profile #f)
(define no-bound-checks #f)
(define no-argc-checks #f)
(define no-procedure-checks #f)
(define block-globals '())
(define source-filename #f)
(define export-list #f)
(define compressed-literals '())
(define literal-compression-threshold #f)
(define compressed-literals-initializer #f)
(define safe-globals-flag #f)
(define explicit-use-flag #f)
(define disable-stack-overflow-checking #f)
(define namespace-table '())
(define require-imports-flag #f)
(define emit-unsafe-marker #f)
(define external-protos-first #f)
(define do-lambda-lifting #f)
(define inline-max-size -1)
(define emit-closure-info #t)
(define emit-line-info #f)
(define export-file-name #f)
(define import-table #f)
(define use-import-table #f)
(define undefine-shadowed-macros #t)
(define constant-declarations '())


;;; These are here so that the backend can access them:

(define default-default-target-heap-size default-target-heap-size)
(define default-default-target-stack-size default-target-stack-size)


;;; Other global variables:

(define verbose-mode #f)
(define original-program-size #f)
(define current-program-size 0)
(define line-number-database-2 #f)
(define immutable-constants '())
(define rest-parameters-promoted-to-vector '())
(define inline-table #f)
(define inline-table-used #f)
(define constant-table #f)
(define constants-used #f)
(define mutable-constants '())
(define broken-constant-nodes '())
(define inline-substitutions-enabled #f)
(define direct-call-ids '())
(define first-analysis #t)
(define foreign-type-table #f)
(define foreign-variables '())
(define foreign-lambda-stubs '())
(define foreign-callback-stubs '())
(define external-variables '())
(define loop-lambda-names '())
(define profile-lambda-list '())
(define profile-lambda-index 0)
(define profile-info-vector-name #f)
(define external-to-pointer '())
(define error-is-extended-binding #f)
(define real-name-table #f)
(define location-pointer-map '())
(define pending-canonicalizations '())
(define defconstant-bindings '())
(define callback-names '())
(define toplevel-scope #t)
(define toplevel-lambda-id #f)
(define custom-declare-alist '())
(define csc-control-file #f)
(define data-declarations '())
(define inline-list '())
(define not-inline-list '())
(define file-requirements #f)
(define postponed-initforms '())


;;; Initialize globals:

(define (initialize-compiler)
  (if line-number-database-2
      (vector-fill! line-number-database-2 '())
      (set! line-number-database-2 (make-vector line-number-database-size '())) )
  (if inline-table
      (vector-fill! inline-table '())
      (set! inline-table (make-vector inline-table-size '())) )
  (if constant-table
      (vector-fill! constant-table '())
      (set! constant-table (make-vector constant-table-size '())) )
  (set! profile-info-vector-name (make-random-name 'profile-info))
  (set! real-name-table (make-vector real-name-table-size '()))
  (set! file-requirements (make-hash-table eq?))
  (if import-table
      (vector-fill! import-table '())
      (set! import-table (make-vector import-table-size '())) )
  (if foreign-type-table
      (vector-fill! foreign-type-table '())
      (set! foreign-type-table (make-vector foreign-type-table-size '())) ) )


;;; Expand macros and canonicalize expressions:

(define (canonicalize-expression exp)

  (define (resolve v ae)
    (cond [(assq v ae) => cdr]
	  [else v] ) )

  (define (set-real-names! as ns)
    (for-each (lambda (a n) (set-real-name! a n)) as ns) )

  (define (write-to-string x)
    (let ([out (open-output-string)])
      (write x out)
      (get-output-string out) ) )

  (define unquotify 
    (match-lambda 
      [('quote x) x]
      [x x] ) )

  (define (walk x ae me dest)
    (cond ((symbol? x)
	   (cond [(assq x ae) 
		  => (lambda (n)
		       (walk (##sys#macroexpand-1-local (cdr n) me) ae me dest) ) ]
		 [(and constants-used (##sys#hash-table-ref constant-table x)) 
		  => (lambda (val) (walk (car val) ae me dest)) ]
		 [(and inline-table-used (##sys#hash-table-ref inline-table x))
		  => (lambda (val) (walk val ae me dest)) ]
		 [(assq x foreign-variables)
		  => (lambda (fv) 
		       (let* ([t (second fv)]
			      [ft (final-foreign-type t)] 
			      [body `(##core#inline_ref (,(third fv) ,t))] )
			 (foreign-type-convert-result
			  (finish-foreign-result ft body)
			  t) ) ) ]
		 [(assq x location-pointer-map)
		  => (lambda (a)
		       (let* ([t (third a)]
			      [ft (final-foreign-type t)] 
			      [body `(##core#inline_loc_ref (,t) ,(second a))] )
			 (foreign-type-convert-result
			  (finish-foreign-result ft body)
			  t) ) ) ]
		 [else x] ) )
	  ((and (not-pair? x) (constant? x)) `(quote ,x))
	  ((not-pair? x) (syntax-error "illegal atomic form" x))
	  ((symbol? (car x))
	   (let* ([head (car x)]
		  [rest (cdr x)]
		  [ln (get-line x)]
		  [name (resolve head ae)] )
	     (emit-syntax-trace-info x #f)
	     (unless (proper-list? x)
	       (if ln
		   (syntax-error (sprintf "(in line ~s) - malformed expression" ln) x)
		   (syntax-error "malformed expression" x)))
	     (set! ##sys#syntax-error-culprit x)
	     (let* ([x2 (cons name rest)]
		    [xexpanded (##sys#macroexpand-1-local x2 me)] )
	       (cond [(not (eq? x2 xexpanded))
		      (when ln (update-line-number-database! xexpanded ln))
		      (walk xexpanded ae me dest) ]
		     [(and inline-table-used (##sys#hash-table-ref inline-table name))
		      => (lambda (val) (walk (cons val (cdr x)) ae me dest)) ]
		     [else
		      (case name

			((if)
			 (##sys#check-syntax 'if x '(if _ _ . #(_)))
			 `(if ,(walk (cadr x) ae me #f)
			      ,(walk (caddr x) ae me #f)
			      ,(if (null? (cdddr x)) 
				   '(##core#undefined)
				   (walk (cadddr x) ae me #f) ) ) )

			((quote)
			 (##sys#check-syntax 'quote x '(quote _))
			 (let* ([lit (cadr x)] 
				[cf (and literal-compression-threshold
					 (compressable-literal lit literal-compression-threshold) ) ] )
			   (if cf
			       (let ([var (gensym 'lf)])
				 (debugging 'o "compressing literal of size" cf)
				 (set! compressed-literals
				   (alist-cons var (write-to-string lit) compressed-literals) )
				 (set! always-bound (cons var always-bound))
				 (set! block-globals (cons var block-globals))
				 var)
			       x) ) )

			((##core#check)
			 (if unsafe
			     ''#t
			     (walk (cadr x) ae me dest) ) )

			((##core#immutable)
			 (let ((c (cadadr x)))
			   (cond [(assoc c immutable-constants) => cdr]
				 [else
				  (let ([var (gensym 'c)])
				    (set! immutable-constants (alist-cons c var immutable-constants))
				    (set! always-bound (cons var always-bound))
				    (set! block-globals (cons var block-globals))
				    var) ] ) ) )

			((##core#undefined ##core#callunit ##core#primitive ##core#inline_ref 
					   ##core#inline_loc_ref) x)

			((##core#require-for-syntax)
			 (let ([ids (map eval (cdr x))])
			   (apply ##sys#require ids)
			   (hash-table-update! 
			    file-requirements 'syntax-requirements (cut lset-union eq? <> ids)
			    (lambda () ids) )
			   '(##core#undefined) ) )

			((##core#require-extension)
			 (walk
			  (let loop ([ids (cdr x)])
			    (if (null? ids)
				'(##core#undefined)
				(let ([id (cadar ids)])
				  (let-values ([(exp f) (##sys#do-the-right-thing id #t)])
				    (if (not (or f 
						 (and (symbol? id)
						     (or (feature? id)
							 (##sys#find-extension
							  (##sys#canonicalize-extension-path 
							   id 'require-extension) #f)) ) ) )
					(compiler-warning 
					 'ext "extension `~A' is currently not installed" id)
					(unless (and-let* (use-import-table
							   ((symbol? id))
							   (info (##sys#extension-information id #f)) 
							   (exps (assq 'exports info)) )
						  (for-each
						   (cut ##sys#hash-table-set! import-table <> id)
						   (cdr exps) )
						  #t)
					  (lookup-exports-file id) ) )
				    `(begin ,exp ,(loop (cdr ids))) ) ) ) )
			  ae me dest) )

			((let)
			 (##sys#check-syntax 'let x '(let #((variable _) 0) . #(_ 1)))
			 (let* ([bindings (cadr x)]
				[vars (unzip1 bindings)]
				[aliases (map gensym vars)] 
				(ae2 (append (map cons vars aliases) ae)) )
			   (set-real-names! aliases vars)
			   `(let ,(map (lambda (alias b)
					 (list alias (walk (cadr b) ae me (car b))) )
				       aliases bindings)
			      ,(walk (##sys#canonicalize-body (cddr x) (cut assq <> ae2) me dest)
				     ae2
				     me dest) ) ) )

			((lambda ##core#internal-lambda)
			 (##sys#check-syntax 'lambda x '(_ lambda-list . #(_ 1)))
			 (let ([llist (cadr x)]
			       [obody (cddr x)] )
			   (when (##sys#extended-lambda-list? llist)
			     (set!-values 
			      (llist obody) 
			      (##sys#expand-extended-lambda-list 
			       llist obody
			       ##sys#error) ) )
			   (decompose-lambda-list
			    llist
			    (lambda (vars argc rest)
			      (let* ([aliases (map gensym vars)]
				     [ae2 (append (map cons vars aliases) ae)]
				     [body0 (##sys#canonicalize-body obody (cut assq <> ae2) me dest)]
				     [body (walk body0 ae2 me #f)]
				     [llist2 
				      (build-lambda-list
				       aliases argc
				       (and rest (list-ref aliases (posq rest vars))) ) ] )
				(when dest
				  (match body0
				    (('begin (or (? string? doc) ('quote doc)) _ . more)
				     (process-lambda-documentation dest doc) )
				    (_ #f) ) )
				(set-real-names! aliases vars)
				(cond [(and dest emit-profile (eq? 'lambda name))
				       (expand-profile-lambda dest llist2 body) ]
				      [else `(lambda ,llist2 ,body)] ) ) ) ) ) )

			((##core#named-lambda)
			 (walk `(lambda ,@(cddr x)) ae me (cadr x)) )

			((##core#loop-lambda)
			 (let* ([vars (cadr x)]
				[obody (cddr x)]
				[aliases (map gensym vars)]
				(ae2 (append (map cons vars aliases) ae))
				[body 
				 (walk 
				  (##sys#canonicalize-body obody (cut assq <> ae2) me dest)
				  ae2
				  me #f) ] )
			   (set-real-names! aliases vars)
			   `(lambda ,aliases ,body) ) )

			((set! ##core#set!) 
			 (##sys#check-syntax 'set! x '(_ variable _))
			 (let* ([var0 (cadr x)]
				[var (resolve var0 ae)]
				[ln (get-line x)]
				[val (walk (caddr x) ae me var0)] )
			   (when (and safe-globals-flag (eq? var var0))
			     (set! always-bound-to-procedure
			       (lset-adjoin eq? always-bound-to-procedure var))
			     (set! always-bound (lset-adjoin eq? always-bound var)) )
			   (when (eq? var var0) ; global?
			     (when (macro? var)
			       (compiler-warning 
				'var "assigned global variable `~S' is a macro ~A"
				var
				(if ln (sprintf "in line ~S" ln) "") )
			       (when undefine-shadowed-macros (undefine-macro! var) ) ) )
			   (when (keyword? var)
			     (compiler-warning 'syntax "assignment to keyword `~S'" var) )
			   (cond [(assq var foreign-variables)
				  => (lambda (fv)
				       (let ([type (second fv)]
					     [tmp (gensym)] )
					 `(let ([,tmp ,(foreign-type-convert-argument val type)])
					    (##core#inline_update 
					     (,(third fv) ,type)
					     ,(foreign-type-check tmp type) ) ) ) ) ]
				 [(assq var location-pointer-map)
				  => (lambda (a)
				       (let* ([type (third a)]
					      [tmp (gensym)] )
					 `(let ([,tmp ,(foreign-type-convert-argument val type)])
					    (##core#inline_loc_update 
					     (,type)
					     ,(second a)
					     ,(foreign-type-check tmp type) ) ) ) ) ]
				 [else `(set! ,var ,val)] ) ) )

			((##core#inline)
			 `(##core#inline ,(unquotify (cadr x)) ,@(mapwalk (cddr x) ae me)))

			((##core#inline_allocate)
			 `(##core#inline_allocate 
			   ,(map unquotify (second x))
			   ,@(mapwalk (cddr x) ae me)))

			((##core#inline_update)
			 `(##core#inline_update ,(cadr x) ,(walk (caddr x) ae me #f)) )

			((##core#inline_loc_update)
			 `(##core#inline_loc_update 
			   ,(cadr x) 
			   ,(walk (caddr x) ae me #f)
			   ,(walk (cadddr x) ae me #f)) )

			((##core#compiletimetoo ##core#elaborationtimetoo)
			 (let ((exp (cadr x)))
			   (eval exp)
			   (walk exp ae me dest) ) )

			((##core#compiletimeonly ##core#elaborationtimeonly)
			 (eval (cadr x))
			 '(##core#undefined) )

			((begin) 
			 (##sys#check-syntax 'begin x '(begin . #(_ 0)))
			 (if (pair? (cdr x))
			     (canonicalize-begin-body
			      (let fold ([xs (cdr x)])
				(let ([x (car xs)]
				      [r (cdr xs)] )
				  (if (null? r)
				      (list (walk x ae me dest))
				      (cons (walk x ae me #f) (fold r)) ) ) ) )
			     '(##core#undefined) ) )

			((##core#foreign-lambda)
			 (walk (expand-foreign-lambda x) ae me dest) )

			((##core#foreign-callback-lambda)
			 (walk (expand-foreign-callback-lambda x) ae me dest) )

			((##core#foreign-lambda*)
			 (walk (expand-foreign-lambda* x) ae me dest) )

			((##core#foreign-callback-lambda*)
			 (walk (expand-foreign-callback-lambda* x) ae me dest) )

			((##core#foreign-primitive)
			 (walk (expand-foreign-primitive x) ae me dest) )

			((##core#define-foreign-variable)
			 (let* ([var (cadr (second x))]
				[type (cadr (third x))]
				[name (if (pair? (cdddr x))
					  (cadr (fourth x))
					  (symbol->string var) ) ] )
			   (set! foreign-variables
			     (cons (list var type (if (string? name) name (symbol->string name)))
				   foreign-variables))
			   '(##core#undefined) ) )

			((##core#define-foreign-type)
			 (let ([name (cadr (second x))]
			       [type (cadr (third x))] 
			       [conv (cdddr x)] )
			   (cond [(pair? conv)
				  (let ([arg (gensym)]
					[ret (gensym)] )
				    (##sys#hash-table-set! foreign-type-table name (vector type arg ret))
				    (set! always-bound (cons* arg ret always-bound))
				    (set! block-globals (cons* arg ret block-globals))
				    (walk
				     `(begin
					(##core#set! ,arg ,(first conv))
					(##core#set! 
					 ,ret 
					 ,(if (pair? (cdr conv)) (second conv) '##sys#values)) ) 
				     ae me dest) ) ]
				 [else
				  (##sys#hash-table-set! foreign-type-table name type)
				  '(##core#undefined) ] ) ) )

			((##core#define-external-variable)
			 (let* ([sym (cadr (second x))]
				[name (symbol->string sym)]
				[type (cadr (third x))] 
				[exported (cadr (fourth x))]
				[rname (make-random-name)] )
			   (unless exported (set! name (symbol->string (cadr (fifth x)))))
			   (set! external-variables (cons (vector name type exported) external-variables))
			   (set! foreign-variables
			     (cons (list rname 'c-pointer (string-append "&" name))
				   foreign-variables) )
			   (set! external-to-pointer (alist-cons sym rname external-to-pointer))
			   '(##core#undefined) ) )

			((##core#let-location)
			 (let* ([var (cadr (second x))]
				[type (cadr (third x))]
				[alias (gensym)]
				[store (gensym)] 
				[init (and (pair? (cddddr x)) (fourth x))] )
			   (set-real-name! alias var)
			   (set! location-pointer-map
			     (cons (list alias store type) location-pointer-map) )
			   `(let (,(let ([size (words (estimate-foreign-result-location-size type))])
				     ;; Add 2 words: 1 for the header, 1 for double-alignment:
				     ;; Note: C_a_i_bytevector takes number of words, not bytes
				     (list 
				      store
				      `(##core#inline_allocate
					("C_a_i_bytevector" ,(+ 2 size))
					',size)) ) )
			      ,(walk
				`(begin
				   ,@(if init
					 `((##core#set! ,alias ,init))
					 '() )
				   ,(if init (fifth x) (fourth x)) )
				(alist-cons var alias ae)
				me dest) ) ) )

			((##core#define-inline)
			 (let* ([name (cadr (second x))]
				[val (third x)] )
			   (receive (val2 mlist)
			       (extract-mutable-constants
				(walk (cons '##core#internal-lambda (cdr val)) ae me name) )
			     (##sys#hash-table-set! inline-table name val2)
			     (set! always-bound (append (unzip1 mlist) always-bound))
			     (set! inline-table-used #t)
			     (walk
			      `(begin ,@(map (lambda (m) `(##core#set! ,(car m) ',(cdr m))) mlist))
			      ae me #f) ) ) )

			((##core#define-constant)
			 (let* ([name (cadr (second x))]
				[valexp (third x)]
				[val (handle-exceptions ex
					 ;; could show line number here
					 (quit "error in constant evaluation of ~S for named constant ~S" 
					       valexp name)
				       (if (collapsable-literal? valexp)
					   valexp
					   (eval `(let ,defconstant-bindings ,valexp)) ) ) ] )
			   (set! constants-used #t)
			   (set! defconstant-bindings (cons (list name `',val) defconstant-bindings))
			   (cond [(collapsable-literal? val)
				  (##sys#hash-table-set! constant-table name (list val))
				  '(##core#undefined) ]
				 [else
				  (unless (basic-literal? val)
				    (compiler-warning 
				     'const
				     "value for constant binding appears not to be a valid literal: ~s"
				     val) )
				  (let ([var (gensym "constant")])
				    (##sys#hash-table-set! constant-table name (list var))
				    (set! mutable-constants (alist-cons var val mutable-constants))
				    (set! block-globals (cons var block-globals))
				    (set! always-bound (cons var always-bound))
				    (walk `(##core#set! ,var ',val) ae me #f) ) ] ) ) )

			((##core#declare)
			 (walk
			  `(begin
			     ,@(map (lambda (d) (process-declaration (second d)))
				    (cdr x) ) )
			  '() me #f) )
	     
			((##core#foreign-callback-wrapper)
			 (let-values ([(args lam) (split-at (cdr x) 4)])
			   (let* ([lam (car lam)]
				  [name (first args)]
				  [rtype (cadr (third args))]
				  [atypes (cadr (fourth args))]
				  [vars (second lam)] )
			     (let ([name (cadr name)])
			       (if (valid-c-identifier? name)
				   (set! callback-names (cons name callback-names))
				   (quit "name `~S' of external definition is not a valid C identifier"
					 name) ) )
			     (when (or (not (proper-list? vars)) 
				       (not (proper-list? atypes))
				       (not (= (length vars) (length atypes))) )
			       (syntax-error 
				"non-matching or invalid argument list to foreign callback-wrapper"
				vars atypes) )
			     `(##core#foreign-callback-wrapper
			       ,@(mapwalk args ae me)
			       ,(walk `(##core#internal-lambda 
					,vars
					(let ,(let loop ([vars vars] [types atypes])
						(if (null? vars)
						    '()
						    (let ([var (car vars)]
							  [type (car types)] )
						      (cons 
						       (list 
							var
							(foreign-type-convert-result
							 (finish-foreign-result (final-foreign-type type) var)
							 type) )
						       (loop (cdr vars) (cdr types)) ) ) ) )
					  ,(foreign-type-convert-argument
					    `(let ()
					       ,@(match rtype
						   ((or '(const nonnull-c-string) 'nonnull-c-string)
						    `((##sys#make-c-string (let () ,@(cddr lam)))))
						   ((or '(const c-string*) 'c-string*)
						    (syntax-error
						     "`c-string*' is not a valid result type for callback procedures"
						     name) )
						   ((or 'c-string '(const c-string))
						    `((let ((r (let () ,@(cddr lam))))
							(and r (##sys#make-c-string r)) ) ) )
						   (_ (cddr lam)) ) )
					    rtype) ) )
				      ae me #f) ) ) ) )

			(else
			 (let ([handle-call
				(lambda ()
				  (let* ([x2 (mapwalk x ae me)]
					 [head2 (car x2)]
					 [old (##sys#hash-table-ref line-number-database-2 head2)] )
				    (when ln
				      (##sys#hash-table-set!
				       line-number-database-2
				       head2
				       (cons name (alist-cons x2 ln (if old (cdr old) '()))) ) )
				    x2) ) ] )

			   (cond [(eq? 'location name)
				  (##sys#check-syntax 'location x '(location _))
				  (let ([sym (cadr x)])
				    (if (symbol? sym)
					(cond [(assq (resolve sym ae) location-pointer-map)
					       => (lambda (a)
						    (walk
						     `(##sys#make-locative ,(second a) 0 #f 'location)
						     ae me #f) ) ]
					      [(assq sym external-to-pointer) 
					       => (lambda (a) (walk (cdr a) ae me #f)) ]
					      [(memq sym callback-names)
					       `(##core#inline_ref (,(symbol->string sym) c-pointer)) ]
					      [else 
					       (walk `(##sys#make-locative ,sym 0 #f 'location) ae me #f) ] )
					(walk `(##sys#make-locative ,sym 0 #f 'location) ae me #f) ) ) ]
				 
				 [else (handle-call)] ) ) ) ) ] ) ) ) )

	  ((not (proper-list? x))
	   (syntax-error "malformed expression" x) )

	  ((constant? (car x))
	   (emit-syntax-trace-info x #f)
	   (compiler-warning 'syntax "literal in operator position: ~S" x) 
	   (mapwalk x ae me) )

	  ((and (pair? (car x)) (eq? 'lambda (caar x)))
	   (let ([lexp (car x)]
		 [args (cdr x)] )
	     (emit-syntax-trace-info x #f)
	     (##sys#check-syntax 'lambda lexp '(lambda lambda-list . #(_ 1)))
	     (let ([llist (cadr lexp)])
	       (if (and (proper-list? llist) (= (length llist) (length args)))
		   (walk `(let ,(map list llist args) ,@(cddr lexp)) ae me dest)
		   (let ((var (gensym 't)))
		     (walk
		      `(let ((,var ,(car x)))
			 (,var ,@(cdr x)) )
		      ae me dest) ) ) ) ) )
	  
	  (else
	   (emit-syntax-trace-info x #f)
	   (mapwalk x ae me)) ) )
  
  (define (mapwalk xs ae me)
    (map (lambda (x) (walk x ae me #f)) xs) )

  (when (memq 'c debugging-chicken) (newline) (pretty-print exp))
  (##sys#clear-trace-buffer)
  ;; Process visited definitions and main expression:
  (walk 
   `(begin
      ,@(let ([p (reverse pending-canonicalizations)])
	  (set! pending-canonicalizations '())
	  p)
      ,(let ([exp (##sys#compiler-toplevel-macroexpand-hook exp)])
	 (set! extended-bindings (append internal-bindings extended-bindings))
	 exp) )
   '() '() #f) )


(define (process-declaration spec)
  (define (check-decl spec minlen . maxlen)
    (let ([n (length (cdr spec))])
      (if (or (< n minlen) (> n (:optional maxlen 99999)))
	  (syntax-error "invalid declaration" spec) ) ) )  
  (call-with-current-continuation
   (lambda (return)
     (unless (pair? spec)
       (syntax-error "invalid declaration specification" spec) )
     (case (car spec)
       ((uses)
	(let ([us (cdr spec)])
	  (apply register-feature! us)
	  (when use-import-table
	    (for-each lookup-exports-file us) )
	  (when (pair? us)
	    (hash-table-update! file-requirements 'uses (cut lset-union eq? us <>) (lambda () us))
	    (let ((units (map (lambda (u) (string->c-identifier (stringify u))) us)))
	      (set! used-units (append used-units units)) ) ) ) )
       ((unit)
	(check-decl spec 1 1)
	(let* ([u (cadr spec)]
	       [un (string->c-identifier (stringify u))] )
	  (hash-table-set! file-requirements 'unit u)
	  (when (and unit-name (not (string=? unit-name un)))
	    (compiler-warning 'usage "unit was already given a name (new name is ignored)") )
	  (set! unit-name un) ) )
       ((standard-bindings)
	(if (null? (cdr spec))
	    (set! standard-bindings default-standard-bindings)
	    (set! standard-bindings (append (cdr spec) standard-bindings)) ) )
       ((extended-bindings)
	(if (null? (cdr spec))
	    (set! extended-bindings default-extended-bindings)
	    (set! extended-bindings (append (cdr spec) extended-bindings)) ) )
       ((usual-integrations)      
	(cond [(null? (cdr spec))
	       (set! standard-bindings default-standard-bindings)
	       (set! extended-bindings default-extended-bindings) ]
	      [else
	       (let ([syms (cdr spec)])
		 (set! standard-bindings (lset-intersection eq? syms default-standard-bindings))
		 (set! extended-bindings (lset-intersection eq? syms default-extended-bindings)) ) ] ) )
       ((number-type)
	(check-decl spec 1 1)
	(set! number-type (cadr spec)) )
       ((fixnum fixnum-arithmetic) (set! number-type 'fixnum))
       ((generic) (set! number-type 'generic))
       ((unsafe)
	(set! unsafe #t)
	(##match#set-error-control #:unspecified) )
       ((safe) (set! unsafe #f))
       ((no-bound-checks) (set! no-bound-checks #t))
       ((no-argc-checks) (set! no-argc-checks #t))
       ((no-procedure-checks) (set! no-procedure-checks #t))
       ((interrupts-enabled) (set! insert-timer-checks #t))
       ((disable-interrupts) (set! insert-timer-checks #f))
       ((disable-warning) (set! disabled-warnings (append (cdr spec) disabled-warnings)))
       ((always-bound) (set! always-bound (append (cdr spec) always-bound)))
       ((safe-globals) (set! safe-globals-flag #t))
       ((no-procedure-checks-for-usual-bindings)
	(set! always-bound-to-procedure
	  (append default-standard-bindings default-extended-bindings always-bound-to-procedure))
	(set! always-bound
	  (append default-standard-bindings default-extended-bindings always-bound)) )
       ((bound-to-procedure)
	(set! always-bound-to-procedure (append (cdr spec) always-bound-to-procedure))
	(set! always-bound (append (cdr spec) always-bound)) )
       ((foreign-declare)
	(let ([fds (cdr spec)])
	  (if (every string? fds)
	      (set! foreign-declarations (append foreign-declarations fds))
	      (syntax-error "invalid declaration" spec) ) ) )
       ((custom-declare)
	(if (or (not (list? spec)) (not (list? (cadr spec))) (< (length (cadr spec)) 3))
	    (syntax-error "invalid declaration" spec)
	    (process-custom-declaration (cadr spec) (cddr spec)) ) )
       ((c-options)
	(emit-control-file-item `(c-options ,@(cdr spec))) )
       ((link-options)
	(emit-control-file-item `(link-options ,@(cdr spec))) )
       ((post-process)
	(emit-control-file-item
	 (let ([file (pathname-strip-extension source-filename)])
	   `(post-process ,@(map (cut string-substitute "\\$@" file <>) (cdr spec))) ) ) )
       ((block) (set! block-compilation #t))
       ((separate) (set! block-compilation #f))
       ((keep-shadowed-macros) (set! undefine-shadowed-macros #f))
       ((not)
	(check-decl spec 1)
	(case (second spec)
	  [(standard-bindings)
	   (if (null? (cddr spec))
	       (set! standard-bindings '())
	       (set! standard-bindings (lset-difference eq? default-standard-bindings (cddr spec))) ) ]
	  [(extended-bindings)
	   (if (null? (cddr spec))
	       (set! extended-bindings '())
	       (set! extended-bindings (lset-difference eq? default-extended-bindings (cddr spec))) ) ]
	  [(inline)
	   (if (null? (cddr spec))
	       (set! inline-max-size -1)
	       (set! not-inline-list (lset-union eq? not-inline-list (cddr spec))) ) ]
	  [(usual-integrations)      
	   (cond [(null? (cddr spec))
		  (set! standard-bindings '())
		  (set! extended-bindings '()) ]
		 [else
		  (let ([syms (cddr spec)])
		    (set! standard-bindings (lset-difference eq? default-standard-bindings syms))
		    (set! extended-bindings (lset-difference eq? default-extended-bindings syms)) ) ] ) ]
	  [else
	   (check-decl spec 1 1)
	   (case (cadr spec)
	     [(interrupts-enabled) (set! insert-timer-checks #f)]
	     [(safe) 
	      (set! unsafe #t)
	      (##match#set-error-control #:unspecified) ]
	     [else (compiler-warning 'syntax "illegal declaration specifier `~s'" spec)] ) ] ) )
       ((run-time-macros) (set! ##sys#enable-runtime-macros #t))
       ((block-global hide) 
	(let ([syms (cdr spec)])
	  (when export-list 
	    (set! export-list (lset-difference eq? export-list syms)) )
	  (set! block-globals (lset-union eq? syms block-globals)) ) )
       ((export) 
	(let ((syms (cdr spec)))
	  (set! block-globals (lset-difference eq? block-globals syms))
	  (set! export-list (lset-union eq? syms (or export-list '())))))
       ((compress-literals)
	(set! literal-compression-threshold 
	  (or (and (pair? (cdr spec)) (number? (cadr spec)) (cadr spec))
	      default-literal-compression-threshold) ) 
	(when (and (list? spec) (= 3 (length spec)))
	  (set! compressed-literals-initializer (third spec)) ) )
       ((emit-exports)
	(cond ((null? (cdr spec))
	       (quit "invalid `emit-exports' declaration" spec) )
	      ((not export-file-name) (set! export-file-name (cadr spec))) ) )
       ((emit-external-prototypes-first)
	(set! external-protos-first #t) )
       ((lambda-lift) (set! do-lambda-lifting #t))
       ((inline)
	(if (null? (cdr spec))
	    (unless (> inline-max-size -1)
	      (set! inline-max-size default-inline-max-size) )
	    (set! inline-list (lset-union eq? inline-list (cdr spec))) ) )
       ((inline-limit)
	(check-decl spec 1 1)
	(set! inline-max-size
	  (let ([n (cadr spec)])
	    (if (number? n)
		n
		(quit "invalid argument to `inline-limit' declaration" spec) ) ) ) )
       ((namespace)
	(check-decl spec 2)
	(let* ([syms (cdr spec)]
	       [ns (car syms)] )
	  (if (every symbol? syms)
	      (let ([oldsyms (or (and-let* ([a (assq ns namespace-table)]) (cdr a)) '())])
		(set! namespace-table 
		  (alist-update! ns (lset-union eq? oldsyms (cdr syms)) namespace-table eq?) ) )
	      (quit "invalid arguments to `namespace' declaration: ~S" spec) ) ) )
       ((constant)
	(let ((syms (cdr spec)))
	  (if (every symbol? syms)
	      (set! constant-declarations (append syms constant-declarations))
	      (quit "invalid arguments to `constant' declaration: ~S" spec)) ) )
       ((import)
	(let-values (((syms strs) 
		      (partition
		       (lambda (x)
			 (cond ((symbol? x) #t)
			       ((string? x) #f)
			       (else (quit "argument to `import' declaration is not a string or symbol" x)) ) )
		       (cdr spec) ) ) )
	  (set! use-import-table #t)
	  (for-each (cut ##sys#hash-table-set! import-table <> "<here>") syms)
	  (for-each lookup-exports-file strs) ) )
       (else (compiler-warning 'syntax "illegal declaration specifier `~s'" spec)) )
     '(##core#undefined) ) ) )


;;; Expand "foreign-lambda"/"foreign-callback-lambda" forms and add item to stub-list:

(define-record foreign-stub
  id					; symbol
  return-type				; type-specifier
  name					; string or #f
  argument-types			; (type-specifier...)
  argument-names			; #f or (symbol ...)
  body					; #f or string
  cps					; boolean
  callback)				; boolean

(define (create-foreign-stub rtype sname argtypes argnames body callback cps)
  (let* ([params (list-tabulate (length argtypes) (lambda (x) (gensym 'a)))]
	 [f-id (gensym 'stub)]
	 [bufvar (gensym)] 
	 [rsize (estimate-foreign-result-size rtype)] )
    (set-real-name! f-id #t)
    (set! foreign-lambda-stubs 
      (cons (make-foreign-stub f-id rtype sname argtypes argnames body cps callback)
	    foreign-lambda-stubs) )
    (let ([rsize (if callback (+ rsize 24) rsize)] ; 24 -> has to hold cons on 64-bit platforms!
	  [head (if cps
		    `((##core#primitive ,f-id))
		    `(##core#inline ,f-id) ) ]
	  [rest (map (lambda (p t) (foreign-type-check (foreign-type-convert-argument p t) t)) params argtypes)] )
      `(lambda ,params
	 ;; Do minor GC (if callback) to make room on stack:
	 ,@(if callback '((##sys#gc #f)) '())
	 ,(if (zero? rsize) 
	      (foreign-type-convert-result (append head (cons '(##core#undefined) rest)) rtype)
	      (let ([ft (final-foreign-type rtype)]
		    [ws (words rsize)] )
		`(let ([,bufvar (##core#inline_allocate ("C_a_i_bytevector" ,(+ 2 ws)) ',ws)])
		   ,(foreign-type-convert-result
		     (finish-foreign-result ft (append head (cons bufvar rest)))
		     rtype) ) ) ) ) ) ) )

(define (expand-foreign-lambda exp)
  (let* ([name (cadr (third exp))]
	 [sname (cond ((symbol? name) (symbol->string name))
		      ((string? name) name)
		      (else (quit "name `~s' of foreign procedure has wrong type" name)) ) ]
	 [rtype (cadr (second exp))]
	 [argtypes (map second (cdddr exp))] )
    (create-foreign-stub rtype sname argtypes #f #f #f #f) ) )

(define (expand-foreign-callback-lambda exp)
  (let* ([name (cadr (third exp))]
	 [sname (cond ((symbol? name) (symbol->string name))
		      ((string? name) name)
		      (else (quit "name `~s' of foreign procedure has wrong type" name)) ) ]
	 [rtype (cadr (second exp))]
	 [argtypes (map second (cdddr exp))] )
    (create-foreign-stub rtype sname argtypes #f #f #t #t) ) )

(define (expand-foreign-lambda* exp)
  (let* ([rtype (cadr (second exp))]
	 [args (cadr (third exp))]
	 [body (apply string-append (map cadr (cdddr exp)))]
 	 [argtypes (map car args)]
	 [argnames (map cadr args)] )
    (create-foreign-stub rtype #f argtypes argnames body #f #f) ) )

(define (expand-foreign-callback-lambda* exp)
  (let* ([rtype (cadr (second exp))]
	 [args (cadr (third exp))]
	 [body (apply string-append (map cadr (cdddr exp)))]
 	 [argtypes (map car args)]
	 [argnames (map cadr args)] )
    (create-foreign-stub rtype #f argtypes argnames body #t #t) ) )

(define (expand-foreign-primitive exp)
  (let* ([hasrtype (and (pair? (cddr exp)) (not (string? (cadr (caddr exp)))))]
	 [rtype (if hasrtype (cadr (second exp)) 'void)]
	 [args (if hasrtype (cadr (third exp)) (cadr (second exp)))]
	 [body (apply string-append (map cadr (if hasrtype (cdddr exp) (cddr exp))))]
 	 [argtypes (map car args)]
	 [argnames (map cadr args)] )
    (create-foreign-stub rtype #f argtypes argnames body #f #t) ) )


;;; Traverse expression and update line-number db with all contained calls:

(define (update-line-number-database! exp ln)

  (define (mapupdate xs)
    (let loop ((xs xs))
      (if (pair? xs)
	  (begin
	    (walk (car xs))
	    (loop (cdr xs)) ) ) ) )

  (define (walk x)
    (cond ((not-pair? x))
	  ((symbol? (car x))
	   (let* ((name (car x))
		  (old (or (##sys#hash-table-ref ##sys#line-number-database name) '())) )
	     (if (not (assq x old))
		 (##sys#hash-table-set! ##sys#line-number-database name (alist-cons x ln old)) )
	     (mapupdate (cdr x)) ) )
	  (else (mapupdate x)) ) )

  (walk exp) )


;;; Convert canonicalized node-graph into continuation-passing-style:

(define (perform-cps-conversion node)

  (define (cps-lambda id llist subs k)
    (let ([t1 (gensym 'k)])
      (k (make-node
	  '##core#lambda (list id #t (cons t1 llist) 0)
	  (list (walk (car subs)
		      (lambda (r) 
			(make-node '##core#call '(#t) (list (varnode t1) r)) ) ) ) ) ) ) )
  
  (define (walk n k)
    (let ((subs (node-subexpressions n))
	  (params (node-parameters n)) 
	  (class (node-class n)) )
      (case (node-class n)
	((##core#variable quote ##core#undefined ##core#primitive ##core#global-ref) (k n))
	((if) (let* ((t1 (gensym 'k))
		     (t2 (gensym 'r))
		     (k1 (lambda (r) (make-node '##core#call '(#t) (list (varnode t1) r)))) )
		(make-node 'let
			   (list t1)
			   (list (make-node '##core#lambda (list (gensym-f-id) #f (list t2) 0) 
					    (list (k (varnode t2))) )
				 (walk (car subs)
				       (lambda (v)
					 (make-node 'if '()
						    (list v
							  (walk (cadr subs) k1)
							  (walk (caddr subs) k1) ) ) ) ) ) ) ) )
	((let) (let loop ((vars params) (vals subs))
		 (if (null? vars)
		     (walk (car vals) k)
		     (walk (car vals)
			   (lambda (r) 
			     (make-node 'let
					(list (car vars))
					(list r (loop (cdr vars) (cdr vals))) ) ) ) ) ) )
	((lambda) (cps-lambda (gensym-f-id) (first params) subs k))
	((set!) (let ((t1 (gensym 't)))
		  (walk (car subs)
			(lambda (r)
			  (make-node 'let (list t1)
				     (list (make-node 'set! (list (first params)) (list r))
					   (k (varnode t1)) ) ) ) ) ) )
	((##core#foreign-callback-wrapper)
	 (let ([id (gensym-f-id)]
	       [lam (first subs)] )
	   (set! foreign-callback-stubs
	     (cons (apply make-foreign-callback-stub id params) foreign-callback-stubs) )
	   (cps-lambda id (first (node-parameters lam)) (node-subexpressions lam) k) ) )
	((##core#inline ##core#inline_allocate ##core#inline_ref ##core#inline_update ##core#inline_loc_ref 
			##core#inline_loc_update)
	 (walk-inline-call class params subs k) )
	((##core#call) (walk-call (car subs) (cdr subs) params k))
	((##core#callunit) (walk-call-unit (first params) k))
	(else (bomb "bad node (cps)")) ) ) )
  
  (define (walk-call fn args params k)
    (let ((t0 (gensym 'k))
          (t3 (gensym 'r)) )
      (make-node
       'let (list t0)
       (list (make-node '##core#lambda (list (gensym-f-id) #f (list t3) 0) 
			(list (k (varnode t3))) )
	     (walk-arguments
	      args
	      (lambda (vars)
		(walk fn
		      (lambda (r) 
			(make-node '##core#call params (cons* r (varnode t0) vars) ) ) ) ) ) ) ) ) )
  
  (define (walk-call-unit unitname k)
    (let ((t0 (gensym 'k))
	  (t3 (gensym 'r)) )
      (make-node
       'let (list t0)
       (list (make-node '##core#lambda (list (gensym-f-id) #f (list t3) 0) 
			(list (k (varnode t3))) )
	     (make-node '##core#callunit (list unitname)
			(list (varnode t0)) ) ) ) ) )

  (define (walk-inline-call class op args k)
    (walk-arguments
     args
     (lambda (vars)
       (k (make-node class op vars)) ) ) )
  
  (define (walk-arguments args wk)
    (let loop ((args args) (vars '()))
      (cond ((null? args) (wk (reverse vars)))
            ((atomic? (car args))
             (loop (cdr args) (cons (car args) vars)) )
            (else
             (let ((t1 (gensym 'a)))
               (walk (car args)
                     (lambda (r)
		       (make-node 'let (list t1)
				  (list r
					(loop (cdr args) 
					      (cons (varnode t1) vars) ) ) ) ) ) ) ) ) ) )
  
  (define (atomic? n)
    (let ((class (node-class n)))
      (or (memq class '(quote ##core#variable ##core#undefined ##core#global-ref))
	  (and (memq class '(##core#inline ##core#inline_allocate ##core#inline_ref ##core#inline_update
					   ##core#inline_loc_ref ##core#inline_loc_update))
	       (every atomic? (node-subexpressions n)) ) ) ) )
  
  (walk node values) )


;;; Foreign callback stub type:

(define-record foreign-callback-stub
  id					; symbol
  name					; string
  qualifiers				; string
  return-type				; type-specifier
  argument-types)			; (type-specifier ...)


;;; Perform source-code analysis:

(define (analyze-expression node)
  (let ([db (make-vector analysis-database-size '())]
	[explicitly-consed '()] )

    (define (grow n)
      (set! current-program-size (+ current-program-size n)) )

    (define (walk n env localenv here call)
      (let ((subs (node-subexpressions n))
	    (params (node-parameters n)) 
	    (class (node-class n)) )
	(grow 1)
	(case class
	  ((quote ##core#undefined ##core#proc) #f)

	  ((##core#variable)
	   (let ((var (first params)))
	     (ref var n)
	     (unless (memq var localenv)
	       (grow 1)
	       (cond ((memq var env) (put! db var 'captured #t))
		     ((not (get db var 'global)) (put! db var 'global #t) ) ) ) ) )
	  
	  ((##core#global-ref)
	   (let ((var (first params)))
	     (ref var n)
	     (grow 1)
	     (put! db var 'global #t) ) )
	  
	  ((##core#callunit ##core#recurse)
	   (grow 1)
	   (walkeach subs env localenv here #f) )

	  ((##core#call)
	   (grow 1)
	   (let ([fun (car subs)])
	     (if (eq? '##core#variable (node-class fun))
		 (let ([name (first (node-parameters fun))])
		   (collect! db name 'call-sites (cons here n))
		   ;; If call to standard-binding & optimizable rest-arg operator: decrease access count:
		   (if (and (get db name 'standard-binding)
			    (memq name optimizable-rest-argument-operators) )
		       (for-each
			(lambda (arg)
			  (and-let* ([(eq? '##core#variable (node-class arg))]
				     [var (first (node-parameters arg))] )
			    (when (get db var 'rest-parameter) (count! db var 'o-r/access-count)) ) )
			(cdr subs) ) ) ) )
	     (walk (first subs) env localenv here #t)
	     (walkeach (cdr subs) env localenv here #f) ) )

	  ((let)
	   (let ([env2 (append params localenv env)])
	     (let loop ([vars params] [vals subs])
	       (if (null? vars)
		   (walk (car vals) env (append params localenv) here #f)
		   (let ([var (car vars)]
			 [val (car vals)] )
		     (put! db var 'home here)
		     (assign var val env2 here)
		     (walk val env localenv here #f) 
		     (loop (cdr vars) (cdr vals)) ) ) ) ) )

	  ((lambda)			; will this actually be ever used? aren't all lambdas now ##core#lambdas?
	   (grow 1)
	   (decompose-lambda-list
	    (first params)
	    (lambda (vars argc rest)
	      (for-each 
	       (lambda (var) (put! db var 'unknown #t))
	       vars)
	      (let ([tl toplevel-scope])
		(set! toplevel-scope #f)
		(walk (car subs) (append localenv env) vars #f #f)
		(set! toplevel-scope tl) ) ) ) )

	  ((##core#lambda ##core#direct_lambda)
	   (grow 1)
	   (decompose-lambda-list
	    (third params)
	    (lambda (vars argc rest)
	      (let ([id (first params)]
		    [size0 current-program-size] )
		(when here
		  (collect! db here 'contains id)
		  (put! db id 'contained-in here) )
		(for-each 
		 (lambda (var)
		   (put! db var 'home here)
		   (put! db var 'unknown #t) )
		 vars)
		(when rest
		  (put! db rest 'rest-parameter
			(if (memq rest rest-parameters-promoted-to-vector)
			    'vector
			    'list) ) )
		(when (simple-lambda-node? n) (put! db id 'simple #t))
		(let ([tl toplevel-scope])
		  (unless toplevel-lambda-id (set! toplevel-lambda-id id))
		  (when (and (second params) (not (eq? toplevel-lambda-id id)))
		    (set! toplevel-scope #f)) ; only if non-CPS lambda
		  (walk (car subs) (append localenv env) vars id #f)
		  (set! toplevel-scope tl)
		  (set-car! (cdddr (node-parameters n)) (- current-program-size size0)) ) ) ) ) )
	  
	  ((set!) 
	   (let* ([var (first params)]
		  [val (car subs)] )
	     (when first-analysis 
	       (cond [(get db var 'standard-binding)
		      (compiler-warning 'redef "redefinition of standard binding `~S'" var) ]
		     [(get db var 'extended-binding)
		      (compiler-warning 'redef "redefinition of extended binding `~S'" var) ] )
	       (put! db var 'potential-value val) )
	     (when (and (not (memq var localenv)) 
			(not (memq var env)) )
	       (grow 1)
	       (when first-analysis
		 (when (or block-compilation (and export-list (not (memq var export-list))))
		   (set! block-globals (lset-adjoin eq? block-globals var)) ) )
	       (put! db var 'global #t) )
	     (assign var val (append localenv env) here)
	     (unless toplevel-scope (put! db var 'assigned-locally #t))
	     (put! db var 'assigned #t)
	     (walk (car subs) env localenv here #f) ) )

	  ((##core#primitive ##core#inline)
	   (let ([id (first params)])
	     (when (and first-analysis here (symbol? id) (##sys#hash-table-ref real-name-table id))
	       (set-real-name! id here) )
	     (walkeach subs env localenv here #f) ) )

	  (else (walkeach subs env localenv here #f)) ) ) )

    (define (walkeach xs env lenv here call) 
      (for-each (lambda (x) (walk x env lenv here call)) xs) )

    (define (assign var val env here)
      (cond ((eq? '##core#undefined (node-class val))
	     (put! db var 'undefined #t) )
	    ((and (eq? '##core#variable (node-class val))
		  (eq? var (first (node-parameters val))) ) )
	    ((or block-compilation
		 (memq var env)
		 (get db var 'constant)
		 ;;(memq var inline-list)       - would be nice, but might be customized...
		 (memq var block-globals) )
	     (let ((props (get-all db var 'unknown 'value))
		   (home (get db var 'home)) )
	       (unless (assq 'unknown props)
		 (if (assq 'value props)
		     (put! db var 'unknown #t)
		     (if (or (not home) (eq? here home))
			 (put! db var 'value val)
			 (put! db var 'unknown #t) ) ) ) ) )
	    (else (put! db var 'unknown #t)) ) )
    
    (define (ref var node)
      (collect! db var 'references node) )

    (define (quick-put! plist prop val)
      (set-cdr! plist (alist-cons prop val (cdr plist))) )

    ;; Return true if <id> directly or indirectly contains any of <other-ids>:
    (define (contains? id other-ids)
      (or (memq id other-ids)
	  (let ((clist (get db id 'contains)))
	    (and clist
		 (any (lambda (id2) (contains? id2 other-ids)) clist) ) ) ) )

    ;; Initialize database:
    (initialize-analysis-database db)

    ;; Walk toplevel expression-node:
    (debugging 'p "analysis traversal phase...")
    (set! current-program-size 0)
    (walk node '() '() #f #f) 

    ;; Complete gathered database information:
    (debugging 'p "analysis gathering phase...")
    (##sys#hash-table-for-each
     (lambda (sym plist)
       (let ([unknown #f]
	     [value #f]
	     [pvalue #f]
	     [references '()]
	     [captured #f]
	     [call-sites '()]
	     [assigned #f]
	     [assigned-locally #f]
	     [undefined #f]
	     [global #f]
	     [o-r/access-count 0]
	     [rest-parameter #f] 
	     [nreferences 0]
	     [ncall-sites 0] )

	 (for-each
	  (lambda (prop)
	    (case (car prop)
	      [(unknown) (set! unknown #t)]
	      [(references) 
	       (set! references (cdr prop))
	       (set! nreferences (length references)) ]
	      [(captured) (set! captured #t)]
	      [(potential-value) (set! pvalue (cdr prop))]
	      [(call-sites)
	       (set! call-sites (cdr prop))
	       (set! ncall-sites (length call-sites)) ]
	      [(assigned) (set! assigned #t)]
	      [(assigned-locally) (set! assigned-locally #t)]
	      [(undefined) (set! undefined #t)]
	      [(global) (set! global #t)]
	      [(value) (set! value (cdr prop))]
	      [(o-r/access-count) (set! o-r/access-count (cdr prop))]
	      [(rest-parameter) (set! rest-parameter #t)] ) )
	  plist)

	 (set! value (and (not unknown) value))

	 ;; If this is the first analysis, register known local or potentially known global lambda-value id's
	 ;;  along with their names:
	 (when (and first-analysis 
		    (eq? '##core#lambda
			 (and-let* ([val (or value (and global pvalue))])
			   (node-class val) ) ) )
	   (set-real-name! (first (node-parameters (or value pvalue))) sym) )

	 ;; If this is the first analysis and the variable is global and has no references and we are
	 ;;  in block mode, then issue warning:
	 (when (and first-analysis global (null? references))
	   (when assigned-locally
	     (compiler-warning 'var "local assignment to unused variable `~S' may be unintended" sym) )
	   (when (and (or block-compilation
			  (and export-list (not (memq sym export-list))) )
		      (not (assq sym mutable-constants)) )
	     (compiler-warning 'var "global variable `~S' is never used" sym) ) )

 	 ;; Make 'boxed, if 'assigned & 'captured:
	 (when (and assigned captured)
	   (quick-put! plist 'boxed #t) )

	 ;; Make 'contractable, if it has a procedure as known value, has only one use and one call-site and
	 ;;  if the lambda has no free non-global variables or is an internal lambda. Make 'inlinable if
	 ;;  use/call count is not 1:
	 (when value
	   (let ((valparams (node-parameters value)))
	     (when (and (eq? '##core#lambda (node-class value))
			(or (not (second valparams))
			    (every (lambda (v) (get db v 'global)) (scan-free-variables value)) ) )
	       (if (and (= 1 nreferences) (= 1 ncall-sites))
		   (quick-put! plist 'contractable #t)
		   (quick-put! plist 'inlinable #t) ) ) ) )

	 ;; Make 'collapsable, if it has a known constant value which is either collapsable or is only
	 ;;  referenced once and if no assignments are made:
	 (when (and value
		    ;; (not (assq 'assigned plist)) - If it has a known value, it's assigned just once!
		    (eq? 'quote (node-class value)) )
	   (let ((val (first (node-parameters value))))
	     (when (or (collapsable-literal? val)
		       (= 1 nreferences) )
	       (quick-put! plist 'collapsable #t) ) ) )
		
	 ;; If it has a known value that is a procedure, and if the number of call-sites is equal to the
	 ;;  number of references (does not escape), then make all formal parameters 'unused which are
	 ;;  never referenced or assigned (if no rest parameter exist):
	 ;;  - also marks the procedure as 'has-unused-parameters (if not in `callback-names')
	 ;;  - if the procedure is internal (a continuation) do NOT mark unused parameters.
	 ;;  - also: if procedure has rest-parameter and no unused params, mark f-id as 'explicit-rest.
	 (when value
	   (let ([has #f])
	     (when (and (eq? '##core#lambda (node-class value))
			(= nreferences ncall-sites) )
	       (let ([lparams (node-parameters value)])
		 (when (second lparams)
		   (decompose-lambda-list
		    (third lparams)
		    (lambda (vars argc rest)
		      (unless rest
			(for-each
			 (lambda (var)
			   (cond [(and (not (get db var 'references))
				       (not (get db var 'assigned)) )
				  (put! db var 'unused #t)
				  (set! has #t)
				  #t]
				 [else #f] ) )
			 vars) )
		      (cond [(and has (not (memq sym callback-names)))
			     (put! db (first lparams) 'has-unused-parameters #t) ]
			    [rest
			     (set! explicitly-consed (cons rest explicitly-consed))
			     (put! db (first lparams) 'explicit-rest #t) ] ) ) ) ) ) ) ) )

	 ;;  Make 'removable, if it has no references and is not assigned to, and if it has either a value that
	 ;;    does not cause any side-effects or if it is 'undefined:
	 (when (and (not assigned)
		    (null? references)
		    (or (and value
			     (or (not (eq? '##core#variable (node-class value)))
				 (not (get db (first (node-parameters value)) 'global)) )
			     (not (expression-has-side-effects? value db)) )
			undefined) )
	   (quick-put! plist 'removable #t) )

	 ;; Make 'replacable, if it has a variable as known value and if either that variable has
	 ;;  a known value itself, or if it is not captured and referenced only once, the target and
	 ;;  the source are never assigned and the source is non-global or we are in block-mode:
	 ;;  - The target-variable is not allowed to be global.
	 ;;  - The variable that can be substituted for the current one is marked as 'replacing.
	 ;;    This is done to prohibit beta-contraction of the replacing variable (It wouldn't be there, if
	 ;;    it was contracted).
	 (when (and value (not global))
	   (when (eq? '##core#variable (node-class value))
	     (let* ([name (first (node-parameters value))]
		    [nrefs (get db name 'references)] )
	       (when (or (and (not (get db name 'unknown)) (get db name 'value))
			 (and (not (get db name 'captured))
			      nrefs
			      (= 1 (length nrefs))
			      (not assigned)
			      (not (get db name 'assigned)) 
			      (or block-compilation (not (get db name 'global))) ) )
		 (quick-put! plist 'replacable name) 
		 (put! db name 'replacing #t) ) ) ) )

	 ;; Make 'replacable, if it has a known value of the form: '(lambda (<xvar>) (<kvar> <xvar>))' and
	 ;;  is an internally created procedure: (See above for 'replacing)
	 (when (and value (eq? '##core#lambda (node-class value)))
	   (let ([params (node-parameters value)])
	     (when (not (second params))
	       (let ([llist (third params)]
		     [body (first (node-subexpressions value))] )
		 (when (and (pair? llist) 
			    (null? (cdr llist))
			    (eq? '##core#call (node-class body)) )
		   (let ([subs (node-subexpressions body)])
		     (when (= 2 (length subs))
		       (let ([v1 (first subs)]
			     [v2 (second subs)] )
			 (when (and (eq? '##core#variable (node-class v1))
				    (eq? '##core#variable (node-class v2))
				    (eq? (first llist) (first (node-parameters v2))) )
			   (let ([kvar (first (node-parameters v1))])
			     (quick-put! plist 'replacable kvar)
			     (put! db kvar 'replacing #t) ) ) ) ) ) ) ) ) ) )

	 ;; If a rest-argument, convert 'rest-parameter property to 'vector, if the variable is never
	 ;;  assigned, and the number of references is identical to the number of accesses in optimizable
	 ;;  rest-argument operators:
	 ;; - Add variable to "rest-parameters-promoted-to-vector", because subsequent optimization will
	 ;;   change variables context (operators applied to it).
	 (when (and rest-parameter
		    (not assigned)
		    (= nreferences o-r/access-count) )
	   (set! rest-parameters-promoted-to-vector (lset-adjoin eq? rest-parameters-promoted-to-vector sym))
	   (put! db sym 'rest-parameter 'vector) ) ) )

     db)

    ;; Remove explicitly consed rest parameters from promoted ones:
    (set! rest-parameters-promoted-to-vector
      (lset-difference eq? rest-parameters-promoted-to-vector explicitly-consed) )

    ;; Set original program-size, if this is the first analysis-pass:
    (unless original-program-size
      (set! original-program-size current-program-size) )
    db) )


;;; Convert closures to explicit data structures (effectively flattens function-binding structure):

(define (perform-closure-conversion node db)
  (let ([direct-calls 0]
	[customizable '()] )

    (define (test sym item) (get db sym item))
  
    (define (register-customizable! var id)
      (set! customizable (lset-adjoin eq? customizable var)) 
      (put! db id 'customizable #t) )

    (define (register-direct-call! id)
      (set! direct-calls (add1 direct-calls))
      (set! direct-call-ids (lset-adjoin eq? direct-call-ids id)) )

    ;; Gather free-variable information:
    ;; (and: - register direct calls
    ;;       - update (by mutation) call information in "##core#call" nodes)
    (define (gather n here env)
      (let ((subs (node-subexpressions n))
	    (params (node-parameters n)) )
	(case (node-class n)

	  ((quote ##core#variable ##core#undefined ##core#proc ##core#primitive ##core#global-ref) #f)

	  ((let)
	   (receive (vals body) (split-at subs (length params))
	     (for-each (lambda (n) (gather n here env)) vals)
	     (gather (first body) here (append params env)) ) )

	  ((##core#call)
	   (let* ([fn (first subs)]
		  [mode (first params)]
		  [name (and (pair? (cdr params)) (second params))]
		  [varfn (eq? '##core#variable (node-class fn))] )
	     (node-parameters-set!
	      n
	      (cons mode
		    (if (or name varfn)
			(cons name
			      (if varfn
				  (let* ([varname (first (node-parameters fn))]
					 [val (and (not (test varname 'unknown)) (test varname 'value))] )
				    (if (and val (eq? '##core#lambda (node-class val)))
					(let* ([params (node-parameters val)]
					       [llist (third params)]
					       [id (first params)]
					       [refs (test varname 'references)]
					       [sites (test varname 'call-sites)] 
					       [custom
						(and refs sites
						     (= (length refs) (length sites)) 
						     (proper-list? llist) ) ] )
					  (when (and name custom (not (= (length llist) (length (cdr subs)))))
					    (compiler-warning
					     'call 
					     "known procedure called with wrong number of arguments: ~A" 
					     (source-info->string name) ) )
					  (register-direct-call! id)
					  (when custom (register-customizable! varname id)) 
					  (list id custom) )
					'() ) )
				  '() ) )
			'() ) ) )
	     (for-each (lambda (n) (gather n here env)) subs) ) )

	  ((##core#lambda ##core#direct_lambda)
	   (decompose-lambda-list
	    (third params)
	    (lambda (vars argc rest)
	      (let* ([id (if here (first params) 'toplevel)]
		     [capturedvars (captured-variables (car subs) env)]
		     [csize (length capturedvars)] )
		(put! db id 'closure-size csize)
		(put! db id 'captured-variables capturedvars)
		(gather (car subs) id (append vars env)) ) ) ) )
	
	  (else (for-each (lambda (n) (gather n here env)) subs)) ) ) )

    ;; Create explicit closures:
    (define (transform n here closure)
      (let ((subs (node-subexpressions n))
	    (params (node-parameters n)) 
	    (class (node-class n)) )
	(case class

	  ((quote ##core#undefined ##core#proc ##core#global-ref) n)

	  ((##core#variable)
	   (let* ((var (first params))
		  (val (ref-var n here closure)) )
	     (if (test var 'boxed)
		 (make-node '##core#unbox '() (list val))
		 val) ) )

	  ((if ##core#call ##core#inline ##core#inline_allocate ##core#callunit ##core#inline_ref ##core#inline_update 
	       ##core#switch ##core#cond ##core#direct_call ##core#recurse ##core#return ##core#inline_loc_ref
	       ##core#inline_loc_update)
	   (make-node (node-class n) params (maptransform subs here closure)) )

	  ((let)
	   (let* ([var (first params)]
		  [boxedvar (test var 'boxed)]
		  [boxedalias (gensym var)] )
	     (if boxedvar
		 (make-node 
		  'let (list boxedalias)
		  (list (transform (first subs) here closure)
			(make-node
			 'let (list var)
			 (list (make-node '##core#box '() (list (varnode boxedalias)))
			       (transform (second subs) here closure) ) ) ) )
		 (make-node
		  'let params
		  (maptransform subs here closure) ) ) ) )

	  ((##core#lambda ##core#direct_lambda)
	   (let ([llist (third params)])
	     (decompose-lambda-list
	      llist
	      (lambda (vars argc rest)
		(let* ([boxedvars (filter (lambda (v) (test v 'boxed)) vars)]
		       [boxedaliases (map cons boxedvars (map gensym boxedvars))]
		       [cvar (gensym 'c)]
		       [id (if here (first params) 'toplevel)]
		       [capturedvars (or (test id 'captured-variables) '())]
		       [csize (or (test id 'closure-size) 0)] 
		       [info (and emit-closure-info (second params) (pair? llist))] )
		  ;; If rest-parameter is boxed: mark it as 'boxed-rest
		  ;;  (if we don't do this than preparation will think the (boxed) alias
		  ;;  of the rest-parameter is never used)
		  (and-let* ([rest]
			     [(test rest 'boxed)]
			     [rp (test rest 'rest-parameter)] )
		    (put! db (cdr (assq rest boxedaliases)) 'boxed-rest #t) )
		  (make-node
		   '##core#closure (list (+ csize (if info 2 1)))
		   (cons
		    (make-node
		     class
		     (list id
			   (second params)
			   (cons 
			    cvar
			    (build-lambda-list
			     (map (lambda (v)
				    (cond ((assq v boxedaliases) => cdr)
					  (else v) ) )
				  vars)
			     argc
			     (cond ((and rest (assq rest boxedaliases)) => cdr)
				   (else rest) ) ) )
			   (fourth params) )
		     (list (let ((body (transform (car subs) cvar capturedvars)))
			     (if (pair? boxedvars)
				 (fold-right
				  (lambda (alias val body) (make-node 'let (list alias) (list val body)))
				  body
				  (unzip1 boxedaliases)
				  (map (lambda (a) (make-node '##core#box '() (list (varnode (cdr a)))))
				       boxedaliases) )
				 body) ) ) )
		    (let ((cvars (map (lambda (v) (ref-var (varnode v) here closure))
				      capturedvars) ) )
		      (if info
			  (append 
			   cvars
			   (list 
			    (qnode 
			     (##sys#make-lambda-info
			      (->string (cons (or (real-name id) '?)
					      (cdr llist) )))))) ; this is not always correct, due to optimizations
			  cvars) ) ) ) ) ) ) ) )

	  ((set!)
	   (let* ([var (first params)]
		  [val (first subs)]
		  [cval (node-class val)]
		  [immf (or (and (eq? 'quote cval) (immediate? (first (node-parameters val))))
			    (eq? '##core#undefined cval) ) ] )
	     (cond ((posq var closure)
		    => (lambda (i)
			 (if (test var 'boxed)
			     (make-node
			      (if immf '##core#updatebox_i '##core#updatebox)
			      '()
			      (list (make-node '##core#ref (list (add1 i)) (list (varnode here)))
				    (transform val here closure) ) )
			     ;; Is the following actually used???
			     (make-node
			      (if immf '##core#update_i '##core#update)
			      (list (add1 i))
			      (list (varnode here)
				    (transform val here closure) ) ) ) ) )
		   ((test var 'boxed)
		    (make-node
		     (if immf '##core#updatebox_i '##core#updatebox)
		     '()
		     (list (varnode var)
			   (transform val here closure) ) ) )
		   (else (make-node
			  'set! (list var)
			  (list (transform val here closure) ) ) ) ) ) )

	  ((##core#primitive) 
	   (make-node
	    '##core#closure (list (if emit-closure-info 2 1))
	    (cons (make-node '##core#proc (list (car params) #t) '())
		  (if emit-closure-info
		      (list (qnode (##sys#make-lambda-info (car params))))
		      '() ) ) ) )

	  (else (bomb "bad node (closure2)")) ) ) )

    (define (maptransform xs here closure)
      (map (lambda (x) (transform x here closure)) xs) )
  
    (define (ref-var n here closure)
      (let ((var (first (node-parameters n))))
	(cond ((posq var closure) 
	       => (lambda (i) 
		    (make-node '##core#ref (list (+ i 1)) 
			       (list (varnode here)) ) ) )
	      (else n) ) ) )

    (define (captured-variables node env)
      (let ([vars '()])
	(let walk ([n node])
	  (let ((subs (node-subexpressions n))
		(params (node-parameters n)) )
	    (case (node-class n)
	      ((##core#variable)
	       (let ([var (first params)])
		 (when (memq var env)
		   (set! vars (lset-adjoin eq? vars var)) ) ) )
	      ((quote ##core#undefined ##core#primitive ##core#proc ##core#inline_ref ##core#global-ref) #f)
	      ((set!) 
	       (let ([var (first params)])
		 (when (memq var env) (set! vars (lset-adjoin eq? vars var)))
		 (walk (car subs)) ) )
	      (else (for-each walk subs)) ) ) )
	vars) )

    (debugging 'p "closure conversion gathering phase...")
    (gather node #f '())
    (debugging 'o "customizable procedures" customizable)
    (debugging 'p "closure conversion transformation phase...")
    (let ((node2 (transform node #f #f)))
      (unless (zero? direct-calls)
	(debugging 'o "calls to known targets" direct-calls (delay (length direct-call-ids))) )
      node2) ) )


;;; Do some preparations before code-generation can commence:

(define-record lambda-literal
  id					; symbol
  external                              ; boolean
  arguments				; (symbol...)
  argument-count			; integer
  rest-argument				; symbol | #f
  temporaries				; integer
  callee-signatures			; (integer...)
  allocated				; integer
  directly-called			; boolean
  closure-size				; integer
  looping				; boolean
  customizable				; boolean
  rest-argument-mode			; #f | LIST | VECTOR | UNUSED
  body					; expression
  direct)				; boolean
  
(define (prepare-for-code-generation node db)
  (let ([literals '()]
        [lambdas '()]
        [temporaries 0]
        [allocated 0]
	[looping 0]
        [signatures '()] 
	[fastinits 0] 
	[fastrefs 0] 
	[fastsets 0] )

    (define (walk-var var e sf)
      (cond [(posq var e) => (lambda (i) (make-node '##core#local (list i) '()))]
	    [(keyword? var) (make-node '##core#literal (list (literal var)) '())]
	    [else (walk-global var sf)] ) )

    (define (walk-global var sf)
      (let* ([safe (or sf 
		       no-bound-checks
		       unsafe
		       (memq var always-bound)
		       (get db var 'standard-binding)
		       (get db var 'extended-binding) ) ]
	     [blockvar (memq var block-globals)] )
	(when blockvar (set! fastrefs (add1 fastrefs)))
	(make-node
	 '##core#global
	 (list (if blockvar
		   (blockvar-literal var)
		   (literal var) )
	       safe
	       blockvar
	       var)
	 '() ) ) )

    (define (walk n e here boxes)
      (let ((subs (node-subexpressions n))
	    (params (node-parameters n))
	    (class (node-class n)) )
	(case class

	  ((##core#undefined ##core#proc) n)

	  ((##core#variable) 
	   (walk-var (first params) e #f) )

	  ((##core#global-ref)
	   (walk-global (first params) #t) )

	  ((##core#direct_call)
	   (set! allocated (+ allocated (fourth params)))
	   (make-node class params (mapwalk subs e here boxes)) )

	  ((##core#inline_allocate)
	   (set! allocated (+ allocated (second params)))
	   (make-node class params (mapwalk subs e here boxes)) )

	  ((##core#inline_ref)
	   (set! allocated (+ allocated (words (estimate-foreign-result-size (second params)))))
	   (make-node class params '()) )

	  ((##core#inline_loc_ref)
	   (set! allocated (+ allocated (words (estimate-foreign-result-size (first params)))))
	   (make-node class params (mapwalk subs e here boxes)) )

	  ((##core#closure) 
	   (set! allocated (+ allocated (first params) 1))
	   (make-node '##core#closure params (mapwalk subs e here boxes)) )

	  ((##core#box)
	   (set! allocated (+ allocated 2))
	   (make-node '##core#box params (list (walk (first subs) e here boxes))) )

	  ((##core#updatebox)
	   (let* ([b (first subs)]
		  [subs (mapwalk subs e here boxes)] )
	     (make-node
	      (cond [(and (eq? '##core#variable (node-class b))
			  (memq (first (node-parameters b)) boxes) )
		     (set! fastinits (add1 fastinits))
		     '##core#updatebox_i]
		    [else class] )
	      '()
	      subs) ) )

	  ((##core#lambda ##core#direct_lambda) 
	   (let ([temps temporaries]
		 [sigs signatures]
		 [lping looping]
		 [alc allocated] 
		 [direct (eq? class '##core#direct_lambda)] )
	     (set! temporaries 0)
	     (set! allocated 0)
	     (set! signatures '())
	     (set! looping 0)
	     (decompose-lambda-list
	      (third params)
	      (lambda (vars argc rest)
		(let* ([id (first params)]
		       [rest-mode
			(and rest
			     (let ([rrefs (get db rest 'references)])
			       (cond [(get db rest 'assigned) 'list]
				     [(and (not (get db rest 'boxed-rest)) (or (not rrefs) (null? rrefs))) 'none] 
				     [else (get db rest 'rest-parameter)] ) ) ) ]
		       [body (walk 
			      (car subs)
			      (if (eq? 'none rest-mode)
				  (butlast vars)
				  vars)
			      id
			      '()) ] )
		  (case rest-mode
		    [(none) (debugging 'o "unused rest argument" rest id)]
		    [(vector) (debugging 'o "rest argument accessed as vector" rest id)] )
		  (when (and direct rest)
		    (bomb "bad direct lambda" id allocated rest) )
		  (set! lambdas
		    (cons (make-lambda-literal
			   id
			   (second params)
			   vars
			   argc
			   rest
			   (add1 temporaries)
			   signatures
			   allocated
			   (or direct (memq id direct-call-ids))
			   (or (get db id 'closure-size) 0)
			   (and (not rest)
				(> looping 0)
				(begin
				  (debugging 'o "identified direct recursive calls" id looping)
				  #t) )
			   (or direct (get db id 'customizable))
			   rest-mode
			   body
			   direct)
			  lambdas) )
		  (set! looping lping)
		  (set! temporaries temps)
		  (set! allocated alc)
		  (set! signatures sigs)
		  (make-node '##core#proc (list (first params)) '()) ) ) ) ) )

	  ((let)
	   (let* ([var (first params)]
		  [val (first subs)] 
		  [boxvars (if (eq? '##core#box (node-class val)) (list var) '())] )
	     (set! temporaries (add1 temporaries))
	     (make-node
	      '##core#bind (list 1)
	      (list (walk val e here boxes)
		    (walk (second subs) (append e params) here (append boxvars boxes)) ) ) ) )

	  ((set!)
	   (let ([var (first params)]
		 [val (first subs)] )
	     (cond ((posq var e)
		    => (lambda (i) 
			 (make-node '##core#setlocal (list i) (list (walk val e here boxes)) ) ) )
		   (else
		    (let* ([cval (node-class val)]
			   [safe (not (or no-bound-checks
					  unsafe
					  (memq var always-bound)
					  (get db var 'standard-binding)
					  (get db var 'extended-binding) ) ) ]
			   [blockvar (memq var block-globals)]
			   [immf (or (and (eq? cval 'quote) (immediate? (first (node-parameters val))))
				     (eq? '##core#undefined cval) ) ] )
		      (when blockvar (set! fastsets (add1 fastsets)))
		      (make-node
		       (if immf '##core#setglobal_i '##core#setglobal)
		       (list (if blockvar
				 (blockvar-literal var)
				 (literal var) )
			     blockvar)
		       (list (walk (car subs) e here boxes)) ) ) ) ) ) )

	  ((##core#call) 
	   (let ([len (length (cdr subs))])
	     (set! signatures (lset-adjoin = signatures len)) 
	     (when (and (>= (length params) 3) (eq? here (third params)))
	       (set! looping (add1 looping)) )
	     (make-node class params (mapwalk subs e here boxes)) ) )

	  ((##core#recurse)
	   (when (first params) (set! looping (add1 looping)))
	   (make-node class params (mapwalk subs e here boxes)) )

	  ((quote)
	   (let ((c (first params)))
	     (cond ((fixnum? c)
		    (immediate-literal c) )
		   ((number? c)
		    (cond ((eq? 'fixnum number-type)
			   (cond ((integer? c)
				  (compiler-warning 
				   'type 
				   "coerced inexact literal number `~S' to fixnum ~S" c (inexact->exact c))
				  (immediate-literal (inexact->exact c)) )
				 (else (quit "can not coerce inexact literal `~S' to fixnum" c)) ) )
			  (else (make-node '##core#literal (list (literal c)) '())) ) )
		   ((immediate? c) (immediate-literal c))
		   (else (make-node '##core#literal (list (literal c)) '())) ) ) )

	  (else (make-node class params (mapwalk subs e here boxes)) ) ) ) )
    
    (define (mapwalk xs e here boxes)
      (map (lambda (x) (walk x e here boxes)) xs) )

    (define (literal x)
      (cond [(immediate? x) (immediate-literal x)]
	    [(and (number? x) (inexact? x) 
		  (list-index (lambda (y) (and (number? y) (inexact? y) (= x y))) literals) )
	     => values]
            [(posq x literals) => values]
	    [else (new-literal x)] ) )

    (define (new-literal x)
      (let ([i (length literals)])
	(set! literals (append literals (list x))) ; could be optimized
	i) )

    (define (blockvar-literal var)
      (or (list-index
	   (lambda (lit) 
	     (and (block-variable-literal? lit)
		  (eq? var (block-variable-literal-name lit)) ) )
	   literals)
	  (new-literal (make-block-variable-literal var)) ) )
    
    (define (immediate-literal x)
      (make-node '##core#immediate
		 (cond ((fixnum? x) `(fix ,x))
		       ((boolean? x) `(bool ,x))
		       ((char? x) `(char ,x))
		       ((null? x) '(nil))
		       ((eof-object? x) '(eof))
		       (else (bomb "bad immediate (prepare)")) )
		 '() ) )
    
    (debugging 'p "preparation phase...")
    (let ((node2 (walk node '() #f '())))
      (debugging 'o "fast box initializations" fastinits)
      (debugging 'o "fast global references" fastrefs)
      (debugging 'o "fast global assignments" fastsets)
      (values node2 literals lambdas) ) ) )
