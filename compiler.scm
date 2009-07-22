;;;; compiler.scm - The CHICKEN Scheme compiler
;
;
; "This is insane. What we clearly want to do is not exactly clear, and is rooted in NCOMPLR."
;
;
;-----------------------------------------------------------------------------------------------------------
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
;
;
; Supported syntax:
;
; - Declaration specifiers:
;
; ([not] extended-bindings {<name>})
; ([not] inline {<var>})
; ([not] interrupts-enabled)
; ([not] safe)
; ([not] standard-bindings {<name>})
; ([not] usual-integrations {<name>})
; (local {<name> ...})
; ([not] inline-global {<name>})
; ([number-type] <type>)
; (always-bound {<name>})
; (block)
; (block-global {<name>})
; (bound-to-procedure {<var>})
; (c-options {<opt>})
; (compile-syntax)
; (disable-interrupts)
; (disable-warning <class> ...)
; (emit-import-library {<module> | (<module> <filename>)})
; (export {<name>})
; (fixnum-arithmetic)
; (foreign-declare {<string>})
; (hide {<name>})
; (inline-limit <limit>)
; (keep-shadowed-macros)
; (lambda-lift)
; (link-options {<opt>})
; (no-argc-checks)
; (no-bound-checks)
; (no-procedure-checks)
; (no-procedure-checks-for-usual-bindings)
; (post-process <string> ...)
; (profile <symbol> ...)
; (safe-globals)
; (separate)
; (type (<symbol> <typespec>) ...)
; (unit <unitname>)
; (unsafe)
; (unused <symbol> ...)
; (uses {<unitname>})
; (scrutinize)
;
;   <type> = fixnum | generic

; - Global symbol properties:
;
;   ##compiler#always-bound -> BOOL
;   ##compiler#always-bound-to-procedure -> BOOL
;   ##compiler#local -> BOOL
;   ##compiler#visibility -> #f | 'hidden | 'exported
;   ##compiler#constant -> BOOL
;   ##compiler#intrinsic -> #f | 'standard | 'extended
;   ##compiler#inline -> 'no | 'yes
;   ##compiler#inline-global -> 'yes | 'no | <node>
;   ##compiler#profile -> BOOL
;   ##compiler#unused -> BOOL
;   ##compiler#foldable -> BOOL

; - Source language:
;
; <variable>
; <constant>
; (##core#declare {<spec>})
; (##core#immutable <exp>)
; (##core#global-ref <variable>)
; (quote <exp>)
; (if <exp> <exp> [<exp>])
; ([##core#]syntax <exp>)
; ([##core#]let <variable> ({(<variable> <exp>)}) <body>)
; ([##core#]let ({(<variable> <exp>)}) <body>)
; ([##core#]letrec ({(<variable> <exp>)}) <body>)
; (##core#let-location <symbol> <type> [<init>] <exp>)
; ([##core#]lambda <variable> <body>)
; ([##core#]lambda ({<variable>}+ [. <variable>]) <body>)
; ([##core#]set! <variable> <exp>)
; ([##core#]begin <exp> ...)
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
; (define-foreign-variable <symbol> <type> [<string>])
; (define-foreign-type <symbol> <type> [<proc1> [<proc2>]])
; (foreign-lambda <type> <string> {<type>})
; (foreign-lambda* <type> ({(<type> <var>)})) {<string>})
; (foreign-safe-lambda <type> <string> {<type>})
; (foreign-safe-lambda* <type> ({(<type> <var>)})) {<string>})
; (foreign-primitive <type> ({(<type> <var>)}) {<string>})
; (##core#define-inline <name> <exp>)
; (define-constant <name> <exp>)
; (##core#foreign-callback-wrapper '<name> <qualifiers> '<type> '({<type>}) <exp>)
; (##core#define-external-variable (quote <name>) (quote <type>) (quote <bool>))
; (##core#check <exp>)
; (##core#require-for-syntax <exp> ...)
; (##core#require-extension (<id> ...) <bool>)
; (##core#app <exp> {<exp>})
; ([##core#]syntax <exp>)
; (<exp> {<exp>})
; (define-syntax <symbol> <expr>)
; (define-syntax (<symbol> . <llist>) <expr> ...)
; (define-compiled-syntax <symbol> <expr>)
; (define-compiled-syntax (<symbol> . <llist>) <expr> ...)
; (##core#define-compiler-syntax <symbol> <expr>)
; (##core#let-compiler-syntax ((<symbol> <expr>) ...) <expr> ...)
; (##core#module <symbol> #t | (<name> | (<name> ...) ...) <body>)

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
; [##core#cond <exp> <exp> <exp>]
; [##core#local {<index>}]
; [##core#setlocal {<index>} <exp>]
; [##core#global {<literal> <safe-flag> <block-mode> [<name>]}]
; [##core#setglobal {<literal> <block-mode> <name>} <exp>]
; [##core#setglobal_i {<literal> <block-mode> <name>} <exp>]
; [##core#literal {<literal>}]
; [##core#immediate {<type> [<immediate>]}]     - type: bool/fix/nil/char
; [##core#proc {<name> [<non-internal>]}]
; [##core#recurse {<tail-flag> <call-id>} <exp1> ...]
; [##core#return <exp>]
; [##core#direct_call {<safe-flag> <debug-info> <call-id> <words>} <exp-f> <exp>...]

; Analysis database entries:
;
; <variable>:
;
;   captured -> <boolean>                    If true: variable is used outside it's home-scope
;   global -> <boolean>                      If true: variable does not occur in any lambda-list
;   call-sites -> ((<lambda-id> <node>) ...) Known call-nodes of a named procedure
;   home -> <lambda-id>                      Procedure which introduces this variable
;   unknown -> <boolean>                     If true: variable cannot have a known value
;   assigned -> <boolean>                    If true: variable is assigned somewhere
;   assigned-locally -> <boolean>            If true: variable has been assigned inside user lambda
;   undefined -> <boolean>                   If true: variable is unknown yet but can be known later
;   value -> <node>                          Variable has a known value
;   local-value -> <node>                    Variable is declared local and has value
;   potential-value -> <node>                Global variable was assigned this value
;   references -> (<node> ...)               Nodes that are accesses of this variable (##core#variable nodes)
;   boxed -> <boolean>                       If true: variable has to be boxed after closure-conversion
;   contractable -> <boolean>                If true: variable names contractable procedure
;   inlinable -> <boolean>                   If true: variable names potentially inlinable procedure
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
;   hidden-refs -> <boolean>                 If true: procedure that refers to hidden global variables
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


(include "compiler-namespace")

(define (d arg1 . more)
  (if (null? more)
      (pp arg1)
      (apply print arg1 more)))

(define-syntax d (syntax-rules () ((_ . _) (void))))

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

(define-constant foreign-type-table-size 301)
(define-constant analysis-database-size 3001)
(define-constant default-line-number-database-size 997)
(define-constant inline-table-size 301)
(define-constant constant-table-size 301)
(define-constant file-requirements-size 301)
(define-constant real-name-table-size 997)
(define-constant default-inline-max-size 20)


;;; Global variables containing compilation parameters:

(define unit-name #f)
(define number-type 'generic)
(define standard-bindings '())
(define extended-bindings '())
(define insert-timer-checks #t)
(define used-units '())
(define unsafe #f)
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
(define source-filename #f)
(define safe-globals-flag #f)
(define explicit-use-flag #f)
(define disable-stack-overflow-checking #f)
(define require-imports-flag #f)
(define emit-unsafe-marker #f)
(define external-protos-first #f)
(define do-lambda-lifting #f)
(define inline-max-size default-inline-max-size)
(define emit-closure-info #t)
(define undefine-shadowed-macros #t)
(define constant-declarations '())
(define profiled-procedures #f)
(define import-libraries '())
(define standalone-executable #t)
(define local-definitions #f)
(define inline-globally #f)
(define inline-locally #f)
(define inline-output-file #f)
(define do-scrutinize #f)
(define enable-inline-files #f)
(define compiler-syntax-enabled #t)


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
(define csc-control-file #f)
(define data-declarations '())
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
  (if file-requirements
      (vector-fill! file-requirements '())
      (set! file-requirements (make-vector file-requirements-size '())) )
  (if foreign-type-table
      (vector-fill! foreign-type-table '())
      (set! foreign-type-table (make-vector foreign-type-table-size '())) ) )


;;; Expand macros and canonicalize expressions:

(define (canonicalize-expression exp)

  (define (find-id id se)		; ignores macro bindings
    (cond ((null? se) #f)
	  ((and (eq? id (caar se)) (symbol? (cdar se))) (cdar se))
	  (else (find-id id (cdr se)))))

  (define (lookup id se)
    (cond ((find-id id se))
	  ((##sys#get id '##core#macro-alias))
	  (else id)))

  (define (macro-alias var se)
    (let ((alias (gensym var)))
      (##sys#put! alias '##core#macro-alias (lookup var se))
      alias) )

  (define (set-real-names! as ns)
    (for-each (lambda (a n) (set-real-name! a n)) as ns) )

  (define (write-to-string x)
    (let ([out (open-output-string)])
      (write x out)
      (get-output-string out) ) )

  (define (unquotify x se)
    (if (and (list? x) 
	     (= 2 (length x))
	     (symbol? (car x))
	     (eq? 'quote (lookup (car x) se)))
	(cadr x)
	x) )

  (define (resolve-variable x0 e se dest)
    (let ((x (lookup x0 se)))
      (d `(RESOLVE-VARIABLE: ,x0 ,x ,(map car se)))
      (cond ((not (symbol? x)) x0)	; syntax?
	    [(and constants-used (##sys#hash-table-ref constant-table x)) 
	     => (lambda (val) (walk (car val) e se dest)) ]
	    [(and inline-table-used (##sys#hash-table-ref inline-table x))
	     => (lambda (val) (walk val e se dest)) ]
	    [(assq x foreign-variables)
	     => (lambda (fv) 
		  (let* ([t (second fv)]
			 [ft (final-foreign-type t)] 
			 [body `(##core#inline_ref (,(third fv) ,t))] )
		    (walk
		     (foreign-type-convert-result
		      (finish-foreign-result ft body)
		      t)
		     e se dest)))]
	    [(assq x location-pointer-map)
	     => (lambda (a)
		  (let* ([t (third a)]
			 [ft (final-foreign-type t)] 
			 [body `(##core#inline_loc_ref (,t) ,(second a))] )
		    (walk
		     (foreign-type-convert-result
		      (finish-foreign-result ft body)
		      t)
		     e se dest))) ]
	    ((##sys#get x '##core#primitive))
	    ((not (memq x e)) (##sys#alias-global-hook x #f)) ; only if global
	    (else x))))
  
  (define (eval/meta form)
    (parameterize ((##sys#current-module #f)
		   (##sys#macro-environment (##sys#meta-macro-environment)))
      ((##sys#compile-to-closure
	form
	'() 
	(##sys#current-meta-environment))
       '() ) ))

  (define (walk x e se dest)
    (cond ((symbol? x)
	   (cond ((keyword? x) `(quote ,x))
		 ((memq x unlikely-variables)
		  (compiler-warning 
		   'var
		   "reference to variable `~s' possibly unintended" x) ))
	   (resolve-variable x e se dest))
	  ((not-pair? x)
	   (if (constant? x)
	       `(quote ,x)
	       (syntax-error "illegal atomic form" x)))
	  ((symbol? (car x))
	   (let ([ln (get-line x)])
	     (emit-syntax-trace-info x #f)
	     (unless (proper-list? x)
	       (if ln
		   (syntax-error (sprintf "(in line ~s) - malformed expression" ln) x)
		   (syntax-error "malformed expression" x)))
	     (set! ##sys#syntax-error-culprit x)
	     (let* ((name0 (lookup (car x) se))
		    (name (or (and (symbol? name0) (##sys#get name0 '##core#primitive)) name0))
		    (xexpanded (##sys#expand x se compiler-syntax-enabled)))
	       (cond ((not (eq? x xexpanded))
		      (walk xexpanded e se dest))
		     
		     [(and inline-table-used (##sys#hash-table-ref inline-table name))
		      => (lambda (val)
			   (walk (cons val (cdr x)) e se dest)) ]
		     
		     [else
		      (when ln (update-line-number-database! xexpanded ln))
		      (case name
			
			((if)
			 (##sys#check-syntax 'if x '(if _ _ . #(_)) #f se)
			 `(if
			   ,(walk (cadr x) e se #f)
			   ,(walk (caddr x) e se #f)
			   ,(if (null? (cdddr x)) 
				'(##core#undefined)
				(walk (cadddr x) e se #f) ) ) )

			((quote syntax ##core#syntax)
			 (##sys#check-syntax name x '(_ _) #f se)
			 `(quote ,(##sys#strip-syntax (cadr x))))

			((##core#check)
			 (if unsafe
			     ''#t
			     (walk (cadr x) e se dest) ) )

			((##core#immutable)
			 (let ((c (cadadr x)))
			   (cond [(assoc c immutable-constants) => cdr]
				 [else
				  (let ([var (gensym 'c)])
				    (set! immutable-constants (alist-cons c var immutable-constants))
				    (mark-variable var '##compiler#always-bound)
				    (hide-variable var)
				    var) ] ) ) )

			((##core#undefined ##core#callunit ##core#primitive) x)
			
			((##core#inline_ref) 
			 `(##core#inline_ref 
			   (,(caadr x) ,(##sys#strip-syntax (cadadr x)))))

			((##core#inline_loc_ref)
			 `(##core#inline_loc_ref 
			   ,(##sys#strip-syntax (cadr x))
			   ,(walk (caddr x) e se dest)))

			((##core#require-for-syntax)
			 (let ([ids (map eval (cdr x))])
			   (apply ##sys#require ids)
			   (##sys#hash-table-update! 
			    file-requirements 'dynamic/syntax 
			    (cut lset-union eq? <> ids)
			    (lambda () ids) )
			   '(##core#undefined) ) )

			((##core#require-extension)
			 (let ((imp? (caddr x)))
			   (walk
			    (let loop ([ids (cadr x)])
			      (if (null? ids)
				  '(##core#undefined)
				  (let ([id (car ids)])
				    (let-values ([(exp f) (##sys#do-the-right-thing id #t imp?)])
				      (unless (or f 
						  (and (symbol? id)
						       (or (feature? id)
							   (##sys#find-extension
							    (##sys#canonicalize-extension-path 
							     id 'require-extension)
							    #f)) ) ) 
					(compiler-warning 
					 'ext "extension `~A' is currently not installed" id))
				      `(##core#begin ,exp ,(loop (cdr ids))) ) ) ) )
			    e se dest) ) )

			((let ##core#let)
			 (##sys#check-syntax 'let x '(_ #((variable _) 0) . #(_ 1)) #f se)
			 (let* ((bindings (cadr x))
				(vars (unzip1 bindings))
				(aliases (map gensym vars))
				(se2 (append (map cons vars aliases) se)) )
			   (set-real-names! aliases vars)
			   `(let
			     ,(map (lambda (alias b)
				     (list alias (walk (cadr b) e se (car b))) )
				   aliases bindings)
			     ,(walk (##sys#canonicalize-body (cddr x) se2 compiler-syntax-enabled)
				    (append aliases e)
				    se2 dest) ) ) )

			((letrec ##core#letrec)
			 (##sys#check-syntax 'letrec x '(_ #((symbol _) 0) . #(_ 1)))
			 (let ((bindings (cadr x))
			       (body (cddr x)) )
			   (walk
			    `(##core#let
			      ,(map (lambda (b)
				      (list (car b) '(##core#undefined))) 
				    bindings)
			      ,@(map (lambda (b)
				       `(##core#set! ,(car b) ,(cadr b))) 
				     bindings)
			      (##core#let () ,@body) )
			    e se dest)))

			((lambda ##core#lambda)
			 (##sys#check-syntax 'lambda x '(_ lambda-list . #(_ 1)) #f se)
			 (let ((llist (cadr x))
			       (obody (cddr x)) )
			   (when (##sys#extended-lambda-list? llist)
			     (set!-values 
			      (llist obody) 
			      (##sys#expand-extended-lambda-list 
			       llist obody ##sys#error se) ) )
			   (decompose-lambda-list
			    llist
			    (lambda (vars argc rest)
			      (let* ((aliases (map gensym vars))
				     (se2 (append (map cons vars aliases) se))
				     (body0 (##sys#canonicalize-body obody se2 compiler-syntax-enabled))
				     (body (walk body0 (append aliases e) se2 #f))
				     (llist2 
				      (build-lambda-list
				       aliases argc
				       (and rest (list-ref aliases (posq rest vars))) ) )
				     (l `(lambda ,llist2 ,body)) )
				(set-real-names! aliases vars)
				(cond ((or (not dest) 
					   (assq dest se)) ; not global?
				       l)
				      ((and (eq? 'lambda (or (lookup name se) name))
					    emit-profile
					    (or (eq? profiled-procedures 'all)
						(and
						 (eq? profiled-procedures 'some)
						 (variable-mark dest '##compiler#profile))))
				       (expand-profile-lambda dest llist2 body) )
				      (else
				       (if (and (> (length body0) 1)
						(symbol? (car body0))
						(eq? 'begin (or (lookup (car body0) se) (car body0)))
						(let ((x1 (cadr body0)))
						  (or (string? x1)
						      (and (list? x1)
							   (= (length x1) 2)
							   (symbol? (car x1))
							   (eq? 'quote (or (lookup (car x1) se) (car x1)))))))
					   (process-lambda-documentation
					    dest (cadr body) l) 
					   l))))))))
			
			((let-syntax)
			 (##sys#check-syntax 'let-syntax x '(let-syntax #((variable _) 0) . #(_ 1)) #f se)
			 (let ((se2 (append
				     (map (lambda (b)
					    (list
					     (car b)
					     se
					     (##sys#er-transformer
					      (eval/meta (cadr b)))))
					  (cadr x) )
				     se) ) )
			   (walk
			    (##sys#canonicalize-body (cddr x) se2 compiler-syntax-enabled)
			    e se2
			    dest) ) )
			       
		       ((letrec-syntax)
			(##sys#check-syntax 'letrec-syntax x '(letrec-syntax #((variable _) 0) . #(_ 1)) #f se)
			(let* ((ms (map (lambda (b)
					  (list
					   (car b)
					   #f
					   (##sys#er-transformer
					    (eval/meta (cadr b)))))
					(cadr x) ) )
			       (se2 (append ms se)) )
			  (for-each 
			   (lambda (sb)
			     (set-car! (cdr sb) se2) )
			   ms)
			  (walk
			   (##sys#canonicalize-body (cddr x) se2 compiler-syntax-enabled)
			   e se2 dest)))
			       
		       ((define-syntax define-compiled-syntax)
			(##sys#check-syntax
			 (car x) x
			 (if (pair? (cadr x))
			     '(_ (variable . lambda-list) . #(_ 1))
			     '(_ variable _) )
			 #f se)
			(let* ((var (if (pair? (cadr x)) (caadr x) (cadr x)))
			       (body (if (pair? (cadr x))
					 `(##core#lambda ,(cdadr x) ,@(cddr x))
					 (caddr x)))
			       (name (lookup var se)))
			  (##sys#register-syntax-export name (##sys#current-module) body)
			  (##sys#extend-macro-environment
			   name
			   (##sys#current-environment)
			   (##sys#er-transformer (eval/meta body)))
			  (walk
			   (if (or ##sys#enable-runtime-macros
				   (eq? 'define-compiled-syntax (car x)))
			       `(##sys#extend-macro-environment
				 ',var
				 (##sys#current-environment)
				 (##sys#er-transformer ,body)) ;*** possibly wrong se?
			       '(##core#undefined) )
			   e se dest)) )

		       ((##core#define-compiler-syntax)
			(let* ((var (cadr x))
			       (body (caddr x))
			       (name (##sys#strip-syntax var se #t)))
			  (##sys#put! 
			   name '##compiler#compiler-syntax
			   (##sys#cons
			    (##sys#er-transformer (eval/meta body))
			    (##sys#current-environment)))
			  (walk 
			   (if ##sys#enable-runtime-macros
			       `(##sys#put! 
				(##core#syntax ,name)
				'##compiler#compiler-syntax
				(##sys#cons
				 (##sys#er-transformer ,body)
				 (##sys#current-environment)))
			       '(##core#undefined) )
			   e se dest)))

		       ((##core#let-compiler-syntax)
			(let ((bs (map (lambda (b)
					 (##sys#check-syntax 'let-compiler-syntax b '(symbol _))
					 (let ((name (##sys#strip-syntax (car b) se #t)))
					   (list 
					    name 
					    (cons (##sys#er-transformer (eval/meta (cadr b))) se)
					    (##sys#get name '##compiler#compiler-syntax) ) ) )
				       (cadr x))))
			  (dynamic-wind	; this ain't thread safe
			      (lambda ()
				(for-each
				 (lambda (b) (##sys#put! (car b) '##compiler#compiler-syntax (cadr b)))
				 bs) )
			      (lambda ()
				(walk 
				 (##sys#canonicalize-body (cddr x) se compiler-syntax-enabled)
				 e se dest) )
			      (lambda ()
				(for-each
				 (lambda (b) (##sys#put! (car b) '##compiler#compiler-syntax (caddr b)))
				 bs) ) ) ) )

		       ((##core#module)
			(let* ((name (lookup (cadr x) se))
			       (exports 
				(or (eq? #t (caddr x))
				    (map (lambda (exp)
					   (cond ((symbol? exp) exp)
						 ((and (pair? exp)
						       (let loop ((exp exp))
							 (or (null? exp)
							     (and (symbol? (car exp))
								  (loop (cdr exp))))))
						  exp)
						 (else
						  (##sys#syntax-error-hook
						   'module
						   "invalid export syntax" exp name))))
					 (##sys#strip-syntax (caddr x))))))
			  (when (##sys#current-module)
			    (##sys#syntax-error-hook 'module "modules may not be nested" name))
			  (let-values (((body mreg)
					(parameterize ((##sys#current-module 
							(##sys#register-module name exports) )
						       (##sys#current-environment '())
						       (##sys#macro-environment ##sys#initial-macro-environment))
					    (let loop ((body (cdddr x)) (xs '()))
					      (cond 
					       ((null? body)
						(handle-exceptions ex
						    (begin
						      ;; avoid backtrace
						      (print-error-message ex (current-error-port))
						      (exit 1))
						  (##sys#finalize-module (##sys#current-module)))
						(cond ((assq name import-libraries) =>
						       (lambda (il)
							 (when verbose-mode
							   (print "generating import library " (cdr il) " for module "
								  name " ..."))
							 (with-output-to-file (cdr il)
							   (lambda ()
							     (for-each
							      pretty-print
							      (##sys#compiled-module-registration
							       (##sys#current-module))))) 
							 (values 
							  (reverse xs)
							  '((##core#undefined)))))
						      (else
						       (values
							(reverse xs)
							(if standalone-executable
							    '()
							    (##sys#compiled-module-registration (##sys#current-module)))))))
					       (else
						(when (and (pair? body)
							   (null? xs)
							   (pair? (car body))
							   (symbol? (caar body))
							   (let ((imp (or (lookup (caar body) se) (caar body))))
							     (and (not (memq imp '(import import-for-syntax)))
								  ;; can it get any uglier? yes, it can
								  (not (eq? imp (cdr (assq 'import ##sys#initial-macro-environment))))
								  (not (eq? imp (cdr (assq 'import-for-syntax ##sys#initial-macro-environment)))))))
						  (compiler-warning 
						   'syntax
						   "module body of `~s' does not begin with `import' form - maybe unintended?"
						   name))
						(loop 
						 (cdr body)
						 (cons (walk 
							(car body)
							e ;?
							(##sys#current-environment)
							#f)
						       xs))))))))
			    (canonicalize-begin-body
			     (append
			      (parameterize ((##sys#current-module #f)
					     (##sys#macro-environment (##sys#meta-macro-environment)))
				(map
				 (lambda (x)
				   (walk 
				    x 
				    e 	;?
				    (##sys#current-meta-environment) #f) )
				 mreg))
			      body)))))

		       ((##core#named-lambda)
			(walk `(##core#lambda ,@(cddr x)) e se (cadr x)) )

		       ((##core#loop-lambda)
			(let* ([vars (cadr x)]
			       [obody (cddr x)]
			       [aliases (map gensym vars)]
			       (se2 (append (map cons vars aliases) se))
			       [body 
				(walk 
				 (##sys#canonicalize-body obody se2 compiler-syntax-enabled)
				 (append aliases e) 
				 se2 #f) ] )
			  (set-real-names! aliases vars)
			  `(##core#lambda ,aliases ,body) ) )

			((set! ##core#set!) 
			 (##sys#check-syntax 'set! x '(_ variable _) #f se)
			 (let* ([var0 (cadr x)]
				[var (lookup var0 se)]
				[ln (get-line x)]
				[val (caddr x)] )
			   (when (memq var unlikely-variables)
			     (compiler-warning 
			      'var
			      "assignment to variable `~s' possibly unintended"
			      var))
			   (cond ((assq var foreign-variables)
				   => (lambda (fv)
					(let ([type (second fv)]
					      [tmp (gensym)] )
					  (walk
					   `(let ([,tmp ,(foreign-type-convert-argument val type)])
					      (##core#inline_update 
					       (,(third fv) ,type)
					       ,(foreign-type-check tmp type) ) )
					   e se #f))))
				 ((assq var location-pointer-map)
				  => (lambda (a)
				       (let* ([type (third a)]
					      [tmp (gensym)] )
					 (walk
					  `(let ([,tmp ,(foreign-type-convert-argument val type)])
					     (##core#inline_loc_update 
					      (,type)
					      ,(second a)
					      ,(foreign-type-check tmp type) ) )
					  e se #f))))
				 (else
				  (unless (memq var e) ; global?
				    (set! var (or (##sys#get var '##core#primitive)
						  (##sys#alias-global-hook var #t)))
				    (when safe-globals-flag
				      (mark-variable var '##compiler#always-bound-to-procedure)
				      (mark-variable var '##compiler#always-bound)))
				  (when (##sys#macro? var)
				    (compiler-warning 
				     'var "assigned global variable `~S' is a macro ~A"
				     var
				     (if ln (sprintf "in line ~S" ln) "") )
				    (when undefine-shadowed-macros (##sys#undefine-macro! var) ) )
				  (when (keyword? var)
				    (compiler-warning 'syntax "assignment to keyword `~S'" var) )
				  (when (pair? var) ; macro
				    (syntax-error
				     'set! "assignment to syntactic identifier" var))
				  `(set! ,var ,(walk val e se var0))))))

			((##core#inline)
			 `(##core#inline ,(unquotify (cadr x) se) ,@(mapwalk (cddr x) e se)))

			((##core#inline_allocate)
			 `(##core#inline_allocate 
			   ,(map (cut unquotify <> se) (second x))
			   ,@(mapwalk (cddr x) e se)))

			((##core#inline_update)
			 `(##core#inline_update ,(cadr x) ,(walk (caddr x) e se #f)) )

			((##core#inline_loc_update)
			 `(##core#inline_loc_update 
			   ,(cadr x) 
			   ,(walk (caddr x) e se #f)
			   ,(walk (cadddr x) e se #f)) )

			((##core#compiletimetoo ##core#elaborationtimetoo)
			 (let ((exp (cadr x)))
			   (eval/meta exp)
			   (walk exp e se dest) ) )

			((##core#compiletimeonly ##core#elaborationtimeonly)
			 (eval/meta (cadr x))
			 '(##core#undefined) )

			((begin ##core#begin) 
			 (##sys#check-syntax 'begin x '(_ . #(_ 0)) #f se)
			 (if (pair? (cdr x))
			     (canonicalize-begin-body
			      (let fold ([xs (cdr x)])
				(let ([x (car xs)]
				      [r (cdr xs)] )
				  (if (null? r)
				      (list (walk x e se dest))
				      (cons (walk x e se #f) (fold r)) ) ) ) )
			     '(##core#undefined) ) )

			((foreign-lambda)
			 (walk (expand-foreign-lambda x #f) e se dest) )

			((foreign-safe-lambda)
			 (walk (expand-foreign-lambda x #t) e se dest) )

			((foreign-lambda*)
			 (walk (expand-foreign-lambda* x #f) e se dest) )

			((foreign-safe-lambda*)
			 (walk (expand-foreign-lambda* x #t) e se dest) )

			((foreign-primitive)
			 (walk (expand-foreign-primitive x) e se dest) )

			((define-foreign-variable)
			 (let* ([var (##sys#strip-syntax (second x))]
				[type (##sys#strip-syntax (third x))]
				[name (if (pair? (cdddr x))
					  (fourth x)
					  (symbol->string var) ) ] )
			   (set! foreign-variables
			     (cons (list var type
					 (if (string? name)
					     name 
					     (symbol->string name)))
				   foreign-variables))
			   '(##core#undefined) ) )

			((define-foreign-type)
			 (let ([name (second x)]
			       [type (##sys#strip-syntax (third x))] 
			       [conv (cdddr x)] )
			   (cond [(pair? conv)
				  (let ([arg (gensym)]
					[ret (gensym)] )
				    (##sys#hash-table-set! foreign-type-table name (vector type arg ret))
				    (mark-variable arg '##compiler#always-bound)
				    (mark-variable ret '##compiler#always-bound)
				    (hide-variable arg)
				    (hide-variable ret)
				    (walk
				     `(##core#begin
					(define ,arg ,(first conv))
					(define 
					 ,ret 
					 ,(if (pair? (cdr conv)) (second conv) '##sys#values)) ) 
				     e se dest) ) ]
				 [else
				  (##sys#hash-table-set! foreign-type-table name type)
				  '(##core#undefined) ] ) ) )

			((define-external-variable)
			 (let* ([sym (second x)]
				[name (symbol->string sym)]
				[type (third x)] 
				[exported (fourth x)]
				[rname (make-random-name)] )
			   (unless exported (set! name (symbol->string (fifth x))))
			   (set! external-variables (cons (vector name type exported) external-variables))
			   (set! foreign-variables
			     (cons (list rname 'c-pointer (string-append "&" name))
				   foreign-variables) )
			   (set! external-to-pointer (alist-cons sym rname external-to-pointer))
			   '(##core#undefined) ) )

			((##core#let-location)
			 (let* ([var (second x)]
				[type (##sys#strip-syntax (third x))]
				[alias (gensym)]
				[store (gensym)] 
				[init (and (pair? (cddddr x)) (fourth x))] )
			   (set-real-name! alias var)
			   (set! location-pointer-map
			     (cons (list alias store type) location-pointer-map) )
			   (walk
			    `(let (,(let ([size (words (estimate-foreign-result-location-size type))])
				      ;; Add 2 words: 1 for the header, 1 for double-alignment:
				      ;; Note: C_a_i_bytevector takes number of words, not bytes
				      (list 
				       store
				       `(##core#inline_allocate
					 ("C_a_i_bytevector" ,(+ 2 size))
					 ',size)) ) )
			       (##core#begin
				,@(if init
				      `((##core#set! ,alias ,init))
				      '() )
				,(if init (fifth x) (fourth x)) ) )
			    e (alist-cons var alias se)
			    dest) ) )

			((##core#define-inline)
			 (let* ((name (second x))
				(val `(##core#lambda ,@(cdaddr x))))
			     (##sys#hash-table-set! inline-table name val)
			     (set! inline-table-used #t)
			     '(##core#undefined)))

			((define-constant)
			 (let* ([name (second x)]
				[valexp (third x)]
				[val (handle-exceptions ex
					 ;; could show line number here
					 (quit "error in constant evaluation of ~S for named constant ~S" 
					       valexp name)
				       (if (collapsable-literal? valexp)
					   valexp
					   (eval
					    `(##core#let
					      ,defconstant-bindings ,valexp)) ) ) ] )
			   (set! constants-used #t)
			   (set! defconstant-bindings (cons (list name `',val) defconstant-bindings))
			   (cond [(collapsable-literal? val)
				  (##sys#hash-table-set! constant-table name (list val))
				  '(##core#undefined) ]
				 [else
				  (let ([var (gensym "constant")])
				    (##sys#hash-table-set! constant-table name (list var))
				    (hide-variable var)
				    (mark-variable var '##compiler#constant)
				    (mark-variable var '##compiler#always-bound)
				    (walk `(define ,var ',val) e se #f) ) ] ) ) )

			((##core#declare)
			 (walk
			  `(##core#begin
			     ,@(map (lambda (d)
				      (process-declaration d se))
				    (cdr x) ) )
			  e '() #f) )
	     
			((##core#foreign-callback-wrapper)
			 (let-values ([(args lam) (split-at (cdr x) 4)])
			   (let* ([lam (car lam)]
				  [name (cadr (first args))]
				  [rtype (cadr (third args))]
				  [atypes (cadr (fourth args))]
				  [vars (second lam)] )
			     (if (valid-c-identifier? name)
				 (set! callback-names (cons name callback-names))
				 (quit "name `~S' of external definition is not a valid C identifier"
				       name) )
			     (when (or (not (proper-list? vars)) 
				       (not (proper-list? atypes))
				       (not (= (length vars) (length atypes))) )
			       (syntax-error 
				"non-matching or invalid argument list to foreign callback-wrapper"
				vars atypes) )
			     `(##core#foreign-callback-wrapper
			       ,@(mapwalk args e se)
			       ,(walk `(##core#lambda 
					,vars
					(##core#let
					 ,(let loop ([vars vars] [types atypes])
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
					   `(##core#let
					     ()
					     ,@(cond
						((member 
						  rtype
						  '((const nonnull-c-string) 
						    (const nonnull-unsigned-c-string)
						    nonnull-unsigned-c-string
						    nonnull-c-string))
						 `((##sys#make-c-string
						    (##core#let
						     () ,@(cddr lam)))))
						((member 
						  rtype
						  '((const c-string*)
						    (const unsigned-c-string*)
						    unsigned-c-string*
						    c-string*
						    c-string-list
						    c-string-list*))
						 (syntax-error
						  "not a valid result type for callback procedures"
						  rtype
						  name) )
						((member 
						  rtype
						  '(c-string
						    (const unsigned-c-string)
						    unsigned-c-string
						    (const c-string)) )
						 `((##core#let
						    ((r (##core#let () ,@(cddr lam))))
						    (,(macro-alias 'and se)
						     r 
						     (##sys#make-c-string r)) ) ) )
						(else (cddr lam)) ) )
					   rtype) ) )
				      e se #f) ) ) ) )

			(else
			 (let ([handle-call
				(lambda ()
				  (let* ([x2 (mapwalk x e se)]
					 [head2 (car x2)]
					 [old (##sys#hash-table-ref line-number-database-2 head2)] )
				    (when ln
				      (##sys#hash-table-set!
				       line-number-database-2
				       head2
				       (cons name (alist-cons x2 ln (if old (cdr old) '()))) ) )
				    x2) ) ] )

			   (cond [(eq? 'location name)
				  (##sys#check-syntax 'location x '(location _) #f se)
				  (let ([sym (cadr x)])
				    (if (symbol? sym)
					(cond [(assq (lookup sym se) location-pointer-map)
					       => (lambda (a)
						    (walk
						     `(##sys#make-locative ,(second a) 0 #f 'location)
						     e se #f) ) ]
					      [(assq sym external-to-pointer) 
					       => (lambda (a) (walk (cdr a) e se #f)) ]
					      [(memq sym callback-names)
					       `(##core#inline_ref (,(symbol->string sym) c-pointer)) ]
					      [else 
					       (walk `(##sys#make-locative ,sym 0 #f 'location) e se #f) ] )
					(walk `(##sys#make-locative ,sym 0 #f 'location) e se #f) ) ) ]
				 
				 [else (handle-call)] ) ) ) ) ] ) ) ) )

	  ((not (proper-list? x))
	   (syntax-error "malformed expression" x) )

	  ((constant? (car x))
	   (emit-syntax-trace-info x #f)
	   (compiler-warning 'syntax "literal in operator position: ~S" x) 
	   (mapwalk x e se) )

	  (else
	   (emit-syntax-trace-info x #f)
	   (let ((x (mapwalk x e se)))
	     (if (and (pair? (car x))
		      (symbol? (caar x))
		      (memq (or (lookup (caar x) se) (caar x)) '(lambda ##core#lambda)))
		 (let ((lexp (car x))
		       (args (cdr x)) )
		   (##sys#check-syntax 'lambda lexp '(_ lambda-list . #(_ 1)) #f se)
		   (let ((llist (cadr lexp)))
		     (if (and (proper-list? llist) (= (llist-length llist) (length args)))
			 `(let ,(map list llist args) ,@(cddr lexp)) 
			 (let ((var (gensym 't)))
			   `(let ((,var ,(car x)))
			     (,var ,@(cdr x)) ) ) ) ) ) 
		 x))) ) )
  
  (define (mapwalk xs e se)
    (map (lambda (x) (walk x e se #f)) xs) )

  (when (memq 'c debugging-chicken) (newline) (pretty-print exp))
  (##sys#clear-trace-buffer)
  ;; Process visited definitions and main expression:
  (walk 
   `(##core#begin
     ,@(let ([p (reverse pending-canonicalizations)])
	 (set! pending-canonicalizations '())
	 p)
     ,(begin
	(set! extended-bindings (append internal-bindings extended-bindings))
	exp) )
   '() (##sys#current-environment)
   #f) )


(define (process-declaration spec se)	; se unused in the moment
  (define (check-decl spec minlen . maxlen)
    (let ([n (length (cdr spec))])
      (if (or (< n minlen) (> n (optional maxlen 99999)))
	  (syntax-error "invalid declaration" spec) ) ) )  
  (define (stripa x)			; global aliasing
    (##sys#strip-syntax x se #t))
  (define (strip x)			; raw symbol
    (##sys#strip-syntax x se))
  (call-with-current-continuation
   (lambda (return)
     (unless (pair? spec)
       (syntax-error "invalid declaration specification" spec) )
     ;(pp `(DECLARE: ,(strip spec)))
     (case (##sys#strip-syntax (car spec)) ; no global aliasing
       ((uses)
	(let ((us (strip (cdr spec))))
	  (apply register-feature! us)
	  (when (pair? us)
	    (##sys#hash-table-update! 
	     file-requirements 'static
	     (cut lset-union eq? us <>) 
	     (lambda () us))
	    (let ((units (map (lambda (u) (string->c-identifier (stringify u))) us)))
	      (set! used-units (append used-units units)) ) ) ) )
       ((unit)
	(check-decl spec 1 1)
	(let* ([u (strip (cadr spec))]
	       [un (string->c-identifier (stringify u))] )
	  (when (and unit-name (not (string=? unit-name un)))
	    (compiler-warning 'usage "unit was already given a name (new name is ignored)") )
	  (set! unit-name un) ) )
       ((standard-bindings)
	(if (null? (cdr spec))
	    (set! standard-bindings default-standard-bindings)
	    (set! standard-bindings (append (stripa (cdr spec)) standard-bindings)) ) )
       ((extended-bindings)
	(if (null? (cdr spec))
	    (set! extended-bindings default-extended-bindings)
	    (set! extended-bindings (append (stripa (cdr spec)) extended-bindings)) ) )
       ((usual-integrations)      
	(cond [(null? (cdr spec))
	       (set! standard-bindings default-standard-bindings)
	       (set! extended-bindings default-extended-bindings) ]
	      [else
	       (let ([syms (stripa (cdr spec))])
		 (set! standard-bindings (lset-intersection eq? syms default-standard-bindings))
		 (set! extended-bindings (lset-intersection eq? syms default-extended-bindings)) ) ] ) )
       ((number-type)
	(check-decl spec 1 1)
	(set! number-type (strip (cadr spec))))
       ((fixnum fixnum-arithmetic) (set! number-type 'fixnum))
       ((generic) (set! number-type 'generic))
       ((unsafe) (set! unsafe #t))
       ((safe) (set! unsafe #f))
       ((no-bound-checks) (set! no-bound-checks #t))
       ((no-argc-checks) (set! no-argc-checks #t))
       ((no-procedure-checks) (set! no-procedure-checks #t))
       ((interrupts-enabled) (set! insert-timer-checks #t))
       ((disable-interrupts) (set! insert-timer-checks #f))
       ((disable-warning)
	(set! disabled-warnings
	  (append (strip (cdr spec)) disabled-warnings)))
       ((always-bound) 
	(for-each (cut mark-variable <> '##compiler#always-bound) (stripa (cdr spec))))
       ((safe-globals) (set! safe-globals-flag #t))
       ((no-procedure-checks-for-usual-bindings)
	(for-each 
	 (cut mark-variable <> '##compiler#always-bound-to-procedure)
	 (append default-standard-bindings default-extended-bindings))
	(for-each
	 (cut mark-variable <> '##compiler#always-bound)
	 (append default-standard-bindings default-extended-bindings)))
       ((bound-to-procedure)
	(let ((vars (stripa (cdr spec))))
	  (for-each (cut mark-variable <> '##compiler#always-bound-to-procedure) vars)
	  (for-each (cut mark-variable <> '##compiler#always-bound) vars)))
       ((foreign-declare)
	(let ([fds (cdr spec)])
	  (if (every string? fds)
	      (set! foreign-declarations (append foreign-declarations fds))
	      (syntax-error "invalid declaration" spec) ) ) )
       ((c-options)
	(emit-control-file-item `(c-options ,@(strip (cdr spec)))) )
       ((link-options)
	(emit-control-file-item `(link-options ,@(strip (cdr spec))) ) )
       ((post-process)
	(emit-control-file-item
	 (let ([file (pathname-strip-extension source-filename)])
	   `(post-process ,@(map (cut string-substitute "\\$@" file <>) (cdr spec))) ) ) )
       ((block) (set! block-compilation #t))
       ((separate) (set! block-compilation #f))
       ((keep-shadowed-macros) (set! undefine-shadowed-macros #f))
       ((unused)
	(for-each (cut mark-variable <> '##compiler#unused) (stripa (cdr spec))))
       ((not)
	(check-decl spec 1)
	(case (##sys#strip-syntax (second spec)) ; strip all
	  [(standard-bindings)
	   (if (null? (cddr spec))
	       (set! standard-bindings '())
	       (set! standard-bindings
		 (lset-difference eq? default-standard-bindings
				  (stripa (cddr spec))))) ]
	  [(extended-bindings)
	   (if (null? (cddr spec))
	       (set! extended-bindings '())
	       (set! extended-bindings 
		 (lset-difference eq? default-extended-bindings
				  (stripa (cddr spec))) )) ]
	  [(inline)
	   (if (null? (cddr spec))
	       (set! inline-locally #f)
	       (for-each 
		(cut mark-variable <> '##compiler#inline 'no)
		(stripa (cddr spec)))) ]
	  [(usual-integrations)      
	   (cond [(null? (cddr spec))
		  (set! standard-bindings '())
		  (set! extended-bindings '()) ]
		 [else
		  (let ([syms (stripa (cddr spec))])
		    (set! standard-bindings (lset-difference eq? default-standard-bindings syms))
		    (set! extended-bindings (lset-difference eq? default-extended-bindings syms)) ) ] ) ]
	  ((inline-global)
	   (set! enable-inline-files #t)
	   (if (null? (cddr spec))
	       (set! inline-globally #f)
	       (for-each
		(cut mark-variable <> '##compiler#inline-global 'no)
		(stripa (cddr spec)))))
	  [else
	   (check-decl spec 1 1)
	   (let ((id (strip (cadr spec))))
	     (case id
	       [(interrupts-enabled) (set! insert-timer-checks #f)]
	       [(safe) (set! unsafe #t)]
	       [else (compiler-warning 'syntax "illegal declaration specifier `~s'" id)]))]))
       ((compile-syntax 
	 run-time-macros)		; DEPRECATED
	(set! ##sys#enable-runtime-macros #t))
       ((block-global hide) 
	(let ([syms (stripa (cdr spec))])
	  (if (null? syms)
	      (set! block-compilation #t)
	      (for-each hide-variable syms))))
       ((export)
	(set! block-compilation #t)
	(let ((syms (stripa (cdr spec))))
	  (for-each export-variable syms)))
       ((emit-external-prototypes-first)
	(set! external-protos-first #t) )
       ((lambda-lift) (set! do-lambda-lifting #t))
       ((inline)
	(if (null? (cdr spec))
	    (set! inline-locally #t)
	    (for-each
	     (cut mark-variable <> '##compiler#inline 'yes)
	     (stripa (cdr spec)))))
       ((inline-limit)
	(check-decl spec 1 1)
	(let ([n (cadr spec)])
	  (if (number? n)
	      (set! inline-max-size n)
	      (compiler-warning 
	       'syntax
	       "invalid argument to `inline-limit' declaration: ~s" spec) ) ) )
       ((constant)
	(let ((syms (cdr spec)))
	  (if (every symbol? syms)
	      (set! constant-declarations (append syms constant-declarations))
	      (quit "invalid arguments to `constant' declaration: ~S" spec)) ) )
       ((emit-import-library)
	(set! import-libraries
	  (append
	   import-libraries
	   (map (lambda (il)
		  (cond ((symbol? il)
			 (cons il (string-append (symbol->string il) ".import.scm")) )
			((and (list? il) (= 2 (length il))
			      (symbol? (car il)) (string (cadr il)))
			 (cons (car il) (cadr il))) 
			(else
			 (compiler-warning 
			  'syntax
			  "invalid import-library specification: ~s" il))))
		(strip (cdr spec))))))
       ((profile)
	(set! emit-profile #t)
	(cond ((null? (cdr spec))
	       (set! profiled-procedures 'all) )
	      (else
	       (set! profiled-propcedures 'some)
	       (for-each 
		(cut mark-variable <> '##compiler#profile)
		(stripa (cdr spec))))))
       ((local)
	(cond ((null? (cdr spec))
	       (set! local-definitions #t) )
	      (else
	       (for-each 
		(cut mark-variable <> '##compiler#local)
		(stripa (cdr spec))))))
       ((inline-global)
	(set! enable-inline-files #t)
	(set! inline-locally #t)
	(if (null? (cdr spec))
	    (set! inline-globally #t)
	    (for-each
	     (cut mark-variable <> '##compiler#inline-global 'yes)
	     (stripa (cdr spec)))))
       ((type)
	(for-each
	 (lambda (spec)
	   (cond ((and (list? spec) (symbol? (car spec)) (= 2 (length spec)))
		  (##sys#put! (car spec) '##core#type (cadr spec))
		  (##sys#put! (car spec) '##core#declared-type #t))
		 (else
		  (compiler-warning 'syntax "illegal `type' declaration item `~s'" spec))))
	 (cdr spec)))
       ((scrutinize)
	(set! do-scrutinize #t))
       (else (compiler-warning 'syntax "illegal declaration specifier `~s'" spec)) )
     '(##core#undefined) ) ) )


;;; Expand "foreign-lambda"/"foreign-callback-lambda" forms and add item to stub-list:

(define-record-type foreign-stub
  (make-foreign-stub id return-type name argument-types argument-names body cps callback)
  foreign-stub?
  (id foreign-stub-id)			; symbol
  (return-type foreign-stub-return-type)	  ; type-specifier
  (name foreign-stub-name)			  ; string or #f
  (argument-types foreign-stub-argument-types) ; (type-specifier...)
  (argument-names foreign-stub-argument-names) ; #f or (symbol ...)
  (body foreign-stub-body)		       ; #f or string
  (cps foreign-stub-cps)		       ; boolean
  (callback foreign-stub-callback))	       ; boolean

(define (create-foreign-stub rtype sname argtypes argnames body callback cps)
  (let* ((rtype (##sys#strip-syntax rtype))
	 (argtypes (##sys#strip-syntax argtypes))
	 [params (list-tabulate (length argtypes) (lambda (x) (gensym 'a)))]
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

(define (expand-foreign-lambda exp callback?)
  (let* ([name (third exp)]
	 [sname (cond ((symbol? name) (symbol->string (##sys#strip-syntax name)))
		      ((string? name) name)
		      (else (quit "name `~s' of foreign procedure has wrong type" name)) ) ]
	 [rtype (second exp)]
	 [argtypes (cdddr exp)] )
    (create-foreign-stub rtype sname argtypes #f #f callback? callback?) ) )

(define (expand-foreign-lambda* exp callback?)
  (let* ([rtype (second exp)]
	 [args (third exp)]
	 [body (apply string-append (cdddr exp))]
 	 [argtypes (map car args)]
         ;; C identifiers aren't hygienically renamed inside body strings
	 [argnames (map cadr (##sys#strip-syntax args))] )
    (create-foreign-stub rtype #f argtypes argnames body callback? callback?) ) )

;; TODO: Try to fold this procedure into expand-foreign-lambda*
(define (expand-foreign-primitive exp)
  (let* ([hasrtype (and (pair? (cddr exp)) (not (string? (caddr exp))))]
	 [rtype (if hasrtype (second exp) 'void)]
	 [args (##sys#strip-syntax (if hasrtype (third exp) (second exp)))]
	 [body (apply string-append (if hasrtype (cdddr exp) (cddr exp)))]
 	 [argtypes (map car args)]
         ;; C identifiers aren't hygienically renamed inside body strings
	 [argnames (map cadr (##sys#strip-syntax args))] )
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
	((let)
	 (let loop ((vars params) (vals subs))
	   (if (null? vars)
	       (walk (car vals) k)
	       (walk (car vals)
		     (lambda (r) 
		       (make-node 'let
				  (list (car vars))
				  (list r (loop (cdr vars) (cdr vals))) ) ) ) ) ) )
	((lambda ##core#lambda) (cps-lambda (gensym-f-id) (first params) subs k))
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

(define-record-type foreign-callback-stub
  (make-foreign-callback-stub id name qualifiers return-type argument-types)
  foreign-callback-stub?
  (id foreign-callback-stub-id)		; symbol
  (name foreign-callback-stub-name)	; string
  (qualifiers foreign-callback-stub-qualifiers)	; string
  (return-type foreign-callback-stub-return-type) ; type-specifier
  (argument-types foreign-callback-stub-argument-types)) ; (type-specifier ...)


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
		     ((not (get db var 'global)) 
		      (put! db var 'global #t) ) ) ) ) )
	  
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
		   (if (and (intrinsic? name)
			    (memq name optimizable-rest-argument-operators) )
		       (for-each
			(lambda (arg)
			  (and-let* ([(eq? '##core#variable (node-class arg))]
				     [var (first (node-parameters arg))] )
			    (when (get db var 'rest-parameter) (count! db var 'o-r/access-count)) ) )
			(cdr subs) ) ) ) )
	     (walk (first subs) env localenv here #t)
	     (walkeach (cdr subs) env localenv here #f) ) )

	  ((let ##core#let)
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

	  ((lambda)
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
		  ;; decorate ##core#call node with size
		  (set-car! (cdddr (node-parameters n)) (- current-program-size size0)) ) ) ) ) )
	  
	  ((set! ##core#set!) 
	   (let* ([var (first params)]
		  [val (car subs)] )
	     (when first-analysis 
	       (case (variable-mark var '##compiler#intrinsic)
		 ((standard)
		  (compiler-warning 'redef "redefinition of standard binding `~S'" var) )
		 ((extended)
		  (compiler-warning 'redef "redefinition of extended binding `~S'" var) ) )
	       (put! db var 'potential-value val) )
	     (when (and (not (memq var localenv)) 
			(not (memq var env)) )
	       (grow 1)
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
	    ((or (memq var env)
		 (variable-mark var '##compiler#constant)
		 (not (variable-visible? var)))
	     (let ((props (get-all db var 'unknown 'value))
		   (home (get db var 'home)) )
	       (unless (assq 'unknown props)
		 (if (assq 'value props)
		     (put! db var 'unknown #t)
		     (if (or (not home) (eq? here home))
			 (put! db var 'value val)
			 (put! db var 'unknown #t) ) ) ) ) )
	    ((and (or local-definitions
		      (variable-mark var '##compiler#local))
		  (not (get db var 'unknown)))
	     (let ((home (get db var 'home)))
	       (if (or (not home) (eq? here home))
		   (put! db var 'local-value val)	       
		   (put! db var 'unknown #t))))
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
	     [local-value #f]
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
	      [(local-value) (set! local-value (cdr prop))]
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
	 (when (and first-analysis 
		    global
		    (null? references)
		    (not (variable-mark sym '##compiler#unused)))
	   (when assigned-locally
	     (compiler-warning 'var "local assignment to unused variable `~S' may be unintended" sym) )
	   (when (and (not (variable-visible? sym))
		      (not (variable-mark sym '##compiler#constant)) )
	     (compiler-warning 'var "global variable `~S' is never used" sym) ) )

 	 ;; Make 'boxed, if 'assigned & 'captured:
	 (when (and assigned captured)
	   (quick-put! plist 'boxed #t) )

	 ;; Make 'contractable, if it has a procedure as known value, has only one use and one call-site and
	 ;;  if the lambda has no free non-global variables or is an internal lambda. Make 'inlinable if
	 ;;  use/call count is not 1:
	 (cond (value
		(let ((valparams (node-parameters value)))
		  (when (and (eq? '##core#lambda (node-class value))
			     (or (not (second valparams))
				 (every 
				  (lambda (v) (get db v 'global))
				  (nth-value 0 (scan-free-variables value)) ) ) )
		    (if (and (= 1 nreferences) (= 1 ncall-sites))
			(quick-put! plist 'contractable #t)
			(quick-put! plist 'inlinable #t) ) ) ) )
	       (local-value
		;; Make 'inlinable, if it is declared local and has a value
		(let ((valparams (node-parameters local-value)))
		  (when (eq? '##core#lambda (node-class local-value))
		    (let-values (((vars hvars) (scan-free-variables local-value)))
		      (when (and (get db sym 'global)
				 (pair? hvars))
			(quick-put! plist 'hidden-refs #t))
		      (when (or (not (second valparams))
				(every 
				 (lambda (v) (get db v 'global)) 
				 vars))
			(quick-put! plist 'inlinable #t) ) ) ) ) )
	       ((variable-mark sym '##compiler#inline-global) =>
		(lambda (n)
		  (when (node? n)
		    (cond (assigned
			   (debugging
			    'i "global inlining candidate was assigned and will not be inlined"
			    sym)
			   (mark-variable sym '##compiler#inline-global 'no))
			  (else
			   (let ((lparams (node-parameters n)))
			     (put! db (first lparams) 'simple #t) ;XXX hack
			     (quick-put! plist 'inlinable #t)
			     (quick-put! plist 'local-value n))))))))

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
			      (or (not (variable-visible? name))
				  (not (get db name 'global))) ) )
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
					  (when (and name 
						     custom
						     (not (= (llist-length llist) (length (cdr subs)))))
					    (quit
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

(define-record-type lambda-literal
  (make-lambda-literal id external arguments argument-count rest-argument temporaries
		       callee-signatures allocated directly-called closure-size
		       looping customizable rest-argument-mode body direct)
  lambda-literal?
  (id lambda-literal-id)			       ; symbol
  (external lambda-literal-external)		       ; boolean
  (arguments lambda-literal-arguments)		       ; (symbol...)
  (argument-count lambda-literal-argument-count)       ; integer
  (rest-argument lambda-literal-rest-argument)	       ; symbol | #f
  (temporaries lambda-literal-temporaries)	       ; integer
  (callee-signatures lambda-literal-callee-signatures) ; (integer...)
  (allocated lambda-literal-allocated)		       ; integer
  (directly-called lambda-literal-directly-called)     ; boolean
  (closure-size lambda-literal-closure-size)	       ; integer
  (looping lambda-literal-looping)		       ; boolean
  (customizable lambda-literal-customizable)	       ; boolean
  (rest-argument-mode lambda-literal-rest-argument-mode) ; #f | LIST | VECTOR | UNUSED
  (body lambda-literal-body)				 ; expression
  (direct lambda-literal-direct))			 ; boolean
  
(define (prepare-for-code-generation node db)
  (let ([literals '()]
	[lambda-info-literals '()]
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
		       (variable-mark var '##compiler#always-bound)
		       (intrinsic? var))]
	     [blockvar (and (get db var 'assigned)
			    (not (variable-visible? var)))])
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
					  (variable-mark var '##compiler#always-bound)
					  (intrinsic? var)))]
			   [blockvar (not (variable-visible? var))]
			   [immf (or (and (eq? cval 'quote) (immediate? (first (node-parameters val))))
				     (eq? '##core#undefined cval) ) ] )
		      (when blockvar (set! fastsets (add1 fastsets)))
		      (make-node
		       (if immf '##core#setglobal_i '##core#setglobal)
		       (list (if blockvar
				 (blockvar-literal var)
				 (literal var) )
			     blockvar
			     var)
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
	     (cond ((and (fixnum? c) (not (big-fixnum? c)))
		    (immediate-literal c) )
		   ((number? c)
		    (cond ((eq? 'fixnum number-type)
			   (cond ((and (integer? c) (not (big-fixnum? c)))
				  (compiler-warning 
				   'type 
				   "coerced inexact literal number `~S' to fixnum ~S" c (inexact->exact c))
				  (immediate-literal (inexact->exact c)) )
				 (else (quit "cannot coerce inexact literal `~S' to fixnum" c)) ) )
			  (else (make-node '##core#literal (list (literal c)) '())) ) )
		   ((immediate? c) (immediate-literal c))
		   (else (make-node '##core#literal (list (literal c)) '())) ) ) )

	  ((if ##core#cond)
	   (let* ((test (walk (first subs) e here boxes))
		  (a0 allocated)
		  (x1 (walk (second subs) e here boxes))
		  (a1 allocated)
		  (x2 (walk (third subs) e here boxes)))
	     (set! allocated (+ a0 (max (- allocated a1) (- a1 a0))))
	     (make-node class params (list test x1 x2))))

	  ((##core#switch)
	   (let* ((exp (walk (first subs) e here boxes))
		  (a0 allocated))
	     (make-node
	      class
	      params
	      (cons 
	       exp
	       (let loop ((j (first params)) (subs (cdr subs)) (ma 0))
		 (set! allocated a0)
		 (if (zero? j)
		     (let ((def (walk (car subs) e here boxes)))
		       (set! allocated (+ a0 (max ma (- allocated a0))))
		       (list def))
		     (let* ((const (walk (car subs) e here boxes))
			    (body (walk (cadr subs) e here boxes)))
		       (cons* 
			const body
			(loop (sub1 j) (cddr subs) (max (- allocated a0) ma))))))))))

	  (else (make-node class params (mapwalk subs e here boxes)) ) ) ) )
    
    (define (mapwalk xs e here boxes)
      (map (lambda (x) (walk x e here boxes)) xs) )

    (define (literal x)
      (cond [(immediate? x) (immediate-literal x)]
	    [(number? x)
	     (or (and (inexact? x) 
		      (list-index (lambda (y) (and (number? y) (inexact? y) (= x y)))
				  literals) )
		 (new-literal x)) ]
	    ((##core#inline "C_lambdainfop" x)
	     (let ((i (length lambda-info-literals)))
	       (set! lambda-info-literals 
		 (append lambda-info-literals (list x))) ;*** see below
	       (vector i) ) )
            [(posq x literals) => identity]
	    [else (new-literal x)] ) )

    (define (new-literal x)
      (let ([i (length literals)])
	(set! literals (append literals (list x))) ;*** could (should) be optimized
	i) )

    (define (blockvar-literal var)
      (or (list-index
	   (lambda (lit) 
	     (and (block-variable-literal? lit)
		  (eq? var (block-variable-literal-name lit)) ) )
	   literals)
	  (new-literal (make-block-variable-literal var)) ) )
    
    (define (immediate-literal x)
      (if (eq? (void) x)
	  (make-node '##core#undefined '() '())
	  (make-node '##core#immediate
		     (cond ((fixnum? x) `(fix ,x))
			   ((boolean? x) `(bool ,x))
			   ((char? x) `(char ,x))
			   ((null? x) '(nil))
			   ((eof-object? x) '(eof))
			   (else (bomb "bad immediate (prepare)")) )
		     '() ) ) )
    
    (debugging 'p "preparation phase...")
    (let ((node2 (walk node '() #f '())))
      (debugging 'o "fast box initializations" fastinits)
      (debugging 'o "fast global references" fastrefs)
      (debugging 'o "fast global assignments" fastsets)
      (values node2 literals lambda-info-literals lambdas) ) ) )
