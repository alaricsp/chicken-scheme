;;;; expand.scm
;
; Copyright (c) 2008, The Chicken Team
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
  (unit expand)
  (disable-interrupts)
  (fixnum)
  (hide match-expression
	macro-alias
	d
	lookup) )


(set! ##sys#features
  (append '(#:hygienic-macros #:syntax-rules) ##sys#features))

(define (d arg1 . more)
  (if (null? more)
      (pp arg1)
      (apply print arg1 more)))

(define-macro (d . _) '(void))


;;; Syntactic environments

(define ##sys#current-environment (make-parameter '()))
(define ##sys#current-meta-environment (make-parameter '()))

(define (lookup id se)
  (cond ((assq id se) => cdr)
	((##sys#get id '##sys#macro-alias))
	(else #f)))

(define (macro-alias var se)
  (let* ((alias (gensym var))
	 (ua (or (lookup var se) var)))
    (##sys#put! alias '##sys#macro-alias ua)
    alias) )

(define (##sys#strip-syntax exp #!optional se)
  ;; if se is given, retain bound vars
  (let walk ((x exp))
    (cond ((symbol? x)
	   (let ((x2 (if se 
			 (lookup x se)
			 (get x '##sys#macro-alias) ) ) )
	     (cond ((not x2) x)
		   ((pair? x2) x)
		   (else x2))))
	  ((pair? x)
	   (cons (walk (##sys#slot x 0))
		 (walk (##sys#slot x 1))))
	  ((vector? x)
	   (list->vector (map walk (vector->list x))))
	  (else x))))

(define (##sys#alias-global-hook s) s)

(define (##sys#rename-global id se)
  (cond ((or (##sys#qualified-symbol? id) 
	     (let ((s (##sys#slot id 1)))
	       (and (fx> (##sys#size s) 0)
		    (char=? #\ (string-ref s 0)))))
	 id)
	((##sys#current-module) =>
	 (lambda (m)
	   (let ((id2 (##sys#string->symbol 
		       (string-append
			(##sys#slot (car m) 1)
			"$$"
			(##sys#slot id 1)))))
	     id2) ) )
	(else (##sys#alias-global-hook id) ) ) )


;;; Macro handling

(define ##sys#macro-environment '())

(define (##sys#extend-macro-environment name se handler)
  (cond ((##sys#current-module) =>
	 (lambda (m)
	   ;; include macro in it's own static se (circular)
	   (let* ((n (list name #f handler))
		  (se2 (cons n se)))
	     (set-car! (cdr n) se2)
	     ;; fixup exports
	     (cond ((assq name (cadr m)) =>
		    (cut set-cdr! <> (list name se2 handler))))
	     (##sys#current-environment
	      (cons (list name se2) (##sys#current-environment))))))
	((lookup name ##sys#macro-environment) =>
	 (lambda (a)
	   (set-car! a se)
	   (set-car! (cdr a) handler) ) )
	(else 
	 (set! ##sys#macro-environment
	   (cons (list name se handler)
		 ##sys#macro-environment)))))

(define (##sys#copy-macro old new)
  (let ((def (lookup old ##sys#macro-environment)))
    (apply ##sys#extend-macro-environment new def) ) )

(define (macro? sym #!optional (senv (##sys#current-environment)))
  (##sys#check-symbol sym 'macro?)
  (##sys#check-list senv 'macro?)
  (or (lookup sym senv)
      (and (lookup sym ##sys#macro-environment) #t) ) )

(define (##sys#unregister-macro name)
  (set! ##sys#macro-environment
    ;; this builds up stack, but isn't used often anyway...
    (let loop ((me ##sys#macro-environment) (me2 '()))
      (cond ((null? me) '())
	    ((eq? x (caar me)) (cdr me))
	    (else (cons (car me) (loop (cdr me))))))))

(define (undefine-macro! name)
  (##sys#check-symbol name 'undefine-macro!)
  (##sys#unregister-macro name) )


;; The basic macro-expander

(define ##sys#macroexpand-0
  (let ([string-append string-append])
    (lambda (exp dse)
      (define (call-handler name handler exp se)
	(d "invoking macro: " name)
	(handle-exceptions ex
	    (##sys#abort
	     (if (and (##sys#structure? ex 'condition)
		      (memv 'exn (##sys#slot ex 1)) )
		 (##sys#make-structure
		  'condition
		  (##sys#slot ex 1)
		  (let copy ([ps (##sys#slot ex 2)])
		    (if (null? ps)
			'()
			(let ([p (car ps)]
			      [r (cdr ps)])
			  (if (and (equal? '(exn . message) p)
				   (pair? r)
				   (string? (car r)) )
			      (cons 
			       '(exn . message)
			       (cons (string-append
				      "during expansion of ("
				      (##sys#slot name 1) 
				      " ...) - "
				      (car r) )
				     (cdr r) ) )
			      (copy r) ) ) ) ) )
		 ex) )
	  (handler exp se dse)))
      (define (expand head exp mdef)
	(d `(EXPAND: ,head ,exp ,(map car dse)))
	(cond ((not (list? exp))
	       (##sys#syntax-error-hook "invalid syntax in macro form" exp) )
	      ((pair? mdef)
	       (values 
		;; if stored se is #f, then this is a lisp macro,
		;; force ref. opaqueness by passing dynamic se
		(call-handler head (cadr mdef) exp (or (car mdef) dse))
		#t))
	      (else (values exp #f)) ) )
      (if (pair? exp)
	  (let ((head (##sys#slot exp 0))
		(body (##sys#slot exp 1)) )
	    (if (symbol? head)
		(let ((head2 (or (lookup head dse) head)))
		  (unless (pair? head2)
		    (set! head2 (or (lookup head2 ##sys#macro-environment) head2)) )
		  (cond [(eq? head2 'let)
			 (##sys#check-syntax 'let body '#(_ 2) #f dse)
			 (let ([bindings (car body)])
			   (cond [(symbol? bindings)
				  (##sys#check-syntax 'let body '(_ #((variable _) 0) . #(_ 1)) #f dse)
				  (let ([bs (cadr body)])
				    (values
				     `(##core#app
				       (,(macro-alias 'letrec dse)
					([,bindings (##core#loop-lambda ,(map (lambda (b) (car b)) bs) ,@(cddr body))])
					,bindings)
				       ,@(##sys#map cadr bs) )
				     #t) ) ]
				 [else (values exp #f)] ) ) ]
			[(and (memq head2 '(set! ##core#set!))
			      (pair? body)
			      (pair? (car body)) )
			 (let ([dest (car body)])
			   (##sys#check-syntax 'set! body '(#(_ 1) _) #f dse)
			   (values
			    (append (list (list '##sys#setter (car dest)))
				    (cdr dest)
				    (cdr body) ) 
			    #t) ) ]
			[else (expand head exp head2)] ) )
		(values exp #f) ) )
	  (values exp #f) ) ) ) )


;;; These are needed to hook other module/macro systems into the evaluator and compiler

(define (##sys#compiler-toplevel-macroexpand-hook exp) exp)
(define (##sys#interpreter-toplevel-macroexpand-hook exp) exp)
(define ##sys#enable-runtime-macros #f)


;;; User-level macroexpansion

(define (##sys#expand exp #!optional (me (##sys#current-environment)))
  (let loop ((exp exp))
    (let-values (((exp2 m) (##sys#macroexpand-0 exp me)))
      (if m
	  (loop exp2)
	  exp2) ) ) )

(define macroexpand ##sys#expand)

(define (macroexpand-1 exp #!optional (me (##sys#current-environment)))
  (##sys#macroexpand-0 exp me) )


;;; Extended (DSSSL-style) lambda lists
;
; Assumptions:
;
; 1) #!rest must come before #!key
; 2) default values may refer to earlier variables
; 3) optional/key args may be either variable or (variable default)
; 4) an argument marker may not be specified more than once
; 5) no special handling of extra keywords (no error)
; 6) default value of optional/key args is #f
; 7) mixing with dotted list syntax is allowed

(define (##sys#extended-lambda-list? llist)
  (let loop ([llist llist])
    (and (pair? llist)
	 (case (##sys#slot llist 0)
	   [(#!rest #!optional #!key) #t]
	   [else (loop (##sys#slot llist 1))] ) ) ) )

(define ##sys#expand-extended-lambda-list
  (let ([reverse reverse]
	[gensym gensym] )
    (lambda (llist0 body errh se)
      (define (err msg) (errh msg llist0))
      (define (->keyword s) (string->keyword (##sys#slot s 1)))
      (let ([rvar #f]
	    [hasrest #f] 
	    (%let* (macro-alias 'let* se))
	    (%lambda (macro-alias 'lambda se))
	    (%opt (macro-alias 'optional se))
	    (%let-optionals (macro-alias 'let-optionals se))
	    (%let-optionals* (macro-alias 'let-optionals* se))
	    (%let (macro-alias 'let se)))
	(let loop ([mode 0]		; req, opt, rest, key, end
		   [req '()]
		   [opt '()]
		   [key '()] 
		   [llist llist0] )
	  (cond [(null? llist)
		 (values 
		  (if rvar (##sys#append (reverse req) rvar) (reverse req))
		  (let ([body 
			 (if (null? key)
			     body
			     `((,%let*
				,(map (lambda (k)
					(let ([s (car k)])
					  `[,s (##sys#get-keyword 
						',(->keyword s) ,rvar
						,@(if (pair? (cdr k)) 
						      `(,%lambda () ,@(cdr k)))
						'() ) ] ) )
				      (reverse key) )
				,@body) ) ) ] )
		    (cond [(null? opt) body]
			  [(and (not hasrest) (null? key) (null? (cdr opt)))
			   `((,%let
			      ([,(caar opt) (,%opt ,rvar ,(cadar opt))])
			      ,@body) ) ]
			  [(and (not hasrest) (null? key))
			   `((,%let-optionals
			      ,rvar ,(reverse opt) ,@body))]
			  [else 
			   `((,%let-optionals*
			      ,rvar ,(##sys#append (reverse opt) (list (or hasrest rvar))) 
			      ,@body))] ) ) ) ]
		[(symbol? llist) 
		 (if (fx> mode 2)
		     (err "rest argument list specified more than once")
		     (begin
		       (if (not rvar) (set! rvar llist))
		       (set! hasrest llist)
		       (loop 4 req opt '() '()) ) ) ]
		[(not (pair? llist))
		 (err "invalid lambda list syntax") ]
		[else
		 (let* ((var (car llist))
			(x (or (and (symbol? var) (lookup var se)) var))
			(r (##sys#slot llist 1)))
		   (case x
		     [(#!optional)
		      (if (not rvar) (set! rvar (macro-alias 'tmp se)))
		      (if (eq? mode 0)
			  (loop 1 req '() '() r)
			  (err "`#!optional' argument marker in wrong context") ) ]
		     [(#!rest)
		      (if (fx<= mode 1)
			  (if (and (pair? r) (symbol? (##sys#slot r 0)))
			      (begin
				(if (not rvar) (set! rvar (##sys#slot r 0)))
				(set! hasrest (##sys#slot r 0))
				(loop 2 req opt '() (##sys#slot r 1)) )
			      (err "invalid syntax of `#!rest' argument") ) 
			  (err "`#!rest' argument marker in wrong context") ) ]
		     [(#!key)
		      (if (not rvar) (set! rvar (macro-alias 'tmp se)))
		      (if (fx<= mode 3)
			  (loop 3 req opt '() r)
			  (err "`#!key' argument marker in wrong context") ) ]
		     [else
		      (cond [(symbol? x)
			     (case mode
			       [(0) (loop 0 (cons x req) '() '() r)]
			       [(1) (loop 1 req (cons (list x #f) opt) '() r)]
			       [(2) (err "invalid lambda list syntax after `#!rest' marker")]
			       [else (loop 3 req opt (cons (list x) key) r)] ) ]
			    [(and (list? x) (eq? 2 (length x)))
			     (case mode
			       [(0) (err "invalid required argument syntax")]
			       [(1) (loop 1 req (cons x opt) '() r)]
			       [(2) (err "invalid lambda list syntax after `#!rest' marker")]
			       [else (loop 3 req opt (cons x key) r)] ) ]
			    [else (err "invalid lambda list syntax")] ) ] ) ) ] ) ) ) ) ) )


;;; Expansion of bodies (and internal definitions)

(define ##sys#canonicalize-body
  (let ([reverse reverse]
	[map map] )
    (lambda (body #!optional (se (##sys#current-environment)))
      (define (fini vars vals mvars mvals body)
	(if (and (null? vars) (null? mvars))
	    (let loop ([body2 body] [exps '()])
	      (if (not (pair? body2)) 
		  (cons 
		   (macro-alias 'begin se)
		   body) ; no more defines, otherwise we would have called `expand'
		  (let ([x (car body2)])
		    (if (and (pair? x) 
			     (let ((d (car x)))
			       (and (symbol? d)
				    (or (eq? (or (lookup d se) d) 'define)
					(eq? (or (lookup d se) d) 'define-values)))) )
			(cons
			 (macro-alias 'begin se)
			 (##sys#append (reverse exps) (list (expand body2))))
			(loop (cdr body2) (cons x exps)) ) ) ) )
	    (let ([vars (reverse vars)]
		  (lam (macro-alias 'lambda se)))
	      `(,(macro-alias 'let se)
		,(##sys#map (lambda (v) (##sys#list v (##sys#list '##core#undefined))) 
			    (apply ##sys#append vars mvars) )
		,@(map (lambda (v x) `(##core#set! ,v ,x)) vars (reverse vals))
		,@(map (lambda (vs x)
			 (let ([tmps (##sys#map gensym vs)])
			   `(##sys#call-with-values
			     (,lam () ,x)
			     (,lam ,tmps 
				   ,@(map (lambda (v t) `(##core#set! ,v ,t)) vs tmps) ) ) ) ) 
		       (reverse mvars)
		       (reverse mvals) )
		,@body) ) ) )
      (define (fini/syntax vars vals mvars mvals body)
	(fini
	 vars vals mvars mvals
	 (let loop ((body body) (defs '()) (done #f))
	   (cond (done `(,(macro-alias 'letrec-syntax se)
			 ,(map cdr (reverse defs)) ,@body) )
		 ((not (pair? body)) (loop body defs #t))
		 ((and (list? (car body))
		       (= 3 (length (car body))) 
		       (symbol? (caar body))
		       (eq? 'define-syntax (or (lookup (caar body) se) (caar body))))
		  (loop (cdr body) (cons (car body) defs) #f))
		 (else (loop body defs #t))))))		       
      (define (expand body)
	(let loop ([body body] [vars '()] [vals '()] [mvars '()] [mvals '()])
	  (if (not (pair? body))
	      (fini vars vals mvars mvals body)
	      (let* ((x (car body))
		     (rest (cdr body))
		     (exp1 (and (pair? x) (car x)))
		     (head (and exp1
				(symbol? exp1)
				(or (lookup exp1 se) exp1))))
		(cond [(not (symbol? head)) (fini vars vals mvars mvals body)]
		      [(eq? 'define head)
		       (##sys#check-syntax 'define x '(define _ . #(_ 0)) #f se)
		       (let loop2 ([x x])
			 (let ([head (cadr x)])
			   (cond [(not (pair? head))
				  (##sys#check-syntax 'define x '(define variable . #(_ 0)) #f se)
				  (loop rest (cons head vars)
					(cons (if (pair? (cddr x))
						  (caddr x)
						  '(##sys#void) )
					      vals)
					mvars mvals) ]
				 [(pair? (##sys#slot head 0))
				  (##sys#check-syntax 'define x '(define (_ . lambda-list) . #(_ 1)) #f se)
				  (loop2 (cons (macro-alias 'define se)
					       (##sys#expand-curried-define head (cddr x) se))) ]
				 [else
				  (##sys#check-syntax 'define x '(define (variable . lambda-list) . #(_ 1)) #f se)
				  (loop rest
					(cons (##sys#slot head 0) vars)
					(cons `(,(macro-alias 'lambda se) ,(##sys#slot head 1) ,@(cddr x)) vals)
					mvars mvals) ] ) ) ) ]
		      ((eq? 'define-syntax head)
		       (##sys#check-syntax 'define-syntax x '(define-syntax variable _) se)
		       (fini/syntax vars vals mvars mvals body) )
		      [(eq? 'define-values head)
		       (##sys#check-syntax 'define-values x '(define-values #(_ 0) _) #f se)
		       (loop rest vars vals (cons (cadr x) mvars) (cons (caddr x) mvals)) ]
		      [(eq? 'begin head)
		       (##sys#check-syntax 'begin x '(begin . #(_ 0)) #f se)
		       (loop (##sys#append (cdr x) rest) vars vals mvars mvals) ]
		      [else
		       (let ([x2 (##sys#macroexpand-0 x se)])
			 (if (eq? x x2)
			     (fini vars vals mvars mvals body)
			     (loop (cons x2 rest) vars vals mvars mvals) ) ) ] ) ) ) ) )
      (expand body) ) ) )


;;; A simple expression matcher

(define match-expression
  (lambda (exp pat vars)
    (let ((env '()))
      (define (mwalk x p)
	(cond ((not (pair? p))
	       (cond ((assq p env) => (lambda (a) (equal? x (##sys#slot a 1))))
		     ((memq p vars)
		      (set! env (cons (cons p x) env))
		      #t)
		     (else (eq? x p)) ) )
	      ((pair? x)
	       (and (mwalk (##sys#slot x 0) (##sys#slot p 0))
		    (mwalk (##sys#slot x 1) (##sys#slot p 1)) ) )
	      (else #f) ) )
      (and (mwalk exp pat) env) ) ) )


;;; Expand "curried" lambda-list syntax for `define'

(define (##sys#expand-curried-define head body se)
  (let* ([name #f]
	 (lam (macro-alias 'lambda se)))
    (define (loop head body)
      (if (symbol? (car head))
	  (begin
	    (set! name (car head))
	    `(,lam ,(cdr head) ,@body) )
	  (loop (car head) `((,lam ,(cdr head) ,@body)) ) ))
    (let ([exp (loop head body)])
      (list name exp) ) ) )


;;; General syntax checking routine:

(define ##sys#line-number-database #f)
(define ##sys#syntax-error-culprit #f)

(define (##sys#syntax-error-hook . args)
  (apply ##sys#signal-hook #:syntax-error
	 (##sys#strip-syntax args)))

(define syntax-error ##sys#syntax-error-hook)

(define (get-line-number sexp)
  (and ##sys#line-number-database
       (pair? sexp)
       (let ([head (car sexp)])
	 (and (symbol? head)
	      (cond [(##sys#hash-table-ref ##sys#line-number-database head)
		     => (lambda (pl)
			  (let ([a (assq sexp pl)])
			    (and a (cdr a)) ) ) ]
		    [else #f] ) ) ) ) )

(define ##sys#check-syntax
  (let ([string-append string-append]
	[keyword? keyword?]
	[get-line-number get-line-number]
	[symbol->string symbol->string] )
    (lambda (id exp pat #!optional culprit (se (##sys#current-environment)))

      (define (test x pred msg)
	(unless (pred x) (err msg)) )

      (define (err msg)
	(let* ([sexp ##sys#syntax-error-culprit]
	       [ln (get-line-number sexp)] )
	  (##sys#syntax-error-hook
	   (if ln 
	       (string-append "(" (symbol->string id) ") in line " (number->string ln) " - " msg)
	       (string-append "(" (symbol->string id) ") " msg) )
	   exp) ) )

      (define (lambda-list? x)
	(or (##sys#extended-lambda-list? x)
	    (let loop ((x x))
	      (cond ((null? x))
		    ((symbol? x) (not (keyword? x)))
		    ((pair? x)
		     (let ((s (##sys#slot x 0)))
		       (and (symbol? s)
			    (loop (##sys#slot x 1)) ) ) )
		    (else #f) ) ) ) )

      (define (proper-list? x)
	(let loop ((x x))
	  (cond ((eq? x '()))
		((pair? x) (loop (##sys#slot x 1)))
		(else #f) ) ) )

      (when culprit (set! ##sys#syntax-error-culprit culprit))
      (let walk ((x exp) (p pat))
	(cond ((vector? p)
	       (let* ((p2 (##sys#slot p 0))
		      (vlen (##sys#size p))
		      (min (if (fx> vlen 1) 
			       (##sys#slot p 1)
			       0) )
		      (max (cond ((eq? vlen 1) 1)
				 ((fx> vlen 2) (##sys#slot p 2))
				 (else 99999) ) ) )
		 (do ((x x (##sys#slot x 1))
		      (n 0 (fx+ n 1)) )
		     ((eq? x '())
		      (if (fx< n min)
			  (err "not enough arguments") ) )
		   (cond ((fx>= n max) 
			  (err "too many arguments") )
			 ((or (not (##core#inline "C_blockp" x)) (not (##core#inline "C_pairp" x)))
			  (err "not a proper list") )
			 (else (walk (##sys#slot x 0) p2) ) ) ) ) )
	      ((##sys#immediate? p)
	       (if (not (eq? p x)) (err "unexpected object")) )
	      ((symbol? p)
	       (case p
		 ((_) #t)
		 ((pair) (test x pair? "pair expected"))
		 ((variable) (test x symbol? "identifier expected"))
		 ((symbol) (test x symbol? "symbol expected"))
		 ((list) (test x proper-list? "proper list expected"))
		 ((number) (test x number? "number expected"))
		 ((string) (test x string? "string expected"))
		 ((lambda-list) (test x lambda-list? "lambda-list expected"))
		 (else (test
			x
			(lambda (y)
			  (let ((y2 (lookup y se)))
			    (eq? (if (symbol? y2) y2 y) p)))
			"missing keyword")) ) )
	      ((not (pair? p))
	       (err "incomplete form") )
	      (else
	       (walk (##sys#slot x 0) (##sys#slot p 0))
	       (walk (##sys#slot x 1) (##sys#slot p 1)) ) ) ) ) ) )


;;; explicit-renaming transformer

(define ((##sys#er-transformer handler) form se dse)
  (let ((renv '()))			; keep rename-environment for this expansion
    (define (rename sym)
      (cond ((assq sym renv) => cdr)
	    ((lookup sym se) => 
	     (lambda (a)
	       (if (symbol? a)
		   a
		   sym) ) )
	    (else
	     (let ((a (macro-alias sym se)))
	       (set! renv (cons (cons sym a) renv))
	       a))))
    (define (compare s1 s2)
      (if (and (symbol? s1) (symbol? s2))
	  (eq? (or (##sys#get s1 '##sys#macro-alias)
		   (lookup s1 dse)
		   s1)
	       (or (##sys#get s2 '##sys#macro-alias)
		   (lookup s2 dse)
		   s2) )
	  (eq? s1 s2)))
    (handler form rename compare) ) )


;;; transformer for normal low-level macros

(define (##sys#lisp-transformer handler #!optional manyargs) 
  (if manyargs
      (lambda (form se dse) (handler (##sys#strip-syntax (cdr form) se)))
      (lambda (form se dse) (apply handler (##sys#strip-syntax (cdr form) se))) ) )


;;; lowlevel module support

(define ##sys#current-module (make-parameter #f))
(define ##sys#module-table '())

(define (##sys#register-module name module)
  (cond ((assq name ##sys#module-table) =>
	 (lambda (entry)
	   (set-cdr! entry (cdr module))
	   entry))
	(else
	 (set! ##sys#module-table (cons module ##sys#module-table))
	 module) ) )

(define (##sys#make-module name exports exprename imports)
  (let ((prefix (##sys#string-append (##sys#slot name 1) "$$")))
    (let loop ((se '()) (imports imports))
      (cond ((null? imports)
	     (let ((exps
		     (map (lambda (ex)
			    (cons (exprename ex)
				  (cond ((assq ex se) => cdr) ; already imported?
					(else
					 (##sys#string->symbol
					  (##sys#string-append prefix (##sys#slot ex 1)))))))
			  exports) ) )
	       (cons name (cons exps se))))
	    ((assq (caar imports) ##sys#module-table) =>
	     (lambda (m)
	       (let ((imprename (cdar imports)))
		 (loop (let filter ((exps (cadr m)))
			 (cond ((null? exps) se)
			       ((imprename (caar exps)) =>
				(lambda (r)
				  (cons 
				   (cons r (cdar exps) )
				   (filter (cdr exps)))))
			       (else (filter (cdr exps)))))
		       (cdr imports)))))
	    (else
	     (syntax-error "importing unknown module" (car imports) name))))))


;;; Macro definitions:

(##sys#extend-macro-environment
 'define
 '()
 (##sys#er-transformer
  (lambda (form r c)
   (let loop ((form (cdr form)))
     (let ((head (car form))
	   (body (cdr form)) )
       (cond ((not (pair? head))
	      (##sys#check-syntax 'define head 'symbol)
	      (##sys#check-syntax 'define body '#(_ 0 1))
	      `(##core#set! ,head ,(if (pair? body) (car body) '(##sys#void))) )
	     ((pair? (##sys#slot head 0))
	      (##sys#check-syntax 'define head '(_ . lambda-list))
	      (##sys#check-syntax 'define body '#(_ 1))
	      (loop (##sys#expand-curried-define head body '())) ) ;*** '() should be se
	     (else
	      (##sys#check-syntax 'define head '(symbol . lambda-list))
	      (##sys#check-syntax 'define body '#(_ 1))
	      `(##core#set!
		,(car head)
		(,(r 'lambda) ,(cdr head) ,@body))) ) ) ) ) ) )

(##sys#extend-macro-environment
 'and
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (let ((body (cdr form)))
      (if (null? body)
	  #t
	  (let ((rbody (cdr body))
		(hbody (car body)) )
	    (if (null? rbody)
		hbody
		`(,(r 'if) ,hbody (,(r 'and) ,@rbody) #f) ) ) ) ) ) ) )

(##sys#extend-macro-environment
 'or 
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (let ((body (cdr form)))
     (if (null? body)
	 #f
	 (let ((rbody (cdr body))
	       (hbody (car body)))
	   (if (null? rbody)
	       hbody
	       (let ((tmp (r 'tmp)))
		 `(,(r 'let) ((,tmp ,hbody))
		    (,(r 'if) ,tmp ,tmp (,(r 'or) ,@rbody)) ) ) ) ) ) ) ) ) )

(##sys#extend-macro-environment
 'cond
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (let ((body (cdr form))
	  (%begin (r 'begin))
	  (%let (r 'let))
	  (%if (r 'if))
	  (%=> (r '=>))
	  (%or (r 'or))
	  (%else (r 'else))
	  (%lambda (r 'lambda)))
      (let expand ((clauses body))
	(if (not (pair? clauses))
	    '(##core#undefined)
	    (let ((clause (car clauses))
		  (rclauses (cdr clauses)) )
	      (##sys#check-syntax 'cond clause '#(_ 1))
	      (cond ((c %else (car clause)) `(,%begin ,@(cdr clause)))
		    ((null? (cdr clause)) `(,%or ,(car clause) ,(expand rclauses)))
		    ((c %=> (cadr clause))
		     (let ((tmp (r 'tmp)))
		       `(,%let ((,tmp ,(car clause)))
			       (,%if ,tmp
				     (,(caddr clause) ,tmp)
				     ,(expand rclauses) ) ) ) )
		    ((and (list? clause) (fx= (length clause) 4)
			  (c %=> (caddr clause)))
		     (let ((tmp (r 'tmp)))
		       `(##sys#call-with-values
			 (,%lambda () ,(car clause))
			 (,%lambda ,tmp
				   (if (##sys#apply ,(cadr clause) ,tmp)
				       (##sys#apply ,(cadddr clause) ,tmp)
				       ,(expand rclauses) ) ) ) ) )
		    (else `(,%if ,(car clause) 
				 (,%begin ,@(cdr clause))
				 ,(expand rclauses) ) ) ) ) ) ) ) ) ))

(##sys#extend-macro-environment
 'case
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'case form '(_ _ . #(_ 0)))
    (let ((exp (cadr form))
	  (body (cddr form)) )
      (let ((tmp (r 'tmp))
	    (%begin (r 'begin))
	    (%if (r 'if))
	    (%or (r 'or))
	    (%eqv? (r 'eqv?))
	    (%else (r 'else)))
	`(let ((,tmp ,exp))
	   ,(let expand ((clauses body))
	      (if (not (pair? clauses))
		  '(##core#undefined)
		  (let ((clause (car clauses))
			(rclauses (cdr clauses)) )
		    (##sys#check-syntax 'case clause '#(_ 1))
		    (if (c %else (car clause))
			`(,%begin ,@(cdr clause))
			`(,%if (,%or ,@(##sys#map
					(lambda (x) `(,%eqv? ,tmp ',x)) (car clause)))
			       (,%begin ,@(cdr clause)) 
			       ,(expand rclauses) ) ) ) ) ) ) ) ) ) ) )

(##sys#extend-macro-environment
 'let*
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'let* form '(_ #((symbol _) 0) . #(_ 1)))
    (let ((bindings (cadr form))
	  (body (cddr form)) 
	  (%let (r 'let)))
      (let expand ((bs bindings))
	(if (eq? bs '())
	    `(,%let () ,@body)
	    `(,%let (,(car bs)) ,(expand (cdr bs))) ) ) ) ) ) )

(##sys#extend-macro-environment
 'letrec
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'letrec form '(_ #((symbol _) 0) . #(_ 1)))
    (let ((bindings (cadr form))
	  (body (cddr form)) 
	  (%let (r 'let)) )
      `(,%let ,(##sys#map (lambda (b) (list (car b) '(##core#undefined))) bindings)
	      ,@(##sys#map (lambda (b) `(##core#set! ,(car b) ,(cadr b))) bindings)
	      (,%let () ,@body) ) ) ) ) )

(##sys#extend-macro-environment
 'do
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'do form '(_ #((symbol _ . #(_)) 0) . #(_ 1)))
    (let ((bindings (cadr form))
	  (test (caddr form))
	  (body (cdddr form))
	  (dovar (r 'doloop))
	  (%let (r 'let))
	  (%if (r 'if))
	  (%begin (r 'begin)))
      `(,%let ,dovar ,(##sys#map (lambda (b) (list (car b) (car (cdr b)))) bindings)
	      (,%if ,(car test)
		    ,(let ((tbody (cdr test)))
		       (if (eq? tbody '())
			   '(##core#undefined)
			   `(,%begin ,@tbody) ) )
		    (,%begin
		     ,(if (eq? body '())
			  '(##core#undefined)
			  `(,%let () ,@body) )
		     (##core#app
		      ,dovar ,@(##sys#map (lambda (b) 
					    (if (eq? (cdr (cdr b)) '())
						(car b)
						(car (cdr (cdr b))) ) )
					  bindings) ) ) ) ) ) ) ) )

(##sys#extend-macro-environment
 'quasiquote
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (let ((%quote (r 'quote))
	  (%quasiquote (r 'quasiquote))
	  (%unquote (r 'unquote))
	  (%unquote-splicing (r 'unquote-splicing)))
      (define (walk x n) (simplify (walk1 x n)))
      (define (walk1 x n)
	(cond ((vector? x)
	       `(##sys#list->vector ,(walk (vector->list x) n)) )
	      ((not (pair? x)) `(,%quote ,x))
	      (else
	       (let ((head (car x))
		     (tail (cdr x)))
		 (cond ((c %unquote head)
			(if (pair? tail)
			    (let ((hx (car tail)))
			      (if (eq? n 0)
				  hx
				  (list '##sys#list `(,%quote ,%unquote)
					(walk hx (fx- n 1)) ) ) )
			    `(,%quote ,%unquote) ) )
		       ((c %quasiquote head)
			(if (pair? tail)
			    `(##sys#list (,%quote ,%quasiquote) 
					 ,(walk (car tail) (fx+ n 1)) ) 
			    (list '##sys#cons (list %quote %quasiquote) 
				  (walk tail n)) ) )
		       ((pair? head)
			(let ((hx (car head))
			      (tx (cdr head)))
			  (if (and (c hx %unquote-splicing) (pair? tx))
			      (let ((htx (car tx)))
				(if (eq? n 0)
				    `(##sys#append ,htx
						   ,(walk tail n) )
				    `(##sys#cons (##sys#list %unquote-splicing
							     ,(walk htx (fx- n 1)) )
						 ,(walk tail n) ) ) )
			      `(##sys#cons ,(walk head n) ,(walk tail n)) ) ) )
		       (else
			`(##sys#cons ,(walk head n) ,(walk tail n)) ) ) ) ) ) )
      (define (simplify x)
	(cond ((match-expression x '(##sys#cons a '()) '(a))
	       => (lambda (env) (simplify `(##sys#list ,(##sys#slot (assq 'a env) 1)))) )
	      ((match-expression x '(##sys#cons a (##sys#list . b)) '(a b))
	       => (lambda (env)
		    (let ([bxs (assq 'b env)])
		      (if (fx< (length bxs) 32)
			  (simplify `(##sys#list ,(##sys#slot (assq 'a env) 1)
						 ,@(##sys#slot bxs 1) ) ) 
			  x) ) ) )
	      ((match-expression x '(##sys#append a '()) '(a))
	       => (lambda (env) (##sys#slot (assq 'a env) 1)) )
	      (else x) ) )
      (##sys#check-syntax 'quasiquote form '(_ _))
      (walk (cadr form) 0) ) ) ) )

(##sys#extend-macro-environment
 'delay
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'delay form '(_ _))
    `(##sys#make-promise (lambda () ,(cadr form))))))

(##sys#extend-macro-environment
 'cond-expand
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (let ((clauses (cdr form))
	  (%or (r 'or))
	  (%not (r 'not))
	  (%else (r 'else))
	  (%begin (r 'begin))
	  (%and (r 'and)))
      (define (err x) 
	(##sys#error "syntax error in `cond-expand' form"
		     x
		     (cons 'cond-expand clauses)) )
      (define (test fx)
	(cond ((symbol? fx) (##sys#feature? fx))
	      ((not (pair? fx)) (err fx))
	      (else
	       (let ((head (car fx))
		     (rest (cdr fx)))
		 (cond ((c %and head)
			(or (eq? rest '())
			    (if (pair? rest)
				(and (test (car rest))
				     (test `(,%and ,@(cdr rest))) )
				(err fx) ) ) )
		       ((c %or head)
			(and (not (eq? rest '()))
			     (if (pair? rest)
				 (or (test (car rest))
				     (test `(,%or ,@(cdr rest))) )
				 (err fx) ) ) )
		       ((c %not head) (not (test (cadr fx))))
		       (else (err fx)) ) ) ) ) )
      (let expand ((cls clauses))
	(cond ((eq? cls '())
	       (##sys#apply
		##sys#error "no matching clause in `cond-expand' form" 
		(map (lambda (x) (car x)) clauses) ) )
	      ((not (pair? cls)) (err cls))
	      (else
	       (let ((clause (car cls))
		    (rclauses (cdr cls)) )
		 (if (not (pair? clause)) 
		     (err clause)
		     (let ((id (car clause)))
		       (cond ((c id %else)
			      (let ((rest (cdr clause)))
				(if (eq? rest '())
				    '(##core#undefined)
				    `(,%begin ,@rest) ) ) )
			     ((test id) `(,%begin ,@(cdr clause)))
			     (else (expand rclauses)) ) ) ) ) ) ) ) ) ) ) )

(##sys#extend-macro-environment		;*** keep this?
 'define-macro
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'define-macro form '(_ . #(_ 1)))
    (let ((head (cadr form))
	  (body (cddr form))
	  (%lambda (r 'lambda)))
      (define (expand name val)
	`(,(if ##sys#enable-runtime-macros
	       '##core#elaborationtimetoo 
	       '##core#elaborationtimeonly)
	  ,(if (symbol? val)
	       `(##sys#copy-macro ',val ',name)
	       `(##sys#extend-macro-environment
		 ',name
		 (##sys#current-environment)
		 (##sys#lisp-transformer ,val)))))
      (cond ((symbol? head)
	     (##sys#check-syntax 'define-macro body '(_))
	     (expand head (car body)) )
	    (else
	     (##sys#check-syntax 'define-macro head '(symbol . lambda-list))
	     (##sys#check-syntax 'define-macro body '#(_ 1)) 
	     (expand (car head) `(,%lambda ,(cdr head) ,@body))))))))

(##sys#extend-macro-environment
 'require-extension
 '()
 (##sys#er-transformer
  (lambda (x r c)
    (let ((ids (cdr x))
	  (%quote (r 'quote)))
      `(##core#require-extension 
	,@(map (lambda (x) (list %quote x)) ids) ) ) ) ))

(##sys#extend-macro-environment
 'define-syntax
 '()
 (lambda (form se dse)
   (##sys#check-syntax 'define-syntax form '(define-syntax variable _) #f se)
   `(,(if ##sys#enable-runtime-macros '##core#elaborationtimetoo '##core#elaborationtimeonly)
     (##sys#extend-macro-environment
      ',(cadr form)
      (##sys#current-environment)
      (##sys#er-transformer ,(caddr form))))))


;;;*** only for backwards compatibility (will break for high argument counts)

(define (##sys#register-macro name h)
  (##sys#extend-macro-environment
   name '() 
   (##sys#lisp-transformer h) ) )

(define (##sys#register-macro-2 name h2)
  (##sys#extend-macro-environment
   name '()
   (##sys#lisp-transformer
    (lambda body (h2 body)))))


;;; syntax-rules

(include "synrules.scm")
