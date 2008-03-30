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


;;; Macro handling

(define ##sys#macro-environment '())

(define (##sys#extend-macro-environment name se handler)
  (cond ((lookup name ##sys#macro-environment) =>
	 (lambda (a)
	   (set-car! a se)
	   (set-car! (cdr a) handler) ) )
	(else 
	 (set! ##sys#macro-environment
	   (cons (list name se handler) ##sys#macro-environment)))))

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
    (lambda (exp se)
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
	  (handler exp se)))
      (define (expand head exp mdef)
	(d `(EXPAND: ,head ,exp ,(map car se)))
	(cond ((not (list? exp))
	       (##sys#syntax-error-hook "invalid syntax in macro form" exp) )
	      ((pair? mdef)
	       (values 
		;; if se is #f, then this is a lisp macro - force ref. opaqueness by 
		;; passing dynamic se
		(call-handler head (cadr mdef) exp (or (car mdef) se))
		#t))
	      (else (values exp #f)) ) )
      (if (pair? exp)
	  (let ((head (##sys#slot exp 0))
		(body (##sys#slot exp 1)) )
	    (if (symbol? head)
		(let ((head2 (or (lookup head se) head)))
		  (unless (pair? head2)
		    (set! head2 (or (lookup head2 ##sys#macro-environment) head2)) )
		  (cond [(eq? head2 'let)
			 (##sys#check-syntax 'let body '#(_ 2) #f se)
			 (let ([bindings (car body)])
			   (cond [(symbol? bindings)
				  (##sys#check-syntax 'let body '(_ #((variable _) 0) . #(_ 1)) #f se)
				  (let ([bs (cadr body)])
				    (values
				     `(##core#app
				       (,(macro-alias 'letrec se)
					([,bindings (##core#loop-lambda ,(map (lambda (b) (car b)) bs) ,@(cddr body))])
					,bindings)
				       ,@(##sys#map cadr bs) )
				     #t) ) ]
				 [else (values exp #f)] ) ) ]
			[(and (memq head2 '(set! ##core#set!))
			      (pair? body)
			      (pair? (car body)) )
			 (let ([dest (car body)])
			   (##sys#check-syntax 'set! body '(#(_ 1) _) #f se)
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
	    [hasrest #f] )
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
			     `((,(macro-alias 'let* se)
				,(map (lambda (k)
					(let ([s (car k)])
					  `[,s (##sys#get-keyword 
						',(->keyword s) ,rvar
						,@(if (pair? (cdr k)) 
						      `((,(macro-alias 'lambda se)
							 () ,@(cdr k)))
						      '() ) ) ] ) )
				      (reverse key) )
				,@body) ) ) ] )
		    (cond [(null? opt) body]
			  [(and (not hasrest) (null? key) (null? (cdr opt)))
			   `((,(macro-alias 'let se)
			      ([,(caar opt) (,(macro-alias 'optional se)
					     ,rvar ,(cadar opt))])
			       ,@body) ) ]
			  [(and (not hasrest) (null? key))
			   `((,(macro-alias 'let-optionals se)
			      ,rvar ,(reverse opt) ,@body))]
			  [else 
			   `((,(macro-alias 'let-optionals* se)
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
		 (let ([x (or (lookup (car llist) se) (car llist))]
		       [r (##sys#slot llist 1)])
		   (case x
		     [(#!optional)
		      (if (not rvar) (set! rvar (gensym)))
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
		      (if (not rvar) (set! rvar (gensym)))
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
			       (or (eq? (or (lookup d se) d) 'define)
				   (eq? (or (lookup d se) d) 'define-values))))
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

(define ((##sys#er-transformer handler) form se)
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
      (eq? (or (lookup s1 se) s1)
	   (or (lookup s2 se) s2)))
    (handler form rename compare) ) )


;;; transformer for normal low-level macros

(define (##sys#lisp-transformer handler #!optional manyargs) 
  (if manyargs
      (lambda (form se) (handler (##sys#strip-syntax (cdr form) se)))
      (lambda (form se) (apply handler (##sys#strip-syntax (cdr form) se))) ) )


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

;;*** need to be done hygienically

(##sys#extend-macro-environment
 'and
 #f
 (##sys#lisp-transformer
  (lambda body
    (if (eq? body '())
	#t
	(let ((rbody (##sys#slot body 1))
	      (hbody (##sys#slot body 0)) )
	  (if (eq? rbody '())
	      hbody
	      `(if ,hbody (and ,@rbody) #f) ) ) ) ) ) )

(##sys#extend-macro-environment
 'or 
 #f
 (##sys#lisp-transformer
   (lambda body
     (if (eq? body '())
	 #f
	 (let ((rbody (##sys#slot body 1))
	       (hbody (##sys#slot body 0)) )
	   (if (eq? rbody '())
	       hbody
	       (let ((tmp (gensym)))
		 `(let ((,tmp ,hbody))
		    (if ,tmp ,tmp (or ,@rbody)) ) ) ) ) ) ) ) )

(##sys#extend-macro-environment
 'cond
 #f
 (##sys#lisp-transformer
  (lambda body
     (let expand ((clauses body))
       (if (not (pair? clauses))
	   '(##core#undefined)
	   (let ((clause (##sys#slot clauses 0))
		 (rclauses (##sys#slot clauses 1)) )
	     (##sys#check-syntax 'cond clause '#(_ 1))
	     (cond ((eq? 'else (car clause)) `(begin ,@(cdr clause)))
		   ((eq? (cdr clause) '()) `(or ,(car clause) ,(expand rclauses)))
		   ((eq? '=> (cadr clause))
		    (let ((tmp (gensym)))
		      `(let ((,tmp ,(car clause)))
			 (if ,tmp
			     (,(caddr clause) ,tmp)
			     ,(expand rclauses) ) ) ) )
		   ((and (list? clause) (fx= (length clause) 4) (eq? '=> (caddr clause)))
		    (let ((tmp (gensym)))
		      `(##sys#call-with-values
			(lambda () ,(car clause))
			(lambda ,tmp
			  (if (##sys#apply ,(cadr clause) ,tmp)
			      (##sys#apply ,(cadddr clause) ,tmp)
			      ,(expand rclauses) ) ) ) ) )
		   (else `(if ,(car clause) 
			      (begin ,@(cdr clause))
			      ,(expand rclauses) ) ) ) ) ) ) ) ) )

(##sys#extend-macro-environment
 'case
 #f
 (##sys#lisp-transformer
  (lambda form
     (let ((exp (car form))
	   (body (cdr form)) )
       (let ((tmp (gensym)))
	 `(let ((,tmp ,exp))
	    ,(let expand ((clauses body))
	       (if (not (pair? clauses))
		   '(##core#undefined)
		   (let ((clause (##sys#slot clauses 0))
			 (rclauses (##sys#slot clauses 1)) )
		     (##sys#check-syntax 'case clause '#(_ 1))
		     (if (eq? 'else (car clause))
			 `(begin ,@(cdr clause))
			 `(if (or ,@(##sys#map (lambda (x) `(eqv? ,tmp ',x)) (car clause)))
			      (begin ,@(cdr clause)) 
			      ,(expand rclauses) ) ) ) ) ) ) ) ) ) ) )

(##sys#extend-macro-environment
 'let*
 #f
 (##sys#lisp-transformer
  (lambda form
    (let ((bindings (car form))
	  (body (cdr form)) )
      (##sys#check-syntax 'let* bindings '#((symbol _) 0))
      (##sys#check-syntax 'let* body '#(_ 1))
      (let expand ((bs bindings))
	(if (eq? bs '())
	    `(let () ,@body)
	    `(let (,(car bs)) ,(expand (cdr bs))) ) ) ) ) ) )

(##sys#extend-macro-environment
 'letrec
 #f
 (##sys#lisp-transformer
  (lambda form
    (let ((bindings (car form))
	  (body (cdr form)) )
      (##sys#check-syntax 'letrec bindings '#((symbol _) 0))
      (##sys#check-syntax 'letrec body '#(_ 1))
      `(let ,(##sys#map (lambda (b) (list (car b) '(##core#undefined))) bindings)
	 ,@(##sys#map (lambda (b) `(##core#set! ,(car b) ,(cadr b))) bindings)
	 (let () ,@body) ) ) ) ) )

(##sys#extend-macro-environment
 'do
 #f
 (##sys#lisp-transformer
   (lambda (bindings test . body)
     (##sys#check-syntax 'do bindings '#((symbol _ . #(_)) 0))
     (##sys#check-syntax 'do test '#(_ 1))
     (let ((dovar (gensym "do")))
       `(let ,dovar ,(##sys#map (lambda (b) (list (car b) (car (cdr b)))) bindings)
	     (if ,(car test)
		 ,(let ((tbody (cdr test)))
		    (if (eq? tbody '())
			'(##core#undefined)
			`(begin ,@tbody) ) )
		 (begin
		   ,(if (eq? body '())
			'(##core#undefined)
			`(let () ,@body) )
		   (##core#app
		    ,dovar ,@(##sys#map (lambda (b) 
					  (if (eq? (cdr (cdr b)) '())
					      (car b)
					      (car (cdr (cdr b))) ) )
					bindings) ) ) ) ) ) ) ) )

(##sys#extend-macro-environment
 'quasiquote
 #f
 (##sys#lisp-transformer
  (lambda (form)
     (define (walk x n) (simplify (walk1 x n)))
     (define (walk1 x n)
       (if (##core#inline "C_blockp" x)
	   (cond ((##core#inline "C_vectorp" x)
		  `(##sys#list->vector ,(walk (vector->list x) n)) )
		 ((not (##core#inline "C_pairp" x)) `(quote ,x))
		 (else
		  (let ((head (##sys#slot x 0))
			(tail (##sys#slot x 1)) )
		    (case head
		      ((unquote)
		       (if (and (##core#inline "C_blockp" tail) (##core#inline "C_pairp" tail))
			   (let ((hx (##sys#slot tail 0)))
			     (if (eq? n 0)
				 hx
				 (list '##sys#list '(quote unquote)
				       (walk hx (fx- n 1)) ) ) )
			   '(quote unquote) ) )
		      ((quasiquote)
		       (if (and (##core#inline "C_blockp" tail) (##core#inline "C_pairp" tail))
			   `(##sys#list (quote quasiquote) 
				   ,(walk (##sys#slot tail 0) (fx+ n 1)) ) 
			   (list '##sys#cons (list 'quote 'quasiquote) (walk tail n)) ) )
		      (else
		       (if (and (##core#inline "C_blockp" head) (##core#inline "C_pairp" head))
			   (let ((hx (##sys#slot head 0))
				 (tx (##sys#slot head 1)) )
			     (if (and (eq? hx 'unquote-splicing)
				      (##core#inline "C_blockp" tx)
				      (##core#inline "C_pairp" tx) )
				 (let ((htx (##sys#slot tx 0)))
				   (if (eq? n 0)
				       `(##sys#append ,htx
						 ,(walk tail n) )
				       `(##sys#cons (##sys#list 'unquote-splicing
							,(walk htx (fx- n 1)) )
					       ,(walk tail n) ) ) )
				 `(##sys#cons ,(walk head n) ,(walk tail n)) ) )
			   `(##sys#cons ,(walk head n) ,(walk tail n)) ) ) ) ) ) )
	   `(quote ,x) ) )
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
     (walk form 0) ) ) )

(##sys#extend-macro-environment
 'delay
 #f
 (##sys#lisp-transformer
  (lambda (x) `(##sys#make-promise (lambda () ,x))) ) )

(##sys#extend-macro-environment
 'cond-expand
 #f
 (##sys#lisp-transformer
   (lambda clauses

     (define (err x) 
       (##sys#error "syntax error in `cond-expand' form" x (cons 'cond-expand clauses)) )

     (define (test fx)
       (cond ((symbol? fx) (##sys#feature? fx))
	     ((not (pair? fx)) (err fx))
	     (else
	      (let ((rest (##sys#slot fx 1)))
		(case (##sys#slot fx 0)
		  ((and)
		   (or (eq? rest '())
		       (if (pair? rest)
			   (and (test (##sys#slot rest 0))
				(test `(and ,@(##sys#slot rest 1))) )
			   (err fx) ) ) )
		  ((or) 
		   (and (not (eq? rest '()))
			(if (pair? rest)
			    (or (test (##sys#slot rest 0))
				(test `(or ,@(##sys#slot rest 1))) )
			    (err fx) ) ) )
		  ((not) (not (test (cadr fx))))
		  (else (err fx)) ) ) ) ) )

     (let expand ((cls clauses))
       (cond ((eq? cls '())
	      (##sys#apply
	       ##sys#error "no matching clause in `cond-expand' form" 
	       (map (lambda (x) (car x)) clauses) ) )
	     ((not (pair? cls)) (err cls))
	     (else
	      (let ((clause (##sys#slot cls 0))
		    (rclauses (##sys#slot cls 1)) )
		(if (not (pair? clause)) 
		    (err clause)
		    (let ((id (##sys#slot clause 0)))
		      (cond ((eq? id 'else)
			     (let ((rest (##sys#slot clause 1)))
			       (if (eq? rest '())
				   '(##core#undefined)
				   `(begin ,@rest) ) ) )
			    ((test id) `(begin ,@(##sys#slot clause 1)))
			    (else (expand rclauses)) ) ) ) ) ) ) ) ) ) )

(##sys#extend-macro-environment
 'define-macro
 #f
 (##sys#lisp-transformer
  (lambda (head . body)
    (define (expand name val)
      `(,(if ##sys#enable-runtime-macros '##core#elaborationtimetoo '##core#elaborationtimeonly)
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
	   (expand (car head) `(lambda ,(cdr head) ,@body)))))))

(##sys#extend-macro-environment
 'require-extension
 #f
 (##sys#lisp-transformer
  (lambda ids
    (##sys#check-syntax 'require-extension ids '#(_ 0))
    `(##core#require-extension ,@(map (lambda (x) (list 'quote x)) ids) ) ) ) )

(##sys#extend-macro-environment
 'define-syntax
 '()
 (lambda (form se)
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
