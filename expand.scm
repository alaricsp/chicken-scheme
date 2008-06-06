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
	macro-alias module-indirect-exports
	d dd dm map-se
	lookup) )


(set! ##sys#features
  (append '(#:hygienic-macros #:syntax-rules) ##sys#features))

(define (d arg1 . more)
  (if (null? more)
      (pp arg1)
      (apply print arg1 more)))

(define dd d)
(define dm d)

#;(begin
  (cond-expand
   (hygienic-macros
    (define-syntax dd (syntax-rules () ((_ . _) (void)))))
   (else					;*** remove later
    (define-macro (dd . _) '(void))))
  (cond-expand
   (hygienic-macros
    (define-syntax dm (syntax-rules () ((_ . _) (void)))))
   (else					;*** remove later
    (define-macro (dm . _) '(void)))))


;;; Syntactic environments

(define ##sys#current-environment (make-parameter '()))
(define ##sys#current-meta-environment (make-parameter '()))

(define (lookup id se)
  (cond ((assq id se) => cdr)
	((##sys#get id '##core#macro-alias))
	(else #f)))

(define (macro-alias var se)
  (if (or (##sys#qualified-symbol? var)
	  (let* ((str (##sys#slot var 1))
		 (len (##sys#size str)))
	    (and (fx> len 0)
		 (char=? #\# (##core#inline "C_subchar" str 0)))))
      var
      (let* ((alias (gensym var))
	     (ua (or (lookup var se) var)))
	(##sys#put! alias '##core#macro-alias ua)
	(dd "aliasing " var " to " 
	    (if (pair? ua)
		`(MACRO: ,@(map-se (car ua)))
		ua))
	alias) ) )

(define (map-se se)
  (map (lambda (a) 
	 (cons (car a) (if (symbol? (cdr a)) (cdr a) '<macro>)))
       se))

(define (##sys#strip-syntax exp #!optional se)
  ;; if se is given, retain bound vars
  (let walk ((x exp))
    (cond ((symbol? x)
	   (let ((x2 (if se 
			 (lookup x se)
			 (get x '##core#macro-alias) ) ) )
	     (cond ((not x2) x)
		   ((pair? x2) x)
		   (else x2))))
	  ((pair? x)
	   (cons (walk (##sys#slot x 0))
		 (walk (cdr x))))
	  ((vector? x)
	   (list->vector (map walk (vector->list x))))
	  (else x))))


;;; Macro handling

(define ##sys#macro-environment (make-parameter '()))

(define (##sys#extend-macro-environment name se handler)
  (let ((me (##sys#macro-environment)))
    (dd "extending: " name " SE: " (map-se se))
    (cond ((lookup name me) =>
	   (lambda (a)
	     (set-car! a se)
	     (set-car! (cdr a) handler) ) )
	  (else 
	   (##sys#macro-environment
	    (cons (list name se handler)
		  me))))))

(define (##sys#copy-macro old new)
  (let ((def (lookup old (##sys#macro-environment))))
    (apply ##sys#extend-macro-environment new def) ) )

(define (macro? sym #!optional (senv (##sys#current-environment)))
  (##sys#check-symbol sym 'macro?)
  (##sys#check-list senv 'macro?)
  (or (lookup sym senv)
      (and (lookup sym (##sys#macro-environment)) #t) ) )

(define (##sys#unregister-macro name)
  (##sys#macro-environment
    ;; this builds up stack, but isn't used often anyway...
    (let loop ((me (##sys#macro-environment)) (me2 '()))
      (cond ((null? me) '())
	    ((eq? x (caar me)) (cdr me))
	    (else (cons (car me) (loop (cdr me))))))))

(define (undefine-macro! name)
  (##sys#check-symbol name 'undefine-macro!)
  (##sys#unregister-macro name) )


;; The basic macro-expander

(define ##sys#expand-0
  (let ([string-append string-append])
    (lambda (exp dse)
      (define (call-handler name handler exp se)
	(dd "invoking macro: " name)
	(dd `(STATIC-SE: ,@(map-se se)))
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
	  (let ((exp2 (handler exp se dse)))
	    (dd exp2)
	    exp2)))
      (define (expand head exp mdef)
	(dd `(EXPAND: 
	      ,head 
	      ,(cond ((get head '##core#macro-alias) =>
		      (lambda (a) (if (symbol? a) a '<macro>)) )
		     (else '_))
	      ,exp 
	      ,(if (pair? mdef)
		   `(SE: ,@(map-se (car mdef)))
		   mdef)))
	(cond ((not (list? exp))
	       (##sys#syntax-error-hook "invalid syntax in macro form" exp) )
	      ((pair? mdef)
	       (values 
		;; force ref. opaqueness by passing dynamic se  [what is this comment meaning? I forgot]
		(call-handler head (cadr mdef) exp (car mdef))
		#t))
	      (else (values exp #f)) ) )
      (if (pair? exp)
	  (let ((head (car exp))
		(body (cdr exp)) )
	    (if (symbol? head)
		(let ((head2 (or (lookup head dse) head)))
		  (unless (pair? head2)
		    (set! head2 (or (lookup head2 (##sys#macro-environment)) head2)) )
		  (cond [(memq head2 '(let ##core#let))
			 (##sys#check-syntax 'let body '#(_ 2) #f dse)
			 (let ([bindings (car body)])
			   (cond [(symbol? bindings)
				  (##sys#check-syntax 'let body '(_ #((variable _) 0) . #(_ 1)) #f dse)
				  (let ([bs (cadr body)])
				    (values
				     `(##core#app
				       (,(macro-alias 'letrec dse) ;*** correct to use dse?
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

(define ##sys#enable-runtime-macros #f)

(define (##sys#module-rename sym prefix)
  (##sys#string->symbol 
   (string-append 
    (##sys#slot prefix 1)
    "#" 
    (##sys#slot sym 1) ) ) )

(define (##sys#alias-global-hook sym assign)
  (define (mrename sym)
    (cond ((##sys#current-module) => 
	   (lambda (mod)
	     (dm "global alias " sym " -> " (module-name mod))
	     (unless assign (##sys#register-undefined sym mod))
	     (##sys#module-rename sym (module-name mod))))
	  (else sym)))
  (cond ((##sys#qualified-symbol? sym) sym)
	((##sys#get sym '##core#aliased) sym)
	((assq sym (##sys#current-environment)) =>
	 (lambda (a)
	   (if (pair? (cdr a))
	       (mrename sym)
	       (cdr a) ) ) )
	(else (mrename sym))))


;;; User-level macroexpansion

(define (##sys#expand exp #!optional (me (##sys#current-environment)))
  (let loop ((exp exp))
    (let-values (((exp2 m) (##sys#expand-0 exp me)))
      (if m
	  (loop exp2)
	  exp2) ) ) )

(define expand ##sys#expand)

(define (expand* exp #!optional (me (##sys#current-environment)))
  (##sys#expand-0 exp me) )


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
	   [else (loop (cdr llist))] ) ) ) )

(define ##sys#expand-extended-lambda-list
  (let ([reverse reverse]
	[gensym gensym] )
    (lambda (llist0 body errh se)
      (define (err msg) (errh msg llist0))
      (define (->keyword s) (string->keyword (##sys#slot s 1)))
      (let ([rvar #f]
	    [hasrest #f] 
	    (%let* (macro-alias 'let* se))
	    (%lambda '##core#lambda)
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
					  `(,s (##sys#get-keyword 
						',(->keyword s) ,rvar
						,@(if (pair? (cdr k)) 
						      `((,%lambda () ,@(cdr k)))
						      '() ) ) ) ) )
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
			(r (cdr llist)))
		   (case x
		     [(#!optional)
		      (if (not rvar) (set! rvar (macro-alias 'tmp se)))
		      (if (eq? mode 0)
			  (loop 1 req '() '() r)
			  (err "`#!optional' argument marker in wrong context") ) ]
		     [(#!rest)
		      (if (fx<= mode 1)
			  (if (and (pair? r) (symbol? (car r)))
			      (begin
				(if (not rvar) (set! rvar (car r)))
				(set! hasrest (car r))
				(loop 2 req opt '() (cdr r)) )
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
	    (let* ((vars (reverse vars))
		   (result 
		    `(##core#let
		      ,(##sys#map (lambda (v) (##sys#list v (##sys#list '##core#undefined))) 
				  (apply ##sys#append vars mvars) )
		      ,@(map (lambda (v x) `(##core#set! ,v ,x)) vars (reverse vals))
		      ,@(map (lambda (vs x)
			       (let ([tmps (##sys#map gensym vs)])
				 `(##sys#call-with-values
				   (##core#lambda () ,x)
				   (##core#lambda 
				    ,tmps 
				    ,@(map (lambda (v t)
					     `(##core#set! ,v ,t)) 
					   vs tmps) ) ) ) ) 
			     (reverse mvars)
			     (reverse mvals) )
		      ,@body) ) )
	      (dd `(BODY: ,result))
	      result)))
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
						  '(##core#undefined) )
					      vals)
					mvars mvals) ]
				 [(pair? (car head))
				  (##sys#check-syntax 'define x '(define (_ . lambda-list) . #(_ 1)) #f se)
				  (loop2 (cons (macro-alias 'define se)
					       (##sys#expand-curried-define head (cddr x) se))) ]
				 [else
				  (##sys#check-syntax 'define x '(define (variable . lambda-list) . #(_ 1)) #f se)
				  (loop rest
					(cons (car head) vars)
					(cons `(##core#lambda ,(cdr head) ,@(cddr x)) vals)
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
		      ((or (memq head vars) (memq head mvars))
		       (fini vars vals mvars mvals body))
		      [else
		       (let ([x2 (##sys#expand-0 x se)])
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
	       (cond ((assq p env) => (lambda (a) (equal? x (cdr a))))
		     ((memq p vars)
		      (set! env (cons (cons p x) env))
		      #t)
		     (else (eq? x p)) ) )
	      ((pair? x)
	       (and (mwalk (car x) (car p))
		    (mwalk (cdr x) (cdr p)) ) )
	      (else #f) ) )
      (and (mwalk exp pat) env) ) ) )


;;; Expand "curried" lambda-list syntax for `define'

(define (##sys#expand-curried-define head body se)
  (let ((name #f))
    (define (loop head body)
      (if (symbol? (car head))
	  (begin
	    (set! name (car head))
	    `(##core#lambda ,(cdr head) ,@body) )
	  (loop (car head) `((##core#lambda ,(cdr head) ,@body)) ) ))
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
		     (let ((s (car x)))
		       (and (symbol? s)
			    (loop (cdr x)) ) ) )
		    (else #f) ) ) ) )

      (define (proper-list? x)
	(let loop ((x x))
	  (cond ((eq? x '()))
		((pair? x) (loop (cdr x)))
		(else #f) ) ) )

      (when culprit (set! ##sys#syntax-error-culprit culprit))
      (let walk ((x exp) (p pat))
	(cond ((vector? p)
	       (let* ((p2 (vector-ref p 0))
		      (vlen (##sys#size p))
		      (min (if (fx> vlen 1) 
			       (vector-ref p 1)
			       0) )
		      (max (cond ((eq? vlen 1) 1)
				 ((fx> vlen 2) (vector-ref p 2))
				 (else 99999) ) ) )
		 (do ((x x (cdr x))
		      (n 0 (fx+ n 1)) )
		     ((eq? x '())
		      (if (fx< n min)
			  (err "not enough arguments") ) )
		   (cond ((fx>= n max) 
			  (err "too many arguments") )
			 ((not (pair? x))
			  (err "not a proper list") )
			 (else (walk (car x) p2) ) ) ) ) )
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
		 (else
		  (test
		   x
		   (lambda (y)
		     (let ((y2 (and (symbol? y) (lookup y se))))
		       (eq? (if (symbol? y2) y2 y) p)))
		   "missing keyword")) ) )
	      ((not (pair? p))
	       (err "incomplete form") )
	      ((not (pair? x)) (err "pair expected"))
	      (else
	       (walk (car x) (car p))
	       (walk (cdr x) (cdr p)) ) ) ) ) ) )


;;; explicit-renaming transformer

(define ((##sys#er-transformer handler) form se dse)
  (let ((renv '()))			; keep rename-environment for this expansion
    (define (rename sym)
      (cond ((assq sym renv) => cdr)
	    ((lookup sym se) => 
	     (lambda (a)
	       (if (symbol? a)
		   a
		   (let ((a2 (macro-alias sym se)))
		     ;;(dd `(SE/RENAME: ,sym ,a2 ,(map-se (car a))))
		     (set! renv (cons (cons sym a2) renv))
		     a2))))
	    (else
	     (let ((a (macro-alias sym se)))
	       (set! renv (cons (cons sym a) renv))
	       a))))
    (define (compare s1 s2)
      (if (and (symbol? s1) (symbol? s2))
	  (eq? (or (##sys#get s1 '##core#macro-alias)
		   (lookup s1 dse)
		   s1)
	       (or (##sys#get s2 '##core#macro-alias)
		   (lookup s2 dse)
		   s2) )
	  (eq? s1 s2)))
    (handler form rename compare) ) )


;;; Macro definitions:

(let ()
  (define (expand-import x r c import-env macro-env loc)
    (let ((%only (r 'only))
	  (%rename (r 'rename))
	  (%except (r 'except))
	  (%prefix (r 'prefix)))
      (define (resolve sym)
	(or (lookup sym '()) sym))	;*** empty se?
      (define (tostr x)
	(cond ((string? x) x)
	      ((keyword? x) (##sys#string-append (##sys#symbol->string x) ":")) ; why not?
	      ((symbol? x) (##sys#symbol->string x))
	      ((number? x) (number->string x))
	      (else (syntax-error loc "invalid prefix" ))))
      (define (import-name spec)
	(let* ((mname (resolve spec))
	       (mod (##sys#find-module mname #f)))
	  (unless mod
	    (let ((il (##sys#find-extension 
		       (string-append (symbol->string mname) ".import")
		       #t)))
	      (cond (il (parameterize ((##sys#current-module #f)
				       (##sys#current-environment '())
				       (##sys#macro-environment (##sys#meta-macro-environment)))
			  (##sys#load il #f #f))
			(set! mod (##sys#find-module mname)))
		    (else
		     (syntax-error
		      loc "can not import from undefined module" 
		      mname)))))
	  (cons (module-vexports mod) 
		(module-sexports mod))))
      (define (import-spec spec)
	(cond ((symbol? spec) (import-name spec))
	      ((or (not (list? spec)) (< (length spec) 2))
	       (syntax-error loc "invalid import specification" spec))
	      (else
	       (let* ((s (car spec))
		      (imp (import-spec (cadr spec)))
		      (impv (car imp))
		      (imps (cdr imp)))
		 (cond ((c %only (car spec))
			(##sys#check-syntax loc spec '(_ _ . #(symbol 0)))
			(let ((ids (map resolve (cddr spec))))
			  (let loop ((ids ids) (v '()) (s '()))
			    (cond ((null? ids) (cons v s))
				  ((assq (car ids) impv) =>
				   (lambda (a) 
				     (loop (cdr ids) (cons a v) s)))
				  ((assq (car ids) imps) =>
				   (lambda (a) 
				     (loop (cdr ids) v (cons a s))))
				  (else (loop (cdr ids) v s))))))
		       ((c %except (car spec))
			(##sys#check-syntax loc spec '(_ _ . #(symbol 0)))
			(let ((ids (map resolve (cddr spec))))
			  (let loop ((impv impv) (v '()))
			    (cond ((null? impv)
				   (let loop ((imps imps) (s '()))
				     (cond ((null? imps) (cons v s))
					   ((memq (caar imps) ids) (loop (cdr imps) s))
					   (else (loop (cdr imps) (cons (car imps) s))))))
				  ((memq (caar impv) ids) (loop (cdr impv) v))
				  (else (loop (cdr impv) (cons (car impv) v)))))))
		       ((c %rename (car spec))
			(##sys#check-syntax loc spec '(_ _ . #((symbol symbol) 0)))
			(let loop ((impv impv) (imps imps) (v '()) (s '()) (ids (cddr spec)))
			  (cond ((null? impv) 
				 (cond ((null? imps)
					(for-each
					 (lambda (id)
					   (##sys#warn "renamed identifier not imported" id) )
					 ids)
					(cons v s))
				       ((assq (caar imps) ids) =>
					(lambda (a)
					  (loop impv (cdr imps)
						v
						(cons (cons (cadr a) (cdar imps)) s)
						(##sys#delq a ids))))
				       (else (loop impv (cdr imps) v (cons (car imps) s) ids))))
				((assq (caar impv) ids) =>
				 (lambda (a)
				   (loop (cdr impv) imps
					 (cons (cons (cadr a) (cdar impv)) v)
					 s
					 (##sys#delq a ids))))
				(else (loop (cdr impv) imps
					    (cons (car impv) v)
					    s ids)))))
		       ((c %prefix (car spec))
			(##sys#check-syntax loc spec '(_ _ _))
			(let ((pref (tostr (caddr spec))))
			  (define (ren imp)
			    (cons 
			     (##sys#string->symbol 
			      (##sys#string-append pref (##sys#symbol->string (car imp))) )
			     (cdr imp) ) )
			  (cons (map ren impv) (map ren imps))))
		       (else (syntax-error loc "invalid import specification" spec)))))))
      (##sys#check-syntax loc x '(_ . #(_ 1)))
      (let ((cm (##sys#current-module)))
	(when cm
	  ;; save import form
	  (set-module-import-forms! cm (append (module-import-forms cm) (cdr x))))
	(for-each
	 (lambda (spec)
	   (let* ((vs (import-spec spec))
		  (vsv (car vs))
		  (vss (cdr vs)))
	     #;(when cm
	       ;; fixup reexports
	       (let ((dlist (module-defined-list cm)))
		 (define (fixup! imports)
		   (for-each
		    (lambda (imp)
		      (when (##sys#find-export (car imp) cm #t) ;*** must process export list for every import
			(dm "fixup reexport: " imp)
			(set! dlist (cons  dlist)))) ;*** incorrect!
		    imports) )
		 (fixup! vsv)
		 (fixup! vss)
		 (set-module-defined-list! cm dlist)) )
	     (dd `(V: ,(if cm (module-name cm) '<toplevel>) ,(map-se vsv)))
	     (dd `(S: ,(if cm (module-name cm) '<toplevel>) ,(map-se vss)))
	     (for-each
	      (lambda (imp)
		(let ((id (car imp))
		      (aid (cdr imp)))
		  (##sys#put! aid '##core#aliased #t)
		  (and-let* ((a (assq id (import-env)))
			     ((not (eq? aid (cdr a)))))
		    (##sys#warn "re-importing already imported identfier: " id))))
	      vsv)
	     (for-each
	      (lambda (imp)
		(and-let* ((a (assq (car imp) (macro-env)))
			   ((not (eq? (cdr imp) (cdr a)))))
		  (##sys#warn "re-importing already imported syntax: " (car imp))) )
	      vss)
	     (import-env (append vsv (import-env)))
	     (macro-env (append vss (macro-env)))))
	 (cdr x))
	'(##core#undefined))))
  (##sys#extend-macro-environment
   'import '() 
   (##sys#er-transformer 
    (cut expand-import <> <> <> ##sys#current-environment ##sys#macro-environment
	 'import) ) )
  (##sys#extend-macro-environment
   'import-for-syntax '() 
   (##sys#er-transformer 
    ;;*** ##sys#import-environment is likely to be wrong here
    (cut expand-import <> <> <> ##sys#current-environment ##sys#meta-macro-environment 
	 'import-for-syntax) ) ) )

(define ##sys#initial-macro-environment (##sys#macro-environment))

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
	       (##sys#register-export head (##sys#current-module))
	       `(##core#set! ,head ,(if (pair? body) (car body) '(##core#undefined))) )
	      ((pair? (car head))
	       (##sys#check-syntax 'define head '(_ . lambda-list))
	       (##sys#check-syntax 'define body '#(_ 1))
	       (loop (##sys#expand-curried-define head body '())) ) ;*** '() should be se
	      (else
	       (##sys#check-syntax 'define head '(symbol . lambda-list))
	       (##sys#check-syntax 'define body '#(_ 1))
	       (##sys#register-export (car head) (##sys#current-module))
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
						 ,@(cdr bxs) ) ) 
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

(##sys#extend-macro-environment
 'require-extension
 '()
 (##sys#er-transformer
  (lambda (x r c)
    (let ((ids (cdr x))
	  (%quote (r 'quote)))
      `(##core#require-extension ,@ids) ) ) ) )

(##sys#extend-macro-environment
 'module
 '()
 (##sys#er-transformer
  (lambda (x r c)
    (##sys#check-syntax 'module x '(_ symbol #(_ 0) . #(_ 0)))
    `(##core#module ,@(cdr x)))))


;;; syntax-rules

(include "synrules.scm")


;;; the base macro environment ("scheme", essentially)

(define ##sys#default-macro-environment (##sys#macro-environment))


;;; low-level module support

(define ##sys#meta-macro-environment (make-parameter (##sys#macro-environment)))
(define ##sys#current-module (make-parameter #f))

(declare 
  (hide make-module module?
	module-name module-vexports module-sexports
	set-module-vexports! set-module-sexports!
	module-export-list module-defined-list set-module-defined-list!
	module-import-forms set-module-import-forms!
	module-defined-syntax-list set-module-defined-syntax-list!))

(define-record-type module
  (make-module name export-list defined-list defined-syntax-list undefined-list 
	       import-forms vexports sexports) 
  module?
  (name module-name)			; SYMBOL
  (export-list module-export-list)	; (SYMBOL | (SYMBOL ...) ...)
  (defined-list module-defined-list set-module-defined-list!) ; ((SYMBOL . VALUE) ...)
  (defined-syntax-list module-defined-syntax-list set-module-defined-syntax-list!) ; ((SYMBOL . VALUE) ...)
  (undefined-list module-undefined-list set-module-undefined-list!) ; (SYMBOL ...)
  (import-forms module-import-forms set-module-import-forms!)	    ; (SPEC ...)
  (vexports module-vexports set-module-vexports!)	      ; (SYMBOL . SYMBOL)
  (sexports module-sexports set-module-sexports!) )	      ; ((SYMBOL SE TRANSFORMER) ...)

(define (##sys#find-module name #!optional (err #t))
  (cond ((assq name ##sys#module-table) => cdr)
	(err (error 'import "module not found" name))
	(else #f)))

(define (##sys#toplevel-definition-hook sym mod exp val) #f)

(define (##sys#register-export sym mod)
  (when mod
    (let ((exp (##sys#find-export sym mod #t))
	  (ulist (module-undefined-list mod)))
      (##sys#toplevel-definition-hook
       (##sys#module-rename sym (module-name mod)) 
       mod exp #f)
      (when (memq sym ulist)
	(set-module-undefined-list! mod (##sys#delq sym ulist)))
      (when exp
	(dm "defined: " sym)
	(set-module-defined-list! 
	 mod
	 (cons (cons sym #f)
	       (module-defined-list mod)))))) )

(define (##sys#register-syntax-export sym mod val)
  (when mod
    (let ((exp (##sys#find-export sym mod #t))
	  (ulist (module-undefined-list mod))
	  (mname (module-name mod)))
      (when (memq sym ulist)
	(##sys#warn "use of syntax precedes definition" sym mname))
      (dm "defined syntax: " sym)
      (when exp
	(set-module-defined-list! 
	 mod
	 (cons (cons sym val)
	       (module-defined-list mod))) )
      (set-module-defined-syntax-list! 
       mod
       (cons sym (module-defined-syntax-list mod))))))

(define (##sys#register-undefined sym mod)
  (when mod
    (let ((ul (module-undefined-list mod)))
      (unless (memq sym ul)
	(set-module-undefined-list! mod (cons sym ul))))))

(define (##sys#register-module name explist #!optional (vexports '()) (sexports '()))
  (let ((mod (make-module name explist '() '() '() '() vexports sexports)))
    (set! ##sys#module-table (cons (cons name mod) ##sys#module-table))
    mod) )

(define (##sys#mark-imported-symbols se)
  (for-each
   (lambda (imp)
     (when (symbol? (cdr imp))
       (dm `(MARKING: ,(cdr imp)))
       (##sys#put! (cdr imp) '##core#aliased #t)))
   se))

(define (module-indirect-exports mod)
  (let ((exports (module-export-list mod))
	(mname (module-name mod)))
    (define (indirect? id)
      (let loop ((exports exports))
	(and (not (null? exports))
	     (or (and (pair? (car exports))
		      (memq id (cdar exports)))
		 (loop (cdr exports))))))
    (let loop ((dlist (module-defined-list mod)))
      (cond ((null? dlist) '())
	    ((indirect? (caar dlist))
	     (cons 
	      (cons 
	       (caar dlist)
	       (or (cdar dlist)
		   (##sys#module-rename (caar dlist) mname)))
	      (loop (cdr dlist))))
	    (else (loop (cdr dlist)))))))

(define (##sys#compiled-module-registration mod)
  (let ((dlist (module-defined-list mod))
	(exports (module-export-list mod))
	(mname (module-name mod)))
    `(begin
       (import ,@(module-import-forms mod))
       (##sys#register-compiled-module
	',(module-name mod)
	(list
	 ,@(map (lambda (ie)
		  (if (symbol? (cdr ie))
		      `'(,(car ie) . ,(cdr ie))
		      `(list ',(car ie) '() ,(cdr ie))))
		(module-indirect-exports mod)))
	',(module-vexports mod)
	(list 
	 ,@(map (lambda (sexport)
		  (let* ((name (car sexport))
			 (a (assq name dlist)))
		    (unless (pair? a)
		      (bomb "exported syntax has no source"))
		    `(cons ',(car sexport) ,(cdr a))))
		(module-sexports mod)))))))

(define (##sys#register-compiled-module name iexports vexports sexports)
  (let* ((sexps
	  (map (lambda (se)
		 (list (car se) #f (##sys#er-transformer (cdr se))))
	       sexports))
	 (iexps 
	  (map (lambda (ie)
		 (if (pair? (cdr ie))
		     (list (car ie) (cadr ie) (##sys#er-transformer (caddr ie)))
		     ie))
	       iexports))
	 (mod (make-module 
	       name '() '() '() '() '()
	       vexports sexps))
	 (exports (append iexps vexports sexps (##sys#current-environment))))
    (##sys#mark-imported-symbols iexps)
    (for-each
     (lambda (sexp)
       (set-car! (cdr sexp) exports))
     sexps)
    (for-each
     (lambda (iexp)
       (when (pair? (cdr iexp))
	 (set-car! (cdr iexp) exports)))
     iexps)
    (set! ##sys#module-table (cons (cons name mod) ##sys#module-table)) 
    mod))

(define (##sys#register-primitive-module name vexports #!optional (sexports '()))
  (let* ((me (##sys#macro-environment))
	 (mod (make-module 
	      name '() '() '() '()'()
	      (map (lambda (ve)
		     (if (symbol? ve)
			 (cons ve ve)
			 ve))
		   vexports)
	      (map (lambda (se)
		     (if (symbol? se)
			 (or (assq se me)
			     (##sys#error "unknown macro referenced while registering module" se name))
			 se))
		   sexports))))
    (set! ##sys#module-table (cons (cons name mod) ##sys#module-table)) 
    mod))

(define (##sys#find-export sym mod indirect)
  (let loop ((xl (module-export-list mod)))
    (cond ((null? xl) #f)
	  ((eq? sym (car xl)))
	  ((pair? (car xl))
	   (or (eq? sym (caar xl))
	       (and indirect (memq sym (cdar xl)))
	       (loop (cdr xl))))
	  (else (loop (cdr xl))))))

(define (##sys#finalize-module mod me0)
  (let* ((explist (module-export-list mod))
	 (name (module-name mod))
	 (dlist (module-defined-list mod))
	 (sdlist (map (lambda (sym) (assq sym (##sys#macro-environment)))
		      (module-defined-syntax-list mod)))
	 (sexports
	  (let loop ((me (##sys#macro-environment)))
	    (cond ((or (null? me) (eq? me0 me)) '())
		  ((##sys#find-export (caar me) mod #f)
		   (cons (car me) (loop (cdr me))))
		  (else (loop (cdr me))))))
	 (vexports
	  (let loop ((xl explist))
	    (if (null? xl)
		'()
		(let* ((h (car xl))
		       (id (if (symbol? h) h (car h))))
		  (if (assq id sexports) 
		      (loop (cdr xl))
		      (cons 
		       (cons 
			id
			(let ((def (assq id dlist)))
			  (if (and def (symbol? (cdr def)))
			      (cdr def)
			      (##sys#module-rename id name))))
		       (loop (cdr xl)))))))))
    (for-each 
     (lambda (x)
       (unless (assq (car x) dlist)
	 (##sys#warn "exported identifier has not been defined" (car x) name)))
     vexports)
    (for-each
     (lambda (u)
       (unless (assq u dlist)
	 (##sys#warn 
	  (string-append
	   "reference to possibly unbound identifier `" 
	   (##sys#symbol->string u)
	   "'"))))
     (module-undefined-list mod))
    (let ((exports 
	   (map (lambda (exp)
		  (cond ((symbol? (cdr exp)) exp)
			((assq (car exp) (##sys#macro-environment)))
			(else (##sys#error "(internal) indirect export not found" (car exp)))) )
		(module-indirect-exports mod))))
      (##sys#mark-imported-symbols exports)
      (for-each
       (lambda (m)
	 (let ((se (append exports (cadr m))))
	   (dm `(FIXUP: ,(car m) ,@(map-se se)))
	   (set-car! (cdr m) se)))
       sdlist)
      (dm `(EXPORTS: 
	    ,(module-name mod) 
	    (DLIST: ,@dlist)
	    (SDLIST: ,@(map-se sdlist))
	    (IEXPORTS: ,@(map-se exports))
	    (VEXPORTS: ,@(map-se vexports))
	    (SEXPORTS: ,@(map-se sexports))))
      (set-module-vexports! mod vexports)
      (set-module-sexports! mod sexports))))

(define ##sys#module-table '())

(define (##sys#macro-subset me0)
  (let loop ((me (##sys#macro-environment)))
    (if (or (null? me) (eq? me me0))
	'()
	(cons (car me) (loop (cdr me))))))
