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
  (hide match-expression) )


;;; identifier objects

(define (make-identifier name context)
  (##sys#make-structure 'identifier name context) )

(define (identifier? x)
  (or (symbol? x) (##sys#structure? x 'identifier)) )

(define (identifier-name id)
  (cond ((symbol? id) id)
	(else
	 (##sys#check-structure id 'identifier 'identifier-name)
	 (##sys#slot id 1) ) ) )

(define (identifier-context id)
  (cond ((symbol? id) '())
	(else
	 (##sys#check-structure id 'identifier 'identifier-name)
	 (##sys#slot id 2) ) ) )

(define (bound-identifier=? id1 id2)
  (and (eq? (identifier-name id1) (identifier-name id2))
       (equal? (identifier-context id1) (identifier-context id2)) ) )

(define (free-identifier=? id1 id2)
  (eq? (identifier-name id1) (identifier-name id2)) )

(define (##sys#strip-context exp)
  (let walk ((x exp))
    (cond ((##sys#structure? x 'identifier) (identifier-name x))
	  ((pair? x) (cons (walk (car x)) (walk (cdr x))))
	  ((vector? x) (list->vector (map walk (vector->list x))))
	  (else x) ) ) )

(define (##sys#decorate/context exp context)
  (let walk ((x exp))
    (cond ((symbol? x) (make-identifier x context))
	  ((pair? x) (cons (walk (car x)) (walk (cdr x))))
	  ((vector? x) (list->vector (map walk (vector->list x))))
	  (else x) ) ) )

(define-record-printer (identifier id port)
  (display "#{" port)
  (write (identifier-name id) port)
  (for-each
   (lambda (c)
     (write-char #\space port)
     (write c port) )
   (identifier-context id) )
  (write-char #\} port) )

(define (##sys#check-identifier x #!optional y) 
  (unless (identifier? x)
    (##sys#error y "bad argument type - not an identifier" x) ) )


;;; Macro handling:

(define ##sys#macro-environment '())

(define (##sys#register-macro-0 name se handler)
  (cond ((assq name ##sys#macro-environment) =>
	 (lambda (a)
	   (set-car! (cdr a) se)
	   (set-car! (cddr a) handler) ) )
	(else 
	 (set! ##sys#macro-environment
	   (cons (list name se handler) ##sys#macro-environment)))))

(define (##sys#register-macro name handler)
  (##sys#register-macro-0 
   name '() 
   (lambda (form) (apply handler (##sys#slot form 1))) ) )

(define (##sys#register-macro-2 name handler)
  (##sys#register-macro-0 
   name '() 
   (lambda (form) (handler (##sys#slot form 1))) ) )

(define (##sys#copy-macro old new)
  (let ((def (assq old ##sys#macro-environment)))
    (apply ##sys#register-macro-0 def) ) )

(define (macro? sym #!optional (senv '()))
  (##sys#check-identifier sym 'macro?)
  (##sys#check-pair? senv 'macro?)
  (or (assq sym senv)
      (and (assq sym ##sys#macro-environment) #t) ) )

(define (##sys#unregister-macro name)
  (set! ##sys#macro-environment
    ;; this builds up stack, but isn't used often anyway...
    (let loop ((me ##sys#macro-environment) (me2 '()))
      (cond ((null? me) '())
	    ((eq? x (caar me)) (cdr me))
	    (else (cons (car me) (loop (cdr me))))))))

(define (undefine-macro! name)
  (##sys#check-identifier name 'undefine-macro!)
  (##sys#unregister-macro name) )


;; The basic macro-expander

(define ##sys#macroexpand-0
  (let ([string-append string-append])
    (lambda (exp me)

      (define (call-handler name handler exp)
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
				      "during expansion of (" (##sys#slot name 1) " ...) - "
				      (car r) )
				     (cdr r) ) )
			      (copy r) ) ) ) ) )
		 ex) )
	  (handler exp) ) )
				   
      (define (expand exp head)
	(cond [(assq head me) => (lambda (mdef) (values ((##sys#slot mdef 1) exp) #t))]
	      [(##sys#hash-table-ref ##sys#macro-environment head) 
	       => (lambda (handler)
		    (cond-expand
		     [unsafe (values (call-handler head handler exp) #t)]
		     [else
		      (let scan ([x exp])
			(cond [(null? x) (values (call-handler head handler exp) #t)]
			      [(pair? x) (scan (##sys#slot x 1))]
			      [else (##sys#syntax-error-hook "invalid syntax in macro form" exp)] ) ) ] ) ) ]
	      [else (values exp #f)] ) )

      (if (pair? exp)
	  (let ([head (##sys#slot exp 0)]
		[body (##sys#slot exp 1)] )
	    (if (symbol? head)
		(cond [(eq? head 'let)
		       (##sys#check-syntax 'let body '#(_ 2))
		       (let ([bindings (car body)])
			 (cond [(symbol? bindings)
				(##sys#check-syntax 'let body '(_ #((variable _) 0) . #(_ 1)))
				(let ([bs (cadr body)])
				  (values
				   `(##core#app
				     (letrec ([,bindings (##core#loop-lambda ,(map (lambda (b) (car b)) bs) ,@(cddr body))])
				       ,bindings)
				     ,@(##sys#map cadr bs) )
				   #t) ) ]
			       [else (values exp #f)] ) ) ]
		      [(and (memq head '(set! ##core#set!))
			    (pair? body)
			    (pair? (##sys#slot body 0)) )
		       (let ([dest (##sys#slot body 0)])
			 (##sys#check-syntax 'set! body '(#(_ 1) _))
			 (values
			  (append (list (list '##sys#setter (##sys#slot dest 0)))
				  (##sys#slot dest 1)
				  (##sys#slot body 1) ) 
			  #t) ) ]
		      [else (expand exp head)] )
		(values exp #f) ) )
	  (values exp #f) ) ) ) )


;;; These are needed to hook other module/macro systems into the evaluator and compiler

(define (##sys#compiler-toplevel-macroexpand-hook exp) exp)
(define (##sys#interpreter-toplevel-macroexpand-hook exp) exp)
(define (##sys#macroexpand-1-local exp me) (##sys#macroexpand-0 exp me))


;;; For the compiler

(define ##sys#enable-runtime-macros #f)


;;; User-level macroexpansion

(define (macroexpand exp #!optional (me '()))
  (let loop ([exp exp])
    (let-values ([(exp2 m) (##sys#macroexpand-0 exp me)])
      (if m
	  (loop exp2)
	  exp2) ) ) )

(define (macroexpand-1 exp #!optional (me '()))
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
    (lambda (llist0 body errh)
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
			     `((let* ,(map (lambda (k)
					     (let ([s (car k)])
					       `[,s (##sys#get-keyword 
						     ',(->keyword s) ,rvar
						     ,@(if (pair? (cdr k)) 
							   `((lambda () ,@(cdr k)))
							   '() ) ) ] ) )
					   (reverse key) )
				 ,@body) ) ) ] )
		    (cond [(null? opt) body]
			  [(and (not hasrest) (null? key) (null? (cdr opt)))
			   `((let ([,(caar opt) (:optional ,rvar ,(cadar opt))])
			       ,@body) ) ]
			  [(and (not hasrest) (null? key)) `((let-optionals ,rvar ,(reverse opt) ,@body))]
			  [else 
			   `((let-optionals* ,rvar ,(##sys#append (reverse opt) (list (or hasrest rvar))) 
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
		 (let ([x (##sys#slot llist 0)]
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
    (lambda (body lookup #!optional me container)
      (define (fini vars vals mvars mvals body)
	(if (and (null? vars) (null? mvars))
	    (let loop ([body2 body] [exps '()])
	      (if (not (pair? body2)) 
		  `(begin ,@body) ; no more defines, otherwise we would have called `expand'
		  (let ([x (##sys#slot body2 0)])
		    (if (and (pair? x) (memq (##sys#slot x 0) `(define define-values)))
			`(begin . ,(##sys#append (reverse exps) (list (expand body2))))
			(loop (##sys#slot body2 1) (cons x exps)) ) ) ) )
	    (let ([vars (reverse vars)])
	      `(let ,(##sys#map (lambda (v) (##sys#list v (##sys#list '##core#undefined))) 
				(apply ##sys#append vars mvars) )
		 ,@(map (lambda (v x) `(##core#set! ,v ,x)) vars (reverse vals))
		 ,@(map (lambda (vs x)
			  (let ([tmps (##sys#map gensym vs)])
			    `(##sys#call-with-values
			      (lambda () ,x)
			      (lambda ,tmps 
				,@(map (lambda (v t) `(##core#set! ,v ,t)) vs tmps) ) ) ) ) 
			(reverse mvars)
			(reverse mvals) )
		 ,@body) ) ) )
      (define (fini/syntax vars vals mvars mvals body)
	(fini
	 vars vals mvars mvals
	 (let loop ((body body) (defs '()) (done #f))
	   (cond (done `(letrec-syntax ,(map cdr (reverse defs)) ,@body) )
		 ((not (pair? body)) (loop body defs #t))
		 ((and (list? (car body))
		       (= 3 (length (car body))) 
		       (eq? 'define-syntax (caar body)) )
		  (loop (cdr body) (cons (car body) defs)))
		 (else (loop body defs #t))))))		       
      (define (expand body)
	(let loop ([body body] [vars '()] [vals '()] [mvars '()] [mvals '()])
	  (if (not (pair? body))
	      (fini vars vals mvars mvals body)
	      (let* ([x (##sys#slot body 0)]
		     [rest (##sys#slot body 1)] 
		     [head (and (pair? x) (##sys#slot x 0))] )
		(cond [(not head) (fini vars vals mvars mvals body)]
		      [(and (symbol? head) (lookup head))
		       (fini vars vals mvars mvals body) ]
		      [(eq? 'define head)
		       (##sys#check-syntax 'define x '(define _ . #(_ 0)) #f)
		       (let loop2 ([x x])
			 (let ([head (cadr x)])
			   (cond [(not (pair? head))
				  (##sys#check-syntax 'define x '(define variable . #(_ 0)) #f)
				  (loop rest (cons head vars)
					(cons (if (pair? (cddr x))
						  (caddr x)
						  '(##sys#void) )
					      vals)
					mvars mvals) ]
				 [(pair? (##sys#slot head 0))
				  (##sys#check-syntax 'define x '(define (_ . lambda-list) . #(_ 1)) #f)
				  (loop2 (cons 'define (##sys#expand-curried-define head (cddr x)))) ]
				 [else
				  (##sys#check-syntax 'define x '(define (variable . lambda-list) . #(_ 1)) #f)
				  (loop rest
					(cons (##sys#slot head 0) vars)
					(cons `(lambda ,(##sys#slot head 1) ,@(cddr x)) vals)
					mvars mvals) ] ) ) ) ]
		      ((eq? 'define-syntax head)
		       (##sys#check-syntax 'define-syntax x '(define-syntax variable _))
		       (fini/syntax vars vals mvars mvals body) )
		      [(eq? 'define-values head)
		       (##sys#check-syntax 'define-values x '(define-values #(_ 0) _) #f)
		       (loop rest vars vals (cons (cadr x) mvars) (cons (caddr x) mvals)) ]
		      [(eq? 'begin head)
		       (##sys#check-syntax 'begin x '(begin . #(_ 0)) #f)
		       (loop (##sys#append (##sys#slot x 1) rest) vars vals mvars mvals) ]
		      [else
		       (let ([x2 (##sys#macroexpand-0 x me)])
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
	      ((not (pair? x))
	      ((mwalk (##sys#slot x 0) (##sys#slot p 0))
	       (mwalk (##sys#slot x 1) (##sys#slot p 1)) )
	      (else #f) ) )
      (and (mwalk exp pat) env) ) ) )


;;; Expand "curried" lambda-list syntax for `define'

(define (##sys#expand-curried-define head body)
  (let* ([name #f])
    (define (loop head body)
      (if (symbol? (##sys#slot head 0))
	  (begin
	    (set! name (##sys#slot head 0))
	    `(lambda ,(##sys#slot head 1) ,@body) )
	  (loop (##sys#slot head 0) `((lambda ,(##sys#slot head 1) ,@body)) ) ))
    (let ([exp (loop head body)])
      (list name exp) ) ) )


;;; Macro definitions:

(##sys#register-macro-2
 'define
 (lambda (form)
   (let loop ([form form])
     (let ((head (car form))
	   (body (cdr form)) )
       (cond ((not (pair? head))
	      (##sys#check-syntax 'define head 'symbol)
	      (##sys#check-syntax 'define body '#(_ 0 1))
	      `(##core#set! ,head ,(if (pair? body) (car body) '(##sys#void))) )
	     ((pair? (##sys#slot head 0))
	      (##sys#check-syntax 'define head '(_ . lambda-list))
	      (##sys#check-syntax 'define body '#(_ 1))
	      (loop (##sys#expand-curried-define head body)) )
	     (else
	      (##sys#check-syntax 'define head '(symbol . lambda-list))
	      (##sys#check-syntax 'define body '#(_ 1))
	      `(##core#set! ,(car head) (lambda ,(cdr head) ,@body)) ) ) ) ) ) )

(##sys#register-macro-2
 'and
 (lambda (body)
   (if (eq? body '())
       #t
       (let ((rbody (##sys#slot body 1))
	     (hbody (##sys#slot body 0)) )
	 (if (eq? rbody '())
	     hbody
	     `(if ,hbody (and ,@rbody) #f) ) ) ) ) )

(##sys#register-macro-2
 'or 
 (let ((gensym gensym))
   (lambda (body)
     (if (eq? body '())
	 #f
	 (let ((rbody (##sys#slot body 1))
	       (hbody (##sys#slot body 0)) )
	   (if (eq? rbody '())
	       hbody
	       (let ((tmp (gensym)))
		 `(let ((,tmp ,hbody))
		    (if ,tmp ,tmp (or ,@rbody)) ) ) ) ) ) ) ) )

(##sys#register-macro-2
 'cond
 (let ((gensym gensym))
   (lambda (body)
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

(##sys#register-macro-2
 'case
 (let ((gensym gensym))
   (lambda (form)
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

(##sys#register-macro-2
 'let*
 (lambda (form)
   (let ((bindings (car form))
	 (body (cdr form)) )
     (##sys#check-syntax 'let* bindings '#((symbol _) 0))
     (##sys#check-syntax 'let* body '#(_ 1))
     (let expand ((bs bindings))
       (if (eq? bs '())
	   `(let () ,@body)
	   `(let (,(car bs)) ,(expand (cdr bs))) ) ) ) ) )

(##sys#register-macro-2
 'letrec
 (lambda (form)
   (let ((bindings (car form))
	 (body (cdr form)) )
     (##sys#check-syntax 'letrec bindings '#((symbol _) 0))
     (##sys#check-syntax 'letrec body '#(_ 1))
     `(let ,(##sys#map (lambda (b) (list (car b) '(##core#undefined))) bindings)
	,@(##sys#map (lambda (b) `(##core#set! ,(car b) ,(cadr b))) bindings)
	(let () ,@body) ) ) ) )

(##sys#register-macro
 'do
 (let ((gensym gensym))
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

(##sys#register-macro
 'quasiquote
 (let ((vector->list vector->list))
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

(##sys#register-macro
 'delay
 (lambda (x) `(##sys#make-promise (lambda () ,x))) )

(##sys#register-macro-2
 'cond-expand
   (lambda (clauses)

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
			    (else (expand rclauses)) ) ) ) ) ) ) ) ) )


