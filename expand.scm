;;;; expand.scm
;
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
  (unit expand)
  (disable-interrupts)
  (fixnum)
  (hide match-expression
	macro-alias module-indirect-exports
	d dd dm dc map-se merge-se
	lookup check-for-redef) )


(set! ##sys#features
  (append '(#:hygienic-macros #:syntax-rules) ##sys#features))

(define (d arg1 . more)
  (when (##sys#fudge 13)
    (if (null? more)
	(pp arg1)
	(apply print arg1 more))) )

(define dd d)
(define dm d)
(define dc d)

(cond-expand
 ((not debugbuild)
  (declare 
    (no-bound-checks)
    (no-procedure-checks)))
 (else))

(begin
  (define-syntax dd (syntax-rules () ((_ . _) (void))))
  (define-syntax dm (syntax-rules () ((_ . _) (void))))
  (define-syntax dc (syntax-rules () ((_ . _) (void)))) )


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
	(##sys#put! alias '##core#real-name var)
	(dd "aliasing " alias " (real: " var ") to " 
	    (if (pair? ua)
		'<macro>
		ua))
	alias) ) )

#+debugbuild
(define (map-se se)
  (map (lambda (a) 
	 (cons (car a) (if (symbol? (cdr a)) (cdr a) '<macro>)))
       se))

(define (##sys#strip-syntax exp #!optional se alias)
 ;; if se is given, retain bound vars
 (let ((seen '()))
   (let walk ((x exp))
     (cond ((assq x seen) => cdr)
           ((symbol? x)
            (let ((x2 (if se
                          (lookup x se)
                          (get x '##core#macro-alias) ) ) )
              (cond ((get x '##core#real-name))
                    ((and alias (not (assq x se)))
                     (##sys#alias-global-hook x #f))
                    ((not x2) x)
                    ((pair? x2) x)
                    (else x2))))
           ((pair? x)
            (let ((cell (cons #f #f)))
              (set! seen (cons (cons x cell) seen))
              (set-car! cell (walk (car x)))
              (set-cdr! cell (walk (cdr x)))
              cell))
           ((vector? x)
            (let* ((len (##sys#size x))
		   (vec (make-vector len)))
              (set! seen (cons (cons x vec) seen))
              (do ((i 0 (fx+ i 1)))
                  ((fx>= i len) vec)
                (##sys#setslot vec i (##sys#slot x i)))))
           (else x)))))

(define strip-syntax ##sys#strip-syntax)


;;; Macro handling

(define ##sys#macro-environment (make-parameter '()))
(define ##sys#chicken-macro-environment '()) ; used later in chicken.import.scm
(define ##sys#chicken-ffi-macro-environment '()) ; used later in foreign.import.scm

; Workalike of '##sys#environment?' for syntactic environments
(define (##sys#syntactic-environment? obj)

  (define (simple-environment? obj)
    (and (list? obj)
         (or (null? obj)
             (simple-environment-entry? (car obj))
             #; ;enough already
             (call-with-current-continuation
               (lambda (return)
                 (##sys#for-each
                  (lambda (x) (unless (simple-environment-entry? x) (return #f) ) )
                  obj)
               #t ) ) ) ) )

  (define (simple-environment-entry? obj)
    (and (pair? obj)
         (symbol? (car obj))
         (symbol? (cdr obj)) ) )

  (define (macro-environment? obj)
    (and (list? obj)
         (or (null? obj)
             (macro-environment-entry? (car obj))
             #; ;enough already
             (call-with-current-continuation
               (lambda (return)
                 (##sys#for-each
                  (lambda (x) (unless (macro-environment-entry? x) (return #f) ) )
                  obj)
               #t ) ) ) ) )

  (define (macro-environment-entry? obj)
    (and (pair? obj) (= 3 (length obj))
         (symbol? (car obj))
         (list? (cadr obj))
         #;(##sys#syntactic-environment? (cadr x)) ;enough already
         (procedure? (caddr obj)) ) )

  (or (simple-environment? obj)
      (macro-environment? obj) ) )

; Workalike of '##sys#environment-symbols' for syntactic environments
; (I think :-)
(define (##sys#syntactic-environment-symbols env pred)
  (define (try-alias id)
    (or (##sys#get id '##core#real-name)
        (let ((alias (##sys#get id '##core#macro-alias)))
          (cond ((not alias) id)
                ((pair? alias) id)
                (else alias) ) ) ) )
  (let ((syms '()))
    (##sys#for-each
     (lambda (cell)
       (let ((id (car cell)))
         (cond ((pred id)
                (set! syms (cons id syms)) )
               ((try-alias id) =>
                (lambda (name)
                  (when (pred name) (set! syms (cons name syms))) ) ) ) ) )
     env)
   syms ) )

(define (##sys#extend-macro-environment name se handler)
  (let ((me (##sys#macro-environment)))
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

(define (##sys#macro? sym #!optional (senv (##sys#current-environment)))
  (or (let ((l (lookup sym senv)))
	(pair? l))
      (and-let* ((l (lookup sym (##sys#macro-environment))))
	(pair? l))))

(define (##sys#unregister-macro name)
  (##sys#macro-environment
    ;; this builds up stack, but isn't used often anyway...
    (let loop ((me (##sys#macro-environment)) (me2 '()))
      (cond ((null? me) '())
	    ((eq? name (caar me)) (cdr me))
	    (else (cons (car me) (loop (cdr me))))))))

(define (##sys#undefine-macro! name)
  (##sys#unregister-macro name) )


;; The basic macro-expander

(define (##sys#expand-0 exp dse cs?)
  (define (call-handler name handler exp se cs)
    (dd "invoking macro: " name)
    (dd `(STATIC-SE: ,@(map-se se)))
    (handle-exceptions ex
	;; modify error message in condition object to include 
	;; currently expanded macro-name
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
      (let ((exp2
	     (if cs
		 (fluid-let ((##sys#syntax-rules-mismatch (lambda (input) exp))) ; a bit of a hack
		   (handler exp se dse))
		 (handler exp se dse))) )
	(when (and (not cs) (eq? exp exp2))
	  (##sys#syntax-error-hook
	   (string-append
	    "syntax transformer for `" (symbol->string name)
	    "' returns original form, which would result in endless expansion")
	   exp))
	(dd `(,name --> ,exp2))
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
	    (call-handler head (cadr mdef) exp (car mdef) #f)
	    #t))
	  (else (values exp #f)) ) )
  (let loop ((exp exp))
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
		       (cond [(symbol? bindings) ; expand named let
			      (##sys#check-syntax 'let body '(_ #((variable _) 0) . #(_ 1)) #f dse)
			      (let ([bs (cadr body)])
				(values
				 `(##core#app
				   (##core#letrec
				    ([,bindings (##core#loop-lambda ,(map (lambda (b) (car b)) bs) ,@(cddr body))])
				    ,bindings)
				   ,@(##sys#map cadr bs) )
				 #t) ) ]
			     [else (values exp #f)] ) ) ]
		    [(and (memq head2 '(set! ##core#set!)) ; "setter" syntax
			  (pair? body)
			  (pair? (car body)) )
		     (let ([dest (car body)])
		       (##sys#check-syntax 'set! body '(#(_ 1) _) #f dse)
		       (values
			(append (list (list '##sys#setter (car dest)))
				(cdr dest)
				(cdr body) ) 
			#t) ) ]
		    ((and cs? (symbol? head2) (##sys#get head2 '##compiler#compiler-syntax)) =>
		     (lambda (cs)
		       (let ((result (call-handler head (car cs) exp (cdr cs) #t)))
			 (cond ((eq? result exp) (expand head exp head2))
			       (else
				(when ##sys#compiler-syntax-hook
				  (##sys#compiler-syntax-hook head result))
				(loop result))))))
		    [else (expand head exp head2)] ) )
	    (values exp #f) ) )
      (values exp #f) ) ) )

(define ##sys#compiler-syntax-hook #f)
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
	     (dm "(ALIAS) global alias " sym " in " (module-name mod))
	     (unless assign (##sys#register-undefined sym mod))
	     (##sys#module-rename sym (module-name mod))))
	  (else sym)))
  (cond ((##sys#qualified-symbol? sym) sym)
	((##sys#get sym '##core#primitive) =>
	 (lambda (p)
	   (dm "(ALIAS) primitive: " p)
	   p))
	((##sys#get sym '##core#aliased) 
	 (dm "(ALIAS) marked: " sym)
	 sym)
	((assq sym (##sys#current-environment)) =>
	 (lambda (a)
	   (dm "(ALIAS) in current environment: " sym)
	   (let ((sym2 (cdr a)))
	     (if (pair? sym2)		; macro (*** can this be?)
		 (mrename sym)
		 (or (##sys#get sym2 '##core#primitive) sym2)))))
	(else (mrename sym))))


;;; User-level macroexpansion

(define (##sys#expand exp #!optional (se (##sys#current-environment)) cs?)
  (let loop ((exp exp))
    (let-values (((exp2 m) (##sys#expand-0 exp se cs?)))
      (if m
	  (loop exp2)
	  exp2) ) ) )

(define expand ##sys#expand)


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
	(let loop ([mode 0]		; req=0, opt=1, rest=2, key=3, end=4
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
		       (unless rvar (set! rvar llist))
		       (set! hasrest llist)
		       (loop 4 req opt '() '()) ) ) ]
		[(not (pair? llist))
		 (err "invalid lambda list syntax") ]
		[else
		 (let* ((var (car llist))
			(x (or (and (symbol? var) (not (eq? 3 mode)) (lookup var se)) var))
			(r (cdr llist)))
		   (case x
		     [(#!optional)
		      (unless rvar (set! rvar (macro-alias 'tmp se)))
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
		      (cond [(symbol? var)
			     (case mode
			       [(0) (loop 0 (cons var req) '() '() r)]
			       [(1) (loop 1 req (cons (list var #f) opt) '() r)]
			       [(2) (err "invalid lambda list syntax after `#!rest' marker")]
			       [else (loop 3 req opt (cons (list var) key) r)] ) ]
			    [(and (list? var) (eq? 2 (length var)))
			     (case mode
			       [(0) (err "invalid required argument syntax")]
			       [(1) (loop 1 req (cons var opt) '() r)]
			       [(2) (err "invalid lambda list syntax after `#!rest' marker")]
			       [else (loop 3 req opt (cons var key) r)] ) ]
			    [else (err "invalid lambda list syntax")] ) ] ) ) ] ) ) ) ) ) )


;;; Expansion of bodies (and internal definitions)

(define ##sys#canonicalize-body
  (let ([reverse reverse]
	[map map] )
    (lambda (body #!optional (se (##sys#current-environment)) cs?)
      (define (fini vars vals mvars mvals body)
	(if (and (null? vars) (null? mvars))
	    (let loop ([body2 body] [exps '()])
	      (if (not (pair? body2)) 
		  (cons 
		   '##core#begin
		   body) ; no more defines, otherwise we would have called `expand'
		  (let ([x (car body2)])
		    (if (and (pair? x) 
			     (let ((d (car x)))
			       (and (symbol? d)
				    (or (eq? (or (lookup d se) d) 'define)
					(eq? (or (lookup d se) d) 'define-values)))) )
			(cons
			 '##core#begin
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
	   (cond (done `((,(macro-alias 'letrec-syntax se)
			  ,(map cdr (reverse defs)) ,@body) ))
		 ((not (pair? body)) (loop body defs #t))
		 ((and (list? (car body))
		       (>= 3 (length (car body))) 
		       (symbol? (caar body))
		       (eq? 'define-syntax (or (lookup (caar body) se) (caar body))))
		  (let ((def (car body)))
		    (loop 
		     (cdr body) 
		     (cons (if (pair? (cadr def))
			       `(define-syntax ,(caadr def)
				  (,(macro-alias 'lambda se) ,(cdadr def) ,@(cddr def)))
			       def)
			   defs) 
		     #f)))
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
				  (##sys#check-syntax
				   'define x '(define (variable . lambda-list) . #(_ 1)) #f se)
				  (loop rest
					(cons (car head) vars)
					(cons `(##core#lambda ,(cdr head) ,@(cddr x)) vals)
					mvars mvals) ] ) ) ) ]
		      ((eq? 'define-syntax head)
		       (##sys#check-syntax 'define-syntax x '(define-syntax _ . #(_ 1)) se)
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
		       (let ([x2 (##sys#expand-0 x se cs?)])
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

(define (##sys#syntax-rules-mismatch input)
  (##sys#syntax-error-hook "no rule matches form" input))

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

(define (er-macro-transformer x) x)

(define ((##sys#er-transformer handler) form se dse)
  (let ((renv '()))			; keep rename-environment for this expansion
    (define (rename sym)
      (cond ((assq sym renv) => 
	     (lambda (a) 
	       (dd `(RENAME/RENV: ,sym --> ,(cdr a)))
	       (cdr a)))
	    ((lookup sym se) => 
	     (lambda (a)
	       (cond ((symbol? a)
		      (dd `(RENAME/LOOKUP: ,sym --> ,a))
		      a)
		     (else
		      (let ((a2 (macro-alias sym se)))
			(dd `(RENAME/LOOKUP/MACRO: ,sym --> ,a2))
			(set! renv (cons (cons sym a2) renv))
			a2)))))
	    (else
	     (let ((a (macro-alias sym se)))
	       (dd `(RENAME: ,sym --> ,a))
	       (set! renv (cons (cons sym a) renv))
	       a))))
    (define (compare s1 s2)
      (let ((result
	     (if (and (symbol? s1) (symbol? s2))
		 (let ((ss1 (or (##sys#get s1 '##core#macro-alias)
				(lookup2 1 s1 dse)
				s1) )
		       (ss2 (or (##sys#get s2 '##core#macro-alias)
				(lookup2 2 s2 dse)
				s2) ) )
		   (cond ((symbol? ss1)
			  (cond ((symbol? ss2) 
				 (eq? (or (##sys#get ss1 '##core#primitive) ss1)
				      (or (##sys#get ss2 '##core#primitive) ss2)))
				((assq ss1 (##sys#macro-environment)) =>
				 (lambda (a) (eq? (cdr a) ss2)))
				(else #f) ) )
			 ((symbol? ss2)
			  (cond ((assq ss2 (##sys#macro-environment)) =>
				 (lambda (a) (eq? ss1 (cdr a))))
				(else #f)))
			 (else (eq? ss1 ss2))))
		 (eq? s1 s2))) )
	(dd `(COMPARE: ,s1 ,s2 --> ,result)) 
	result))
    (define (lookup2 n sym dse)
      (let ((r (lookup sym dse)))
	(dd "  (lookup/DSE " (list n) ": " sym " --> " 
	    (if (and r (pair? r))
		'<macro>
		r)
	    ")")
	r))
    (handler form rename compare) ) )


;;; Macro definitions:

(define (##sys#expand-import x r c import-env macro-env meta? loc)
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
				     (##sys#current-meta-environment (##sys#current-meta-environment))
				     (##sys#macro-environment (##sys#meta-macro-environment)))
			(##sys#load il #f #f))
		      (set! mod (##sys#find-module mname)))
		  (else
		   (syntax-error
		    loc "cannot import from undefined module" 
		    mname)))))
	(let ((vexp (module-vexports mod))
	      (sexp (module-sexports mod)))
	  (cons vexp sexp))))	  
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
	(if meta?
	    (set-module-meta-import-forms! 
	     cm
	     (append (module-meta-import-forms cm) (cdr x)))
	    (set-module-import-forms!
	     cm 
	     (append (module-import-forms cm) (cdr x)))))
      (for-each
       (lambda (spec)
	 (let* ((vs (import-spec spec))
		(vsv (car vs))
		(vss (cdr vs)))
	   (dd `(IMPORT: ,loc))
	   (dd `(V: ,(if cm (module-name cm) '<toplevel>) ,(map-se vsv)))
	   (dd `(S: ,(if cm (module-name cm) '<toplevel>) ,(map-se vss)))
	   (##sys#mark-imported-symbols vsv) ; mark imports as ##core#aliased
	   (for-each
	    (lambda (imp)
	      (let ((id (car imp))
		    (aid (cdr imp)))
		(and-let* ((a (assq id (import-env)))
			   ((not (eq? aid (cdr a)))))
		  (##sys#warn "re-importing already imported identifier" id))))
	    vsv)
	   (for-each
	    (lambda (imp)
	      (and-let* ((a (assq (car imp) (macro-env)))
			 ((not (eq? (cdr imp) (cdr a)))))
		(##sys#warn "re-importing already imported syntax" (car imp))) )
	    vss)
	   (import-env (append vsv (import-env)))
	   (macro-env (append vss (macro-env)))))
       (cdr x))
      '(##core#undefined))))

(##sys#extend-macro-environment
 'import '() 
 (##sys#er-transformer 
  (cut ##sys#expand-import <> <> <> ##sys#current-environment ##sys#macro-environment
       #f 'import) ) )

(##sys#extend-macro-environment
 'import-for-syntax '() 
 (##sys#er-transformer 
  (cut ##sys#expand-import <> <> <> ##sys#current-meta-environment ##sys#meta-macro-environment 
       #t 'import-for-syntax) ) )

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
	      (cond ((c %else (car clause)) `(##core#begin ,@(cdr clause)))
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
				 (##core#begin ,@(cdr clause))
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
	    (%if (r 'if))
	    (%or (r 'or))
	    (%else (r 'else)))
	`(let ((,tmp ,exp))
	   ,(let expand ((clauses body))
	      (if (not (pair? clauses))
		  '(##core#undefined)
		  (let ((clause (car clauses))
			(rclauses (cdr clauses)) )
		    (##sys#check-syntax 'case clause '#(_ 1))
		    (if (c %else (car clause))
			`(##core#begin ,@(cdr clause))
			`(,%if (,%or ,@(##sys#map
					(lambda (x) `(##sys#eqv? ,tmp ',x)) (car clause)))
			       (##core#begin ,@(cdr clause)) 
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
	  (%if (r 'if)))
      `(,%let ,dovar ,(##sys#map (lambda (b) (list (car b) (car (cdr b)))) bindings)
	      (,%if ,(car test)
		    ,(let ((tbody (cdr test)))
		       (if (eq? tbody '())
			   '(##core#undefined)
			   `(##core#begin ,@tbody) ) )
		    (##core#begin
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
				    `(##core#begin ,@rest) ) ) )
			     ((test id) `(##core#begin ,@(cdr clause)))
			     (else (expand rclauses)) ) ) ) ) ) ) ) ) ) ) )

(##sys#extend-macro-environment
 'require-library
 '()
 (##sys#er-transformer
  (lambda (x r c)
    (let ((ids (cdr x)))
      `(##core#require-extension ,ids #f) ) ) ) )

(##sys#extend-macro-environment
 'require-extension
 '()
 (##sys#er-transformer
  (lambda (x r c)
    (let ((ids (cdr x)))
      `(##core#require-extension ,ids #t) ) ) ) )

(##sys#extend-macro-environment
 'module
 '()
 (##sys#er-transformer
  (lambda (x r c)
    (##sys#check-syntax 'module x '(_ symbol _ . #(_ 0)))
    `(##core#module 
      ,(cadr x)
      ,(if (eq? '* (strip-syntax (caddr x))) 
	   #t 
	   (caddr x))
      ,@(cdddr x)))))

(##sys#extend-macro-environment
 'begin-for-syntax
 '()
 (##sys#er-transformer
  (lambda (x r c)
    (##sys#check-syntax 'begin-for-syntax x '(_ . #(_ 0)))
    (##sys#register-meta-expression `(##core#begin ,@(cdr x)))
    `(##core#elaborationtimeonly (##core#begin ,@(cdr x))))))

(##sys#extend-macro-environment
 'export
 '()
 (##sys#er-transformer
  (lambda (x r c)
    (let ((exps (cdr x))
	  (mod (##sys#current-module)))
      (unless mod
	(syntax-error 'export "`export' used outside module body"))
      (for-each
       (lambda (exp)
	 (when (and (not (symbol? exp)) 
		    (let loop ((iexp exp))
		      (cond ((null? iexp) #f)
			    ((not (pair? iexp)) #t)
			    ((not (symbol? (car iexp))) #t)
			    (else (loop (cdr iexp))))))
	   (syntax-error 'export "invalid export syntax" exp (module-name mod))))
       exps)
      (set-module-export-list! 
       mod
       (append (module-export-list mod) 
	       (map ##sys#strip-syntax exps)))
      '(##sys#void)))))


;;; syntax-rules

(include "synrules.scm")


;;; the base macro environment ("scheme", essentially)

(define (##sys#macro-subset me0 #!optional parent-env)
  (let ((se (let loop ((me (##sys#macro-environment)))
	      (if (or (null? me) (eq? me me0))
		  '()
		  (cons (car me) (loop (cdr me)))))))
    (##sys#fixup-macro-environment se parent-env)))

(define (##sys#fixup-macro-environment se #!optional parent-env)
  (let ((se2 (if parent-env (##sys#append se parent-env) se)))
    (for-each				; fixup se
     (lambda (sdef)
       (when (pair? (cdr sdef))
	 (set-car!
	  (cdr sdef) 
	  (if (null? (cadr sdef)) 
	      se2
	      (##sys#append (cadr sdef) se2)))))
     se)
    se))

(define ##sys#default-macro-environment
  (##sys#fixup-macro-environment (##sys#macro-environment)))


;;; low-level module support

(define ##sys#meta-macro-environment (make-parameter (##sys#macro-environment)))
(define ##sys#current-module (make-parameter #f))

(declare 
  (hide make-module module? %make-module
	module-name module-vexports module-sexports
	set-module-vexports! set-module-sexports!
	module-export-list set-module-export-list! 
	module-defined-list set-module-defined-list!
	module-import-forms set-module-import-forms!
	module-meta-import-forms set-module-meta-import-forms!
	module-exist-list set-module-exist-list!
	module-meta-expressions set-module-meta-expressions!
	module-defined-syntax-list set-module-defined-syntax-list!))

(define-record-type module
  (%make-module name export-list defined-list exist-list defined-syntax-list
		undefined-list import-forms meta-import-forms meta-expressions 
		vexports sexports) 
  module?
  (name module-name)			; SYMBOL
  (export-list module-export-list set-module-export-list!) ; (SYMBOL | (SYMBOL ...) ...)
  (defined-list module-defined-list set-module-defined-list!) ; ((SYMBOL . VALUE) ...)    - *exported* value definitions
  (exist-list module-exist-list set-module-exist-list!)	      ; (SYMBOL ...)    - only for checking refs to undef'd
  (defined-syntax-list module-defined-syntax-list set-module-defined-syntax-list!) ; ((SYMBOL . VALUE) ...)
  (undefined-list module-undefined-list set-module-undefined-list!) ; (SYMBOL ...)
  (import-forms module-import-forms set-module-import-forms!)	    ; (SPEC ...)
  (meta-import-forms module-meta-import-forms set-module-meta-import-forms!)	    ; (SPEC ...)
  (meta-expressions module-meta-expressions set-module-meta-expressions!) ; (EXP ...)
  (vexports module-vexports set-module-vexports!)	      ; (SYMBOL . SYMBOL)
  (sexports module-sexports set-module-sexports!) )	      ; ((SYMBOL SE TRANSFORMER) ...)

(define ##sys#module-name module-name)

(define (##sys#module-exports m)
  (values 
   (module-export-list m)
   (module-vexports m)
   (module-sexports m)))

(define (make-module name explist vexports sexports)
  (%make-module name explist '() '() '() '() '() '() '() vexports sexports))

(define (##sys#find-module name #!optional (err #t))
  (cond ((assq name ##sys#module-table) => cdr)
	(err (error 'import "module not found" name))
	(else #f)))

(declare (not inline ##sys#toplevel-definition-hook))

(define (##sys#toplevel-definition-hook sym mod exp val) #f)

(define (##sys#register-meta-expression exp)
  (and-let* ((mod (##sys#current-module)))
    (set-module-meta-expressions! mod (cons exp (module-meta-expressions mod)))))

(define (check-for-redef sym env senv)
  (and-let* ((a (assq sym env)))
    (##sys#warn "redefinition of imported value binding" sym) )
  (and-let* ((a (assq sym senv)))
    (##sys#warn "redefinition of imported syntax binding" sym)))

(define (##sys#register-export sym mod)
  (when mod
    (let ((exp (or (eq? #t (module-export-list mod))
		   (##sys#find-export sym mod #t)))
	  (ulist (module-undefined-list mod)))
      (##sys#toplevel-definition-hook	; in compiler, hides unexported bindings
       (##sys#module-rename sym (module-name mod)) 
       mod exp #f)
      (when (memq sym ulist)
	(set-module-undefined-list! mod (##sys#delq sym ulist)))
      (check-for-redef sym (##sys#current-environment) (##sys#macro-environment))
      (set-module-exist-list! mod (cons sym (module-exist-list mod)))
      (when exp
	(dm "defined: " sym)
	(set-module-defined-list! 
	 mod
	 (cons (cons sym #f)
	       (module-defined-list mod)))))) )

(define (##sys#register-syntax-export sym mod val)
  (when mod
    (let ((exp (or (eq? #t (module-export-list mod))
		   (##sys#find-export sym mod #t)))
	  (ulist (module-undefined-list mod))
	  (mname (module-name mod)))
      (when (memq sym ulist)
	(##sys#warn "use of syntax precedes definition" sym))
      (check-for-redef sym (##sys#current-environment) (##sys#macro-environment))
      (dm "defined syntax: " sym)
      (when exp
	(set-module-defined-list! 
	 mod
	 (cons (cons sym val)
	       (module-defined-list mod))) )
      (set-module-defined-syntax-list! 
       mod
       (cons (cons sym val) (module-defined-syntax-list mod))))))

(define (##sys#register-undefined sym mod)
  (when mod
    (let ((ul (module-undefined-list mod)))
      (unless (memq sym ul)
	(set-module-undefined-list! mod (cons sym ul))))))

(define (##sys#register-module name explist #!optional (vexports '()) (sexports '()))
  (let ((mod (make-module name explist vexports sexports)))
    (set! ##sys#module-table (cons (cons name mod) ##sys#module-table))
    mod) )

(define (##sys#mark-imported-symbols se)
  (for-each
   (lambda (imp)
     (when (and (symbol? (cdr imp)) (not (eq? (car imp) (cdr imp))))
       (dm `(MARKING: ,(cdr imp)))
       (##sys#put! (cdr imp) '##core#aliased #t)))
   se))

(define (module-indirect-exports mod)
  (let ((exports (module-export-list mod))
	(mname (module-name mod))
	(dlist (module-defined-list mod)))
    (define (indirect? id)
      (let loop ((exports exports))
	(and (not (null? exports))
	     (or (and (pair? (car exports))
		      (memq id (cdar exports)))
		 (loop (cdr exports))))))
    (define (warn msg id)
      (##sys#warn
       (string-append msg " in module `" (symbol->string mname) "'")
       id))
    (if (eq? #t exports)
	'()
	(let loop ((exports exports))	; walk export list
	  (cond ((null? exports) '())
		((symbol? (car exports)) (loop (cdr exports))) ; normal export
		(else
		 (let loop2 ((iexports (cdar exports))) ; walk indirect exports for a given entry
		   (cond ((null? iexports) (loop (cdr exports)))
			 ((assq (car iexports) (##sys#macro-environment))
			  (warn "indirect export of syntax binding" (car iexports))
			  (loop2 (cdr iexports)))
			 ((assq (car iexports) dlist) => ; defined in current module?
			  (lambda (a) 
			    (cons 
			     (cons 
			      (car iexports)
			      (or (cdr a) (##sys#module-rename (car iexports) mname)))
			     (loop2 (cdr iexports)))))
			 ((assq (car iexports) (##sys#current-environment)) =>
			  (lambda (a)	; imported in current env.
			    (cond ((symbol? (cdr a)) ; not syntax
				   (cons (cons (car iexports) (cdr a)) (loop2 (cdr iexports))) )
				  (else
				   (warn "indirect reexport of syntax" (car iexports))
				   (loop2 (cdr iexports))))))
			 (else 
			  (warn "indirect export of unknown binding" (car iexports))
			  (loop2 (cdr iexports)))))))))))

(define (merge-se . ses)		; later occurrences take precedence to earlier ones
  (let ((se (apply append ses)))
    (dm "merging " (length ses) " se's with total length of " (length se))
    (let ((se2
	   (let loop ((se se))
	     (cond ((null? se) '())
		   ((assq (caar se) (cdr se)) (loop (cdr se)))
		   (else (cons (car se) (loop (cdr se))))))))
      (dm "  merged has length " (length se2))
      se2)))

(define (##sys#compiled-module-registration mod)
  (let ((dlist (module-defined-list mod))
	(mname (module-name mod))
	(ifs (module-import-forms mod))
	(sexports (module-sexports mod))
	(mifs (module-meta-import-forms mod)))
    `(,@(if (pair? ifs) `((eval '(import ,@ifs))) '())
      ,@(if (pair? mifs) `((import ,@mifs)) '())
      ,@(reverse (map ##sys#strip-syntax (module-meta-expressions mod)))
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
		   (cond ((pair? a) 
			  `(cons ',(car sexport) ,(##sys#strip-syntax (cdr a))))
			 (else
			  (dm "re-exported syntax" name mname)
			  `',name))))
	       sexports))
       (list 
	,@(if (null? sexports)
	      '() 			; no syntax exported - no more info needed
	      (let loop ((sd (module-defined-syntax-list mod)))
		(cond ((null? sd) '())
		      ((assq (caar sd) sexports) (loop (cdr sd)))
		      (else
		       (let ((name (caar sd)))
			 (cons `(cons ',(caar sd) ,(##sys#strip-syntax (cdar sd)))
			       (loop (cdr sd)))))))))))))

(define (##sys#register-compiled-module name iexports vexports sexports #!optional
					(sdefs '()))
  (define (find-reexport name)
    (let ((a (assq name (##sys#macro-environment))))
      (if (and a (pair? (cdr a)))
	  a
	  (##sys#error
	   'import "cannot find implementation of re-exported syntax"
	   name))))
  (let* ((sexps
	  (map (lambda (se)
		 (if (symbol? se)
		     (find-reexport se)
		     (list (car se) #f (##sys#er-transformer (cdr se)))))
	       sexports))
	 (iexps 
	  (map (lambda (ie)
		 (if (pair? (cdr ie))
		     (list (car ie) (cadr ie) (##sys#er-transformer (caddr ie)))
		     ie))
	       iexports))
	 (nexps
	  (map (lambda (ne)
		 (list (car ne) #f (##sys#er-transformer (cdr ne))))
	       sdefs))
	 (mod (make-module name '() vexports sexps))
	 (senv (merge-se 
		(##sys#macro-environment)
		(##sys#current-environment)
		iexps vexports sexps nexps)))
    (##sys#mark-imported-symbols iexps)
    (for-each
     (lambda (sexp)
       (set-car! (cdr sexp) senv))
     sexps)
    (for-each
     (lambda (iexp)
       (when (pair? (cdr iexp))
	 (set-car! (cdr iexp) senv)))
     iexps)
    (for-each
     (lambda (nexp)
       (set-car! (cdr nexp) senv))
     nexps)
    (set! ##sys#module-table (cons (cons name mod) ##sys#module-table)) 
    mod))

(define (##sys#primitive-alias sym)
  (let ((palias 
	 (##sys#string->symbol 
	  (##sys#string-append "#%" (##sys#slot sym 1)))))
    (##sys#put! palias '##core#primitive sym)
    palias))

(define (##sys#register-primitive-module name vexports #!optional (sexports '()))
  (let* ((me (##sys#macro-environment))
	 (mod (make-module 
	       name '()
	       (map (lambda (ve)
		      (if (symbol? ve)
			  (cons ve (##sys#primitive-alias ve))
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
  (let ((exports (module-export-list mod)))
    (let loop ((xl (if (eq? #t exports) (module-exists-list mod) exports)))
      (cond ((null? xl) #f)
	    ((eq? sym (car xl)))
	    ((pair? (car xl))
	     (or (eq? sym (caar xl))
		 (and indirect (memq sym (cdar xl)))
		 (loop (cdr xl))))
	    (else (loop (cdr xl)))))))

(define (##sys#finalize-module mod)
  (let* ((explist (module-export-list mod))
	 (name (module-name mod))
	 (dlist (module-defined-list mod))
	 (elist (module-exist-list mod))
	 (missing #f)
	 (sdlist (map (lambda (sym) (assq (car sym) (##sys#macro-environment)))
		      (module-defined-syntax-list mod)))
	 (sexports
	  (if (eq? #t explist)
	      sdlist
	      (let loop ((me (##sys#macro-environment)))
		(cond ((null? me) '())
		      ((##sys#find-export (caar me) mod #f)
		       (cons (car me) (loop (cdr me))))
		      (else (loop (cdr me)))))))
	 (vexports
	  (let loop ((xl (if (eq? #t explist) elist explist)))
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
			      (let ((a (assq id (##sys#current-environment))))
				(cond ((and a (symbol? (cdr a))) 
				       (dm "reexporting: " id " -> " (cdr a))
				       (cdr a)) 
				      ((not def)
				       (set! missing #t)
				       (##sys#warn 
					(string-append 
					 "exported identifier for module `" 
					 (symbol->string name)
					 "' has not been defined")
					id)
				       #f)
				      (else (##sys#module-rename id name)))))))
		       (loop (cdr xl)))))))))
    (for-each
     (lambda (u)
       (unless (memq u elist)
	 (set! missing #t)
	 (##sys#warn "reference to possibly unbound identifier" u)
	 (and-let* ((a (##sys#get u '##core#db)))
	   (if (= 1 (length a))
	       (##sys#warn
		(string-append 
		 "  suggesting: `(import " (symbol->string (cadar a)) 
		 ")'"))
	       (##sys#warn
		(string-append
		 "  suggesting one of:\n"
		 (let loop ((lst a))
		   (if (null? lst)
		       ""
		       (string-append
			"Warning:     `(import " (symbol->string (cadar lst)) ")'\n"
			(loop (cdr lst)))))))))))
     (module-undefined-list mod))
    (when missing
      (##sys#error "module unresolved" name))
    (let* ((exports 
	    (map (lambda (exp)
		   (cond ((symbol? (cdr exp)) exp)
			 ((assq (car exp) (##sys#macro-environment)))
			 (else (##sys#error "(internal) indirect export not found" (car exp)))) )
		 (module-indirect-exports mod)))
	   (new-se (merge-se 
		    (##sys#macro-environment) 
		    (##sys#current-environment) 
		    exports)))
      (##sys#mark-imported-symbols exports)
      (for-each
       (lambda (m)
	 (let ((se (merge-se (cadr m) new-se)))
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
