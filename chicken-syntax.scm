;;;; chicken-syntax.scm - non-standard syntax extensions
;
; Copyright (c) 2008-2009, The Chicken Team
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


(declare
  (unit chicken-syntax)
  (disable-interrupts)
  (no-bound-checks)
  (no-procedure-checks)
  (fixnum) )

(##sys#provide
 'chicken-more-macros 			; historical, remove later
 'chicken-syntax)


;;; Non-standard macros:

(define ##sys#chicken-macro-environment
  (let ((me0 (##sys#macro-environment)))

(##sys#extend-macro-environment
 'define-record '()
 (##sys#er-transformer
  (lambda (x r c)
    (##sys#check-syntax 'define-record x '(_ symbol . #(symbol 0)))
    (let* ((name (cadr x))
	   (slots (cddr x))
	   (prefix (symbol->string name))
	   (%quote (r 'quote))
	   (setters (memq #:record-setters ##sys#features))
	   (%begin (r 'begin))
	   (%define (r 'define))
	   (%getter-with-setter (r 'getter-with-setter))
	   (%lambda (r 'lambda)) )
      `(,%begin
	  (,%define 
	   ,(string->symbol (string-append "make-" prefix))
	   (,%lambda ,slots (##sys#make-structure (,%quote ,name) ,@slots)) )
	  (,%define
	   ,(string->symbol (string-append prefix "?"))
	   (,%lambda (x) (##sys#structure? x ',name)) )
	  ,@(let mapslots ((slots slots) (i 1))
	      (if (eq? slots '())
		  slots
		  (let* ((slotname (symbol->string (##sys#slot slots 0)))
			 (setr (string->symbol (string-append prefix "-" slotname "-set!")))
			 (getr (string->symbol (string-append prefix "-" slotname)) ) )
		    (cons
		     `(,%begin
			(,%define
			 ,setr
			 (,%lambda (x val)
				   (##core#check (##sys#check-structure x (,%quote ,name)))
				   (##sys#block-set! x ,i val) ) )
			(,%define
			 ,getr
			 ,(if setters
			      `(,%getter-with-setter
				(,%lambda (x) 
					  (##core#check (##sys#check-structure x (,%quote ,name)))
					  (##sys#block-ref x ,i) )
				,setr)
			      `(,%lambda (x)
					 (##core#check (##sys#check-structure x (,%quote ,name)))
					 (##sys#block-ref x ,i) ) ) ) )
		     (mapslots (##sys#slot slots 1) (fx+ i 1)) ) ) ) ) ) ) ) ) )

(##sys#extend-macro-environment
 'receive
 '()
 (##sys#er-transformer
 (lambda (form r c)
   (let ((%lambda (r 'lambda))
	 (%let (r 'let)))
     (##sys#check-syntax 'receive form '(_ _ . #(_ 0)))
     (cond ((null? (cddr form))
	    `(##sys#call-with-values (,%lambda () ,@(cdr form)) ##sys#list) )
	   (else
	    (##sys#check-syntax 'receive form '(_ lambda-list _ . #(_ 1)))
	    (let ((vars (cadr form))
		  (exp (caddr form))
		  (rest (cdddr form)))
	      (if (and (pair? vars) (null? (cdr vars)))
		  `(,%let ((,(car vars) ,exp)) ,@rest)
		  `(##sys#call-with-values 
		    (,%lambda () ,exp)
		    (,%lambda ,vars ,@rest)) ) ) ) ) ))))

(##sys#extend-macro-environment
 'time '()
 (##sys#er-transformer
 (lambda (form r c)
   (let ((rvar (r 't))
	 (%begin (r 'begin))
	 (%lambda (r 'lambda)))
    `(,%begin
       (##sys#start-timer)
       (##sys#call-with-values 
	(,%lambda () ,@(cdr form))
	(,%lambda ,rvar
		  (##sys#display-times (##sys#stop-timer))
		  (##sys#apply ##sys#values ,rvar) ) ) ) ) ) ) )

(##sys#extend-macro-environment
 'declare '()
 (##sys#er-transformer
 (lambda (form r c)
   `(##core#declare ,@(cdr form)))))

(##sys#extend-macro-environment
 'include '()
 (##sys#er-transformer
 (lambda (form r c)
   (##sys#check-syntax 'include form '(_ string))
   (let ((path (##sys#resolve-include-filename (cadr form) #t))
	 (%begin (r 'begin)))
     (when (load-verbose) (print "; including " path " ..."))
     `(,%begin
       ,@(with-input-from-file path
	   (lambda ()
	     (fluid-let ((##sys#current-source-filename path))
	       (do ([x (read) (read)]
		    [xs '() (cons x xs)] )
		   ((eof-object? x) 
		    (reverse xs))) ) ) ) ) ) ) ) )

(##sys#extend-macro-environment
 'assert '()
 (##sys#er-transformer
 (lambda (form r c)
   (##sys#check-syntax 'assert form '#(_ 1))
   (let* ((exp (cadr form))
	  (msg-and-args (cddr form))
	  (%if (r 'if))
	  (%quote (r 'quote))
	  (msg (if (eq? '() msg-and-args)
		   `(##core#immutable '"assertion failed")
		   (car msg-and-args) ) ) )
     `(,%if (##core#check ,exp)
	    (##core#undefined)
	    (##sys#error 
	     ,msg 
	     (,%quote ,exp)
	     ,@(if (fx> (length msg-and-args) 1)
		   (cdr msg-and-args)
		   '() ) ) ) ) )) )

(##sys#extend-macro-environment
 'ensure
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'ensure form '#(_ 3))
    (let ((pred (cadr form))
	  (exp (caddr form))
	  (args (cdddr form))
	  (tmp (r 'tmp))
	  (%let (r 'let))
	  (%if (r 'if)) )
      `(,%let ([,tmp ,exp])
	      (,%if (##core#check (,pred ,tmp))
		    ,tmp
		    (##sys#signal-hook
		     #:type-error
		     ,@(if (pair? args)
			   args
			   `((##core#immutable '"argument has incorrect type")
			     ,tmp ',pred) ) ) ) ) ) ) ) )

(##sys#extend-macro-environment
 'fluid-let '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'fluid-let form '(_ #((symbol _) 0) . _))
     (let* ((clauses (cadr form))
	   (body (cddr form))
	   (ids (##sys#map car clauses))
	   (new-tmps (##sys#map (lambda (x) (r (gensym))) clauses))
	   (old-tmps (##sys#map (lambda (x) (r (gensym))) clauses))
	   (%let (r 'let))
	   (%lambda (r 'lambda)))
       `(,%let (,@(map ##sys#list new-tmps (##sys#map cadr clauses))
		,@(map ##sys#list old-tmps
		       (let loop ((n (length clauses)))
			 (if (eq? n 0)
			     '()
			     (cons #f (loop (fx- n 1))) ) ) ) )
	       (##sys#dynamic-wind
		(,%lambda ()
			  ,@(map (lambda (ot id) `(##core#set! ,ot ,id))
				 old-tmps ids)
			  ,@(map (lambda (id nt) `(##core#set! ,id ,nt))
				 ids new-tmps)
			  (##core#undefined) )
		(,%lambda () ,@body)
		(,%lambda ()
			  ,@(map (lambda (nt id) `(##core#set! ,nt ,id))
				 new-tmps ids)
			  ,@(map (lambda (id ot) `(##core#set! ,id ,ot))
				 ids old-tmps)
			  (##core#undefined) ) ) ) ) )))

(##sys#extend-macro-environment
 'eval-when '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'eval-when form '#(_ 2))
    (let* ((situations (cadr form))
	   (%begin (r 'begin))
	   (body `(,%begin ,@(cddr form)))
	   (%eval (r 'eval))
	   (%compile (r 'compile))
	   (%load (r 'load))
	   (e #f)
	   (co #f)
	   (l #f))
      (let loop ([ss situations])
	(if (pair? ss)
	    (let ((s (car ss)))
	      (cond ((c s %eval) (set! e #t))
		    ((c s %load) (set! l #t))
		    ((c s %compile) (set! co #t))
		    (else (##sys#error "invalid situation specifier" (car ss)) ))
	      (loop (##sys#slot ss 1)) ) ) )
      (if (memq '#:compiling ##sys#features)
	  (cond [(and co l) `(##core#compiletimetoo ,body)]
		[co `(##core#compiletimeonly ,body)]
		[l body]
		[else '(##core#undefined)] )
	  (if e 
	      body
	      '(##core#undefined) ) ) ) ) ) )

(##sys#extend-macro-environment
 'parameterize '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'parameterize form '#(_ 2))
     (let* ((bindings (cadr form))
	    (body (cddr form))
	    (swap (r 'swap))
	    (%let (r 'let))
	    (%lambda (r 'lambda))
	    [params (##sys#map car bindings)]
	    [vals (##sys#map cadr bindings)]
	    [aliases (##sys#map (lambda (z) (r (gensym))) params)]
	    [aliases2 (##sys#map (lambda (z) (r (gensym))) params)] )
       `(,%let ,(##sys#append (map ##sys#list aliases params) (map ##sys#list aliases2 vals))
	  (,%let ((,swap (,%lambda ()
				   ,@(map (lambda (a a2)
					    `(,%let ((t (,a))) (,a ,a2)
						    (##core#set! ,a2 t)))
					  aliases aliases2) ) ) )
		 (##sys#dynamic-wind 
		  ,swap
		  (,%lambda () ,@body)
		  ,swap) ) ) ) )))

(##sys#extend-macro-environment
 'when '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'when form '#(_ 2))
    `(,(r 'if) ,(cadr form)
      (,(r 'begin) ,@(cddr form))))))

(##sys#extend-macro-environment
 'unless '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'unless form '#(_ 2))
    `(,(r 'if) ,(cadr form)
      (##core#undefined)
      (,(r 'begin) ,@(cddr form))))))

(##sys#extend-macro-environment
 'set!-values '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'set!-values form '(_ #(variable 0) _))
    (let ((vars (cadr form))
	  (exp (caddr form))
	  (%lambda (r 'lambda)))
      (cond ((null? vars)
	     ;; may this be simply "exp"?
	     `(##sys#call-with-values
	       (,%lambda () ,exp)
	       (,%lambda () (##core#undefined))) )
	    ((null? (cdr vars))
	     `(##core#set! ,(car vars) ,exp)) 
	    (else
	     (let ([aliases (map gensym vars)])
	       `(##sys#call-with-values
		 (,%lambda () ,exp)
		 (,%lambda ,aliases
			   ,@(map (lambda (v a)
				    `(##core#set! ,v ,a))
				  vars aliases) ) ) ) ) ) ))))

(##sys#extend-macro-environment
 'define-values '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'define-values form '(_ #(variable 0) _))
    (for-each (cut ##sys#register-export <> (##sys#current-module)) (cadr form))
    `(,(r 'set!-values) ,@(cdr form)))))

(##sys#extend-macro-environment
 'let-values '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'let-values form '(_ list . _))
    (let ((vbindings (cadr form))
	  (body (cddr form))
	  (%let (r 'let))
	  (%lambda (r 'lambda)))
      (letrec ((append* (lambda (il l)
			  (if (not (pair? il))
			      (cons il l)
			      (cons (car il)
				    (append* (cdr il) l)))))
	       (map* (lambda (proc l)
		       (cond ((null? l) '())
			     ((not (pair? l)) (proc l))
			     (else (cons (proc (car l)) (map* proc (cdr l))))))))
	(let* ([llists (map car vbindings)]
	       [vars (let loop ((llists llists) (acc '()))
		       (if (null? llists)
			   acc
			   (let* ((llist (car llists))
				  (new-acc
				   (cond ((list? llist) (append llist acc))
					 ((pair? llist) (append* llist acc))
					 (else (cons llist acc)))))
			     (loop (cdr llists) new-acc))))]
	       [aliases (map (lambda (v) (cons v (r (gensym v)))) vars)]
	       [lookup (lambda (v) (cdr (assq v aliases)))]
	       [llists2 (let loop ((llists llists) (acc '()))
			  (if (null? llists)
			      (reverse acc)
			      (let* ((llist (car llists))
				     (new-acc
				      (cond ((not (pair? llist)) (cons (lookup llist) acc))
					    (else (cons (map* lookup llist) acc)))))
				(loop (cdr llists) new-acc))))])
	  (let fold ([llists llists]
		     [exps (map (lambda (x) (cadr x)) vbindings)]
		     [llists2 llists2] )
	    (cond ((null? llists)
		   `(,%let ,(map (lambda (v) (##sys#list v (lookup v))) vars) ,@body) )
		  ((and (pair? (car llists2)) (null? (cdar llists2)))
		   `(,%let ((,(caar llists2) ,(car exps)))
			   ,(fold (cdr llists) (cdr exps) (cdr llists2)) ) )
		  (else
		   `(##sys#call-with-values
		     (,%lambda () ,(car exps))
		     (,%lambda ,(car llists2) ,(fold (cdr llists) (cdr exps) (cdr llists2))) ) ) ) ) ) ) ) ) ) )

(##sys#extend-macro-environment
 'let*-values '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'let*-values form '(_ list . _))
    (let ((vbindings (cadr form))
	  (body (cddr form))
	  (%let (r 'let))
	  (%let-values (r 'let-values)) )
      (let fold ([vbindings vbindings])
	(if (null? vbindings)
	    `(,%let () ,@body)
	    `(,%let-values (,(car vbindings))
			   ,(fold (cdr vbindings))) ) ) ))))

(##sys#extend-macro-environment
 'letrec-values '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'letrec-values form '(_ list . _))
    (let ((vbindings (cadr form))
	  (body (cddr form))
	  (%let (r 'let))
	  (%lambda (r 'lambda)))
      (let* ([vars (apply ##sys#append (map (lambda (x) (car x)) vbindings))] 
	     [aliases (map (lambda (v) (cons v (r (gensym v)))) vars)] 
	     [lookup (lambda (v) (cdr (assq v aliases)))] )
	`(,%let ,(map (lambda (v) (##sys#list v '(##core#undefined))) vars)
		,@(map (lambda (vb)
			 `(##sys#call-with-values 
			   (,%lambda () ,(cadr vb))
			   (,%lambda ,(map lookup (car vb))
				     ,@(map (lambda (v) `(##core#set! ,v ,(lookup v))) (car vb)) ) ) )
		       vbindings)
		,@body) ) ) ) ) )

(##sys#extend-macro-environment
 'nth-value 
 `((list-ref . ,(##sys#primitive-alias 'list-ref)))
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'nth-value form '(_ _ _))
    (let ((v (r 'tmp))
	  (%lambda (r 'lambda)))
      `(##sys#call-with-values
	(,%lambda () ,(caddr form))
	(,%lambda ,v (,(r 'list-ref) ,v ,(cadr form))))))))

(##sys#extend-macro-environment
 'define-inline '()
 (##sys#er-transformer
  (lambda (form r c)
    (let ((%lambda (r 'lambda)))
      (letrec ([quotify-proc 
		(lambda (xs id)
		  (##sys#check-syntax id xs '#(_ 1))
		  (let* ([head (car xs)]
			 [name (if (pair? head) (car head) head)]
			 [val (if (pair? head)
				  `(,%lambda ,(cdr head) ,@(cdr xs))
				  (cadr xs) ) ] )
		    (when (or (not (pair? val)) (not (c %lambda (car val))))
		      (syntax-error 
		       'define-inline "invalid substitution form - must be lambda"
		       name) )
		    (list name val) ) ) ] )
	`(##core#define-inline ,@(quotify-proc (cdr form) 'define-inline)))) ) ) )

(##sys#extend-macro-environment
 'and-let* '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'and-let* form '(_ #(_ 0) . _))
    (let ((bindings (cadr form))
	  (body (cddr form))
	  (%if (r 'if))
	  (%let (r 'let)))
      (let fold ([bs bindings])
	(if (null? bs)
	    `(,(r 'begin) ,@body)
	    (let ([b (car bs)]
		  [bs2 (cdr bs)] )
	      (cond [(not (pair? b)) `(,%if ,b ,(fold bs2) #f)]
		    [(null? (cdr b)) `(,%if ,(car b) ,(fold bs2) #f)]
		    [else
		     (let ((var (car b)))
		       `(,%let ((,var ,(cadr b)))
			 (,%if ,var ,(fold bs2) #f) ) ) ] ) ) ) ) ) ) ) )

(##sys#extend-macro-environment
 'select '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'select form '(_ _ . _))
    (let ((exp (cadr form))
	  (body (cddr form))
	  (tmp (r 'tmp))
	  (%if (r 'if))
	  (%else (r 'else))
	  (%or (r 'or))
	  (%begin (r 'begin)))
      `(,(r 'let) ((,tmp ,exp))
	,(let expand ((clauses body))
	   (if (not (pair? clauses))
	       '(##core#undefined)
	       (let ((clause (##sys#slot clauses 0))
		     (rclauses (##sys#slot clauses 1)) )
		 (##sys#check-syntax 'select clause '#(_ 1))
		 (if (c %else (car clause))
		     `(,%begin ,@(cdr clause))
		     `(,%if (,%or ,@(map (lambda (x) `(##sys#eqv? ,tmp ,x)) 
					 (car clause) ) )
			    (,%begin ,@(cdr clause)) 
			    ,(expand rclauses) ) ) ) ) ) ) ) ) ) )


;;; Optional argument handling:

;;; Copyright (C) 1996 by Olin Shivers.
;;;
;;; This file defines three macros for parsing optional arguments to procs:
;;; 	(LET-OPTIONALS  arg-list ((var1 default1) ...) . body)
;;; 	(LET-OPTIONALS* arg-list ((var1 default1) ...) . body)
;;; 	(:OPTIONAL rest-arg default-exp)
;;;
;;; The LET-OPTIONALS macro is defined using the Clinger/Rees
;;; explicit-renaming low-level macro system. You'll have to do some work to
;;; port it to another macro system.
;;;
;;; The LET-OPTIONALS* and :OPTIONAL macros are defined with simple
;;; high-level macros, and should be portable to any R4RS system.
;;;
;;; These macros are all careful to evaluate their default forms *only* if
;;; their values are needed.
;;;
;;; The only non-R4RS dependencies in the macros are ERROR 
;;; and CALL-WITH-VALUES.
;;; 	-Olin

;;; (LET-OPTIONALS arg-list ((var1 default1) ...) 
;;;   body
;;;   ...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This form is for binding a procedure's optional arguments to either
;;; the passed-in values or a default.
;;;
;;; The expression takes a rest list ARG-LIST and binds the VARi to
;;; the elements of the rest list. When there are no more elements, then
;;; the remaining VARi are bound to their corresponding DEFAULTi values.
;;; It is an error if there are more args than variables.
;;;
;;; - The default expressions are *not* evaluated unless needed.
;;;
;;; - When evaluated, the default expressions are carried out in the *outer*
;;;   environment. That is, the DEFAULTi forms do *not* see any of the VARi
;;;   bindings.
;;;
;;;   I originally wanted to have the DEFAULTi forms get eval'd in a LET*
;;;   style scope -- DEFAULT3 would see VAR1 and VAR2, etc. But this is
;;;   impossible to implement without side effects or redundant conditional
;;;   tests. If I drop this requirement, I can use the efficient expansion
;;;   shown below. If you need LET* scope, use the less-efficient 
;;;   LET-OPTIONALS* form defined below.
;;;
;;; Example:
;;; (define (read-string! str . maybe-args)
;;;   (let-optionals maybe-args ((port (current-input-port))
;;;                              (start 0)
;;;                              (end (string-length str)))
;;;     ...))
;;;
;;; expands to:
;;; 
;;; (let* ((body (lambda (port start end) ...))
;;;        (end-def (lambda (%port %start) (body %port %start <end-default>)))
;;;        (start-def (lambda (%port) (end-def %port <start-default>)))
;;;        (port-def  (lambda () (start-def <port-def>))))
;;;   (if (null? rest) (port-def)
;;;       (let ((%port (car rest))
;;; 	        (rest (cdr rest)))
;;; 	  (if (null? rest) (start-def %port)
;;; 	      (let ((%start (car rest))
;;; 		    (rest (cdr rest)))
;;; 	        (if (null? rest) (end-def %port %start)
;;; 		    (let ((%end (car rest))
;;; 			  (rest (cdr rest)))
;;; 		      (if (null? rest) (body %port %start %end)
;;; 			  (error ...)))))))))


;;; (LET-OPTIONALS args ((var1 default1) ...) body1 ...)

(##sys#extend-macro-environment
 'let-optionals 
 `((car . ,(##sys#primitive-alias 'car))
   (cdr . ,(##sys#primitive-alias 'cdr)))
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'let-optionals form '(_ _ . _))
    (let ((arg-list (cadr form))
	  (var/defs (caddr form))
	  (body (cdddr form))
	  (%if (r 'if))
	  (%let (r 'let))
	  (%lambda (r 'lambda)))

      ;; This guy makes the END-DEF, START-DEF, PORT-DEF definitions above.
      ;; I wish I had a reasonable loop macro.

      (define (make-default-procs vars body-proc defaulter-names defs rename)
	(let recur ((vars (reverse vars))
		    (defaulter-names (reverse defaulter-names))
		    (defs (reverse defs))
		    (next-guy body-proc))
	  (if (null? vars) '()
	      (let ((vars (cdr vars)))
		`((,(car defaulter-names)
		   (,%lambda ,(reverse vars)
			     (,next-guy ,@(reverse vars) ,(car defs))))
		  . ,(recur vars
			    (cdr defaulter-names)
			    (cdr defs)
			    (car defaulter-names)))))))


      ;; This guy makes the (IF (NULL? REST) (PORT-DEF) ...) tree above.

      (define (make-if-tree vars defaulters body-proc rest rename)
	(let recur ((vars vars) (defaulters defaulters) (non-defaults '()))
	  (if (null? vars)
	      `(,%if (##core#check (,(r 'null?) ,rest))
		     (,body-proc . ,(reverse non-defaults))
		     (##sys#error (##core#immutable '"too many optional arguments") ,rest))
	      (let ((v (car vars)))
		`(,%if (null? ,rest)
		       (,(car defaulters) . ,(reverse non-defaults))
		       (,%let ((,v (,(r 'car) ,rest)) ; we use car/cdr, because of rest-list optimization
			       (,rest (,(r 'cdr) ,rest)))
			      ,(recur (cdr vars)
				      (cdr defaulters)
				      (cons v non-defaults))))))))

      (##sys#check-syntax 'let-optionals var/defs '#((variable _) 0))
      (##sys#check-syntax 'let-optionals body '#(_ 1))
      (let* ((vars (map car var/defs))
	     (prefix-sym (lambda (prefix sym)
			   (string->symbol (string-append prefix (symbol->string sym)))))

	     ;; Private vars, one for each user var.
	     ;; We prefix the % to help keep macro-expanded code from being
	     ;; too confusing.
	     (vars2 (map (lambda (v) (r (prefix-sym "%" v)))
			 vars))

	     (defs (map cadr var/defs))
	     (body-proc (r 'body))

	     ;; A private var, bound to the value of the ARG-LIST expression.
	     (rest-var (r '%rest))

	     (defaulter-names (map (lambda (var) (r (prefix-sym "def-" var)))
				   vars))

	     (defaulters (make-default-procs vars2 body-proc
					     defaulter-names defs gensym))
	     (if-tree (make-if-tree vars2 defaulter-names body-proc
				    rest-var gensym)))

	`(,(r 'let*) ((,rest-var ,arg-list)
		      (,body-proc (,%lambda ,vars . ,body))
		      . ,defaulters)
	  ,if-tree) ) ))))


;;; (optional rest-arg default-exp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This form is for evaluating optional arguments and their defaults
;;; in simple procedures that take a *single* optional argument. It is
;;; a macro so that the default will not be computed unless it is needed.
;;; 
;;; REST-ARG is a rest list from a lambda -- e.g., R in
;;;     (lambda (a b . r) ...)
;;; - If REST-ARG has 0 elements, evaluate DEFAULT-EXP and return that.
;;; - If REST-ARG has 1 element, return that element.
;;; - If REST-ARG has >1 element, error.

(##sys#extend-macro-environment
 'optional 
 `((null? . ,(##sys#primitive-alias 'null?))
   (car . ,(##sys#primitive-alias 'car))
   (cdr . ,(##sys#primitive-alias 'cdr)) )
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'optional form '(_ _ . #(_ 0 1)))
    (let ((var (r 'tmp))
	  (%if (r 'if)))
      `(,(r 'let) ((,var ,(cadr form)))
	(,%if (,(r 'null?) ,var) 
	      ,(optional (cddr form) #f)
	      (,%if (##core#check (,(r 'null?) (,(r 'cdr) ,var)))
		    (,(r 'car) ,var)
		    (##sys#error
		     (##core#immutable '"too many optional arguments") 
		     ,var))))))))


;;; (LET-OPTIONALS* args ((var1 default1) ... [rest]) body1 ...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is just like LET-OPTIONALS, except that the DEFAULTi forms
;;; are evaluated in a LET*-style environment. That is, DEFAULT3 is evaluated
;;; within the scope of VAR1 and VAR2, and so forth.
;;;
;;; - If the last form in the ((var1 default1) ...) list is not a 
;;;   (VARi DEFAULTi) pair, but a simple variable REST, then it is
;;;   bound to any left-over values. For example, if we have VAR1 through
;;;   VAR7, and ARGS has 9 values, then REST will be bound to the list of
;;;   the two values of ARGS. If ARGS is too short, causing defaults to
;;;   be used, then REST is bound to '().
;;; - If there is no REST variable, then it is an error to have excess
;;;   values in the ARGS list.

(##sys#extend-macro-environment
 'let-optionals* 
 `((null? . ,(##sys#primitive-alias 'null?)))
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'let-optionals* form '(_ _ list . _))
    (let ((args (cadr form))
	  (var/defs (caddr form))
	  (body (cdddr form))
	  (%let (r 'let))
	  (%null? (r 'null?))
	  (%car (r 'car))
	  (%cdr (r 'cdr))
	  (%if (r 'if)))
      (let ((rvar (r 'tmp)))
	`(,%let ((,rvar ,args))
		,(let loop ([args rvar] [vardefs var/defs])
		   (if (null? vardefs)
		       `(,%if (##core#check (,%null? ,args))
			      (,%let () ,@body)
			      (##sys#error 
			       (##core#immutable '"too many optional arguments") 
			       ,args) )
		       (let ([head (car vardefs)])
			 (if (pair? head)
			     (let ((rvar2 (r 'tmp2)))
			       `(,%let ((,(car head) (,%if (,%null? ,args)
							   ,(cadr head)
							   (,%car ,args)))
					(,rvar2 (,%if (,%null? ,args) 
						      '()
						      (,%cdr ,args))) )
				       ,(loop rvar2 (cdr vardefs)) ) )
			     `(,%let ((,head ,args)) ,@body) ) ) ) ) ) ) ))))


;;; case-lambda (SRFI-16):

(##sys#extend-macro-environment
 'case-lambda 
 `((>= . ,(##sys#primitive-alias '>=))
   (car . ,(##sys#primitive-alias 'car))
   (cdr . ,(##sys#primitive-alias 'cdr))
   (eq? . ,(##sys#primitive-alias 'eq?)))
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'case-lambda form '(_ . _))
    (define (genvars n)
      (let loop ([i 0])
	(if (fx>= i n)
	    '()
	    (cons (r (gensym)) (loop (fx+ i 1))) ) ) )
    (require 'srfi-1)			; ugh...
    (let* ((mincount (apply min (map (lambda (c)
				       (##sys#decompose-lambda-list 
					(car c)
					(lambda (vars argc rest) argc) ) )
				     (cdr form))))
	   (minvars (genvars mincount))
	   (rvar (r 'rvar))
	   (lvar (r 'lvar))
	   (%lambda (r 'lambda))
	   (%let (r 'let))
	   (%>= (r '>=))
	   (%eq? (r 'eq?))
	   (%car (r 'car))
	   (%cdr (r 'cdr))
	   (%if (r 'if)))
      `(,%lambda ,(append minvars rvar)
		 (,%let ((,lvar (length ,rvar)))
			,(fold-right
			  (lambda (c body)
			    (##sys#decompose-lambda-list
			     (car c)
			     (lambda (vars argc rest)
			       (##sys#check-syntax 'case-lambda (car c) 'lambda-list)
			       `(,%if ,(let ([a2 (fx- argc mincount)])
					 (if rest
					     (if (zero? a2)
						 #t
						 `(,%>= ,lvar ,a2) )
					     `(,%eq? ,lvar ,a2) ) )
				      ,(receive (vars1 vars2)
					   (split-at! (take vars argc) mincount)
					 (let ((bindings
						(let build ((vars2 vars2) (vrest rvar))
						  (if (null? vars2)
						      (cond (rest `(,%let ((,rest ,vrest)) ,@(cdr c)))
							    ((null? (cddr c)) (cadr c))
							    (else `(,%let () ,@(cdr c))) )
						      (let ((vrest2 (r (gensym))))
							`(,%let ((,(car vars2) (,%car ,vrest))
								 (,vrest2 (,%cdr ,vrest)) )
								,(if (pair? (cdr vars2))
								     (build (cdr vars2) vrest2)
								     (build '() vrest2) ) ) ) ) ) ) )
					   (if (null? vars1)
					       bindings
					       `(,%let ,(map list vars1 minvars) ,bindings) ) ) )
				      ,body) ) ) )
			  '(##core#check (##sys#error (##core#immutable '"no matching clause in call to 'case-lambda' form")))
			  (cdr form))))))))


;;; Record printing:

(##sys#extend-macro-environment
 'define-record-printer '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'define-record-printer form '(_ _ . _))
    (let ([head (cadr form)]
	  [body (cddr form)])
      (cond [(pair? head)
	     (##sys#check-syntax 
	      'define-record-printer (cons head body)
	      '((symbol symbol symbol) . #(_ 1)))
	     `(##sys#register-record-printer 
	       ',(##sys#slot head 0)
	       (,(r 'lambda) ,(##sys#slot head 1) ,@body)) ]
	    [else
	     (##sys#check-syntax 'define-record-printer (cons head body) '(symbol _))
	     `(##sys#register-record-printer ',head ,@body) ] ) ))))


;;; Exceptions:

(##sys#extend-macro-environment
 'handle-exceptions 
 `((call-with-current-continuation . ,(##sys#primitive-alias 'call-with-current-continuation))
   (with-exception-handler . ,(##sys#primitive-alias 'with-exception-handler)))
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'handle-exceptions form '(_ variable _ . _))
  (let ((k (r 'k))
	(args (r 'args))
	(%lambda (r 'lambda)))
    `((,(r 'call-with-current-continuation)
       (,%lambda (,k)
	 (,(r 'with-exception-handler)
	  (,%lambda (,(cadr form)) (,k (,%lambda () ,(caddr form))))
	  (,%lambda ()
	    (##sys#call-with-values
	     (,%lambda () ,@(cdddr form))
	     (,%lambda 
	      ,args 
	      (,k (lambda () (##sys#apply ##sys#values ,args)))) ) ) ) ) ) ) ) ) ) )

(##sys#extend-macro-environment
 'condition-case 
 `((else . ,(##sys#primitive-alias 'else))
   (memv . ,(##sys#primitive-alias 'memv)))
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'condition-case form '(_ _ . _))
    (let ((exvar (r 'exvar))
	  (kvar (r 'kvar))
	  (%and (r 'and))
	  (%let (r 'let))
	  (%quote (r 'quote))
	  (%memv (r 'memv))
	  (%else (r 'else)))
      (define (parse-clause c)
	(let* ([var (and (symbol? (car c)) (car c))]
	       [kinds (if var (cadr c) (car c))]
	       [body (if var (cddr c) (cdr c))] )
	  (if (null? kinds)
	      `(,%else 
		,(if var
		     `(,%let ([,var ,exvar]) ,@body)
		     `(,%let () ,@body) ) )
	      `((,%and ,kvar ,@(map (lambda (k) `(,%memv (,%quote ,k) ,kvar)) kinds))
		,(if var
		     `(,%let ([,var ,exvar]) ,@body)
		     `(,%let () ,@body) ) ) ) ) )
      `(,(r 'handle-exceptions) ,exvar
	(,%let ([,kvar (,%and (##sys#structure? ,exvar (,%quote condition) )
			      (##sys#slot ,exvar 1))])
	       (,(r 'cond) ,@(map parse-clause (cddr form))
		(,%else (##sys#signal ,exvar)) ) )
	,(cadr form))))))


;;; SRFI-9:

(##sys#extend-macro-environment
 'define-record-type
 `((getter-with-setter . (##sys#primitive-alias 'getter-with-setter)))
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'define-record-type form '(_ variable #(variable 1) variable . _)) 
    (let* ((t (cadr form))
	  (conser (caddr form))
	  (pred (cadddr form))
	  (slots (cddddr form))
	  (%begin (r 'begin))
	  (%lambda (r 'lambda))
	  (%define (r 'define))
	  (%quote (r 'quote))
	  (%getter-with-setter (r 'getter-with-setter))
	  (vars (cdr conser))
	  (x (r 'x))
	  (y (r 'y))
	  (slotnames (map car slots)))
      `(,%begin
	(,%define ,conser
		  (##sys#make-structure 
		   (,%quote ,t)
		   ,@(map (lambda (sname)
			    (if (memq sname vars)
				sname
				'(##core#undefined) ) )
			  slotnames) ) )
	(,%define (,pred ,x) (##sys#structure? ,x (,%quote ,t)))
	,@(let loop ([slots slots] [i 1])
	    (if (null? slots)
		'()
		(let* ([slot (car slots)]
		       (setters (memq #:record-setters ##sys#features))
		       (setr? (pair? (cddr slot))) 
		       (getr `(,%lambda (,x)
					(##core#check (##sys#check-structure ,x (,%quote ,t)))
					(##sys#block-ref ,x ,i) ) ) )
		  `(,@(if setr?
			  `((,%define (,(caddr slot) ,x ,y)
				      (##core#check (##sys#check-structure ,x (,%quote ,t)))
				      (##sys#block-set! ,x ,i ,y)) )
			  '() )
		    (,%define ,(cadr slot) 
			      ,(if (and setr? setters)
				   `(,%getter-with-setter ,getr ,(caddr slot))
				   getr) )
		    ,@(loop (cdr slots) (add1 i)) ) ) ) ) ) ) ) ) )


;;; SRFI-26:

(##sys#extend-macro-environment
 'cut 
 `((apply . ,(##sys#primitive-alias 'apply)))
 (##sys#er-transformer
  (lambda (form r c)
    (let ((%<> (r '<>))
	  (%<...> (r '<...>))
	  (%apply (r 'apply))
	  (%begin (r 'begin))
	  (%lambda (r 'lambda)))
      (let loop ([xs (cdr form)] [vars '()] [vals '()] [rest #f])
	(if (null? xs)
	    (let ([rvars (reverse vars)]
		  [rvals (reverse vals)] )
	      (if rest
		  (let ([rv (r (gensym))])
		    `(,%lambda (,@rvars . ,rv)
			       (,%apply ,(car rvals) ,@(cdr rvals) ,rv) ) )
		  `(,%lambda ,rvars ((,%begin ,(car rvals)) ,@(cdr rvals)) ) ) )
	    (cond ((c %<> (car xs))
		   (let ([v (r (gensym))])
		     (loop (cdr xs) (cons v vars) (cons v vals) #f) ) )
		  ((c %<...> (car xs)) (loop '() vars vals #t))
		  (else (loop (cdr xs) vars (cons (car xs) vals) #f)) ) ) ) ) )))

(##sys#extend-macro-environment
 'cute 
 `((apply . ,(##sys#primitive-alias 'apply)))
 (##sys#er-transformer
  (lambda (form r c)
    (let ((%let (r 'let))
	  (%lambda (r 'lambda))
	  (%apply (r 'apply))
	  (%<> (r '<>))
	  (%<...> (r '<...>)))
      (let loop ([xs (cdr form)] [vars '()] [bs '()] [vals '()] [rest #f])
	(if (null? xs)
	    (let ([rvars (reverse vars)]
		  [rvals (reverse vals)] )
	      (if rest
		  (let ([rv (r (gensym))])
		    `(,%let 
		      ,bs
		      (,%lambda (,@rvars . ,rv)
				(,%apply ,(car rvals) ,@(cdr rvals) ,rv) ) ) )
		  `(,%let ,bs
			  (,%lambda ,rvars (,(car rvals) ,@(cdr rvals)) ) ) ) )
	    (cond ((c %<> (car xs))
		   (let ([v (r (gensym))])
		     (loop (cdr xs) (cons v vars) bs (cons v vals) #f) ) )
		  ((c %<...> (car xs)) (loop '() vars bs vals #t))
		  (else 
		   (let ([v (r (gensym))])
		     (loop (cdr xs) 
			   vars
			   (cons (list v (car xs)) bs)
			   (cons v vals) #f) ) ))))))))


;;; Extension helper:

(##sys#extend-macro-environment		; DEPRECATED
 'define-extension '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'define-extension form '(_ symbol . _))
    (let ((%declare (r 'declare))
	  (%begin (r 'begin))
	  (%static (r 'static))
	  (%dynamic (r 'dynamic))
	  (%export (r 'export)))
      (let loop ((s '()) (d '()) (cs (cddr form)) (exports #f))
	(cond ((null? cs)
	       (let ((exps (if exports
			       `(,%declare (,%export ,@exports))
			       '(,%begin))))
		 `(,(r 'cond-expand)
		   (chicken-compile-shared ,exps ,@d)
		   ((,(r 'not) compiling) ,@d)
		   (,(r 'else)
		    (,%declare (unit ,name))
		    ,exps
		    (,(r 'provide) (,(r 'quote) ,name))
		    ,@s) ) ) )
	      ((and (pair? cs) (pair? (car cs)))
	       (let ((t (caar cs))
		     (next (cdr cs)) )
		 (cond ((c %static t)
			(loop (cons `(,%begin ,@(cdar cs)) s) d next exports))
		       ((c %dynamic t) 
			(loop s (cons `(,%begin ,@(cdar cs)) d) next exports))
		       ((c %export t)
			(loop s d next (append (or exports '()) (cdar cs))))
		       (else
			(syntax-error 'define-extension "invalid clause specifier" (caar cs))) ) ) )
	      (else
	       (syntax-error
		'define-extension
		"invalid clause syntax" cs)) ) ) ))))


;;; SRFI-31

(##sys#extend-macro-environment
 'rec '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'rec form '(_ _ . _))
    (let ((head (cadr form))
	  (%letrec (r 'letrec)))
      (if (pair? head)
	  `(,%letrec ((,(car head) 
		       (,(r 'lambda) ,(cdr head)
			,@(cddr form))))
		     ,(car head))
	  `(,%letrec ((,head ,@(cddr form))) ,head))))))


;;; Definitions available at macroexpansion-time:

(##sys#extend-macro-environment
 'define-for-syntax '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'define-for-syntax form '(_ _ . _))
    `(,(r 'begin-for-syntax)
      (,(r 'define) ,@(cdr form))))))


;;; use

(##sys#extend-macro-environment
 'use '()
 (##sys#er-transformer
  (lambda (x r c)
    (##sys#check-syntax 'use x '(_ . #(_ 0)))
    `(##core#require-extension ,(cdr x) #t))))


;;; compiler syntax

(##sys#extend-macro-environment
 'define-compiler-syntax '()
 (##sys#er-transformer
  (syntax-rules ()
    ((_ (name . llist) body ...)
     (define-compiler-syntax name (lambda llist body ...)))
    ((_ name transformer)
     (##core#define-compiler-syntax name transformer)))))

(##sys#extend-macro-environment
 'let-compiler-syntax '()
 (##sys#er-transformer
  (syntax-rules ()
    ((_ ((name transformer) ...) body ...)
     (##core#let-compiler-syntax ((name transformer) ...) body ...)))))


;;; Just in case someone forgets

(##sys#extend-macro-environment
 'define-macro '()
 (##sys#er-transformer
  (lambda (form r c)
    (syntax-error 'define-macro "`define-macro' is not supported - please use `define-syntax'"))))


(##sys#macro-subset me0 ##sys#default-macro-environment)))

;; register features

(eval-when (compile load eval)
  (register-feature! 'srfi-8 'srfi-16 'srfi-26 'srfi-31 'srfi-15 'srfi-11) )
