;;;; chicken-more-macros.scm - More syntax extensions
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


(##sys#provide 'chicken-more-macros)


;;; Non-standard macros:

(##sys#register-macro 
 'define-record
 (let ((symbol->string symbol->string)
       (string->symbol string->symbol)
       (string-append string-append) )
   (lambda (name . slots)
     (##sys#check-syntax 'define-record name 'symbol)
     (##sys#check-syntax 'define-record slots '#(symbol 0))
     (let ([prefix (symbol->string name)]
	   [setters (memq #:record-setters ##sys#features)]
	   [nsprefix (##sys#qualified-symbol-prefix name)] )
       `(begin
	  (define ,(##sys#string->qualified-symbol nsprefix (string-append "make-" prefix))
	    (lambda ,slots (##sys#make-structure ',name ,@slots)) )
	  (define ,(##sys#string->qualified-symbol nsprefix (string-append prefix "?"))
	    (lambda (x) (##sys#structure? x ',name)) )
	  ,@(let mapslots ((slots slots) (i 1))
	      (if (eq? slots '())
		  slots
		  (let* ((slotname (symbol->string (##sys#slot slots 0)))
			 (setr (##sys#string->qualified-symbol nsprefix (string-append prefix "-" slotname "-set!")))
			 (getr (##sys#string->qualified-symbol nsprefix (string-append prefix "-" slotname)) ) )
		    (cons
		     `(begin
			(define ,setr
			  (lambda (x val)
			    (##core#check (##sys#check-structure x ',name))
			    (##sys#block-set! x ,i val) ) )
			(define ,getr
			  ,(if setters
			       `(getter-with-setter
				 (lambda (x) 
				   (##core#check (##sys#check-structure x ',name))
				   (##sys#block-ref x ,i) )
				 ,setr)
			       `(lambda (x)
				  (##core#check (##sys#check-structure x ',name))
				  (##sys#block-ref x ,i) ) ) ) )
		     (mapslots (##sys#slot slots 1) (fx+ i 1)) ) ) ) ) ) ) ) ) )

(##sys#register-macro
 'receive
 (lambda (vars . rest)
   (if (null? rest)
       `(##sys#call-with-values (lambda () ,vars) ##sys#list)
       (begin
	 (##sys#check-syntax 'receive vars 'lambda-list)
	 (##sys#check-syntax 'receive rest '(_ . _))
	 (if (and (pair? vars) (null? (cdr vars)))
	     `(let ((,(car vars) ,(car rest)))
		,@(cdr rest))
	     `(##sys#call-with-values 
	       (lambda () ,(car rest))
	       (lambda ,vars ,@(cdr rest)) ) ) ) ) ) )

(##sys#register-macro
 'time 
 (let ((gensym gensym))
   (lambda exps
     (let ((rvar (gensym 't)))
       `(begin
	  (##sys#start-timer)
	  (##sys#call-with-values 
	   (lambda () ,@exps)
	   (lambda ,rvar
	     (##sys#display-times (##sys#stop-timer))
	     (##sys#apply ##sys#values ,rvar) ) ) ) ) ) ) )

(##sys#register-macro
 'declare
 (lambda specs 
   `(##core#declare ,@(##sys#map (lambda (x) `(quote ,x)) specs)) ) ); hides specifiers from macroexpand

(##sys#register-macro
 'include
 (let ([with-input-from-file with-input-from-file]
       [read read]
       [reverse reverse] )
   (lambda (filename)
     (let ((path (##sys#resolve-include-filename filename #t)))
       (when (load-verbose) (print "; including " path " ..."))
       `(begin
	  ,@(with-input-from-file path
	      (lambda ()
		(do ([x (read) (read)]
		     [xs '() (cons x xs)] )
		    ((eof-object? x) 
		     (reverse xs))) ) ) ) ) ) ) )

(##sys#register-macro
 'assert
 (lambda (exp . msg-and-args)
   (let ((msg (if (eq? '() msg-and-args)
		  `(##core#immutable '"assertion failed")
		  (##sys#slot msg-and-args 0) ) ) )
     `(if (##core#check ,exp)
	  (##core#undefined)
	  (##sys#error ,msg ',exp ,@(if (fx> (length msg-and-args) 1)
				  (##sys#slot msg-and-args 1)
				  '() ) ) ) ) ) )

(##sys#register-macro
 'ensure
 (lambda (pred exp . args)
   (let ([tmp (gensym)])
     `(let ([,tmp ,exp])
	(if (##core#check (,pred ,tmp))
	    ,tmp
	    (##sys#signal-hook
	     #:type-error
	     ,@(if (pair? args)
		   args
		   `((##core#immutable '"argument has incorrect type") ,tmp ',pred) ) ) ) ) ) ) )

(##sys#register-macro
 'fluid-let
 (let ((gensym gensym))
   (lambda (clauses . body)
     (##sys#check-syntax 'fluid-let clauses '#((symbol _) 0))
     (let ((ids (##sys#map car clauses))
	   (new-tmps (##sys#map (lambda (x) (gensym)) clauses))
	   (old-tmps (##sys#map (lambda (x) (gensym)) clauses)))
       `(let (,@(map ##sys#list new-tmps (##sys#map cadr clauses))
	      ,@(map ##sys#list old-tmps
		     (let loop ((n (length clauses)))
		       (if (eq? n 0)
			   '()
			   (cons #f (loop (fx- n 1))) ) ) ) )
	  (##sys#dynamic-wind
	      (lambda ()
		,@(map (lambda (ot id) `(##core#set! ,ot ,id))
		       old-tmps ids)
		,@(map (lambda (id nt) `(##core#set! ,id ,nt))
		       ids new-tmps)
		(##sys#void) )
	      (lambda () ,@body)
	      (lambda ()
		,@(map (lambda (nt id) `(##core#set! ,nt ,id))
		       new-tmps ids)
		,@(map (lambda (id ot) `(##core#set! ,id ,ot))
		       ids old-tmps)
		(##sys#void) ) ) ) ) ) ) )

(##sys#register-macro
 'eval-when
 (lambda (situations . body)
   (let ([e #f]
	 [c #f]
	 [l #f] 
	 [body `(begin ,@body)] )
     (let loop ([ss situations])
       (if (pair? ss)
	   (begin
	     (case (##sys#slot ss 0)
	       [(eval) (set! e #t)]
	       [(load run-time) (set! l #t)]
	       [(compile compile-time) (set! c #t)]
	       [else (##sys#error "invalid situation specifier" (##sys#slot ss 0))] )
	     (loop (##sys#slot ss 1)) ) ) )
     (if (memq '#:compiling ##sys#features)
	 (cond [(and c l) `(##core#compiletimetoo ,body)]
	       [c `(##core#compiletimeonly ,body)]
	       [l body]
	       [else '(##core#undefined)] )
	 (if e 
	     body
	     '(##core#undefined) ) ) ) ) )

(##sys#register-macro
 'parameterize 
 (let ([car car]
       [cadr cadr] 
       [map map] )
   (lambda (bindings . body)
     (##sys#check-syntax 'parameterize bindings '#((_ _) 0))
     (let* ([swap (gensym)]
	    [params (##sys#map car bindings)]
	    [vals (##sys#map cadr bindings)]
	    [aliases (##sys#map (lambda (z) (gensym)) params)]
	    [aliases2 (##sys#map (lambda (z) (gensym)) params)] )
       `(let ,(##sys#append (map ##sys#list aliases params) (map ##sys#list aliases2 vals))
	  (let ((,swap (lambda ()
			 ,@(map (lambda (a a2) `(let ((t (,a))) (,a ,a2) (##core#set! ,a2 t)))
				aliases aliases2) ) ) )
	    (##sys#dynamic-wind 
		,swap
		(lambda () ,@body)
		,swap) ) ) ) ) ) )

(##sys#register-macro
 'when
 (lambda (test . body)
   `(if ,test (begin ,@body)) ) )

(##sys#register-macro
 'unless
 (lambda (test . body)
   `(if ,test (##core#undefined) (begin ,@body)) ) )

(let* ((map map)
       (assign
	(lambda (vars exp)
	  (##sys#check-syntax 'set!-values/define-values vars '#(symbol 0))
	  (cond ((null? vars)
		 ;; may this be simply "exp"?
		 `(##sys#call-with-values (lambda () ,exp) (lambda () (##core#undefined))) )
		((null? (cdr vars))
		 `(##core#set! ,(car vars) ,exp)) 
		(else
		 (let ([aliases (map gensym vars)])
		   `(##sys#call-with-values
		     (lambda () ,exp)
		     (lambda ,aliases
		       ,@(map (lambda (v a) `(##core#set! ,v ,a)) vars aliases) ) ) ) ) ) ) ) )
  (##sys#register-macro 'set!-values assign)
  (##sys#register-macro 'define-values assign) )

(##sys#register-macro-2
 'let-values
 (letrec ((append* (lambda (il l)
		      (if (not (pair? il))
			  (cons il l)
			  (cons (car il)
				(append* (cdr il) l)))))
	    (map* (lambda (proc l)
		    (cond ((null? l) '())
			  ((not (pair? l)) (proc l))
			  (else (cons (proc (car l)) (map* proc (cdr l))))))))
   (lambda (form)
     (##sys#check-syntax 'let-values form '(#(_ 0) . #(_ 1)))
     (let* ([vbindings (car form)]
	    [body (cdr form)]
	    [llists (map car vbindings)]
	    [vars (let loop ((llists llists) (acc '()))
		    (if (null? llists)
			acc
			(let* ((llist (car llists))
			       (new-acc
				(cond ((list? llist) (append llist acc))
				      ((pair? llist) (append* llist acc))
				      (else (cons llist acc)))))
			  (loop (cdr llists) new-acc))))]
	    [aliases (map (lambda (v) (cons v (gensym v))) vars)]
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
		`(let ,(map (lambda (v) (##sys#list v (lookup v))) vars) ,@body) )
	       ((and (pair? (car llists2)) (null? (cdar llists2)))
		`(let ((,(caar llists2) ,(car exps)))
		   ,(fold (cdr llists) (cdr exps) (cdr llists2)) ) )
	       (else
		`(##sys#call-with-values
		  (lambda () ,(car exps))
		  (lambda ,(car llists2) ,(fold (cdr llists) (cdr exps) (cdr llists2))) ) ) ) ) ) ) ) )

(##sys#register-macro-2
 'let*-values
 (lambda (form)
   (##sys#check-syntax 'let*-values form '(#(_ 0) . #(_ 1)))
   (let ([vbindings (car form)]
	 [body (cdr form)] )
     (let fold ([vbindings vbindings])
       (if (null? vbindings)
	   `(let () ,@body)
	   `(let-values (,(car vbindings))
	      ,(fold (cdr vbindings))) ) ) ) ) )

(##sys#register-macro-2 
 'letrec-values
 (lambda (form)
   (##sys#check-syntax 'letrec-values form '(#(_ 0) . #(_ 1)))
   (let* ([vbindings (car form)]
	  [body (cdr form)] 
	  [vars (apply ##sys#append (map (lambda (x) (car x)) vbindings))] 
	  [aliases (map (lambda (v) (cons v (gensym v))) vars)] 
	  [lookup (lambda (v) (cdr (assq v aliases)))] )
     `(let ,(map (lambda (v) (##sys#list v '(##core#undefined))) vars)
	,@(map (lambda (vb)
		 `(##sys#call-with-values (lambda () ,(cadr vb))
		    (lambda ,(map lookup (car vb))
		      ,@(map (lambda (v) `(##core#set! ,v ,(lookup v))) (car vb)) ) ) )
	       vbindings)
	,@body) ) ) )

(##sys#register-macro
 'nth-value
 (lambda (i exp)
   (let ([v (gensym)])
     `(##sys#call-with-values
       (lambda () ,exp)
       (lambda ,v (list-ref ,v ,i)) ) ) ) )

(letrec ([quotify-proc 
           (lambda (xs id)
	     (##sys#check-syntax id xs '#(_ 1))
             (let* ([head (car xs)]
                    [name (if (pair? head) (car head) head)]
                    [val (if (pair? head)
                           `(lambda ,(cdr head) ,@(cdr xs))
                           (cadr xs) ) ] )
	       (when (or (not (pair? val)) (not (eq? 'lambda (car val))))
		 (syntax-error 'define-inline "invalid substitution form - must be lambda"
			       name) )
               (list (list 'quote name) val) ) ) ] )
  (##sys#register-macro-2 
   'define-inline
   (lambda (form) `(##core#define-inline ,@(quotify-proc form 'define-inline)))) )

(##sys#register-macro-2
 'define-constant
 (lambda (form)
   (##sys#check-syntax 'define-constant form '(symbol _))
   `(##core#define-constant ',(car form) ,(cadr form)) ) )

(##sys#register-macro-2			; DEPRECATED
 'critical-section
 (lambda (form)
   `(##sys#dynamic-wind
	##sys#disable-interrupts
	(lambda () ,@form)
	##sys#enable-interrupts) ) )

(##sys#register-macro-2
 'and-let*
   (lambda (forms)
     (##sys#check-syntax 'and-let* forms '(#(_ 0) . #(_ 1)))
     (if (or (not (list? forms)) (fx< (length forms) 2))
	 (##sys#syntax-error-hook "syntax error in 'and-let*' form" forms) 
	 (let ([bindings (##sys#slot forms 0)]
	       [body (##sys#slot forms 1)] )
	   (let fold ([bs bindings])
	     (if (null? bs)
		 `(begin ,@body)
		 (let ([b (##sys#slot bs 0)]
		       [bs2 (##sys#slot bs 1)] )
		   (cond [(not-pair? b) `(if ,b ,(fold bs2) #f)]
			 [(null? (##sys#slot b 1)) `(if ,(##sys#slot b 0) ,(fold bs2) #f)]
			 [else
			  (let ([var (##sys#slot b 0)])
			    `(let ((,var ,(cadr b)))
			       (if ,var ,(fold bs2) #f) ) ) ] ) ) ) ) ) ) ) )

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

(##sys#register-macro-2
 'switch
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
		     (##sys#check-syntax 'switch clause '#(_ 1))
		     (if (eq? 'else (car clause))
			 `(begin ,@(cdr clause))
			 `(if (eqv? ,tmp ,(car clause))
			      (begin ,@(cdr clause)) 
			      ,(expand rclauses) ) ) ) ) ) ) ) ) ) ) )


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

(define-macro (let-optionals arg-list var/defs . body)

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
	       (lambda ,(reverse vars)
		 (,next-guy ,@(reverse vars) ,(car defs))))
	      . ,(recur vars
			(cdr defaulter-names)
			(cdr defs)
			(car defaulter-names)))))))


    ;; This guy makes the (IF (NULL? REST) (PORT-DEF) ...) tree above.

  (define (make-if-tree vars defaulters body-proc rest rename)
    (let recur ((vars vars) (defaulters defaulters) (non-defaults '()))
      (if (null? vars)
	  `(if (##core#check (null? ,rest))
	       (,body-proc . ,(reverse non-defaults))
	       (##sys#error (##core#immutable '"too many optional arguments") ,rest))
	  (let ((v (car vars)))
	    `(if (null? ,rest)
		 (,(car defaulters) . ,(reverse non-defaults))
		 (let ((,v (car ,rest))
		       (,rest (cdr ,rest)))
		   ,(recur (cdr vars)
			   (cdr defaulters)
			   (cons v non-defaults))))))))

  (##sys#check-syntax 'let-optionals var/defs '#((symbol _) 0))
  (##sys#check-syntax 'let-optionals body '#(_ 1))
  (let* ((vars (map car var/defs))
	 (prefix-sym (lambda (prefix sym)
		       (string->symbol (string-append prefix (symbol->string sym)))))

	 ;; Private vars, one for each user var.
	 ;; We prefix the % to help keep macro-expanded code from being
	 ;; too confusing.
	 (vars2 (map (lambda (v) (gensym (prefix-sym "%" v)))
		     vars))

	 (defs (map cadr var/defs))
	 (body-proc (gensym 'body))

	 ;; A private var, bound to the value of the ARG-LIST expression.
	 (rest-var (gensym '%rest))

	 (defaulter-names (map (lambda (var) (gensym (prefix-sym "def-" var)))
			       vars))

	 (defaulters (make-default-procs vars2 body-proc
					 defaulter-names defs gensym))
	 (if-tree (make-if-tree vars2 defaulter-names body-proc
				rest-var gensym)))

    `(let* ((,rest-var ,arg-list)
	    (,body-proc (lambda ,vars . ,body))
	    . ,defaulters)
       ,if-tree) ) )


;;; (:optional rest-arg default-exp)
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

(define-macro (:optional rest default-exp)
  (let ([var (gensym)])
    `(let ((,var ,rest))
       (if (null? ,var) 
	   ,default-exp
	   (if (##core#check (null? (cdr ,var)))
	       (car ,var)
	       (##sys#error (##core#immutable '"too many optional arguments") ,var))))))


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

(define-macro (let-optionals* args var/defs . body)
  (##sys#check-syntax 'let-optionals* var/defs '#(_ 0))
  (##sys#check-syntax 'let-optionals* body '#(_ 1))
  (let ([rvar (gensym)])
    `(let ((,rvar ,args))
       ,(let loop ([args rvar] [vardefs var/defs])
	  (if (null? vardefs)
	      `(if (##core#check (null? ,args))
		   (let () ,@body)
		   (##sys#error (##core#immutable '"too many optional arguments") ,args) )
	      (let ([head (car vardefs)])
		(if (pair? head)
		    (let ([rvar2 (gensym)])
		      `(let ((,(car head) (if (null? ,args) ,(cadr head) (car ,args)))
			     (,rvar2 (if (null? ,args) '() (cdr ,args))) )
			 ,(loop rvar2 (cdr vardefs)) ) )
		    `(let ((,head ,args)) ,@body) ) ) ) ) ) ) )


;;; case-lambda (SRFI-16):

(define-macro (case-lambda . clauses)
  (define (genvars n)
    (let loop ([i 0])
      (if (fx>= i n)
	  '()
	  (cons (gensym) (loop (fx+ i 1))) ) ) )
  (##sys#check-syntax 'case-lambda clauses '#(_ 0))
  (require 'srfi-1)			; Urgh...
  (let* ((mincount (apply min (map (lambda (c)
				     (##sys#decompose-lambda-list 
				      (car c)
				      (lambda (vars argc rest) argc) ) )
				   clauses) ) ) 
	 (minvars (genvars mincount))
	 (rvar (gensym)) 
	 (lvar (gensym)) )
    `(lambda ,(append minvars rvar)
       (let ((,lvar (length ,rvar)))
	 ,(fold-right
	   (lambda (c body)
	     (##sys#decompose-lambda-list
	      (car c)
	      (lambda (vars argc rest)
		(##sys#check-syntax 'case-lambda (car c) 'lambda-list)
		`(if ,(let ([a2 (fx- argc mincount)])
			(if rest
			    (if (zero? a2)
				#t
				`(fx>= ,lvar ,a2) )
			    `(fx= ,lvar ,a2) ) )
		     ,(receive
		       (vars1 vars2) (split-at! (take vars argc) mincount)
		       (let ((bindings
			      (let build ((vars2 vars2) (vrest rvar))
				(if (null? vars2)
				    (cond (rest `(let ((,rest ,vrest)) ,@(cdr c)))
					  ((null? (cddr c)) (cadr c))
					  (else `(let () ,@(cdr c))) )
				    (let ((vrest2 (gensym)))
				      `(let ((,(car vars2) (car ,vrest))
					     (,vrest2 (cdr ,vrest)) )
					 ,(if (pair? (cdr vars2))
					      (build (cdr vars2) vrest2)
					      (build '() vrest2) ) ) ) ) ) ) )
			 (if (null? vars1)
			     bindings
			     `(let ,(map list vars1 minvars) ,bindings) ) ) )
		     ,body) ) ) )
	   '(##core#check (##sys#error (##core#immutable '"no matching clause in call to 'case-lambda' form")))
	   clauses) ) ) ) )


;;; Record printing:

(define-macro (define-record-printer head . body)
  (cond [(pair? head)
	 (##sys#check-syntax 'define-record-printer (cons head body) '((symbol symbol symbol) . #(_ 1)))
	 `(##sys#register-record-printer ',(##sys#slot head 0) (lambda ,(##sys#slot head 1) ,@body)) ]
	[else
	 (##sys#check-syntax 'define-record-printer (cons head body) '(symbol _))
	 `(##sys#register-record-printer ',head ,@body) ] ) )


;;; Exceptions:

(define-macro (handle-exceptions var handler . body)
  (let ([k (gensym)]
	[args (gensym)] )
    `((call-with-current-continuation
       (lambda (,k)
	 (with-exception-handler
	  (lambda (,var) (,k (lambda () ,handler)))
	  (lambda ()
	    (##sys#call-with-values
	     (lambda () ,@body)
	     (lambda ,args (,k (lambda () (##sys#apply ##sys#values ,args)))) ) ) ) ) ) ) ) )

(define-macro (condition-case exp . clauses)
  (let ([exvar (gensym)]
	[kvar (gensym)] )
    (define (parse-clause c)
      (let* ([var (and (symbol? (car c)) (car c))]
	     [kinds (if var (cadr c) (car c))]
	     [body (if var (cddr c) (cdr c))] )
	(if (null? kinds)
	    `(else 
	      ,(if var
		   `(let ([,var ,exvar]) ,@body)
		   `(let () ,@body) ) )
	    `((and ,kvar ,@(map (lambda (k) `(memv ',k ,kvar)) kinds))
	      ,(if var
		   `(let ([,var ,exvar]) ,@body)
		   `(let () ,@body) ) ) ) ) )
    `(handle-exceptions ,exvar
	 (let ([,kvar (and (##sys#structure? ,exvar 'condition) (##sys#slot ,exvar 1))])
	   (cond ,@(map parse-clause clauses)
		 (else (##sys#signal ,exvar)) ) )
       ,exp) ) )


;;; SRFI-9:

(define-macro (define-record-type t conser pred . slots)
  (let ([vars (cdr conser)]
	[slotnames (map car slots)] )
    `(begin
       (define ,conser
	 (##sys#make-structure 
	  ',t 
	  ,@(map (lambda (sname)
		   (if (memq sname vars)
		       sname
		       '(##sys#void) ) )
		 slotnames) ) )
       (define (,pred x) (##sys#structure? x ',t))
       ,@(let loop ([slots slots] [i 1])
	   (if (null? slots)
	       '()
	       (let* ([slot (car slots)]
		      (setters (memq #:record-setters ##sys#features))
		      (setr? (pair? (cddr slot))) 
		      (getr `(lambda (x)
			       (##core#check (##sys#check-structure x ',t))
			       (##sys#block-ref x ,i) ) ) )
		 `(,@(if setr?
			 `((define (,(caddr slot) x y)
			     (##core#check (##sys#check-structure x ',t))
			     (##sys#block-set! x ,i y)) )
			 '() )
		   (define ,(cadr slot) 
		     ,(if (and setr? setters)
			  `(getter-with-setter ,getr ,(caddr slot))
			  getr) )
		   ,@(loop (cdr slots) (add1 i)) ) ) ) ) ) ) )


;;; Compile-time `require':

(define-macro (require-for-syntax . names)
  (##sys#check-syntax 'require-for-syntax names '#(_ 0))
  `(##core#require-for-syntax ,@names) )

(define-macro (require-extension . ids)
  (##sys#check-syntax 'require-extension ids '#(_ 0))
  `(##core#require-extension ,@(map (lambda (x) (list 'quote x)) ids) ) )

(define-macro (use . ids)
  (##sys#check-syntax 'use ids '#(_ 0))
  `(##core#require-extension ,@(map (lambda (x) (list 'quote x)) ids) ) )


;;; SRFI-26:

(define-macro (cut . more)
  (let loop ([xs more] [vars '()] [vals '()] [rest #f])
    (if (null? xs)
	(let ([rvars (reverse vars)]
	      [rvals (reverse vals)] )
	  (if rest
	      (let ([rv (gensym)])
		`(lambda (,@rvars . ,rv)
		   (apply ,(car rvals) ,@(cdr rvals) ,rv) ) )
	      `(lambda ,rvars ((begin ,(car rvals)) ,@(cdr rvals)) ) ) )
	(case (car xs)
	  [(<>)
	   (let ([v (gensym)])
	     (loop (cdr xs) (cons v vars) (cons v vals) #f) ) ]
	  [(<...>) (loop '() vars vals #t)]
	  [else (loop (cdr xs) vars (cons (car xs) vals) #f)] ) ) ) )

(define-macro (cute . more)
  (let loop ([xs more] [vars '()] [bs '()] [vals '()] [rest #f])
    (if (null? xs)
	(let ([rvars (reverse vars)]
	      [rvals (reverse vals)] )
	  (if rest
	      (let ([rv (gensym)])
		`(let ,bs
		   (lambda (,@rvars . ,rv)
		     (apply ,(car rvals) ,@(cdr rvals) ,rv) ) ) )
	      `(let ,bs
		 (lambda ,rvars (,(car rvals) ,@(cdr rvals)) ) ) ) )
	(case (car xs)
	  [(<>)
	   (let ([v (gensym)])
	     (loop (cdr xs) (cons v vars) bs (cons v vals) #f) ) ]
	  [(<...>) (loop '() vars bs vals #t)]
	  [else 
	   (let ([v (gensym)])
	     (loop (cdr xs) vars (cons (list v (car xs)) bs) (cons v vals) #f) ) ] ) ) ) )


;;; SRFI-13:

(define-macro (let-string-start+end s-e-r proc s-exp args-exp . body)
  (if (pair? (cddr s-e-r))
      `(receive (,(caddr s-e-r) ,(car s-e-r) ,(cadr s-e-r))
	   (string-parse-start+end ,proc ,s-exp ,args-exp)
	 ,@body)
      `(receive ,s-e-r
	   (string-parse-final-start+end ,proc ,s-exp ,args-exp)
	 ,@body) ) )


;;; Extension helper:

(define-macro (define-extension name . clauses)
  (let loop ((s '()) (d '()) (cs clauses) (exports #f))
    (cond ((null? cs)
	   (let ((exps (if exports `(declare (export ,@exports)) '(begin))))
	     `(cond-expand
	       (chicken-compile-shared ,exps ,@d)
	       ((not compiling) ,@d)
	       (else 
		(declare (unit ,name))
		,exps
		(provide ',name) 
		,@s) ) ) )
	  ((and (pair? cs) (pair? (car cs)))
	   (let ((t (caar cs))
		 (next (cdr cs)) )
	     (cond ((eq? 'static t) (loop (cons `(begin ,@(cdar cs)) s) d next exports))
		   ((eq? 'dynamic t) (loop s (cons `(begin ,@(cdar cs)) d) next exports))
		   ((eq? 'export t) (loop s d next (append (or exports '()) (cdar cs))))
		   (else (syntax-error 'define-extension "invalid clause specifier" (caar cs))) ) ) )
	  (else (syntax-error 'define-extension "invalid clause syntax" cs)) ) ) )


;;;; SRFI-31

(define-macro (rec head . args)
  (if (pair? head)
      `(letrec ((,(car head) (lambda ,(cdr head) ,@args))) ,(car head))
      `(letrec ((,head ,@args)) ,head)))


;;; Definitions available at macroexpansion-time:

(define-macro (define-for-syntax head . body)
  (let* ((body (if (null? body) '((void)) body))
	 (name (if (pair? head) (car head) head)) 
	 (body (if (pair? head) `(lambda ,(cdr head) ,@body) (car body))))
    (if (symbol? name)
	(##sys#setslot name 0 (eval body))
	(syntax-error 'define-for-syntax "invalid identifier" name) )
    (if ##sys#enable-runtime-macros
	`(define ,name ,body)
	'(begin) ) ) )


;;;; Register features provided by this file

(eval-when (compile load eval)
  (register-feature! 'srfi-8 'srfi-16 'srfi-26 'srfi-31 'srfi-15 'srfi-11) )
