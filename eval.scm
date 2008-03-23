;;;; eval.scm - Interpreter for CHICKEN
;
; Copyright (c) 2000-2007, Felix L. Winkelmann
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
  (unit eval)
  (uses expand)
  (disable-warning var)
  (hide ##sys#split-at-separator
	##sys#r4rs-environment ##sys#r5rs-environment 
	##sys#interaction-environment pds pdss pxss) )

#>
#ifndef C_INSTALL_EGG_HOME
# define C_INSTALL_EGG_HOME    "."
#endif

#ifndef C_INSTALL_SHARE_HOME
# define C_INSTALL_SHARE_HOME NULL
#endif
<#

(cond-expand
 [paranoia]
 [else
  (declare
    (no-bound-checks)
    (no-procedure-checks-for-usual-bindings)
    (bound-to-procedure 
     ##sys#check-char ##sys#check-exact ##sys#check-port ##sys#check-string ##sys#load-library
     ##sys#for-each ##sys#map ##sys#setslot ##sys#allocate-vector ##sys#check-pair ##sys#not-a-proper-list-error
     ##sys#check-symbol ##sys#check-vector ##sys#floor ##sys#ceiling ##sys#truncate ##sys#round 
     ##sys#check-number ##sys#cons-flonum ##sys#copy-env-table
     ##sys#flonum-fraction ##sys#make-port ##sys#fetch-and-check-port-arg ##sys#print ##sys#check-structure 
     ##sys#make-structure ##sys#feature? ##sys#interpreter-toplevel-macroexpand-hook
     ##sys#error-handler ##sys#hash-symbol ##sys#register-macro ##sys#check-syntax
     ##sys#hash-table-ref ##sys#hash-table-set! ##sys#canonicalize-body ##sys#decompose-lambda-list
     ##sys#make-c-string ##sys#resolve-include-filename ##sys#register-macro-2 
     ##sys#load ##sys#error ##sys#warn ##sys#hash-table-location ##sys#expand-home-path
     ##sys#make-flonum ##sys#make-pointer ##sys#null-pointer ##sys#address->pointer 
     ##sys#pointer->address ##sys#compile-to-closure ##sys#make-string ##sys#make-lambda-info
     ##sys#number? ##sys#symbol->qualified-string ##sys#decorate-lambda ##sys#string-append
     ##sys#ensure-heap-reserve ##sys#syntax-error-hook ##sys#read-prompt-hook
     ##sys#repl-eval-hook ##sys#append ##sys#eval-decorator ##sys#alias-global-hook
     open-output-string get-output-string make-parameter software-type software-version machine-type
     build-platform getenv set-extensions-specifier! ##sys#string->symbol list->vector
     extension-information syntax-error ->string chicken-home ##sys#expand-curried-define
     vector->list store-string open-input-string eval ##sys#gc
     with-exception-handler print-error-message read-char read ##sys#read-error
     ##sys#reset-handler call-with-current-continuation ##sys#peek-char-0 ##sys#read-char-0
     ##sys#clear-trace-buffer ##sys#write-char-0 print-call-chain ##sys#with-print-length-limit
     repl-prompt ##sys#flush-output ##sys#extended-lambda-list? keyword? get-line-number
     symbol->string string-append display ##sys#repository-path ##sys#file-info make-vector
     ##sys#make-vector string-copy vector->list ##sys#do-the-right-thing ##sys#->feature-id
     ##sys#extension-information ##sys#symbol->string ##sys#canonicalize-extension-path
     file-exists? ##sys#load-extension ##sys#find-extension ##sys#substring reverse
     dynamic-load-libraries ##sys#string->c-identifier load-verbose ##sys#load ##sys#get-keyword
     port? ##sys#file-info ##sys#signal-hook ##sys#dload open-input-file close-input-port
     read write newline ##sys#eval-handler ##sys#set-dlopen-flags! cadadr ##sys#lookup-runtime-requirements
     map string->keyword ##sys#abort
     ##sys#macroexpand-0 ##sys#macroexpand-1-local) ) ] )

(cond-expand
 [unsafe
  (eval-when (compile)
    (define-macro (##sys#check-structure . _) '(##core#undefined))
    (define-macro (##sys#check-range . _) '(##core#undefined))
    (define-macro (##sys#check-pair . _) '(##core#undefined))
    (define-macro (##sys#check-list . _) '(##core#undefined))
    (define-macro (##sys#check-symbol . _) '(##core#undefined))
    (define-macro (##sys#check-string . _) '(##core#undefined))
    (define-macro (##sys#check-char . _) '(##core#undefined))
    (define-macro (##sys#check-exact . _) '(##core#undefined))
    (define-macro (##sys#check-port . _) '(##core#undefined))
    (define-macro (##sys#check-number . _) '(##core#undefined))
    (define-macro (##sys#check-byte-vector . _) '(##core#undefined)) ) ]
 [else
  (declare (emit-exports "eval.exports"))])


(define-foreign-variable install-egg-home c-string "C_INSTALL_EGG_HOME")
(define-foreign-variable installation-home c-string "C_INSTALL_SHARE_HOME")

(define ##sys#core-library-modules
  '(extras lolevel utils tcp regex regex-extras posix match srfi-1 srfi-4 srfi-13 srfi-14 srfi-18))

(define ##sys#explicit-library-modules '())

(define-constant default-dynamic-load-libraries '("libchicken"))
(define-constant cygwin-default-dynamic-load-libraries '("cygchicken-0"))
(define-constant macosx-load-library-extension ".dylib")
(define-constant windows-load-library-extension ".dll")
(define-constant hppa-load-library-extension ".sl")
(define-constant default-load-library-extension ".so")
(define-constant environment-table-size 301)
(define-constant source-file-extension ".scm")
(define-constant setup-file-extension "setup-info")
(define-constant repository-environment-variable "CHICKEN_REPOSITORY")
(define-constant prefix-environment-variable "CHICKEN_PREFIX")
(define-constant special-syntax-files '(chicken-ffi-macros chicken-more-macros))
(define-constant default-binary-version 3)

; these are actually in unit extras, but that is used by default
; srfi-12 in unit library
(define-constant builtin-features
  '(chicken srfi-2 srfi-6 srfi-10 srfi-12 srfi-23 srfi-28 srfi-30 srfi-31 srfi-39 srfi-69) )

(define-constant builtin-features/compiled
  '(srfi-6 srfi-8 srfi-9 srfi-11 srfi-15 srfi-16 srfi-17 srfi-26 srfi-55) )

(define ##sys#chicken-prefix
  (let ((prefix (and-let* ((p (getenv prefix-environment-variable)))
		  (##sys#string-append 
		   p
		   (if (memq (string-ref p (fx- (##sys#size p) 1)) '(#\\ #\/))
		       "" "/") ) ) ) )
    (lambda (#!optional dir)
      (and prefix
	   (if dir (##sys#string-append prefix dir) prefix) ) ) ) )
	  

;;; System settings

(define chicken-home
  (let ([getenv getenv])
    (lambda ()
      (or (##sys#chicken-prefix "share/chicken")
	  installation-home) ) ) )


;;; Lo-level hashtable support:

(define ##sys#hash-symbol
  (let ([cache-s #f]
	[cache-h #f] )
    (lambda (s n)
      (if (eq? s cache-s)
	  (##core#inline "C_fixnum_modulo" cache-h n)
	  (let ([h (##core#inline "C_hash_string" (##sys#slot s 1))])
	    (set! cache-s s)
	    (set! cache-h h)
	    (##core#inline "C_fixnum_modulo" h n) ) ) ) ) )

(define (##sys#hash-table-ref ht key)
  (let ((k (##sys#hash-symbol key (##core#inline "C_block_size" ht))))
    (let loop ((bucket (##sys#slot ht k)))
      (if (eq? bucket '())
	  #f
	  (let ((b (##sys#slot bucket 0)))
	    (if (eq? key (##sys#slot b 0))
		(##sys#slot b 1)
		(loop (##sys#slot bucket 1)) ) ) ) ) ) )

(define ##sys#hash-table-set! 
  (lambda (ht key val)
    (let* ((k (##sys#hash-symbol key (##core#inline "C_block_size" ht)))
	   (bucket0 (##sys#slot ht k)) )
      (let loop ((bucket bucket0))
	(if (eq? bucket '())
	    (##sys#setslot ht k (cons (cons key val) bucket0))
	    (let ((b (##sys#slot bucket 0)))
	      (if (eq? key (##sys#slot b 0))
		  (##sys#setslot b 1 val)
		  (loop (##sys#slot bucket 1)) ) ) ) ) ) ) )

(define (##sys#hash-table-for-each p ht)
  (let ((len (##core#inline "C_block_size" ht)))
    (do ((i 0 (fx+ i 1)))
	((fx>= i len))
      (##sys#for-each (lambda (bucket) 
		   (p (##sys#slot bucket 0)
		      (##sys#slot bucket 1) ) )
		 (##sys#slot ht i) ) ) ) )

(define ##sys#hash-table-location
  (let ([unbound (##sys#slot '##sys#arbitrary-unbound-symbol 0)])
    (lambda (ht key addp)
      (let* ([k (##sys#hash-symbol key (##sys#size ht))]
	     [bucket0 (##sys#slot ht k)] )
	(let loop ([bucket bucket0])
	  (if (null? bucket)
	      (and addp
		   (let ([p (vector key unbound #t)])
		     (##sys#setslot ht k (cons p bucket0))
		     p) )
	      (let ([b (##sys#slot bucket 0)])
		(if (eq? key (##sys#slot b 0))
		    b
		    (loop (##sys#slot bucket 1)) ) ) ) ) ) ) ) )


;;; Compile lambda to closure:

(define ##sys#eval-environment #f)
(define ##sys#environment-is-mutable #f)

(define (##sys#eval-decorator p ll h cntr)
  (##sys#decorate-lambda
   p 
   (lambda (x) (and (not (##sys#immediate? x)) (##core#inline "C_lambdainfop" x)))
   (lambda (p i)
     (##sys#setslot 
      p i 
      (##sys#make-lambda-info 
       (let ((o (open-output-string)))
	 (write ll o)
	 (get-output-string o))))
     p) ) )

(define ##sys#unbound-in-eval #f)
(define ##sys#eval-debug-level 1)
(define (##sys#alias-global-hook s) s)

(define ##sys#compile-to-closure
  (let ([macro? macro?]
	[write write]
	[cadadr cadadr]
	[reverse reverse]
	[open-output-string open-output-string]
	[get-output-string get-output-string] 
	[with-input-from-file with-input-from-file]
	[unbound (##sys#slot '##sys#arbitrary-unbound-symbol 0)]
	[display display] )
    (lambda (exp env me . cntr)

      (define (lookup var e)
	(let loop ((envs e) (ei 0))
	  (cond ((null? envs) (values #f var))
		((posq var (##sys#slot envs 0)) => (lambda (p) (values ei p)))
		(else (loop (##sys#slot envs 1) (fx+ ei 1))) ) ) )

      (define (defined? var e)
	(receive (i j) (lookup var e) i) )

      (define (undefine vars e)
	(let loop ([envs e])
	  (if (null? envs)
	      '()
	      (let ([envi (##sys#slot envs 0)])
		(cons
		 (let delq ([ee envi])
		   (if (null? ee)
		       '()
		       (let ([h (##sys#slot ee 0)]
			     [r (##sys#slot ee 1)] )
			 (if (memq h vars)
			     r
			     (cons h (delq r)) ) ) ) )
		 (loop (##sys#slot envs 1)) ) ) ) ) )

      (define (posq x lst)
	(let loop ((lst lst) (i 0))
	  (cond ((null? lst) #f)
		((eq? x (##sys#slot lst 0)) i)
		(else (loop (##sys#slot lst 1) (fx+ i 1))) ) ) )

      (define (macroexpand-1-checked x e)
	(let ([x2 (##sys#macroexpand-1-local x me)])
	  (if (pair? x2)
	      (let ([h (##sys#slot x2 0)])
		(if (and (eq? h 'let) (not (defined? 'let e)))
		    (let ([next (##sys#slot x2 1)])
		      (if (and (pair? next) (symbol? (##sys#slot next 0)))
			  (macroexpand-1-checked x2 e)
			  x2) )
		    x2) )
	      x2) ) )

      (define (emit-trace-info tf info cntr) 
	(when tf
	  (##core#inline "C_emit_eval_trace_info" info cntr ##sys#current-thread) ) )

      (define (emit-syntax-trace-info tf info cntr) 
	(when tf
	  (##core#inline "C_emit_syntax_trace_info" info cntr ##sys#current-thread) ) )
	
      (define (decorate p ll h cntr)
	(##sys#eval-decorator p ll h cntr) )

      (define (compile x e h tf cntr)
	(cond [(symbol? x)
	       (receive (i j) (lookup x e)
		 (cond [(not i)
			(let ((x (##sys#alias-global-hook x)))
			  (if ##sys#eval-environment
			      (let ([loc (##sys#hash-table-location ##sys#eval-environment x #t)])
				(unless loc (##sys#syntax-error-hook "reference to undefined identifier" x))
				(cond-expand 
				 [unsafe (lambda v (##sys#slot loc 1))]
				 [else
				  (lambda v 
				    (let ([val (##sys#slot loc 1)])
				      (if (eq? unbound val)
					  (##sys#error "unbound variable" x)
					  val) ) ) ] ) )
			      (cond-expand
			       [unsafe (lambda v (##core#inline "C_slot" x 0))]
			       [else
				(when (and ##sys#unbound-in-eval (not (##sys#symbol-has-toplevel-binding? x)))
				  (set! ##sys#unbound-in-eval (cons (cons x cntr) ##sys#unbound-in-eval)) )
				(lambda v (##core#inline "C_retrieve" x))] ) ) ) ]
		       [(zero? i) (lambda (v) (##sys#slot (##sys#slot v 0) j))]
		       [else (lambda (v) (##sys#slot (##core#inline "C_u_i_list_ref" v i) j))] ) ) ]
	      [(##sys#number? x)
	       (case x
		 [(-1) (lambda v -1)]
		 [(0) (lambda v 0)]
		 [(1) (lambda v 1)]
		 [(2) (lambda v 2)]
		 [else (lambda v x)] ) ]
	      [(boolean? x)
	       (if x
		   (lambda v #t)
		   (lambda v #f) ) ]
	      [(or (char? x)
		   (eof-object? x)
		   (string? x) )
	       (lambda v x) ]
	      [(not (pair? x)) (##sys#syntax-error-hook "illegal non-atomic object" x)]
	      [(symbol? (##sys#slot x 0))
	       (emit-syntax-trace-info tf x cntr)
	       (let ([head (##sys#slot x 0)])
		 (if (defined? head e)
		     (compile-call x e tf cntr)
		     (let ([x2 (macroexpand-1-checked x e)])
		       (if (eq? x2 x)
			   (case head

			     [(quote)
			      (##sys#check-syntax 'quote x '(quote _) #f)
			      (let* ((c (##sys#strip-context (cadr x))))
				(case c
				  [(-1) (lambda v -1)]
				  [(0) (lambda v 0)]
				  [(1) (lambda v 1)]
				  [(2) (lambda v 2)]
				  [(#t) (lambda v #t)]
				  [(#f) (lambda v #f)]
				  [(()) (lambda v '())]
				  [else (lambda v c)] ) ) ]

			     [(##core#global-ref)
			      (let ([var (cadr x)])
				(if ##sys#eval-environment
				    (let ([loc (##sys#hash-table-location ##sys#eval-environment var #t)])
				      (lambda v (##sys#slot loc 1)) )
				    (lambda v (##core#inline "C_slot" var 0)) ) ) ]

			     [(##core#check)
			      (compile (cadr x) e h tf cntr) ]

			     [(##core#immutable)
			      (compile (cadr x) e #f tf cntr) ]
		   
			     [(##core#undefined) (lambda (v) (##core#undefined))]

			     [(if)
			      (##sys#check-syntax 'if x '(if _ _ . #(_)) #f)
			      (let* ([test (compile (cadr x) e #f tf cntr)]
				     [cns (compile (caddr x) e #f tf cntr)]
				     [alt (if (pair? (cdddr x))
					      (compile (cadddr x) e #f tf cntr)
					      (compile '(##core#undefined) e #f tf cntr) ) ] )
				(lambda (v) (if (##core#app test v) (##core#app cns v) (##core#app alt v))) ) ]

			     [(begin)
			      (##sys#check-syntax 'begin x '(begin . #(_ 0)) #f)
			      (let* ([body (##sys#slot x 1)]
				     [len (length body)] )
				(case len
				  [(0) (compile '(##core#undefined) e #f tf cntr)]
				  [(1) (compile (##sys#slot body 0) e #f tf cntr)]
				  [(2) (let* ([x1 (compile (##sys#slot body 0) e #f tf cntr)]
					      [x2 (compile (cadr body) e #f tf cntr)] )
					 (lambda (v) (##core#app x1 v) (##core#app x2 v)) ) ]
				  [else
				   (let* ([x1 (compile (##sys#slot body 0) e #f tf cntr)]
					  [x2 (compile (cadr body) e #f tf cntr)] 
					  [x3 (compile `(begin ,@(##sys#slot (##sys#slot body 1) 1)) e #f tf cntr)] )
				     (lambda (v) (##core#app x1 v) (##core#app x2 v) (##core#app x3 v)) ) ] ) ) ]

			     [(set! ##core#set!)
			      (##sys#check-syntax 'set! x '(_ variable _) #f)
			      (let ((var (cadr x)))
				(receive (i j) (lookup var e)
				  (let ((val (compile (caddr x) e var tf cntr)))
				    (cond [(not i)
					   (let ([var (##sys#alias-global-hook var)])
					     (if ##sys#eval-environment
						 (let ([loc (##sys#hash-table-location
							     ##sys#eval-environment 
							     var
							     ##sys#environment-is-mutable) ] )
						   (unless loc (##sys#error "assignment of undefined identifier" var))
						   (if (##sys#slot loc 2)
						       (lambda (v) (##sys#setslot loc 1 (##core#app val v)))
						       (lambda v (##sys#error "assignment to immutable variable" var)) ) )
						 (lambda (v)
						   (##sys#setslot var 0 (##core#app val v))) ) ) ]
					  [(zero? i) (lambda (v) (##sys#setslot (##sys#slot v 0) j (##core#app val v)))]
					  [else
					   (lambda (v)
					     (##sys#setslot
					      (##core#inline "C_u_i_list_ref" v i) j (##core#app val v)) ) ] ) ) ) ) ]

			     [(let)
			      (##sys#check-syntax 'let x '(let #((variable _) 0) . #(_ 1)) #f)
			      (let* ([bindings (cadr x)]
				     [n (length bindings)] 
				     [vars (map (lambda (x) (car x)) bindings)] 
				     [e2 (cons vars e)]
				     [body (##sys#compile-to-closure
					    (##sys#canonicalize-body (cddr x) (cut defined? <> e2) me cntr)
					    e2
					    me
					    cntr) ] )
				(case n
				  [(1) (let ([val (compile (cadar bindings) e (car vars) tf cntr)])
					 (lambda (v)
					   (##core#app body (cons (vector (##core#app val v)) v)) ) ) ]
				  [(2) (let ([val1 (compile (cadar bindings) e (car vars) tf cntr)]
					     [val2 (compile (cadadr bindings) e (cadr vars) tf cntr)] )
					 (lambda (v)
					   (##core#app body (cons (vector (##core#app val1 v) (##core#app val2 v)) v)) ) ) ]
				  [(3) (let* ([val1 (compile (cadar bindings) e (car vars) tf cntr)]
					      [val2 (compile (cadadr bindings) e (cadr vars) tf cntr)] 
					      [t (cddr bindings)]
					      [val3 (compile (cadar t) e (caddr vars) tf cntr)] )
					 (lambda (v)
					   (##core#app 
					    body
					    (cons (vector (##core#app val1 v) (##core#app val2 v) (##core#app val3 v)) v)) ) ) ]
				  [(4) (let* ([val1 (compile (cadar bindings) e (car vars) tf cntr)]
					      [val2 (compile (cadadr bindings) e (cadr vars) tf cntr)] 
					      [t (cddr bindings)]
					      [val3 (compile (cadar t) e (caddr vars) tf cntr)] 
					      [val4 (compile (cadadr t) e (cadddr vars) tf cntr)] )
					 (lambda (v)
					   (##core#app 
					    body
					    (cons (vector (##core#app val1 v)
							  (##core#app val2 v)
							  (##core#app val3 v)
							  (##core#app val4 v))
						  v)) ) ) ]
				  [else
				   (let ([vals (map (lambda (x) (compile (cadr x) e (car x) tf cntr)) bindings)])
				     (lambda (v)
				       (let ([v2 (##sys#make-vector n)])
					 (do ([i 0 (fx+ i 1)]
					      [vlist vals (##sys#slot vlist 1)] )
					     ((fx>= i n))
					   (##sys#setslot v2 i (##core#app (##sys#slot vlist 0) v)) )
					 (##core#app body (cons v2 v)) ) ) ) ] ) ) ]

			     [(lambda)
			      (##sys#check-syntax 'lambda x '(lambda lambda-list . #(_ 1)) #f)
			      (let* ([llist (cadr x)]
				     [body (cddr x)] 
				     [info (cons (or h '?) llist)] )
				(when (##sys#extended-lambda-list? llist)
				  (set!-values 
				   (llist body) 
				   (##sys#expand-extended-lambda-list 
				    llist body
				    ##sys#syntax-error-hook) ) ) 
				(##sys#decompose-lambda-list
				 llist
				 (lambda (vars argc rest)
				   (let* ((e2 (cons vars e))
					  (body 
					   (##sys#compile-to-closure
					    (##sys#canonicalize-body body (cut defined? <> e2) me (or h cntr))
					    e2
					    me
					    (or h cntr) ) ) )
				     (case argc
				       [(0) (if rest
						(lambda (v)
						  (decorate
						   (lambda r
						     (##core#app body (cons (vector r) v)))
						   info h cntr) )
						(lambda (v)
						  (decorate
						   (lambda () (##core#app body (cons #f v)))
						   info h cntr) ) ) ]
				       [(1) (if rest
						(lambda (v)
						  (decorate
						   (lambda (a1 . r)
						     (##core#app body (cons (vector a1 r) v)))
						   info h cntr) ) 
						(lambda (v)
						  (decorate 
						   (lambda (a1)
						     (##core#app body (cons (vector a1) v)))
						   info h cntr) ) ) ]
				       [(2) (if rest
						(lambda (v) 
						  (decorate
						   (lambda (a1 a2 . r)
						     (##core#app body (cons (vector a1 a2 r) v)))
						   info h cntr) )
						(lambda (v)
						  (decorate
						   (lambda (a1 a2)
						     (##core#app body (cons (vector a1 a2) v)))
						   info h cntr) ) ) ]
				       [(3) (if rest
						(lambda (v) 
						  (decorate
						   (lambda (a1 a2 a3 . r)
						     (##core#app body (cons (vector a1 a2 a3 r) v)))
						   info h cntr) )
						(lambda (v)
						  (decorate
						   (lambda (a1 a2 a3)
						     (##core#app body (cons (vector a1 a2 a3) v)))
						   info h cntr) ) ) ]
				       [(4) (if rest
						(lambda (v)
						  (decorate
						   (lambda (a1 a2 a3 a4 . r)
						     (##core#app body (cons (vector a1 a2 a3 a4 r) v)))
						   info h cntr) )
						(lambda (v)
						  (decorate
						   (lambda (a1 a2 a3 a4)
						     (##core#app body (##sys#cons (##sys#vector a1 a2 a3 a4) v)))
						   info h cntr) ) ) ]
				       [else 
					(if rest
					    (lambda (v)
					      (decorate
					       (lambda as
						 (##core#app
						  body
						  (##sys#cons (apply ##sys#vector (fudge-argument-list argc as)) v)) )
					       info h cntr) )
					    (lambda (v)
					      (decorate
					       (lambda as 
						 (let ([len (length as)])
						   (if (not (fx= len argc))
						       (##sys#error "bad argument count" argc len)
						       (##core#app body (##sys#cons (apply ##sys#vector as) v)))))
					       info h cntr) ) ) ] ) ) ) ) ) ]
				
			     [(##core#loop-lambda)
			      (compile `(lambda ,@(cdr x)) e #f tf cntr) ]

			     [(##core#named-lambda)
			      (compile `(lambda ,@(cddr x)) e (cadr x) tf cntr) ]

			     [(##core#require-for-syntax)
			      (let ([ids (map (lambda (x) ((##sys#compile-to-closure x '() '()) '() #f)) (cdr x))])
				(apply ##sys#require ids)
				(let ([rs (##sys#lookup-runtime-requirements ids)])
				  (compile
				   (if (null? rs)
				       '(##core#undefined)
				       `(##sys#require ,@(map (lambda (x) `',x) rs)) )
				   e #f tf cntr) ) ) ]

			     [(##core#require-extension)
			      (compile
			       (let loop ([ids (cdr x)])
				 (if (null? ids)
				     '(##core#undefined)
				     (let-values ([(exp _) (##sys#do-the-right-thing (cadar ids) #f)])
				       `(begin ,exp ,(loop (cdr ids))) ) ) )
			       e #f tf cntr) ]

			     [(##core#elaborationtimeonly ##core#elaborationtimetoo) ; <- Note this!
			      (##core#app (##sys#compile-to-closure (cadr x) '() '() #f) '())
			      (compile '(##core#undefined) e #f tf cntr) ]

			     [(##core#compiletimetoo)
			      (compile (cadr x) e #f tf cntr) ]

			     [(##core#compiletimeonly ##core#callunit) 
			      (compile '(##core#undefined) e #f tf cntr) ]

			     [(##core#declare)
			      (if (memq #:compiling ##sys#features)
				  (for-each (lambda (d) (##compiler#process-declaration (cadr d))) (cdr x)) 
				  (##sys#warn "declarations are ignored in interpreted code" x) )
			      (compile '(##core#undefined) e #f tf cntr) ]

			     [(##core#define-inline ##core#define-constant)
			      (compile `(set! ,(cadadr x) ,@(cddr x)) e #f tf cntr) ]
                   
			     [(##core#primitive ##core#inline ##core#inline_allocate ##core#foreign-lambda 
						##core#define-foreign-variable 
						##core#define-external-variable ##core#let-location
						##core#foreign-primitive
						##core#foreign-lambda* ##core#define-foreign-type)
			      (##sys#syntax-error-hook "can not evaluate compiler-special-form" x) ]

			     [(##core#app)
			      (compile-call (cdr x) e tf cntr) ]

			     [else
			      (cond [(eq? head 'location)
				     (##sys#syntax-error-hook "can not evaluate compiler-special-form" x) ]

				    [else (compile-call x e tf cntr)] ) ] )

			   (compile x2 e h tf cntr) ) ) ) ) ]

	      [else
	       (emit-syntax-trace-info tf x cntr)
	       (compile-call x e tf cntr)] ) )

      (define (fudge-argument-list n alst)
	(if (null? alst) 
	    (list alst)
	    (do ([n n (fx- n 1)]
		 [args alst (##sys#slot args 1)]
		 [last #f args] )
		((fx= n 0)
		 (##sys#setslot last 1 (list args))
		 alst) ) ) )

      (define (checked-length lst)
	(let loop ([lst lst] [n 0])
	  (cond [(null? lst) n]
		[(pair? lst) (loop (##sys#slot lst 1) (fx+ n 1))]
		[else #f] ) ) )

      (define (compile-call x e tf cntr)
	(let* ([fn (compile (##sys#slot x 0) e #f tf cntr)]
	       [args (##sys#slot x 1)]
	       [argc (checked-length args)]
	       [info x] )
	  (case argc
	    [(#f) (##sys#syntax-error-hook "malformed expression" x)]
	    [(0) (lambda (v)
		   (emit-trace-info tf info cntr)
		   ((fn v)))]
	    [(1) (let ([a1 (compile (##sys#slot args 0) e #f tf cntr)])
		   (lambda (v)
		     (emit-trace-info tf info cntr)
		     ((##core#app fn v) (##core#app a1 v))) ) ]
	    [(2) (let* ([a1 (compile (##sys#slot args 0) e #f tf cntr)]
			[a2 (compile (##core#inline "C_u_i_list_ref" args 1) e #f tf cntr)] )
		   (lambda (v)
		     (emit-trace-info tf info cntr)
		     ((##core#app fn v) (##core#app a1 v) (##core#app a2 v))) ) ]
	    [(3) (let* ([a1 (compile (##sys#slot args 0) e #f tf cntr)]
			[a2 (compile (##core#inline "C_u_i_list_ref" args 1) e #f tf cntr)]
			[a3 (compile (##core#inline "C_u_i_list_ref" args 2) e #f tf cntr)] )
		   (lambda (v)
		     (emit-trace-info tf info cntr)
		     ((##core#app fn v) (##core#app a1 v) (##core#app a2 v) (##core#app a3 v))) ) ]
	    [(4) (let* ([a1 (compile (##sys#slot args 0) e #f tf cntr)]
			[a2 (compile (##core#inline "C_u_i_list_ref" args 1) e #f tf cntr)]
			[a3 (compile (##core#inline "C_u_i_list_ref" args 2) e #f tf cntr)] 
			[a4 (compile (##core#inline "C_u_i_list_ref" args 3) e #f tf cntr)] )
		   (lambda (v)
		     (emit-trace-info tf info cntr)
		     ((##core#app fn v) (##core#app a1 v) (##core#app a2 v) (##core#app a3 v) (##core#app a4 v))) ) ]
	    [else (let ([as (##sys#map (lambda (a) (compile a e #f tf cntr)) args)])
		    (lambda (v)
		      (emit-trace-info tf info cntr)
		      (apply (##core#app fn v) (##sys#map (lambda (a) (##core#app a v)) as))) ) ] ) ) )

      (compile exp env #f (fx> ##sys#eval-debug-level 0) (:optional cntr #f)) ) ) )

(define ##sys#eval-handler 
  (make-parameter
   (lambda (x . env)
     (let ([mut ##sys#environment-is-mutable]
	   [e #f] )
       (when (pair? env)
	 (let ([env (car env)])
	   (when env
	     (##sys#check-structure env 'environment)
	     (set! e (##sys#slot env 1)) 
	     (set! mut (##sys#slot env 2)) ) ) )
       ((fluid-let ([##sys#environment-is-mutable mut]
		    [##sys#eval-environment e] )
	  (##sys#compile-to-closure x '() '()) )
	'() ) ) ) ) )

(define eval-handler ##sys#eval-handler)

(define (eval x . env)
  (apply (##sys#eval-handler) 
	 (##sys#interpreter-toplevel-macroexpand-hook x)
	 env) )

;;; Split lambda-list into its parts:

(define ##sys#decompose-lambda-list
  (let ([reverse reverse])
    (lambda (llist0 k)

      (define (err)
	(set! ##sys#syntax-error-culprit #f)
	(##sys#syntax-error-hook "illegal lambda-list syntax" llist0) )

      (let loop ([llist llist0] [vars '()] [argc 0])
	(cond [(eq? llist '()) (k (reverse vars) argc #f)]
	      [(not (##core#inline "C_blockp" llist)) (err)]
	      [(##core#inline "C_symbolp" llist) (k (reverse (cons llist vars)) argc llist)]
	      [(not (##core#inline "C_pairp" llist)) (err)]
	      [else (loop (##sys#slot llist 1)
			  (cons (##sys#slot llist 0) vars)
			  (fx+ argc 1) ) ] ) ) ) ) )


;;; Loading source/object files:

(define load-verbose (make-parameter (##sys#fudge 13)))

(define (##sys#abort-load) #f)
(define ##sys#current-source-filename #f)
(define ##sys#current-load-path "")

(define-foreign-variable _dlerror c-string "C_dlerror")

(define (set-dynamic-load-mode! mode)
  (let ([mode (if (pair? mode) mode (list mode))]
	[now #f]
	[global #t] )
    (let loop ([mode mode])
      (when (pair? mode)
	(case (##sys#slot mode 0)
	  [(global) (set! global #t)]
	  [(local) (set! global #f)]
	  [(lazy) (set! now #f)]
	  [(now) (set! now #t)]
	  [else (##sys#signal-hook 'set-dynamic-load-mode! "invalid dynamic-load mode" (##sys#slot mode 0))] )
	(loop (##sys#slot mode 1)) ) )
    (##sys#set-dlopen-flags! now global) ) )

(let ([read read]
      [write write]
      [display display]
      [newline newline]
      [eval eval]
      [open-input-file open-input-file]
      [close-input-port close-input-port]
      [string-append string-append] 
      [load-verbose load-verbose]
      [topentry (##sys#make-c-string "C_toplevel")] )
  (define (has-sep? str)
    (let loop ([i (fx- (##sys#size str) 1)])
      (and (not (zero? i))
	   (if (memq (##core#inline "C_subchar" str i) '(#\\ #\/))
	       i
	       (loop (fx- i 1)) ) ) ) )
  (define (badfile x)
    (##sys#signal-hook #:type-error 'load "bad argument type - not a port or string" x) )
  (set! ##sys#load 
    (lambda (input evaluator pf #!optional timer printer)
      (when (string? input) 
	(set! input (##sys#expand-home-path input)) )
      (let* ([isdir #f]
	     [fname 
	     (cond [(port? input) #f]
		   [(not (string? input)) (badfile input)]
		   [(and-let* ([info (##sys#file-info input)]
			       [id (##sys#slot info 4)] )
		      (set! isdir (eq? 1 id)) 
		      (not id) )
		    input]
		   [else
		    (let ([fname2 (##sys#string-append input ##sys#load-dynamic-extension)])
		      (if (##sys#file-info fname2)
			  fname2
			  (let ([fname3 (##sys#string-append input source-file-extension)])
			    (if (##sys#file-info fname3)
				fname3
				(and (not isdir) input) ) ) ) ) ] ) ]
	    [evproc (or evaluator eval)] )
	(cond [(and (string? input) (not fname))
	       (##sys#signal-hook #:file-error 'load "can not open file" input) ]
	      [(and (load-verbose) fname)
	       (display "; loading ")
	       (display fname)
	       (display " ...\n") ] )
	(or (and fname
		 (or (##sys#dload (##sys#make-c-string fname) topentry #t) 
		     (and (not (has-sep? fname))
			  (##sys#dload (##sys#make-c-string (##sys#string-append "./" fname)) topentry #t) ) ) )
	    (call-with-current-continuation
	     (lambda (abrt)
	       (fluid-let ([##sys#read-error-with-line-number #t]
			   [##sys#current-source-filename fname]
			   [##sys#current-load-path
			    (and fname
				 (let ((i (has-sep? fname)))
				   (if i (##sys#substring fname 0 (fx+ i 1)) "") ) ) ]
			   [##sys#abort-load (lambda () (abrt #f))] )
		 (let ([in (if fname (open-input-file fname) input)])
		   (##sys#dynamic-wind
		    (lambda () #f)
		    (lambda ()
		      (let ([c1 (peek-char in)])
			(when (char=? c1 (integer->char 127))
			  (##sys#error 'load "unable to load compiled module" fname _dlerror) ) )
		      (let ((x1 (read in)))
			(do ((x x1 (read in)))
			    ((eof-object? x))
			  (when printer (printer x))
			  (##sys#call-with-values
			   (lambda () 
			     (if timer
				 (time (evproc x)) 
				 (evproc x) ) )
			   (lambda results
			     (when pf
			       (for-each
				(lambda (r) 
				  (write r)
				  (newline) )
				results) ) ) ) ) ) )
		    (lambda () (close-input-port in)) ) ) ) ) ) )
	(##core#undefined) ) ) )
  (set! load
    (lambda (filename . evaluator)
      (##sys#load filename (:optional evaluator #f) #f) ) )
  (set! load-relative
    (lambda (filename . evaluator)
      (##sys#load
       (if (memq (string-ref filename 0) '(#\\ #\/))
	   filename
	   (##sys#string-append ##sys#current-load-path filename) )
       (:optional evaluator #f) #f) ) )
  (set! load-noisily
    (lambda (filename #!key (evaluator #f) (time #f) (printer #f))
      (##sys#load filename evaluator #t time printer) ) ) )

(define ##sys#load-library-extension 	; this is crude...
  (cond [(eq? (software-type) 'windows) windows-load-library-extension]
	[(eq? (software-version) 'macosx) macosx-load-library-extension]
	[(and (eq? (software-version) 'hpux) 
	      (eq? (machine-type) 'hppa)) hppa-load-library-extension]
	[else default-load-library-extension] ) )

(define ##sys#load-dynamic-extension default-load-library-extension)

(define ##sys#default-dynamic-load-libraries 
  (case (build-platform)
    ((cygwin) cygwin-default-dynamic-load-libraries)
    (else default-dynamic-load-libraries) ) )

(define dynamic-load-libraries 
  (make-parameter
   (map (cut ##sys#string-append <> ##sys#load-library-extension) ##sys#default-dynamic-load-libraries)
   (lambda (x)
     (##sys#check-list x)
     x) ) )

(define ##sys#load-library
  (let ([load-verbose load-verbose]
	[string-append string-append]
	[dynamic-load-libraries dynamic-load-libraries]
	[display display] )
    (lambda (uname lib)
      (let ([id (##sys#->feature-id uname)])
	(or (memq id ##sys#features)
	    (let ([libs
		   (if lib
		       (##sys#list lib)
		       (cons (##sys#string-append (##sys#slot uname 1) ##sys#load-library-extension)
			     (dynamic-load-libraries) ) ) ]
		  [top 
		   (##sys#make-c-string
		    (string-append 
		     "C_"
		     (##sys#string->c-identifier (##sys#slot uname 1)) 
		     "_toplevel") ) ] )
	      (when (load-verbose)
		(display "; loading library ")
		(display uname)
		(display " ...\n") )
	      (let loop ([libs libs])
		(cond [(null? libs) #f]
		      [(##sys#dload (##sys#make-c-string (##sys#slot libs 0)) top #f)
		       (unless (memq id ##sys#features) (set! ##sys#features (cons id ##sys#features)))
		       #t]
		      [else (loop (##sys#slot libs 1))] ) ) ) ) ) ) ) )

(define load-library
  (lambda (uname . lib)
    (##sys#check-symbol uname 'load-library)
    (or (##sys#load-library uname (and (pair? lib) (car lib)))
	(##sys#error 'load-library "unable to load library" uname _dlerror) ) ) )

(define ##sys#split-at-separator
  (let ([reverse reverse] )
    (lambda (str sep)
      (let ([len (##sys#size str)])
	(let loop ([items '()] [i 0] [j 0])
	  (cond [(fx>= i len)
		 (reverse (cons (##sys#substring str j len) items)) ]
		[(char=? (##core#inline "C_subchar" str i) sep)
		 (let ([i2 (fx+ i 1)])
		   (loop (cons (##sys#substring str j i) items) i2 i2) ) ]
		[else (loop items (fx+ i 1) j)] ) ) ) ) ) )


;;; Extensions:

(define ##sys#canonicalize-extension-path
  (let ([string-append string-append])
    (lambda (id loc)
      (define (err) (##sys#error loc "invalid extension path" id))
      (define (sep? c) (or (char=? #\\ c) (char=? #\/ c)))
      (let ([p (cond [(string? id) id]
		     [(symbol? id) (##sys#symbol->string id)]
		     [(list? id) 
		      (let loop ([id id])
			(if (null? id)
			    ""
			    (string-append 
			     (let ([id0 (##sys#slot id 0)])
			       (cond [(symbol? id0) (##sys#symbol->string id0)]
				     [(string? id0) id0]
				     [else (err)] ) )
			     (if (null? (##sys#slot id 1))
				 ""
				 "/")
			     (loop (##sys#slot id 1)) ) ) ) ] ) ] )
	(let check ([p p])
	  (let ([n (##sys#size p)])
	    (cond [(fx= 0 n) (err)]
		  [(sep? (string-ref p 0))
		   (check (##sys#substring p 1 n)) ]
		  [(sep? (string-ref p (fx- n 1)))
		   (check (##sys#substring p 0 (fx- n 1))) ]
		  [else p] ) ) ) ) ) ) )

(define ##sys#repository-path
  (make-parameter 
   (or (getenv repository-environment-variable)
       (##sys#chicken-prefix 
	(##sys#string-append 
	 "lib/chicken/"
	 (##sys#number->string (or (##sys#fudge 42) default-binary-version)) ) )
       install-egg-home) ) )

(define repository-path ##sys#repository-path)

(define ##sys#find-extension
  (let ([file-exists? file-exists?]
	[string-append string-append] )
    (lambda (p inc?)
	(define (check path)
	  (let ([p0 (string-append path "/" p)])
	    (and (or (file-exists? (##sys#string-append p0 ##sys#load-dynamic-extension))
		     (file-exists? (##sys#string-append p0 source-file-extension)) )
		 p0) ) )
	(let loop ([paths (##sys#append (list (##sys#repository-path))
					(if inc? (##sys#append ##sys#include-pathnames '(".")) '()) ) ] )
	  (and (pair? paths)
	       (let ([pa (##sys#slot paths 0)])
		 (or (check pa)
		     (loop (##sys#slot paths 1)) ) ) ) ) ) ) )

(define ##sys#loaded-extensions '())

(define ##sys#load-extension
  (let ((string->symbol string->symbol))
    (lambda (id loc . err?)
      (cond ((string? id) (set! id (string->symbol id)))
	    (else (##sys#check-symbol id loc)) )
      (let ([p (##sys#canonicalize-extension-path id loc)])
	(cond ((member p ##sys#loaded-extensions))
	      ((memq id ##sys#core-library-modules)
	       (##sys#load-library id #f) )
	      (else
	       (let ([id2 (##sys#find-extension p #t)])
		 (cond (id2
			(##sys#load id2 #f #f)
			(set! ##sys#loaded-extensions (cons p ##sys#loaded-extensions)) 
			#t)
		       ((:optional err? #t) (##sys#error loc "can not load extension" id))
		       (else #f) ) ) ) ) ) ) ) )

(define (##sys#provide . ids)
  (for-each
   (lambda (id)
     (##sys#check-symbol id 'provide)
     (let ([p (##sys#canonicalize-extension-path id 'provide)])
       (set! ##sys#loaded-extensions (cons p ##sys#loaded-extensions)) ) ) 
   ids) )

(define provide ##sys#provide)

(define (##sys#provided? id)
  (and (member (##sys#canonicalize-extension-path id 'provided?) ##sys#loaded-extensions) 
       #t) )

(define provided? ##sys#provided?)

(define ##sys#require
  (lambda ids
    (for-each
     (cut ##sys#load-extension <> 'require) 
     ids) ) )

(define require ##sys#require)

(define ##sys#extension-information
  (let ([with-input-from-file with-input-from-file]
	[file-exists? file-exists?]
	[string-append string-append]
	[read read] )
    (lambda (id loc)
      (let* ((p (##sys#canonicalize-extension-path id loc))
	     (rpath (string-append (##sys#repository-path) "/" p ".")) )
	(cond ((file-exists? (string-append rpath setup-file-extension))
	       => (cut with-input-from-file <> read) )
	      (else #f) ) ) ) ) )

(define (extension-information ext)
  (##sys#extension-information ext 'extension-information) )

(define ##sys#lookup-runtime-requirements 
  (let ([with-input-from-file with-input-from-file]
	[read read] )
    (lambda (ids)
      (let loop1 ([ids ids])
	(if (null? ids)
	    '()
	    (append
	     (or (and-let* ([info (##sys#extension-information (car ids) #f)]
			    [a (assq 'require-at-runtime info)] )
		   (cdr a) )
		 '() )
	     (loop1 (cdr ids)) ) ) ) ) ) )

(define ##sys#do-the-right-thing
  (let ((vector->list vector->list))
    (lambda (id comp?)
      (define (add-req id)
	(when comp?
	  (hash-table-update! 		; assumes compiler has extras available - will break in the interpreter
	   ##compiler#file-requirements
	   'syntax-requirements
	   (cut lset-adjoin eq? <> id) 
	   (lambda () (list id)))))
      (define (doit id)
	(cond ((or (memq id builtin-features)
		   (if comp?
		       (memq id builtin-features/compiled)
		       (##sys#feature? id) ) )
	       (values '(##sys#void) #t) )
	      ((memq id special-syntax-files)
	       (let ((fid (##sys#->feature-id id)))
		 (unless (memq fid ##sys#features)
		   (##sys#load (##sys#resolve-include-filename (##sys#symbol->string id) #t) #f #f) 
		   (set! ##sys#features (cons fid ##sys#features)) )
		 (values '(##sys#void) #t) ) )
	      ((memq id ##sys#core-library-modules)
	       (values
		(if comp?
		    `(##core#declare '(uses ,id))
		    `(load-library ',id) )
		#t) )
	      ((memq id ##sys#explicit-library-modules)
	       (let* ((info (##sys#extension-information id 'require-extension))
		      (s (assq 'syntax info)))
		 (values
		  `(begin
		     ,@(if s `((##core#require-for-syntax ',id)) '())
		     ,(if comp?
			  `(##core#declare '(uses ,id)) 
			  `(load-library ',id) ) )
		  #t) ) )
	      (else
	       (let ((info (##sys#extension-information id 'require-extension)))
		 (cond (info
			(let ((s (assq 'syntax info))
			      (rr (assq 'require-at-runtime info)) )
			  (when s (add-req id))
			  (values 
			   `(begin
			      ,@(if s `((##core#require-for-syntax ',id)) '())
			      ,@(if (and (not rr) s)
				   '()
				   `((##sys#require
				      ,@(map (lambda (id) `',id)
					     (cond (rr (cdr rr))
						   (else (list id)) ) ) ) ) ) )
			   #t) ) )
		       (else
			(add-req id)
			(values `(##sys#require ',id) #f)) ) ) ) ) )
      (if (and (pair? id) (symbol? (car id)))
	  (let ((a (assq (##sys#slot id 0) ##sys#extension-specifiers)))
	    (if a
		(let ((a ((##sys#slot a 1) id)))
		  (cond ((string? a) (values `(load ,a) #f))
			((vector? a) 
			 (let loop ((specs (vector->list a))
				    (exps '())
				    (f #f) )
			   (if (null? specs)
			       (values `(begin ,@(reverse exps)) f)
			       (let-values (((exp fi) (##sys#do-the-right-thing (car specs) comp?)))
				 (loop (cdr specs)
				       (cons exp exps)
				       (or fi f) ) ) ) ) )
			(else (##sys#do-the-right-thing a comp?)) ) )
		(##sys#error "undefined extension specifier" id) ) )
	  (if (symbol? id)
	      (doit id) 
	      (##sys#error "invalid extension specifier" id) ) ) ) ) )

(define ##sys#extension-specifiers '())

(define (set-extension-specifier! name proc)
  (##sys#check-symbol name 'set-extension-specifier!)
  (let ([a (assq name ##sys#extension-specifiers)])
    (if a
	(let ([old (##sys#slot a 1)])
	  (##sys#setslot a 1 (lambda (spec) (proc spec old))) )
	(set! ##sys#extension-specifiers
	  (cons (cons name (lambda (spec) (proc spec #f)))
		##sys#extension-specifiers)) ) ) )


;;; SRFI-55

(set-extension-specifier!
 'srfi 
 (let ([list->vector list->vector])
   (lambda (spec old)
     (list->vector
      (let loop ([ids (cdr spec)])
	(if (null? ids)
	    '()
	    (let ([id (car ids)])
	      (##sys#check-exact id 'require-extension)
	      (cons (##sys#string->symbol (##sys#string-append "srfi-" (number->string id)))
		    (loop (cdr ids)) ) ) ) ) ) ) ) )


;;; Version checking

(set-extension-specifier!
 'version
 (lambda (spec _)
   (define (->string x)
     (cond ((string? x) x)
	   ((symbol? x) (##sys#slot x 1))
	   ((number? x) (##sys#number->string x))
	   (else (error "invalid extension version" x)) ) )
   (match spec
     (('version id v)
      (let* ((info (extension-information id))
	     (vv (and info (assq 'version info))) )
	(unless (and vv (string>=? (->string (car vv)) (->string v)))
	  (error "installed extension does not match required version" id vv v) )
	id) )
     (_ (syntax-error 'require-extension "invalid version specification" spec)) ) ) )


;;; Convert string into valid C-identifier:

(define ##sys#string->c-identifier
  (let ([string-copy string-copy])
    (lambda (str)
      (let* ([s2 (string-copy str)]
	     [n (##sys#size s2)] )
	(do ([i 0 (fx+ i 1)])
	    ((fx>= i n) s2)
	  (let ([c (##core#inline "C_subchar" s2 i)])
	    (when (and (not (char-alphabetic? c)) (or (not (char-numeric? c)) (fx= i 0)))
	      (##core#inline "C_setsubchar" s2 i #\_) ) ) ) ) ) ) )


;;; Environments:

(define ##sys#r4rs-environment (make-vector environment-table-size '()))
(define ##sys#r5rs-environment #f)
(define ##sys#interaction-environment (##sys#make-structure 'environment #f #t))

(define ##sys#copy-env-table
  (lambda (e mff mf . args)
    (let ([syms (and (pair? args) (car args))])
      (let* ([s (##sys#size e)]
             [e2 (##sys#make-vector s '())] )
       (do ([i 0 (fx+ i 1)])
           ((fx>= i s) e2)
         (##sys#setslot 
          e2 i
          (let copy ([b (##sys#slot e i)])
            (if (null? b)
                '()
                (let ([bi (##sys#slot b 0)])
                  (let ([sym (##sys#slot bi 0)])
                    (if (or (not syms) (memq sym syms))
                      (cons (vector
                              sym
                              (##sys#slot bi 1)
                              (if mff mf (##sys#slot bi 2)))
                            (copy (##sys#slot b 1)))
                      (copy (##sys#slot b 1)) ) ) ) ) ) ) ) ) ) ) )

(define ##sys#environment-symbols
  (lambda (env . args)
    (##sys#check-structure env 'environment)
    (let ([pred (and (pair? args) (car args))])
      (let ([envtbl (##sys#slot env 1)])
        (if envtbl
            ;then "real" environment
          (let ([envtblsiz (vector-length envtbl)])
            (do ([i 0 (fx+ i 1)]
                 [syms
                   '()
                   (let loop ([bucket (vector-ref envtbl i)] [syms syms])
                     (if (null? bucket)
                       syms
                       (let ([sym (vector-ref (car bucket) 0)])
                         (if (or (not pred) (pred sym))
                           (loop (cdr bucket) (cons sym syms))
                           (loop (cdr bucket) syms) ) ) ) )])
	        ((fx>= i envtblsiz) syms) ) )
	    ;else interaction-environment
	  (let ([syms '()])
	    (##sys#walk-namespace
	      (lambda (sym)
	        (when (or (not pred) (pred sym))
	          (set! syms (cons sym syms)) ) ) )
	    syms ) ) ) ) ) )

(define (interaction-environment) ##sys#interaction-environment)

(define scheme-report-environment
  (lambda (n . mutable)
    (##sys#check-exact n 'scheme-report-environment)
    (let ([mf (and (pair? mutable) (car mutable))])
      (case n
	[(4) (##sys#make-structure 'environment (##sys#copy-env-table ##sys#r4rs-environment #t mf) mf)]
	[(5) (##sys#make-structure 'environment (##sys#copy-env-table ##sys#r5rs-environment #t mf) mf)]
	[else (##sys#error 'scheme-report-environment "no support for version" n)] ) ) ) )

(define null-environment
  (let ([make-vector make-vector])
    (lambda (n . mutable)
      (##sys#check-exact n 'null-environment)
      (when (or (fx< n 4) (fx> n 5))
	(##sys#error 'null-environment "no support for version" n) )
      (##sys#make-structure
       'environment
       (make-vector environment-table-size '())
       (and (pair? mutable) (car mutable)) ) ) ) )

(let ()
  (define (initb ht) 
    (lambda (b)
      (let ([loc (##sys#hash-table-location ht b #t)])
        (##sys#setslot loc 1 (##sys#slot b 0)) ) ) )
  (for-each 
   (initb ##sys#r4rs-environment)
   '(not boolean? eq? eqv? equal? pair? cons car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar
     cdadr cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr cadddr cdaaar cdaadr cdadar cdaddr
     cddaar cddadr cdddar cddddr set-car! set-cdr! null? list? list length list-tail list-ref
     append reverse memq memv member assq assv assoc symbol? symbol->string string->symbol
     number? integer? exact? real? complex? inexact? rational? zero? odd? even? positive? negative?
     max min + - * / = > < >= <= quotient remainder modulo gcd lcm abs floor ceiling truncate round
     exact->inexact inexact->exact exp log expt sqrt sin cos tan asin acos atan number->string
     string->number char? char=? char>? char<? char>=? char<=? char-ci=? char-ci<? char-ci>?
     char-ci>=? char-ci<=? char-alphabetic? char-whitespace? char-numeric? char-upper-case?
     char-lower-case? char-upcase char-downcase char->integer integer->char string? string=?
     string>? string<? string>=? string<=? string-ci=? string-ci<? string-ci>? string-ci>=? string-ci<=?
     make-string string-length string-ref string-set! string-append string-copy string->list 
     list->string substring string-fill! vector? make-vector vector-ref vector-set! string vector
     vector-length vector->list list->vector vector-fill! procedure? map for-each apply force 
     call-with-current-continuation input-port? output-port? current-input-port current-output-port
     call-with-input-file call-with-output-file open-input-file open-output-file close-input-port
     close-output-port load read eof-object? read-char peek-char
     write display write-char newline with-input-from-file with-output-to-file ##sys#call-with-values
     ##sys#values ##sys#dynamic-wind ##sys#void
     ##sys#list->vector ##sys#list ##sys#append ##sys#cons ##sys#make-promise) )
  (set! ##sys#r5rs-environment (##sys#copy-env-table ##sys#r4rs-environment #t #t))
  (for-each
   (initb ##sys#r5rs-environment)
   '(dynamic-wind values call-with-values eval scheme-report-environment null-environment interaction-environment) ) )


;;; Find included file:

(define ##sys#include-pathnames 
  (let ((h (chicken-home)))
    (if h (list h) '())) )

(define ##sys#resolve-include-filename
  (let ((string-append string-append) )
    (define (exists? fname)
      (let ([info (##sys#file-info fname)])
	(and info (not (eq? 1 (##sys#slot info 4)))) ) )
    (lambda (fname prefer-source #!optional repo)
      (define (test2 fname lst)
	(if (null? lst)
	    (and (exists? fname) fname)
	    (let ([fn (##sys#string-append fname (car lst))])
	      (if (exists? fn)
		  fn
		  (test2 fname (cdr lst)) ) ) ) )
      (define (test fname)
	(test2 
	 fname 
	 (if prefer-source
	     (list source-file-extension ##sys#load-dynamic-extension)
	     (list ##sys#load-dynamic-extension source-file-extension) ) ) )
      (or (test fname)
	  (let loop ((paths (if repo
				(##sys#append ##sys#include-pathnames (list (##sys#repository-path)))
				##sys#include-pathnames) ) )
	    (cond ((eq? paths '()) fname)
		  ((test (string-append (##sys#slot paths 0)
					"/"
					fname) ) )
		  (else (loop (##sys#slot paths 1))) ) ) ) ) ) )


;;; Print timing information (support for "time" macro):

(define ##sys#display-times
  (let* ((display display)
	 (spaces 
	  (lambda (n)
	    (do ((i n (fx- i 1)))
		((fx<= i 0))
	      (display #\space) ) ) )
	 (display-rj 
	  (lambda (x w)
	    (let* ((xs (if (zero? x) "0" (number->string x)))
		   (xslen (##core#inline "C_block_size" xs)) )
	      (spaces (fx- w xslen))
	      (display xs) ) ) ) )
    (lambda (info)
      (display-rj (##sys#slot info 0) 8)
      (display " seconds elapsed\n") 
      (display-rj (##sys#slot info 1) 8)
      (display " seconds in (major) GC\n")
      (display-rj (##sys#slot info 2) 8)
      (display " mutations\n")
      (display-rj (##sys#slot info 3) 8)
      (display " minor GCs\n")
      (display-rj (##sys#slot info 4) 8)
      (display " major GCs\n") ) ) )


;;; General syntax checking routine:

(define ##sys#line-number-database #f)
(define (##sys#syntax-error-hook . args) (apply ##sys#signal-hook #:syntax-error args))
(define ##sys#syntax-error-culprit #f)

(define syntax-error ##sys#syntax-error-hook)

(define (get-line-number sexp)
  (and ##sys#line-number-database
       (pair? sexp)
       (let ([head (##sys#slot sexp 0)])
	 (and (symbol? head)
	      (cond [(##sys#hash-table-ref ##sys#line-number-database head)
		     => (lambda (pl)
			  (let ([a (assq sexp pl)])
			    (and a (##sys#slot a 1)) ) ) ]
		    [else #f] ) ) ) ) )

(define ##sys#check-syntax
  (let ([string-append string-append]
	[keyword? keyword?]
	[get-line-number get-line-number]
	[symbol->string symbol->string] )
    (lambda (id exp pat . culprit)

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
	      (cond ((eq? x '()))
		    ((not (##core#inline "C_blockp" x)) #f)
		    ((identfier? x) (not (keyword? x)))
		    ((##core#inline "C_pairp" x)
		     (let ((s (##sys#slot x 0)))
		       (if (or (not (##core#inline "C_blockp" s)) (not (##core#inline "C_symbolp" s)))
			   #f
			   (loop (##sys#slot x 1)) ) ) ) 
		    (else #f) ) ) ) )

      (define (proper-list? x)
	(let loop ((x x))
	  (cond ((eq? x '()))
		((and (##core#inline "C_blockp" x) (##core#inline "C_pairp" x)) (loop (##sys#slot x 1)))
		(else #f) ) ) )

      (when (pair? culprit) (set! ##sys#syntax-error-culprit (car culprit)))
      (let walk ((x exp) (p pat))
	(cond ((and (##core#inline "C_blockp" p) (##core#inline "C_vectorp" p))
	       (let* ((p2 (##sys#slot p 0))
		      (vlen (##core#inline "C_block_size" p))
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
	      ((not (##core#inline "C_blockp" p))
	       (if (not (eq? p x)) (err "unexpected object")) )
	      ((symbol? x)
	       (case p
		 ((_) #t)
		 ((pair) (test x pair? "pair expected"))
		 ((variable) (test x identifier? "identifier expected"))
		 ((symbol) (test x symbol? "symbol expected"))
		 ((list) (test x proper-list? "proper list expected"))
		 ((number) (test x number? "number expected"))
		 ((string) (test x string? "string expected"))
		 ((lambda-list) (test x lambda-list? "lambda-list expected"))
		 (else (test x (lambda (y) (eq? y p)) "missing keyword")) ) )
	      ((or (not (##core#inline "C_blockp" x)) (not (##core#inline "C_pairp" x)))
	       (err "incomplete form") )
	      (else
	       (walk (##sys#slot x 0) (##sys#slot p 0))
	       (walk (##sys#slot x 1) (##sys#slot p 1)) ) ) ) ) ) )


;;; SRFI-0 support code:

(set! ##sys#features
  (append '(#:srfi-8 #:srfi-6 #:srfi-2 #:srfi-0 #:srfi-10 #:srfi-9 #:srfi-55 #:srfi-61) 
	  ##sys#features))


;;;; Read-Eval-Print loop:

(define ##sys#repl-eval-hook #f)
(define ##sys#repl-print-length-limit #f)
(define ##sys#repl-read-hook #f)

(define (##sys#repl-print-hook x port)
  (##sys#with-print-length-limit ##sys#repl-print-length-limit (cut ##sys#print x #t port))
  (##sys#write-char-0 #\newline port) )

(define repl-prompt (make-parameter (lambda () "#;> ")))

(define ##sys#read-prompt-hook
  (let ([repl-prompt repl-prompt])
    (lambda () 
      (##sys#print ((repl-prompt)) #f ##sys#standard-output)
      (##sys#flush-output ##sys#standard-output) ) ) )

(define ##sys#clear-trace-buffer (foreign-lambda void "C_clear_trace_buffer"))

(define repl
  (let ((eval eval)
	(read read)
	(call-with-current-continuation call-with-current-continuation)
	(print-call-chain print-call-chain)
	(flush-output flush-output)
	(load-verbose load-verbose)
	(reset reset) )
    (lambda ()

      (define (write-err xs)
	(for-each (cut ##sys#repl-print-hook <> ##sys#standard-error) xs) )

      (define (write-results xs)
	(unless (or (null? xs) (eq? (##core#undefined) (car xs)))
	  (for-each (cut ##sys#repl-print-hook <> ##sys#standard-output) xs) ) )

      (let ((stdin ##sys#standard-input)
	    (stdout ##sys#standard-output)
	    (stderr ##sys#standard-error)
	    (ehandler (##sys#error-handler))
	    (rhandler (##sys#reset-handler)) 
	    (lv #f)
	    (uie ##sys#unbound-in-eval) )

	(define (saveports)
	  (set! stdin ##sys#standard-input)
	  (set! stdout ##sys#standard-output)
	  (set! stderr ##sys#standard-error) )

	(define (resetports)
	  (set! ##sys#standard-input stdin)
	  (set! ##sys#standard-output stdout)
	  (set! ##sys#standard-error stderr) )

	(##sys#dynamic-wind
	 (lambda ()
	   (set! lv (load-verbose))
	   (load-verbose #t)
	   (##sys#error-handler
	    (lambda (msg . args)
	      (resetports)
	      (##sys#print "Error" #f ##sys#standard-error)
	      (when msg
		(##sys#print ": " #f ##sys#standard-error)
		(##sys#print msg #f ##sys#standard-error) )
	      (if (and (pair? args) (null? (cdr args)))
		  (begin
		    (##sys#print ": " #f ##sys#standard-error)
		    (write-err args) )
		  (begin
		    (##sys#write-char-0 #\newline ##sys#standard-error)
		    (write-err args) ) )
	      (print-call-chain ##sys#standard-error)
	      (flush-output ##sys#standard-error) ) ) )
	 (lambda ()
	   (let loop ()
	     (saveports)
	     (call-with-current-continuation
	      (lambda (c)
		(##sys#reset-handler
		 (lambda ()
		   (set! ##sys#read-error-with-line-number #f)
		   (set! ##sys#enable-qualifiers #t)
		   (resetports)
		   (c #f) ) ) ) )
	     (##sys#read-prompt-hook)
	     (let ([exp ((or ##sys#repl-read-hook read))])
	       (unless (eof-object? exp)
		 (when (char=? #\newline (##sys#peek-char-0 ##sys#standard-input))
		   (##sys#read-char-0 ##sys#standard-input) )
		 (##sys#clear-trace-buffer)
		 (set! ##sys#unbound-in-eval '())
		 (receive result ((or ##sys#repl-eval-hook eval) exp)
		   (when (and ##sys#warnings-enabled (pair? ##sys#unbound-in-eval))
		     (let loop ((vars ##sys#unbound-in-eval) (u '()))
		       (cond ((null? vars)
			      (when (pair? u)
				(##sys#print 
				 "Warning: the following toplevel variables are referenced but unbound:\n" 
				 #f ##sys#standard-error)
				(for-each 
				 (lambda (v)
				   (##sys#print "  " #f ##sys#standard-error)
				   (##sys#print (car v) #t ##sys#standard-error)
				   (when (cdr v)
				     (##sys#print " (in " #f ##sys#standard-error)
				     (##sys#print (cdr v) #t ##sys#standard-error) 
				     (##sys#write-char-0 #\) ##sys#standard-error) )
				   (##sys#write-char-0 #\newline ##sys#standard-error) )
				 u) ) )
			     ((or (memq (caar vars) u) 
				  (##sys#symbol-has-toplevel-binding? (caar vars)) )
			      (loop (cdr vars) u) )
			     (else (loop (cdr vars) (cons (car vars) u))) ) 9 ) )
		   (write-results result) 
		   (loop) ) ) ) ) )
	 (lambda ()
	   (load-verbose lv)
	   (set! ##sys#unbound-in-eval uie)
	   (##sys#error-handler ehandler)
	   (##sys#reset-handler rhandler) ) ) ) ) ) )


;;; SRFI-10:

(define ##sys#sharp-comma-reader-ctors (make-vector 301 '()))

(define (define-reader-ctor spec proc)
  (##sys#check-symbol spec 'define-reader-ctor)
  (##sys#hash-table-set! ##sys#sharp-comma-reader-ctors spec proc) )

(set! ##sys#user-read-hook
  (let ((old ##sys#user-read-hook)
	(read-char read-char)
	(read read) )
    (lambda (char port)
      (cond ((char=? char #\,)
	     (read-char port)
	     (let* ((exp (read port))
		    (err (lambda () (##sys#read-error port "invalid sharp-comma external form" exp))) )
	       (if (or (null? exp) (not (list? exp)))
		   (err)
		   (let ([spec (##sys#slot exp 0)])
		     (if (not (symbol? spec))
			 (err) 
			 (let ((ctor (##sys#hash-table-ref ##sys#sharp-comma-reader-ctors spec)))
			   (if ctor
			       (apply ctor (##sys#slot exp 1))
			       (##sys#read-error port "undefined sharp-comma constructor" spec) ) ) ) ) ) ) )
	    ((char=? char #\{)
	     (let ((lst (read port)))
	       (make-identifier (car lst) (cdr lst))))
	    (else (old char port)) ) ) ) )


;;; Handy to have by default:

(##sys#register-macro
 'define-macro
 (lambda (head . body)
   (define (expand name val)
     (let ((m2 (and (pair? val) (eq? 'lambda (car val))
		    (pair? (cdr val)) (symbol? (cadr val))) ))
       `(,(if ##sys#enable-runtime-macros '##core#elaborationtimetoo '##core#elaborationtimeonly)
	 ,(cond (m2 `(##sys#register-macro-2 ',name (lambda (,(cadr val)) ,@(cddr val))))
		((symbol? val) `(##sys#copy-macro ',val ',name))
		(else `(##sys#register-macro ',name ,val) ) ) ) ) )
   (cond ((symbol? head)
	  (##sys#check-syntax 'define-macro body '(_))
	  (expand head (car body)) )
	 (else
	  (##sys#check-syntax 'define-macro head '(symbol . lambda-list))
	  (##sys#check-syntax 'define-macro body '#(_ 1)) 
	  (expand (car head) `(lambda ,(cdr head) ,@body))))))

(##sys#register-macro
 'require-extension
 (lambda ids
   (##sys#check-syntax 'require-extension ids '#(_ 0))
   `(##core#require-extension ,@(map (lambda (x) (list 'quote x)) ids) ) ) )


;;; To catch common errors:

(##sys#register-macro-2
 'define-syntax
 (lambda (form)
   (##sys#syntax-error-hook 'define-syntax "highlevel macros are not supported")))

(##sys#register-macro-2
 'module
 (lambda (form)
   (##sys#syntax-error-hook 'module "modules are not supported")))


;;; Simple invocation API:

(declare
  (hide last-error run-safe store-result store-string
	CHICKEN_yield CHICKEN_apply_to_string
	CHICKEN_eval CHICKEN_eval_string CHICKEN_eval_to_string CHICKEN_eval_string_to_string
	CHICKEN_apply CHICKEN_eval_apply CHICKEN_eval_to_string
	CHICKEN_read CHICKEN_load CHICKEN_get_error_message) )
	
(define last-error #f)

(define (run-safe thunk)
  (set! last-error #f)
  (handle-exceptions ex 
      (let ((o (open-output-string)))
	(print-error-message ex o)
	(set! last-error (get-output-string o))
	#f)
    (thunk) ) )

#>
#define C_store_result(x, ptr)   (*((C_word *)C_block_item(ptr, 0)) = (x), C_SCHEME_TRUE)
<#

(define (store-result x result)
  (##sys#gc #f)
  (when result
    (##core#inline "C_store_result" x result) )
  #t)

(define-external (CHICKEN_yield) bool
  (run-safe (lambda () (begin (thread-yield!) #t))) )

(define-external (CHICKEN_eval (scheme-object exp) ((c-pointer "C_word") result)) bool
  (run-safe
   (lambda ()
     (store-result (eval exp) result) ) ) )

(define-external (CHICKEN_eval_string (c-string str) ((c-pointer "C_word") result)) bool
  (run-safe
   (lambda ()
     (let ([i (open-input-string str)])
       (store-result (eval (read i)) result)) )))

#>
#define C_copy_result_string(str, buf, n)  (C_memcpy((char *)C_block_item(buf, 0), C_c_string(str), C_unfix(n)), ((char *)C_block_item(buf, 0))[ C_unfix(n) ] = '\0', C_SCHEME_TRUE)
<#

(define (store-string str bufsize buf)
  (let ((len (##sys#size str)))
    (cond ((fx>= len bufsize)
	   (set! last-error "Error: not enough room for result string")
	   #f)
	  (else (##core#inline "C_copy_result_string" str buf len)) ) ) )

(define-external (CHICKEN_eval_to_string (scheme-object exp) ((c-pointer "char") buf)
					  (int bufsize))
  bool
  (run-safe
   (lambda ()
     (let ([o (open-output-string)])
       (write (eval exp) o) 
       (store-string (get-output-string o) bufsize buf)) ) ) )

(define-external (CHICKEN_eval_string_to_string (c-string str) ((c-pointer "char") buf)
						 (int bufsize) ) 
  bool
  (run-safe
   (lambda ()
     (let ([o (open-output-string)])
       (write (eval (read (open-input-string str))) o)
       (store-string (get-output-string o) bufsize buf)) ) ) )

(define-external (CHICKEN_apply (scheme-object func) (scheme-object args) 
				 ((c-pointer "C_word") result))
  bool
  (run-safe (lambda () (store-result (apply func args) result))) )

(define-external (CHICKEN_apply_to_string (scheme-object func) (scheme-object args) 
					   ((c-pointer "char") buf) (int bufsize))
  bool
  (run-safe
   (lambda ()
     (let ([o (open-output-string)])
       (write (apply func args) o) 
       (store-string (get-output-string o) bufsize buf)) ) ) )

(define-external (CHICKEN_read (c-string str) ((c-pointer "C_word") result)) bool
  (run-safe
   (lambda ()
     (let ([i (open-input-string str)])
       (store-result (read i) result) ) ) ) )

(define-external (CHICKEN_load (c-string str)) bool
  (run-safe (lambda () (load str) #t)) )

(define-external (CHICKEN_get_error_message ((c-pointer "char") buf) (int bufsize)) void
  (store-string (or last-error "No error") bufsize buf) )


;;; Create lambda-info object

(define (##sys#make-lambda-info str)
  (let* ((sz (##sys#size str))
	 (info (##sys#make-string sz)) )
    (##core#inline "C_copy_memory" info str sz)
    (##core#inline "C_string_to_lambdainfo" info)
    info) )
