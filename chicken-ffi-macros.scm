;;;; chicken-ffi-macros.scm
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


(define-macro (define-foreign-type . xs)
  (##sys#check-syntax 'define-foreign-type xs '(symbol . #(_ 1 3)))
  `(##core#define-foreign-type
    ,(list 'quote (car xs))
    ,(list 'quote (cadr xs))
    ,@(cddr xs) ) )

(define-macro (define-foreign-variable . xs)
  (##sys#check-syntax 'define-foreign-variable xs '(symbol . #(_ 1)))
  `(##core#define-foreign-variable ,@(map (lambda (x) (list 'quote x)) xs)) )

(define-macro (foreign-lambda . xs)
  (##sys#check-syntax 'foreign-lambda xs '(_ _ . #(_ 0)))
  `(##core#foreign-lambda ,@(map (lambda (x) (list 'quote x)) xs)) )

(define-macro (foreign-lambda* . xs)
  (##sys#check-syntax 'foreign-lambda* xs '(_ #((_ _) 0) . #(string 1)))
  `(##core#foreign-lambda* ,@(map (lambda (x) (list 'quote x)) xs)) )

(define-macro (foreign-safe-lambda . xs)
  (##sys#check-syntax 'foreign-safe-lambda xs '(_ _ . #(_ 0)))
  `(##core#foreign-callback-lambda ,@(map (lambda (x) (list 'quote x)) xs)) )

(define-macro (foreign-safe-lambda* . xs)
  (##sys#check-syntax 'foreign-safe-lambda* xs '(_ #((_ _) 0) . #(string 1)))
  `(##core#foreign-callback-lambda* ,@(map (lambda (x) (list 'quote x)) xs)) )

(define-macro (foreign-primitive . xs)
  (##sys#check-syntax 'foreign-primitive xs '#(_ 1)) 
  `(##core#foreign-primitive ,@(map (lambda (x) (list 'quote x)) xs)) )

(define-macro (foreign-safe-wrapper . form) ; DEPRECATED
  (##sys#check-syntax 'foreign-safe-wrapper form '(_ string . #(_ 1)))
  (if (string? (caddr form))
      (begin
	(##sys#check-syntax 'foreign-safe-wrapper form '(_ string string _ (lambda lambda-list . #(_ 1))))
	`(##core#foreign-callback-wrapper
	  ',(cadr form)
	  ',(caddr form)
	  ',(car form)
	  ',(cadddr form)
	  ,(cadddr (cdr form)) ) )
      (begin
	(##sys#check-syntax 'foreign-safe-wrapper form '(_ string _ (lambda lambda-list . #(_ 1))))
	`(##core#foreign-callback-wrapper
	  ',(cadr form)
	  ',""
	  ',(car form)
	  ',(caddr form)
	  ,(cadddr form) ) ) ) )

(define-macro (define-external . form)
  (let* ([quals (and (pair? form) (string? (car form)))]
	 [var (and (not quals) (pair? form) (symbol? (car form)))] )
    (cond [var
	   (##sys#check-syntax 'define-external form '(symbol _ . #(_ 0 1)))
	   (let ([var (car form)])
	     `(begin
		(##core#define-foreign-variable ',var ',(cadr form))
		(##core#define-external-variable ',var ',(cadr form) '#t)
		,@(if (pair? (cddr form))
		      `((##core#set! ,var ,(caddr form)))
		      '() ) ) ) ]
	  [else
	   (if quals
	       (##sys#check-syntax 'define-external form '(string (symbol . #((_ symbol) 0)) _ . #(_ 1)))
	       (##sys#check-syntax 'define-external form '((symbol . #((_ symbol) 0)) _ . #(_ 1))) )
	   (let* ([head (if quals (cadr form) (car form))]
		  [args (cdr head)] )
	     `(define ,(car head)
		(##core#foreign-callback-wrapper
		 ',(car head)
		 ',(if quals (car form) "")
		 ',(if quals (caddr form) (cadr form))
		 ',(map (lambda (a) (car a)) args)
		 (lambda ,(map (lambda (a) (cadr a)) args) ,@(if quals (cdddr form) (cddr form)) ) ) ) ) ] ) ) )



;;; External locations:

(define-macro (define-location var type . init)
  (let ([name (gensym)])
    `(begin
       (##core#define-foreign-variable ',var ',type ',(symbol->string name))
       (##core#define-external-variable ',var ',type '#f ',name)
       ,@(if (pair? init)
	     `((##core#set! ,var ,(car init)))
	     '() ) ) ) ) 

(define-macro (let-location bindings . body)
  (let ([aliases (map (lambda (_) (gensym)) bindings)])
    `(let ,(append-map
	    (lambda (b a)
	      (if (pair? (cddr b))
		  (list (cons a (cddr b)))
		  '() ) )
	    bindings aliases)
       ,(fold-right
	  (lambda (b a rest)
	    (if (= 3 (length b))
	       `(##core#let-location
		 (quote ,(car b))
		 (quote ,(cadr b))
		 ,a
		 ,rest)
	       `(##core#let-location
		 (quote ,(car b))
		 (quote ,(cadr b))
		 ,rest) ) )
	  `(let () ,@body)
	  bindings aliases) ) ) )


;;; Embedding code directly:

(define-macro (foreign-code . strs)
  (let ([tmp (gensym 'code_)])
    `(begin
       (declare 
	 (foreign-declare
	  ,(sprintf "static C_word ~A() { ~A\n; return C_SCHEME_UNDEFINED; }\n" tmp (string-intersperse strs "\n")) ) )
       (##core#inline ,tmp) ) ) )

(define-macro (foreign-value str type)
  (let ([tmp (gensym 'code_)])
    `(begin
       (define-foreign-variable ,tmp ,type ,str)
       ,tmp) ) )


;;; Foreign records:

(define-macro (define-foreign-record name . slots)
  (let ([fname (if (pair? name) (->string (cadr name)) (sprintf "struct ~A" name))]
	[tname (if (pair? name) (car name) name)] 
	[var (gensym)] 
	[renamer identity]
	[ctor #f]
	[dtor #f]
	[cvar (gensym)]
	[xvar (gensym)]
	[svar (gensym)] )
    (define (stype type)
      (cond [(not (pair? type)) type]
	    [(eq? 'const (car type)) (stype (cadr type))]
	    [(memq (car type) '(struct union)) `(c-pointer ,type)]
	    [else type] ) )
    (define (strtype type)
      (or (eq? type tname)
	  (and (pair? type)
	       (or (and (eq? 'const (car type)) (strtype (cadr type)))
		   (memq (car type) '(struct union)) ) ) ) ) ; handle instances?
    (do ((slts slots (cdr slts)))
	((or (null? slts) (not (pair? (car slts))) (not (keyword? (caar slts))))
	 (set! slots slts) )
      (case (caar slts)
	((rename:) (set! renamer (eval (cadar slts))))
	((constructor:) (set! ctor (cadar slts)))
	((destructor:) (set! dtor (cadar slts)))
	(else (syntax-error 'define-foreign-record "invalid foreign record-type specification" (car slts))) ) )
    (##sys#hash-table-set! ##compiler#foreign-type-table tname `(c-pointer ,fname))
    `(begin
       ,@(if (pair? name)
	     '() 
	     `((declare
		 (foreign-declare
		  ,(string-intersperse
		    (append
		     (cons 
		      (string-append "struct " (->string name) " { ")
		      (map (lambda (slot)
			     (case (length slot)
			       [(3) 
				(sprintf "~A[~A];"
					 (##compiler#foreign-type-declaration
					  (car slot)
					  (->string (cadr slot)) )
					 (caddr slot) ) ]
			       [(2)
				(sprintf "~A;"
					 (##compiler#foreign-type-declaration 
					  (car slot)
					  (->string (cadr slot)) ) ) ]
			       [else (syntax-error 'define-foreign-record "bad slot spec" slot)] ) )
			   slots) )
		     (list "};") )
		    "\n") ) ) ) )
       ,@(if (not ctor)
	     '()
	     `((define ,ctor
		 (foreign-lambda* ,tname () ,(sprintf "return((~a *)C_malloc(sizeof(~a)));" fname fname)))))
       ,@(if (not dtor)
	     '()
	     `((define (,dtor ptr) (and ptr (##core#inline "C_qfree" ptr)))) )
       ,@(map (lambda (slot)
		(case (length slot)
		  [(3)
		   (let* ([type (car slot)]
			  [sname (cadr slot)]
			  [size (caddr slot)]
			  [type2 (stype type)] )
		     `(begin
			(define ,(string->symbol (renamer (sprintf "~A-~A" tname sname)))
			  (let ([,cvar 
				 (foreign-lambda* ,type2 ([,tname ,var] [int ,svar])
				   ,(sprintf "return(~A~A->~A[~A]);" 
					     (if (not (strtype type)) "" "&")
					     var sname svar) ) ] )
			    (lambda (,var ,svar)
			      (if (##core#check (and (fx>= ,svar 0) (fx< ,svar ,size)))
				  (,cvar ,var ,svar)
				  ;; this should signal a range exn...
				  (syntax-error 'define-foreign-record "array access out of range" ',tname ',svar ,size) ) ) ) )
			,@(if (and (pair? type) (eq? 'const (car type))) 
			      '()
			      (if (eq? type type2)
				  `((define ,(string->symbol (renamer (sprintf "~A-~A-set!" tname sname)))
				      (let ([,cvar
					     (foreign-lambda* void ([,tname ,var] [int ,svar] [,type ,xvar])
					       ,(sprintf "~A->~A[~A] = ~A;" var sname svar xvar) ) ] )
					(lambda (,var ,svar ,xvar)
					  (if (##core#check (and (fx>= ,svar 0) (fx< ,svar ,size)))
					      (,cvar ,var ,svar ,xvar)
					      (syntax-error 
					       'define-foreign-record
					       "array access out of range" ',tname ',svar ,size) ) ) ) ) )
				  '() ) ) ) ) ]
		  [(2)
		   (let* ([type (car slot)]
			  [sname (cadr slot)] 
			  [type2 (stype type)] )
		     `(begin
			(define ,(string->symbol (renamer (sprintf "~A-~A" tname sname)))
			  (foreign-lambda* ,type2 ([,tname ,var])
			    ,(sprintf "return(~A~A->~A);" 
				      (if (not (strtype type)) "" "&")
				      var sname) ) )
			,@(if (and (pair? type) (eq? 'const (car type)))
			      '()
			      (if (eq? type type2)
				  `((define ,(string->symbol (renamer (sprintf "~A-~A-set!" tname sname)))
				      (foreign-lambda* void ([,tname ,var] [,type ,xvar])
					,(sprintf "~A->~A = ~A;" var sname xvar) ) ) )
				  '() ) ) ) ) ]
		  [else (syntax-error 'define-foreign-record "bad slot spec" slot)] ) )
	      slots) ) ) )


;;; Include/parse foreign code fragments

(define-macro (foreign-declare . strs)
  `(##core#declare '(foreign-declare ,@strs)))


;;; Foreign enumerations (or enum-like constants)

(define-macro (define-foreign-enum typename . enums)
  (let ((name typename)
	(type (->string typename)) )
    (when (and (list? typename) (= 2 (length typename)))
      (set! name (car typename))
      (set! type (cadr typename)) )
    (let* ((symbols (map (lambda (e) (if (pair? e) (car e) e)) enums))
	   (aliases (map gensym symbols)) 
	   (vals (map (lambda (e) (if (pair? e) (cadr e) e)) enums))
	   (s->e (string->symbol (conc name "->number")))
	   (e->s (string->symbol (conc "number->" name)) ) )
      `(begin
	 ,@(map (lambda (a v) `(define-foreign-variable ,a integer ,(->string v))) aliases vals)
	 (define (,s->e syms)
	   (let loop ((syms (if (symbol? syms) (list syms) syms)) (sum 0))
	     (if (null? syms) 
		 sum
		 (loop
		  (cdr syms)
		  (bitwise-ior
		   sum
		   (let ((val (car syms)))
		     (case val
		       ,@(map (lambda (a s) `((,s) ,a)) aliases symbols)
		       (else (error "not a member of enum" val ',name)) ) ) ) ) ) ) )
	 (define (,e->s val)
	   (cond 
	    ,@(map (lambda (a s) `((= val ,a) ',s)) aliases symbols)
	    (else '()) ) )
	 (define-foreign-type ,name ,type ,s->e ,e->s) ) ) ) )


;;; Deprecated FFI macros

(define-macro (define-deprecated-macro old new)
  `(define-macro (,old . args)
     (warning (sprintf "`~s' is deprecated, use `~s' instead" ',old ',new))
     (cons ',new args) ) )

(define-deprecated-macro foreign-callback-lambda foreign-safe-lambda)
(define-deprecated-macro foreign-callback-lambda* foreign-safe-lambda*)
(define-deprecated-macro foreign-callback-wrapper foreign-safe-wrapper)
