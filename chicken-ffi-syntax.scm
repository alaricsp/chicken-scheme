;;;; chicken-ffi-syntax.scm
;
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


(define ##sys#chicken-ffi-macro-environment
  (let ((me0 (##sys#macro-environment)))

(##sys#extend-macro-environment
 'define-external
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (let* ((form (cdr form))
	   (%quote (r 'quote))
	   (quals (and (pair? form) (string? (car form))))
	   (var (and (not quals) (pair? form) (symbol? (car form)))) )
      (cond [var
	     (##sys#check-syntax 'define-external form '(symbol _ . #(_ 0 1)))
	     (let ([var (car form)])
	       `(,(r 'begin)
		 (,(r 'define-foreign-variable) ,var ,(cadr form))
		 (,(r 'define-external-variable) ,var ,(cadr form) #t)
		 ,@(if (pair? (cddr form))
		       `((##core#set! ,var ,(caddr form)))
		       '() ) ) ) ]
	    [else
	     (if quals
		 (##sys#check-syntax 'define-external form '(string (symbol . #((_ symbol) 0)) _ . #(_ 1)))
		 (##sys#check-syntax 'define-external form '((symbol . #((_ symbol) 0)) _ . #(_ 1))) )
	     (let* ([head (if quals (cadr form) (car form))]
		    [args (cdr head)] )
	       `(,(r 'define) ,(car head)
		 (##core#foreign-callback-wrapper
		  ',(car head)
		  ,(if quals (car form) "")
		  ',(if quals (caddr form) (cadr form))
		  ',(map (lambda (a) (car a)) args)
		  (,(r 'lambda) 
		   ,(map (lambda (a) (cadr a)) args)
		   ,@(if quals (cdddr form) (cddr form)) ) ) ) ) ] ) ) ) ) )



;;; External locations:

(##sys#extend-macro-environment
 'define-location
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'define-location form '(_ variable _ . #(_ 0 1)))
    (let ((var (cadr form))
	  (type (caddr form))
	  (init (optional (cdddr form) #f))
	  (name (r (gensym))))
      `(,(r 'begin)
	(,(r 'define-foreign-variable) ,var ,type ,(symbol->string name))
	(,(r 'define-external-variable) ,var ,type #f ,name)
	,@(if (pair? init)
	      `((##core#set! ,var ,(car init)))
	      '() ) ) ) ) ) )

(##sys#extend-macro-environment
 'let-location
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'let-location form '(_ #((variable _ . #(_ 0 1)) 0) . _))
    (let* ((bindings (cadr form))
	   (body (cddr form))
	   (%let (r 'let))
	   [aliases (map (lambda (_) (r (gensym))) bindings)])
      `(,%let ,(append-map
		(lambda (b a)
		  (if (pair? (cddr b))
		      (list (cons a (cddr b)))
		      '() ) )
		bindings aliases)
	      ,(fold-right
		(lambda (b a rest)
		  (if (= 3 (length b))
		      `(##core#let-location
			,(car b)
			,(cadr b)
			,a
			,rest)
		      `(##core#let-location
			,(car b)
			,(cadr b)
			,rest) ) )
		`(,%let () ,@body)
		bindings aliases) ) ) ) ) )


;;; Embedding code directly:

(##sys#extend-macro-environment
 'foreign-code
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'foreign-code form '(_ . #(string 0)))
    (let ([tmp (gensym 'code_)])
      `(,(r 'begin)
	 (,(r 'declare)
	  (foreign-declare
	   ,(sprintf "static C_word ~A() { ~A\n; return C_SCHEME_UNDEFINED; }\n" 
		     tmp
		     (string-intersperse (cdr form) "\n")) ) )
	 (##core#inline ,tmp) ) ) ) ) )

(##sys#extend-macro-environment
 'foreign-value
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'foreign-value form '(_ _ _))
    (let ((tmp (gensym 'code_))
	  (code (cadr form)))
      `(,(r 'begin)
	(,(r 'define-foreign-variable) ,tmp
	 ,(caddr form)
	 ,(cond ((string? code) code)
		((symbol? code) (symbol->string code))
		(else (syntax-error 'foreign-value "bad argument type - not a string or symbol" code))))
	,tmp) ) ) ) )


;;; Include/parse foreign code fragments

(##sys#extend-macro-environment
 'foreign-declare
 '()
 (##sys#er-transformer
  (lambda (form r c)
    (##sys#check-syntax 'foreign-declare form '(_ . #(string 0)))
    `(##core#declare (foreign-declare ,@(cdr form))))))


(##sys#macro-subset me0)))
