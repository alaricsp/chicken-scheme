;; Copyright (c) 1993-2001 by Richard Kelsey and Jonathan Rees.
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the authors may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

; The syntax-rules macro (new in R5RS)

;;; [Hacked slightly by Taylor R. Campbell to make it work in his
;;; macro expander `riaxpander'.]

;; [Hacked even more by Felix L. Winkelmann to make it work in his
;; Hi-Lo expander]

; Example:
;
; (define-syntax or
;   (syntax-rules ()
;     ((or)          #f)
;     ((or e)        e)
;     ((or e1 e ...) (let ((temp e1))
;		       (if temp temp (or e ...))))))


(##sys#extend-macro-environment
 'syntax-rules
 '()
 (##sys#er-transformer
  (lambda (exp r c)
    (##sys#check-syntax 'syntax-rules exp '#(_ 2))
    (let ((subkeywords (cadr exp))
	  (rules (cddr exp))
	  (ellipsis '...))
      (when (symbol? subkeywords)
	(##sys#check-syntax 'syntax-rules exp '(_ _ list . #(_ 0)))
	(set! ellipsis subkeywords)
	(set! subkeywords (car rules))
	(set! rules (cdr rules)))
      (##sys#process-syntax-rules ellipsis rules subkeywords r c)))))


(define (##sys#process-syntax-rules ellipsis rules subkeywords r c)

  (define %append '##sys#append)
  (define %apply '##sys#apply)
  (define %and (r 'and))
  (define %car '##sys#car)
  (define %cdr '##sys#cdr)
  (define %vector? '##sys#vector?)
  (define %vector-length '##sys#vector-length)
  (define %vector-ref '##sys#vector-ref)
  (define %vector->list '##sys#vector->list)
  (define %list->vector '##sys#list->vector)
  (define %>= '##sys#>=)
  (define %= '##sys#=)
  (define %+ '##sys#+)
  (define %i (r 'i))
  (define %compare (r 'compare))
  (define %cond (r 'cond))
  (define %cons '##sys#cons)
  (define %else (r 'else))
  (define %eq? '##sys#eq?)
  (define %equal? '##sys#equal?)
  (define %input (r 'input))
  (define %l (r 'l))
  (define %lambda (r 'lambda))
  (define %let (r 'let))
  (define %let* (r 'let*))
  (define %list? '##sys#list?)
  (define %list (r 'list))
  (define %loop (r 'loop))
  (define %map1 '##sys#map)
  (define %map '##sys#map-n)
  (define %null? '##sys#null?)
  (define %or (r 'or))
  (define %pair? '##sys#pair?)
  (define %quote (r 'quote))
  (define %rename (r 'rename))
  (define %tail (r 'tail))
  (define %temp (r 'temp))
  (define %syntax-error '##sys#syntax-error-hook)
  (define %ellipsis (r ellipsis))

  (define (ellipsis? x)
    (c x %ellipsis))

  (define (make-transformer rules)
    `(,%lambda (,%input ,%rename ,%compare)
	       (,%let ((,%tail (,%cdr ,%input)))
		      (,%cond ,@(map process-rule rules)
			      (,%else 
			       (##sys#syntax-rules-mismatch ,%input))))))

  (define (process-rule rule)
    (if (and (pair? rule)
	     (pair? (cdr rule))
	     (null? (cddr rule)))
	(let ((pattern (cdar rule))
	      (template (cadr rule)))
	  `((,%and ,@(process-match %tail pattern))
	    (,%let* ,(process-pattern pattern
				      %tail
				      (lambda (x) x))
		    ,(process-template template
				       0
				       (meta-variables pattern 0 '())))))
	(##sys#syntax-error-hook "ill-formed syntax rule" rule)))

  ;; Generate code to test whether input expression matches pattern

  (define (process-match input pattern)
    (cond ((symbol? pattern)
	   (if (memq pattern subkeywords)
	       `((,%compare ,input (,%rename (syntax ,pattern))))
	       `()))
	  ((segment-pattern? pattern)
	   (process-segment-match input (car pattern)))
	  ((pair? pattern)
	   `((,%let ((,%temp ,input))
		    (,%and (,%pair? ,%temp)
			   ,@(process-match `(,%car ,%temp) (car pattern))
			   ,@(process-match `(,%cdr ,%temp) (cdr pattern))))))
	  ((vector? pattern)
	   (process-vector-match input pattern))
	  ((or (null? pattern) (boolean? pattern) (char? pattern))
	   `((,%eq? ,input ',pattern)))
	  (else
	   `((,%equal? ,input ',pattern)))))

  (define (process-segment-match input pattern)
    (let ((conjuncts (process-match `(,%car ,%l) pattern)))
      (if (null? conjuncts)
	  `((,%list? ,input))		;+++
	  `((,%let ,%loop ((,%l ,input))
		   (,%or (,%null? ,%l)
			 (,%and (,%pair? ,%l)
				,@conjuncts
				(,%loop (,%cdr ,%l)))))))))

   (define (process-vector-match input pattern)
     (let* ((len (vector-length pattern))
            (segment? (and (>= len 2)
                           (ellipsis? (vector-ref pattern (- len 1))))))
       `((,%let ((,%temp ,input))
          (,%and (,%vector? ,%temp)
                 ,(if segment?
                      `(,%>= (,%vector-length ,%temp) ,(- len 2))
                      `(,%= (,%vector-length ,%temp) ,len))
                 ,@(let lp ((i 0))
                     (cond
                      ((>= i len)
                       '())
                      ((and (= i (- len 2)) segment?)
                       `((,%let ,%loop ((,%i ,i))
                            (,%or (,%>= ,%i ,len)
                                  (,%and ,@(process-match
                                            `(,%vector-ref ,%temp ,%i)
                                            (vector-ref pattern (- len 2)))
                                         (,%loop (,%+ ,%i 1)))))))
                      (else
                       (append (process-match `(,%vector-ref ,%temp ,i)
                                              (vector-ref pattern i))
                               (lp (+ i 1)))))))))))
 
  ;; Generate code to take apart the input expression
  ;; This is pretty bad, but it seems to work (can't say why).

  (define (process-pattern pattern path mapit)
    (cond ((symbol? pattern)
	   (if (memq pattern subkeywords)
	       '()
	       (list (list pattern (mapit path)))))
	  ((segment-pattern? pattern)
	   (process-pattern (car pattern)
			    %temp
			    (lambda (x)	;temp is free in x
			      (mapit (if (eq? %temp x)
					 path ;+++
					 `(,%map1 (,%lambda (,%temp) ,x)
						  ,path))))))
	  ((pair? pattern)
	   (append (process-pattern (car pattern) `(,%car ,path) mapit)
		   (process-pattern (cdr pattern) `(,%cdr ,path) mapit)))
          ((vector? pattern)
           (let* ((len (vector-length pattern))
                  (segment? (and (>= len 2)
                                 (ellipsis? (vector-ref pattern (- len 1))))))
             (if segment?
                 (process-pattern (vector->list pattern) 
                                  `(,%vector->list ,path)
                                  mapit)
                 (let lp ((i 0))
                   (cond
                    ((>= i len)
                     '())
                    (else
                     (append (process-pattern (vector-ref pattern i)
                                              `(,%vector-ref ,path ,i)
                                              mapit)
                             (lp (+ i 1)))))))))
	  (else '())))

  ;; Generate code to compose the output expression according to template

  (define (process-template template dim env)
    (cond ((symbol? template)
	   (let ((probe (assq template env)))
	     (if probe
		 (if (<= (cdr probe) dim)
		     template
		     (##sys#syntax-error-hook "template dimension error (too few ellipses?)"
					      template))
		 `(,%rename (syntax ,template)))))
	  ((segment-template? template)
	   (let* ((depth (segment-depth template))
		  (seg-dim (+ dim depth))
		  (vars
		   (free-meta-variables (car template) seg-dim env '())))
	     (if (null? vars)
		 (##sys#syntax-error-hook "too many ellipses" template)
		 (let* ((x (process-template (car template)
					     seg-dim
					     env))
			(gen (if (and (pair? vars)
				      (null? (cdr vars))
				      (symbol? x)
				      (eq? x (car vars)))
				 x	;+++
				 `(,%map (,%lambda ,vars ,x)
					 ,@vars)))
			(gen (do ((d depth (- d 1))
				  (gen gen `(,%apply ,%append ,gen)))
				 ((= d 1)
				  gen))))
		   (if (null? (segment-tail template))
		       gen		;+++
		       `(,%append ,gen ,(process-template (segment-tail template)
							  dim env)))))))
	  ((pair? template)
	   `(,%cons ,(process-template (car template) dim env)
		    ,(process-template (cdr template) dim env)))
	  ((vector? template)
	   `(,%list->vector
	     ,(process-template (vector->list template) dim env)))
	  (else
	   `(,%quote ,template))))

  ;; Return an association list of (var . dim)

  (define (meta-variables pattern dim vars)
    (cond ((symbol? pattern)
	   (if (memq pattern subkeywords)
	       vars
	       (cons (cons pattern dim) vars)))
	  ((segment-pattern? pattern)
	   (meta-variables (car pattern) (+ dim 1) vars))
	  ((pair? pattern)
	   (meta-variables (car pattern) dim
			   (meta-variables (cdr pattern) dim vars)))
	  ((vector? pattern)
	   (meta-variables (vector->list pattern) dim vars))
	  (else vars)))

  ;; Return a list of meta-variables of given higher dim

  (define (free-meta-variables template dim env free)
    (cond ((symbol? template)
	   (if (and (not (memq template free))
		    (let ((probe (assq template env)))
		      (and probe (>= (cdr probe) dim))))
	       (cons template free)
	       free))
	  ((segment-template? template)
	   (free-meta-variables (car template)
				dim env
				(free-meta-variables (cddr template)
						     dim env free)))
	  ((pair? template)
	   (free-meta-variables (car template)
				dim env
				(free-meta-variables (cdr template)
						     dim env free)))
	  ((vector? template)
	   (free-meta-variables (vector->list template) dim env free))
	  (else free)))

  (define (segment-pattern? pattern)
    (and (segment-template? pattern)
	 (or (null? (cddr pattern))
	     (##sys#syntax-error-hook "segment matching not implemented" pattern))))

  (define (segment-template? pattern)
    (and (pair? pattern)
	 (pair? (cdr pattern))
	 (ellipsis? (cadr pattern))))

  ;; Count the number of `...'s in PATTERN.

  (define (segment-depth pattern)
    (if (segment-template? pattern)
	(+ 1 (segment-depth (cdr pattern)))
	0))

  ;; Get whatever is after the `...'s in PATTERN.

  (define (segment-tail pattern)
    (let loop ((pattern (cdr pattern)))
      (if (and (pair? pattern)
	       (ellipsis? (car pattern)))
	  (loop (cdr pattern))
	  pattern)))

  (make-transformer rules))
