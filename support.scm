;;;; support.scm - Miscellaneous support code for the CHICKEN compiler
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


(declare (unit support))


#{compiler
  compiler-arguments process-command-line dump-nodes dump-undefined-globals
  default-standard-bindings default-extended-bindings side-effecting-standard-bindings
  non-foldable-standard-bindings foldable-standard-bindings non-foldable-extended-bindings foldable-extended-bindings
  standard-bindings-that-never-return-false side-effect-free-standard-bindings-that-never-return-false
  installation-home optimization-iterations compiler-cleanup-hook decompose-lambda-list
  file-io-only banner custom-declare-alist disabled-warnings
  unit-name insert-timer-checks used-units source-filename pending-canonicalizations
  foreign-declarations block-compilation line-number-database-size
  target-heap-size target-stack-size check-global-exports check-global-imports
  default-default-target-heap-size default-default-target-stack-size verbose-mode original-program-size
  current-program-size line-number-database-2 foreign-lambda-stubs immutable-constants foreign-variables
  rest-parameters-promoted-to-vector inline-table inline-table-used constant-table constants-used mutable-constants
  dependency-list broken-constant-nodes inline-substitutions-enabled emit-syntax-trace-info
  always-bound-to-procedure block-variable-literal copy-node! valid-c-identifier? tree-copy copy-node-tree-and-rename
  direct-call-ids foreign-type-table first-analysis scan-sharp-greater-string
  expand-profile-lambda profile-lambda-list profile-lambda-index profile-info-vector-name
  initialize-compiler canonicalize-expression expand-foreign-lambda update-line-number-database scan-toplevel-assignments
  perform-cps-conversion analyze-expression simplifications perform-high-level-optimizations perform-pre-optimization!
  reorganize-recursive-bindings substitution-table simplify-named-call
  perform-closure-conversion prepare-for-code-generation compiler-source-file create-foreign-stub expand-foreign-lambda*
  transform-direct-lambdas! finish-foreign-result compressable-literal csc-control-file
  debugging-chicken bomb check-signature posq stringify symbolify build-lambda-list
  string->c-identifier c-ify-string words words->bytes check-and-open-input-file close-checked-input-file fold-inner
  constant? basic-literal? source-info->string import-table
  collapsable-literal? immediate? canonicalize-begin-body extract-mutable-constants string->expr get get-all
  put! collect! count! get-line get-line-2 find-lambda-container display-analysis-database varnode qnode 
  build-node-graph build-expression-tree fold-boolean inline-lambda-bindings match-node expression-has-side-effects?
  simple-lambda-node? compute-database-statistics print-program-statistics output gen gen-list 
  pprint-expressions-to-file foreign-type-check estimate-foreign-result-size scan-used-variables scan-free-variables
  topological-sort print-version print-usage initialize-analysis-database estimate-foreign-result-location-size
  real-name real-name-table set-real-name! real-name2 display-real-name-table display-line-number-database
  default-declarations units-used-by-default words-per-flonum emit-control-file-item compiler-warning
  foreign-string-result-reserve parameter-limit eq-inline-operator optimizable-rest-argument-operators
  membership-test-operators membership-unfold-limit valid-compiler-options valid-compiler-options-with-argument
  default-optimization-iterations chop-separator chop-extension follow-without-loop dump-exported-globals
  generate-code make-variable-list make-argument-list generate-foreign-stubs foreign-type-declaration
  foreign-argument-conversion foreign-result-conversion final-foreign-type debugging export-list block-globals
  lookup-exports-file constant-declarations process-lambda-documentation
  make-random-name foreign-type-convert-result foreign-type-convert-argument process-custom-declaration}


(include "tweaks")
(include "banner")


;;; Debugging and error-handling stuff:

(define (compiler-cleanup-hook) #f)

(define debugging-chicken '())
(define disabled-warnings '())		; usage type load var const syntax redef use call ffi

(define (bomb . msg-and-args)
  (if (pair? msg-and-args)
      (apply error (string-append "[internal compiler screwup] " (car msg-and-args)) (cdr msg-and-args))
      (error "[internal compiler screwup]") ) )

(define (debugging mode msg . args)
  (and (memq mode debugging-chicken)
       (begin
	 (printf "~a" msg)
	 (if (pair? args)
	     (begin
	       (display ": ")
	       (for-each (lambda (x) (printf "~s " (force x))) args) ) )
	 (newline)
	 (flush-output)
	 #t) ) )

(define (compiler-warning class msg . args)	       
  (when (and ##sys#warnings-enabled (not (memq class disabled-warnings)))
    (let ((out (current-error-port)))
      (apply fprintf out (string-append "Warning: " msg) args)
      (newline out) ) ) )

(define (quit msg . args)
  (let ([out (current-error-port)])
    (apply fprintf out (string-append "Error: " msg) args)
    (newline out)
    (exit 1) ) )

(set! ##sys#syntax-error-hook
  (lambda (msg . args)
    (let ([out (current-error-port)])
      (fprintf out "Syntax error: ~a~%~%" msg) 
      (for-each (cut fprintf out "\t~s~%" <>) args)
      (print-call-chain out 0 ##sys#current-thread "\n\tExpansion history:\n")
      (exit 70) ) ) )

(set! syntax-error ##sys#syntax-error-hook)

(define (emit-syntax-trace-info info cntr) 
  (##core#inline "C_emit_syntax_trace_info" info cntr ##sys#current-thread) )

(define (map-llist proc llist)
  (let loop ([llist llist])
    (cond [(null? llist) '()]
	  [(symbol? llist) (proc llist)]
	  [else (cons (proc (car llist)) (loop (cdr llist)))] ) ) )

(define (check-signature var args llist)
  (define (err)
    (quit "Arguments to inlined call of `~A' do not match parameter-list ~A" 
	  (real-name var)
	  (map-llist real-name (cdr llist)) ) )
  (let loop ([as args] [ll llist])
    (cond [(null? ll) (unless (null? as) (err))]
	  [(symbol? ll)]
	  [(null? as) (err)]
	  [else (loop (cdr as) (cdr ll))] ) ) )


;;; Generic utility routines:

(define (posq x lst)
  (let loop ([lst lst] [i 0])
    (cond [(null? lst) #f]
	  [(eq? x (car lst)) i]
	  [else (loop (cdr lst) (add1 i))] ) ) )

(define (stringify x)
  (cond ((string? x) x)
	((symbol? x) (symbol->string x))
	(else (sprintf "~a" x)) ) )

(define (symbolify x)
  (cond ((symbol? x) x)
	((string? x) (string->symbol x))
	(else (string->symbol (sprintf "~a" x))) ) )

(define (build-lambda-list vars argc rest)
  (let loop ((vars vars) (n argc))
    (cond ((or (zero? n) (null? vars)) (or rest '()))
          (else (cons (car vars) (loop (cdr vars) (sub1 n)))) ) ) )

(define string->c-identifier ##sys#string->c-identifier)

(define (c-ify-string str)
  (list->string
   (cons 
    #\"
    (let loop ((chars (string->list str)))
      (if (null? chars)
	  '(#\")
	  (let* ((c (car chars))
		 (code (char->integer c)) )
	    (if (or (< code 32) (> code 128) (memq c '(#\" #\' #\\ #\?)))
		(append '(#\\)
			(cond ((< code 8) '(#\0 #\0))
			      ((< code 64) '(#\0))
			      (else '()) )
			(string->list (number->string code 8))
			(loop (cdr chars)) )
		(cons c (loop (cdr chars))) ) ) ) ) ) ) )

(define (valid-c-identifier? name)
  (let ([str (string->list (->string name))])
    (and (pair? str)
	 (let ([c0 (car str)])
	   (and (or (char-alphabetic? c0) (char=? #\_ c0))
		(any (lambda (c) (or (char-alphabetic? c) (char-numeric? c) (char=? #\_ c)))
		     (cdr str) ) ) ) ) ) )

(eval-when (load)
  (define words (foreign-lambda int "C_bytestowords" int)) 
  (define words->bytes (foreign-lambda int "C_wordstobytes" int)) )

(eval-when (eval)
  (define (words n)
    (let ([wordsize (##sys#fudge 7)])
      (+ (quotient n wordsize) (if (zero? (modulo n wordsize)) 0 1)) ) )
  (define (words->bytes n)
    (* n (##sys#fudge 7)) ) )

(define (check-and-open-input-file fname . line)
  (cond [(string=? fname "-") (current-input-port)]
	[(file-exists? fname) (open-input-file fname)]
	[(or (null? line) (not (car line))) (quit "Can not open file ~s" fname)]
	[else (quit "Can not open file ~s in line ~s" fname (car line))] ) )

(define (close-checked-input-file port fname)
  (unless (string=? fname "-") (close-input-port port)) )

(define (fold-inner proc lst)
  (if (null? (cdr lst)) 
      lst
      (let fold ((xs (reverse lst)))
	(apply
	 proc 
	 (if (null? (cddr xs))
	     (list (cadr xs) (car xs))
	     (list (fold (cdr xs)) (car xs)) ) ) ) ) )

(define (follow-without-loop seed proc abort)
  (let loop ([x seed] [done '()])
    (if (member x done)
	(abort)
	(proc x (lambda (x2) (loop x2 (cons x done)))) ) ) )


;;; Predicates on expressions and literals:

(define (constant? x)
  (or (number? x)
      (char? x)
      (string? x)
      (boolean? x)
      (eof-object? x)
      (and (pair? x) (eq? 'quote (car x))) ) )

(define (collapsable-literal? x)
  (or (boolean? x)
      (char? x)
      (eof-object? x)
      (number? x)
      (symbol? x) ) )

(define (immediate? x)
  (or (fixnum? x)
      (eq? (##core#undefined) x)
      (null? x)
      (eof-object? x)
      (char? x)
      (boolean? x) ) )

(define (compressable-literal lit t)
  (let* ([count 0]
	 [f (let rec ([x lit])
	      (set! count (add1 count))
	      (cond [(or (number? x) (char? x) (string? x) (boolean? x) (null? x) (symbol? x))] ; 1
		    [(pair? x)		; car + cdr
		     (set! count (sub1 count))
		     (and (rec (car x)) (rec (cdr x))) ]
		    [(vector? x) (every rec (vector->list x))] ; 1 + elements
		    [else #f] ) ) ] )
    (and f (> count t) count) ) )

(define (basic-literal? x)
  (or (null? x)
      (symbol? x)
      (constant? x)
      (and (vector? x) (every basic-literal? (vector->list x)))
      (and (pair? x) 
	   (basic-literal? (car x))
	   (basic-literal? (cdr x)) ) ) )


;;; Expression manipulation:

(define (canonicalize-begin-body body)
  (let loop ((xs body))
    (cond ((null? xs) '(##core#undefined))
	  ((null? (cdr xs)) (car xs))
	  ((let ([h (car xs)])
	     (or (equal? h '(##core#undefined))
		 (constant? h) 
		 (equal? h '(##sys#void)) ) )
	   (loop (cdr xs)) )
	  (else `(let ((,(gensym 't) ,(car xs)))
		   ,(loop (cdr xs))) ) ) ) )

(define (extract-mutable-constants exp)
  (let ([mlist '()])
    (define (walk x)
      (match x
	[(? not-pair? x) x]
	[`(quote ,c)
	 (if (not (collapsable-literal? c))
	     (let ([var (make-random-name)])
	       (set! mlist (alist-cons var c mlist))
	       var)
	     x) ]
	[`(let ((,vars ,vals) ...) . ,body)
	 `(let ,(map (lambda (var val) (list var (walk val))) vars vals) ,@(map walk body)) ]
	[(op . args)
	 (case op
	   [(##core#include ##core#declare ##core#immutable ##core#undefined ##core#primitive ##core#inline_ref) x]
	   [(##core#set! set! lambda ##core#inline ##core#inline_allocate ##core#inline_update ##core#inline_loc_ref
			 ##core#inline_loc_update)
	    (cons* op (first args) (map walk (cdr args))) ]
	   [(if ##core#compiletimeonly ##core#compiletimetoo)
	    (cons op (map walk args)) ]
	   [else (map walk x)] ) ]
	[_ x] ) ) 
    (let ([exp2 (walk exp)])
      (values exp2 mlist) ) ) )

(define string->expr
  (let ([exn? (condition-predicate 'exn)]
	[exn-msg (condition-property-accessor 'exn 'message)] )
    (lambda (str)
      (handle-exceptions ex
	  (quit "can not parse expression: ~s [~a]~%" 
		str
		(if (exn? ex) 
		    (exn-msg ex)
		    (->string ex) ) ) 
	(let ([xs (with-input-from-string str (lambda () (unfold eof-object? values (lambda (x) (read)) (read))))])
	  (cond [(null? xs) '(##core#undefined)]
		[(null? (cdr xs)) (car xs)]
		[else `(begin ,@xs)] ) ) ) ) ) )

(define decompose-lambda-list ##sys#decompose-lambda-list)

(define (process-lambda-documentation id doc)
  #f)					; Hook this


;;; Profiling instrumentation:

(define (expand-profile-lambda name llist body)
  (let ([index profile-lambda-index] 
	[args (gensym)] )
    (set! profile-lambda-list (alist-cons index name profile-lambda-list))
    (set! profile-lambda-index (add1 index))
    `(lambda ,args
       (##sys#dynamic-wind
	(lambda () (##sys#profile-entry ',index ,profile-info-vector-name))
	(lambda () (apply (lambda ,llist ,body) ,args))
	(lambda () (##sys#profile-exit ',index ,profile-info-vector-name)) ) ) ) )


;;; Database operations:
;
; - 'get' and 'put' shadow the routines in the extras-unit, we use low-level
;   symbol-keyed hash-tables here.

(define (initialize-analysis-database db)
  (for-each
   (lambda (s) 
     (put! db s 'standard-binding #t)
     (when (memq s side-effecting-standard-bindings) (put! db s 'side-effecting #t))
     (when (memq s foldable-standard-bindings) (put! db s 'foldable #t)) )
   standard-bindings)
  (for-each
   (lambda (s)
     (put! db s 'extended-binding #t)
     (when (memq s foldable-extended-bindings) (put! db s 'foldable #t)) )
   extended-bindings)
  (for-each
   (lambda (s) (put! db (car s) 'constant #t))
   mutable-constants) )

(define (get db key prop)
  (let ((plist (##sys#hash-table-ref db key)))
    (and plist
	 (let ([a (assq prop plist)])
	   (and a (##sys#slot a 1)) ) ) ) )

(define (get-all db key . props)
  (let ((plist (##sys#hash-table-ref db key)))
    (if plist
	(filter-map (lambda (prop) (assq prop plist)) props)
	'() ) ) )

(define (put! db key prop val)
  (let ([plist (##sys#hash-table-ref db key)])
    (if plist
	(let ([a (assq prop plist)])
	  (cond [a (##sys#setslot a 1 val)]
		[val (##sys#setslot plist 1 (alist-cons prop val (##sys#slot plist 1)))] ) )
	(when val (##sys#hash-table-set! db key (list (cons prop val)))) ) ) )

(define (collect! db key prop val)
  (let ((plist (##sys#hash-table-ref db key)))
    (if plist
	(let ([a (assq prop plist)])
	  (cond [a (##sys#setslot a 1 (cons val (##sys#slot a 1)))]
		[else (##sys#setslot plist 1 (alist-cons prop (list val) (##sys#slot plist 1)))] ) )
	(##sys#hash-table-set! db key (list (list prop val)))) ) )

(define (count! db key prop . val)
  (let ([plist (##sys#hash-table-ref db key)]
	[n (if (pair? val) (car val) 1)] )
    (if plist
	(let ([a (assq prop plist)])
	  (cond [a (##sys#setslot a 1 (+ (##sys#slot a 1) n))]
		[else (##sys#setslot plist 1 (alist-cons prop n (##sys#slot plist 1)))] ) )
	(##sys#hash-table-set! db key (list (cons prop val)))) ) )


;;; Line-number database management:

(define (get-line exp)
  (get ##sys#line-number-database (car exp) exp) )

(define (get-line-2 exp)
  (let* ((name (car exp))
	 (lst (##sys#hash-table-ref ##sys#line-number-database name)) )
    (cond ((and lst (assq exp (cdr lst)))
	   => (lambda (a) (values (car lst) (cdr a))) )
	  (else (values name #f)) ) ) )

(define (find-lambda-container id cid db)
  (let loop ([id id])
    (or (eq? id cid)
	(let ([c (get db id 'contained-in)])
	  (and c (loop c)) ) ) ) )

(define (display-line-number-database)
  (##sys#hash-table-for-each
   (lambda (key val)
     (when val (printf "~S ~S~%" key (map cdr val))) )
   ##sys#line-number-database) )


;;; Display analysis database:

(define display-analysis-database
  (let ((names '((captured . cpt) (assigned . set) (boxed . box) (global . glo) (assigned-locally . stl)
		 (contractable . con) (standard-binding . stb) (foldable . fld) (simple . sim) (inlinable . inl)
		 (side-effecting . sef) (collapsable . col) (removable . rem) (constant . con)
		 (undefined . und) (replacing . rpg) (unused . uud) (extended-binding . xtb) (inline-export . ilx)
		 (customizable . cst) (has-unused-parameters . hup) (boxed-rest . bxr) ) ) )
    (lambda (db)
      (##sys#hash-table-for-each
       (lambda (sym plist)
	 (let ([val #f]
	       [pval #f]
	       [csites '()]
	       [refs '()] )
	   (write sym)
	   (let loop ((es plist))
	     (if (pair? es)
		 (begin
		   (case (caar es)
		     ((captured assigned boxed global contractable standard-binding foldable assigned-locally
		       side-effecting collapsable removable undefined replacing unused simple inlinable inline-export
		       has-unused-parameters extended-binding customizable constant boxed-rest)
		      (printf "\t~a" (cdr (assq (caar es) names))) )
		     ((unknown)
		      (set! val 'unknown) )
		     ((value)
		      (unless (eq? val 'unknown) (set! val (cdar es))) )
		     ((potential-value)
		      (set! pval (cdar es)) )
		     ((replacable home contains contained-in use-expr closure-size rest-parameter
		       o-r/access-count captured-variables explicit-rest)
		      (printf "\t~a=~s" (caar es) (cdar es)) )
		     ((references)
		      (set! refs (cdar es)) )
		     ((call-sites)
		      (set! csites (cdar es)) )
		     (else (bomb "Illegal property" (car es))) )
		   (loop (cdr es)) ) ) )
	   (cond [(and val (not (eq? val 'unknown)))
		  (printf "\tval=~s" (cons (node-class val) (node-parameters val))) ]
		 [pval (printf "\tpval=~s" (cons (node-class val) (node-parameters val)))] )
	   (when (pair? refs) (printf "\trefs=~s" (length refs)))
	   (when (pair? csites) (printf "\tcss=~s" (length csites)))
	   (newline) ) )
       db) ) ) )       


;;; Node creation and -manipulation:

;; Note: much of this stuff will be overridden by the inline-definitions in "tweaks.scm".

(define-record node
  class					; symbol
  parameters				; (value...)
  subexpressions)			; (node...)

(define (make-node c p s)
  (##sys#make-structure 'node c p s) ) ; this kludge is for allowing the inlined `make-node'

(define (varnode var) (make-node '##core#variable (list var) '()))
(define (qnode const) (make-node 'quote (list const) '()))

(define (build-node-graph exp)
  (let ([count 0])
    (define (walk x)
      (cond ((symbol? x) (varnode x))
	    ((not-pair? x) (bomb "bad expression"))
	    ((symbol? (car x))
	     (case (car x)
	       ((##core#global-ref) (make-node '##core#global-ref (list (cadr x)) '()))
	       ((if ##core#undefined) (make-node (car x) '() (map walk (cdr x))))
	       ((quote)
		(let ((c (cadr x)))
		  (qnode (if (and (number? c)
				  (eq? 'fixnum number-type)
				  (not (integer? c)) )
			     (begin
			       (compiler-warning
				'type
				"literal '~s' is out of range - will be truncated to integer" c)
			       (inexact->exact (truncate c)) )
			     c) ) ) )
	       ((let)
		(let ([bs (cadr x)]
		      [body (caddr x)] )
		  (if (null? bs)
		      (walk body)
		      (make-node 'let (unzip1 bs)
				 (append (map (lambda (b) (walk (cadr b))) (cadr x))
					 (list (walk body)) ) ) ) ) )
	       ((lambda) (make-node 'lambda (list (cadr x)) (list (walk (caddr x)))))
	       ((##core#primitive)
		(let ([arg (cadr x)])
		  (make-node
		   (car x)
		   (list (if (and (pair? arg) (eq? 'quote (car arg))) (cadr arg) arg))
		   (map walk (cddr x)) ) ) )
	       ((set! ##core#inline ##core#callunit) 
		(make-node (car x) (list (cadr x)) (map walk (cddr x))) )
	       ((##core#proc)
		(make-node '##core#proc (list (cadr x) #t) '()) )
	       ((##core#set!) (make-node 'set! (list (cadr x)) (map walk (cddr x))))
	       ((##core#foreign-callback-wrapper)
		(let ([name (cadr (second x))])
		  (make-node
		   '##core#foreign-callback-wrapper
		   (list name (cadr (third x)) (cadr (fourth x)) (cadr (fifth x)))
		   (list (walk (sixth x))) ) ) )
	       ((##core#inline_allocate ##core#inline_ref ##core#inline_update
					##core#inline_loc_ref ##core#inline_loc_update)
		(make-node (first x) (second x) (map walk (cddr x))) )
	       ((##core#app)
		(make-node '##core#call '(#t) (map walk (cdr x))) )
	       (else
		(receive
		    (name ln) (get-line-2 x)
		  (make-node
		   '##core#call
		   (list (cond [(memq name always-bound-to-procedure)
				(set! count (add1 count))
				#t]
			       [else #f] )
			 (if ln
			     (let ([rn (real-name name)])
			       (list source-filename ln (or rn (##sys#symbol->qualified-string name))) )
			     (##sys#symbol->qualified-string name) ) )
		   (map walk x) ) ) ) ) )
	    (else (make-node '##core#call '(#f) (map walk x))) ) )
    (let ([exp2 (walk exp)])
      (debugging 'o "eliminated procedure checks" count)
      exp2) ) )

(define (build-expression-tree node)
  (let walk ((n node))
    (let ((subs (node-subexpressions n))
	  (params (node-parameters n)) 
	  (class (node-class n)) )
      (case class
	((if ##core#box ##core#cond) (cons class (map walk subs)))
	((##core#closure)
	 `(##core#closure ,params ,@(map walk subs)) )
	((##core#variable ##core#global-ref) (car params))
	((quote) `(quote ,(car params)))
	((let)
	 `(let ,(map list params (map walk (butlast subs)))
	    ,(walk (last subs)) ) )
	((##core#lambda) 
	 (list (if (second params)
		   'lambda
		   '##core#lambda)
	       (third params)
	       (walk (car subs)) ) )
	((##core#call) (map walk subs))
	((##core#callunit) (cons* '##core#callunit (car params) (map walk subs)))
	((##core#undefined) (list class))
	((##core#bind) 
	 (let loop ((n (car params)) (vals subs) (bindings '()))
	   (if (zero? n)
	       `(##core#bind ,(reverse bindings) ,(walk (car vals)))
	       (loop (- n 1) (cdr vals) (cons (walk (car vals)) bindings)) ) ) )
	((##core#unbox ##core#ref ##core#update ##core#update_i)
	 (cons* class (walk (car subs)) params (map walk (cdr subs))) ) 
	(else (cons class (append params (map walk subs)))) ) ) ) )

(define (fold-boolean proc lst)
  (let fold ([vars lst])
    (if (null? (cddr vars))
	(apply proc vars)
	(make-node 
	 '##core#inline '("C_and") 
	 (list (proc (first vars) (second vars))
	       (fold (cdr vars)) ) ) ) ) )

(define (inline-lambda-bindings llist args body copy?)
  (decompose-lambda-list
   llist
   (lambda (vars argc rest)
     (receive (largs rargs) (split-at args argc)
       (let* ([rlist (if copy? (map gensym vars) vars)]
	      [body (if copy? 
			(copy-node-tree-and-rename body vars rlist)
			body) ] )
	 (fold-right
	  (lambda (var val body) (make-node 'let (list var) (list val body)) )
	  (if rest
	      (make-node
	       'let (list (last rlist))
	       (list (if (null? rargs)
			 (qnode '())
			 (make-node '##core#inline_allocate (list "C_a_i_list" (* 3 (length rargs))) rargs) )
		     body) )
	      body)
	  (take rlist argc)
	  largs) ) ) ) ) )

(define (copy-node-tree-and-rename node vars aliases)
  (let ([rlist (map cons vars aliases)])
    (define (rename v rl) (alist-ref v rl eq? v))
    (define (walk n rl)
      (let ([subs (node-subexpressions n)]
	    [params (node-parameters n)]
	    [class (node-class n)] )
	(case class
	  [(##core#variable) (varnode (rename (first params) rl))]
	  [(set!) (make-node 'set! (list (rename (first params) rl)) (map (cut walk <> rl) subs))]
	  [(let) 
	   (let* ([v (first params)]
		  [a (gensym v)]
		  [rl2 (alist-cons v a rl)] )
	     (make-node 'let (list a) (map (cut walk <> rl2) subs)) ) ]
	  [(##core#lambda)
	   (decompose-lambda-list
	    (third params)
	    (lambda (vars argc rest)
	      (let* ([as (map gensym vars)]
		     [rl2 (append as rl)] )
		(make-node 
		 '##core#lambda
		 (list (first params) (second params) 
		       (build-lambda-list as argc (and rest (rename rest rl2)))
		       (fourth params) )
		 (map (cut walk <> rl2) subs) ) ) ) ) ]
	  [else (make-node class (tree-copy params) (map (cut walk <> rl) subs))] ) ) )
    (walk node rlist) ) )

(define (tree-copy t)
  (let rec ([t t])
    (if (pair? t)
	(cons (rec (car t)) (rec (cdr t)))
	t) ) )

(define (copy-node! from to)
  (node-class-set! to (node-class from))
  (node-parameters-set! to (node-parameters from))
  (node-subexpressions-set! to (node-subexpressions from)) 
  (let ([len-from (##sys#size from)]
	[len-to (##sys#size to)] )
    (do ([i 4 (fx+ i 1)])
	((or (fx>= i len-from) (fx>= i len-to)))
      (##sys#setslot to i (##sys#slot from i)) ) ) )


;;; Match node-structure with pattern:

(define (match-node node pat vars)
  (let ((env '()))

    (define (resolve v x)
      (cond ((assq v env) => (lambda (a) (equal? x (cdr a))))
	    ((memq v vars)
	     (set! env (alist-cons v x env))
	     #t)
	    (else (eq? v x)) ) )

    (define (match1 x p)
      (cond ((not-pair? p) (resolve p x))
	    ((not-pair? x) #f)
	    ((match1 (car x) (car p)) (match1 (cdr x) (cdr p)))
	    (else #f) ) )
    
    (define (matchn n p)
      (if (not-pair? p)
	  (resolve p n)
	  (and (eq? (node-class n) (first p))
	       (match1 (node-parameters n) (second p))
	       (let loop ((ns (node-subexpressions n))
			  (ps (cddr p)) )
		 (cond ((null? ps) (null? ns))
		       ((not-pair? ps) (resolve ps ns))
		       ((null? ns) #f)
		       (else (and (matchn (car ns) (car ps))
				  (loop (cdr ns) (cdr ps)) ) ) ) ) ) ) )

    (let ((r (matchn node pat)))
      (and r
	   (begin
	     (debugging 'a "matched" (node-class node) (node-parameters node) pat)
	     env) ) ) ) )


;;; Test nodes for certain properties:

(define (expression-has-side-effects? node db)
  (let walk ([n node])
    (let ([subs (node-subexpressions n)])
      (case (node-class n)
	[(##core#variable quote ##core#undefined ##core#proc ##core#global-ref) #f]
	[(##core#lambda) 
	 (let ([id (first (node-parameters n))])
	   (find (lambda (fs) (eq? id (foreign-callback-stub-id fs))) foreign-callback-stubs) ) ]
	[(if let) (any walk subs)]
	[else #t] ) ) ) )

(define (simple-lambda-node? node)
  (let* ([params (node-parameters node)]
	 [llist (third params)]
	 [k (and (pair? llist) (first llist))] ) ; leaf-routine has no continuation argument
    (and k 
	 (second params)
	 (let rec ([n node])
	   (case (node-class n)
	     [(##core#call)
	      (let* ([subs (node-subexpressions n)]
		     [f (first subs)] )
		(and (eq? '##core#variable (node-class f)) 
		     (eq? k (first (node-parameters f)))
		     (every rec (cdr subs)) ) ) ]
	     [(##core#callunit) #f]
	     [else (every rec (node-subexpressions n))] ) ) ) ) )


;;; Some safety checks and database dumping:

(define (dump-exported-globals db file)
  (unless block-compilation
    (with-output-to-file file
      (lambda ()
	(let ((exports '()))
	  (##sys#hash-table-for-each
	   (lambda (sym plist)
	     (when (and (assq 'global plist) 
			(assq 'assigned plist)
			(or (and export-list (memq sym export-list))
			    (not (memq sym block-globals)) ) )
	       (set! exports (cons sym exports)) ) )
	   db)
	  (for-each 
	   (lambda (s)
	     (write s)
	     (newline) )
	   (sort exports
		 (lambda (s1 s2)
		   (string<? (##sys#slot s1 1) (##sys#slot s2 1)))) ) ) ) ) ) )

(define (dump-undefined-globals db)
  (##sys#hash-table-for-each
   (lambda (sym plist)
     (when (and (assq 'global plist)
		(not (assq 'assigned plist)) )
       (write sym)
       (newline) ) )
   db) )

(define (check-global-exports db)
  (when export-list
    (let ([exps export-list])
      (##sys#hash-table-for-each
       (lambda (sym plist)
	 (when (and (memq sym exps) (not (assq 'assigned plist)))
	   (compiler-warning 'var "exported global variable `~S' is used but not defined" sym) )
	 (set! exps (delete sym exps eq?)) )
       db)
      (for-each (cut compiler-warning 'var "exported global variable `~S' is not defined" <>) exps) ) ) )

(define (check-global-imports db)
  (##sys#hash-table-for-each
   (lambda (sym plist)
     (let ((imp (##sys#hash-table-ref import-table sym))
	   (refs (assq 'references plist))
	   (assgn (assq 'assigned plist)) )
       (when (assq 'global plist)
	 (cond (assgn
		(when imp
		  (compiler-warning 'redef "redefinition of imported variable `~s' from `~s'" sym imp) ) )
	       ((and (pair? refs) (not imp) (not (keyword? sym)))
		(compiler-warning 'var "variable `~s' used but not imported" sym) ) ) ) ) )
   db) )

(define (lookup-exports-file id)
  (and-let* ((xfile (##sys#resolve-include-filename 
		     (string-append (->string id) ".exports")
		     #t #t) )
	     ((file-exists? xfile)) )
    (when verbose-mode 
      (printf "loading exports file ~a ...~%" xfile) )
    (for-each (cut ##sys#hash-table-set! import-table <> id) (read-file xfile)) ) )


;;; Compute general statistics from analysis database:
;
; - Returns:
;
;   current-program-size
;   original-program-size
;   number of known variables
;   number of known procedures
;   number of global variables
;   number of known call-sites
;   number of database entries
;   average bucket load

(define (compute-database-statistics db)
  (let ((nprocs 0)
	(nvars 0)
	(nglobs 0)
	(entries 0)
	(nsites 0) )
    (##sys#hash-table-for-each
     (lambda (sym plist)
       (for-each
	(lambda (prop)
	  (set! entries (+ entries 1))
	  (case (car prop)
	    ((global) (set! nglobs (+ nglobs 1)))
	    ((value)
	     (set! nvars (+ nvars 1))
	     (if (eq? '##core#lambda (node-class (cdr prop)))
		 (set! nprocs (+ nprocs 1)) ) )
	    ((call-sites) (set! nsites (+ nsites (length (cdr prop))))) ) )
	plist) )
     db)
    (values current-program-size
	    original-program-size
	    nvars
	    nprocs
	    nglobs
	    nsites
	    entries) ) )

(define (print-program-statistics db)
  (receive
   (size osize kvars kprocs globs sites entries) (compute-database-statistics db)
   (when (debugging 's "program statistics:")
     (printf ";   program size: \t~s \toriginal program size: \t~s\n" size osize)
     (printf ";   variables with known values: \t~s\n" kvars)
     (printf ";   known procedures: \t~s\n" kprocs)
     (printf ";   global variables: \t~s\n" globs)
     (printf ";   known call sites: \t~s\n" sites) 
     (printf ";   database entries: \t~s\n" entries) ) ) )


;;; Pretty-print expressions:

(define (pprint-expressions-to-file exps filename)
  (let ([port (if filename (open-output-file filename) (current-output-port))])
    (with-output-to-port port
      (lambda ()
	(for-each
	 (lambda (x)
	   (pretty-print x)
	   (newline) ) 
	 exps) ) )
    (when filename (close-output-port port)) ) )


;;; Create foreign type checking expression:

(define foreign-type-check
  (let ([tmap '((nonnull-u8vector . u8vector) (nonnull-u16vector . u16vector)
		(nonnull-s8vector . s8vector) (nonnull-s16vector . s16vector)
		(nonnull-u32vector . u32vector) (nonnull-s32vector . s32vector)
		(nonnull-f32vector . f32vector) (nonnull-f64vector . f64vector) ) ] )
    (lambda (param type)
      (follow-without-loop
       type
       (lambda (t next)
	 (let repeat ([t t])
	   (case t
	     [(char unsigned-char) (if unsafe param `(##sys#foreign-char-argument ,param))]
	     [(int unsigned-int short unsigned-short byte unsigned-byte int32 unsigned-int32)
	      (if unsafe param `(##sys#foreign-fixnum-argument ,param))]
	     [(float double number) (if unsafe param `(##sys#foreign-flonum-argument ,param))]
	     [(pointer byte-vector scheme-pointer) ; pointer is DEPRECATED
	      (let ([tmp (gensym)])
		`(let ([,tmp ,param])
		   (if ,tmp
		       ,(if unsafe
			    tmp
			    `(##sys#foreign-block-argument ,tmp) )
		       '#f) ) ) ]
	     [(nonnull-pointer nonnull-scheme-pointer nonnull-byte-vector) ; nonnull-pointer is DEPRECATED
	      (if unsafe
		  param
		  `(##sys#foreign-block-argument ,param) ) ]
	     [(u8vector u16vector s8vector s16vector u32vector s32vector f32vector f64vector)
	      (let ([tmp (gensym)])
		`(let ([,tmp ,param])
		   (if ,tmp
		       ,(if unsafe
			    tmp
			    `(##sys#foreign-number-vector-argument ',t ,tmp) )
		       '#f) ) ) ]
	     [(nonnull-u8vector nonnull-u16vector nonnull-s8vector nonnull-s16vector nonnull-u32vector nonnull-s32vector 
				nonnull-f32vector nonnull-f64vector)
	      (if unsafe
		  param
		  `(##sys#foreign-number-vector-argument 
		    ',(##sys#slot (assq t tmap) 1)
		    ,param) ) ]
	     [(integer long integer32) (if unsafe param `(##sys#foreign-integer-argument ,param))]
	     [(unsigned-integer unsigned-integer32 unsigned-long)
	      (if unsafe
		  param
		  `(##sys#foreign-unsigned-integer-argument ,param) ) ]
	     [(c-pointer)
	      (let ([tmp (gensym)])
		`(let ([,tmp ,param])
		   (if ,tmp
		       (##sys#foreign-pointer-argument ,tmp)
		       '#f) ) ) ]
	     [(nonnull-c-pointer)
	      `(##sys#foreign-pointer-argument ,param) ]
	     [(c-string c-string*)
	      (let ([tmp (gensym)])
		`(let ([,tmp ,param])
		   (if ,tmp
		       ,(if unsafe 
			    `(##sys#make-c-string ,tmp)
			    `(##sys#make-c-string (##sys#foreign-string-argument ,tmp)) )
		       '#f) ) ) ]
	     [(nonnull-c-string nonnull-c-string*)
	      (if unsafe 
		  `(##sys#make-c-string ,param)
		  `(##sys#make-c-string (##sys#foreign-string-argument ,param)) ) ]
	     [(symbol)
	      (if unsafe 
		  `(##sys#make-c-string (##sys#symbol->string ,param))
		  `(##sys#make-c-string (##sys#foreign-string-argument (##sys#symbol->string ,param))) ) ]
	     [else
	      (cond [(and (symbol? t) (##sys#hash-table-ref foreign-type-table t))
		     => (lambda (t)
			  (next (if (vector? t) (vector-ref t 0) t)) ) ]
		    [(pair? t)
		     (match t
		       [((or 'ref 'pointer 'function 'c-pointer) . _)
			(let ([tmp (gensym)])
			  `(let ([,tmp ,param])
			     (if ,tmp
				 (##sys#foreign-pointer-argument ,tmp)
				 '#f) ) )  ]
		       [((or 'instance 'instance-ref) . _)
			(let ([tmp (gensym)])
			  `(let ([,tmp ,param])
			     (if ,tmp
				 (slot-ref ,param 'this)
				 '#f) ) ) ]
		       [('nonnull-instance . _)
			`(slot-ref ,param 'this) ]
		       [('const t) (repeat t)]
		       [((or 'nonnull-pointer 'nonnull-c-pointer) . _)
			`(##sys#foreign-pointer-argument ,param) ]
		       [_ param] ) ]
		    [else param] ) ] ) ) )
       (lambda () (quit "foreign type `~S' refers to itself" type)) ) ) ) )


;;; Compute foreign-type conversions:

(define (foreign-type-convert-result r t)
  (or (and-let* ([(symbol? t)]
		 [ft (##sys#hash-table-ref foreign-type-table t)] 
		 [(vector? ft)] )
	(list (vector-ref ft 2) r) )
      r) )

(define (foreign-type-convert-argument a t)
  (or (and-let* ([(symbol? t)]
		 [ft (##sys#hash-table-ref foreign-type-table t)] 
		 [(vector? ft)] )
	(list (vector-ref ft 1) a) )
      a) )

(define (final-foreign-type t0)
  (follow-without-loop
   t0
   (lambda (t next)
     (cond [(and (symbol? t) (##sys#hash-table-ref foreign-type-table t))
	    => (lambda (t2)
		 (next (if (vector? t2) (vector-ref t2 0) t2)) ) ]
	   [else t] ) )
   (lambda () (quit "foreign type `~S' refers to itself" t0)) ) )


;;; Compute foreign result size:

(define (estimate-foreign-result-size type)
  (follow-without-loop
   type
   (lambda (t next)
     (case t
       ((char int short bool void unsigned-short scheme-object unsigned-char unsigned-int byte unsigned-byte
	      int32 unsigned-int32) 
	0)
       ((c-string nonnull-c-string c-pointer nonnull-c-pointer symbol c-string* nonnull-c-string*)
	(words->bytes 3) )
       ((unsigned-integer long integer unsigned-long integer32 unsigned-integer32)
	(words->bytes 4) )
       ((float double number integer64) 
	(words->bytes 4) )		; possibly 8-byte aligned 64-bit double
       (else
	(cond [(and (symbol? t) (##sys#hash-table-ref foreign-type-table t))
	       => (lambda (t2)
		    (next (if (vector? t2) (vector-ref t2 0) t2)) ) ]
	      [(pair? t)
	       (case (car t)
		 [(ref nonnull-pointer pointer c-pointer nonnull-c-pointer function instance instance-ref nonnull-instance) 
		  (words->bytes 3) ]
		 [else 0] ) ]
	      [else 0] ) ) ) )
   (lambda () (quit "foreign type `~S' refers to itself" type)) ) )

(define (estimate-foreign-result-location-size type)
  (define (err t) 
    (quit "can not compute size of location for foreign type `~S'" t) )
  (follow-without-loop
   type
   (lambda (t next)
     (case t
       ((char int short bool unsigned-short unsigned-char unsigned-int long unsigned-long byte unsigned-byte
	      c-pointer pointer nonnull-c-pointer unsigned-integer integer float c-string symbol
	      scheme-pointer nonnull-scheme-pointer int32 unsigned-int32 integer32 unsigned-integer32
	      nonnull-c-string c-string* nonnull-c-string*) ; pointer and nonnull-pointer are DEPRECATED
	(words->bytes 1) )
       ((double number)
	(words->bytes 2) )
       (else
	(cond [(and (symbol? t) (##sys#hash-table-ref foreign-type-table t))
	       => (lambda (t2)
		    (next (if (vector? t2) (vector-ref t2 0) t2)) ) ]
	      [(pair? t)
	       (case (car t)
		 [(ref nonnull-pointer pointer c-pointer nonnull-c-pointer function) (words->bytes 1)]
		 [else (err t)] ) ]
	      [else (err t)] ) ) ) )
   (lambda () (quit "foreign type `~S' refers to itself" type)) ) )


;;; Convert result value, if a string:

(define (finish-foreign-result type body)
  (case type
    [(c-string) `(##sys#peek-c-string ,body '0)]
    [(nonnull-c-string) `(##sys#peek-nonnull-c-string ,body '0)]
    [(c-string*) `(##sys#peek-and-free-c-string ,body '0)]
    [(nonnull-c-string*) `(##sys#peek-and-free-nonnull-c-string ,body '0)]
    [(symbol) `(##sys#intern-symbol (##sys#peek-c-string ,body '0))]
    [else
     (match type
       [((or 'instance 'instance-ref) cname sname)
	`(##tinyclos#make-instance-from-pointer ,body ,sname) ] ;XXX eggified, needs better treatment...
       [('nonnull-instance cname sname)
	`(make ,sname 'this ,body) ]
       [_ body] ) ] ) )


;;; Scan expression-node for variable usage:

(define (scan-used-variables node vars)
  (let ([used '()])
    (let walk ([n node])
      (let ([subs (node-subexpressions n)])
	(case (node-class n)
	  [(##core#variable set!) 
	   (let ([var (first (node-parameters n))])
	     (when (and (memq var vars) (not (memq var used)))
	       (set! used (cons var used)) ) 
	     (for-each walk subs) ) ]
	  [(quote ##core#undefined ##core#primitive) #f]
	  [else (for-each walk subs)] ) ) )
    used) )


;;; Scan expression-node for free variables (that are not in env):

(define (scan-free-variables node)
  (let ((vars '()))

    (define (walk n e)
      (let ([subs (node-subexpressions n)]
	    [params (node-parameters n)] )
	(case (node-class n)
	  ((quote ##core#undefined ##core#primitive ##core#proc ##core#inline_ref) #f)
	  ((##core#variable) 
	   (let ((var (first params)))
	     (unless (memq var e) (set! vars (lset-adjoin eq? vars var))) ) )
	  ((set!)
	   (let ((var (first params)))
	     (unless (memq var e) (set! vars (lset-adjoin eq? vars var)))
	     (walk (car subs) e) ) )
	  ((let) 
	   (walk (first subs) e)
	   (walk (second subs) (append params e)) )
	  ((##core#lambda)
	   (decompose-lambda-list
	    (third params)
	    (lambda (vars argc rest)
	      (walk (first subs) (append vars e)) ) ) )
	  (else (walkeach subs e)) ) ) )

    (define (walkeach ns e)
      (for-each (lambda (n) (walk n e)) ns) )

    (walk node '())
    vars) )


;;; Simple topological sort:
;
; - Taken from SLIB (slightly adapted): Copyright (C) 1995 Mikael Djurfeldt

(define (topological-sort dag pred)
  (if (null? dag)
      '()
      (let* ((adj-table '())
	     (sorted '()))

	(define (insert x y)
	  (let loop ([at adj-table])
	    (cond [(null? at) (set! adj-table (cons (cons x y) adj-table))]
		  [(pred x (caar at)) (set-cdr! (car at) y)]
		  [else (loop (cdr at))] ) ) )
	
	(define (lookup x)
	  (let loop ([at adj-table])
	    (cond [(null? at) #f]
		  [(pred x (caar at)) (cdar at)]
		  [else (loop (cdr at))] ) ) )
	
	(define (visit u adj-list)
	  ;; Color vertex u
	  (insert u 'colored)
	  ;; Visit uncolored vertices which u connects to
	  (for-each (lambda (v)
		      (let ((val (lookup v)))
			(if (not (eq? val 'colored))
			    (visit v (or val '())))))
		    adj-list)
	  ;; Since all vertices downstream u are visited
	  ;; by now, we can safely put u on the output list
	  (set! sorted (cons u sorted)) )
	
	;; Hash adjacency lists
	(for-each (lambda (def) (insert (car def) (cdr def)))
		  (cdr dag))
	;; Visit vertices
	(visit (caar dag) (cdar dag))
	(for-each (lambda (def)
		    (let ((val (lookup (car def))))
		      (if (not (eq? val 'colored))
			  (visit (car def) (cdr def)))))
		  (cdr dag)) 
	sorted) ) )


;;; Some pathname operations:

(define (chop-separator str)
  (let ([len (sub1 (string-length str))])
    (if (and (> len 0) (char=? (string-ref str len) ##sys#pathname-directory-separator))
	(substring str 0 len)
	str) ) )

(define (chop-extension str)
  (let ([len (sub1 (string-length str))])
    (let loop ([i len])
      (cond [(zero? i) str]
	    [(char=? #\. (string-ref str i)) (substring str 0 i)]
	    [else (loop (sub1 i))] ) ) ) )


;;; Print version/usage information:

(define (print-version . b)
  (when (:optional b #f) (printf "~A" banner))
  (printf "~A~%~A" (chicken-version #t) copyright) )

(define (print-usage)
  (print-version)
  (newline)
  (display #<<EOF
Usage: chicken FILENAME OPTION ...

  FILENAME should be a complete source file name with extension, or "-" for
  standard input. OPTION may be one of the following:

  General options:

    -help                       display this text and exit
    -version                    display compiler version and exit
    -release                    print release number and exit
    -verbose                    display information on compilation progress
    -quiet                      do not display compile information

  File and pathname options:

    -output-file FILENAME       specifies output-filename, default is 'out.c'
    -include-path PATHNAME      specifies alternative path for included files
    -to-stdout                  write compiled file to stdout instead of file

  Language options:

    -feature SYMBOL             register feature identifier

  Syntax related options:

    -case-insensitive           don't preserve case of read symbols
    -keyword-style STYLE        allow alternative keyword syntax (none, prefix or suffix)
    -run-time-macros            macros are made available at run-time

  Translation options:

    -explicit-use               do not use units 'library' and 'eval' by default
    -check-syntax               stop compilation after macro-expansion
    -analyze-only               stop compilation after first analysis pass

  Debugging options:

    -no-warnings                disable warnings
    -disable-warning CLASS      disable specific class of warnings
    -debug-level NUMBER         set level of available debugging information
    -no-trace                   disable tracing information
    -profile                    executable emits profiling information 
    -profile-name FILENAME      name of the generated profile information file
    -accumulate-profile         executable emits profiling information in append mode
    -no-lambda-info             omit additional procedure-information
    -emit-exports FILENAME      write exported toplevel variables to FILENAME
    -check-imports              look for undefined toplevel variables
    -import FILENAME            read externally exported symbols from FILENAME

  Optimization options:

    -optimize-level NUMBER      enable certain sets of optimization options
    -optimize-leaf-routines     enable leaf routine optimization
    -lambda-lift                enable lambda-lifting
    -no-usual-integrations      standard procedures may be redefined
    -unsafe                     disable safety checks
    -block                      enable block-compilation
    -disable-interrupts         disable interrupts in compiled code
    -fixnum-arithmetic          assume all numbers are fixnums
    -benchmark-mode             fixnum mode, no interrupts and opt.-level 3
    -disable-stack-overflow-checks  disables detection of stack-overflows.
    -inline                     enable inlining
    -inline-limit               set inlining threshold

  Configuration options:

    -unit NAME                  compile file as a library unit
    -uses NAME                  declare library unit as used.
    -heap-size NUMBER           specifies heap-size of compiled executable
    -heap-initial-size NUMBER   specifies heap-size at startup time
    -heap-growth PERCENTAGE     specifies growth-rate of expanding heap
    -heap-shrinkage PERCENTAGE  specifies shrink-rate of contracting heap
    -nursery NUMBER
    -stack-size NUMBER          specifies nursery size of compiled executable
    -extend FILENAME            load file before compilation commences
    -prelude EXPRESSION         add expression to front of source file
    -postlude EXPRESSION        add expression to end of source file
    -prologue FILENAME          include file before main source file
    -epilogue FILENAME          include file after main source file
    -dynamic                    compile as dynamically loadable code
    -require-extension NAME     require extension NAME in compiled code
    -extension                  compile as extension (dynamic or static)

  Obscure options:

    -debug MODES                display debugging output for the given modes
    -compress-literals NUMBER   compile literals above threshold as strings
    -unsafe-libraries           marks the generated file as being linked
                                with the unsafe runtime system
    -raw                        do not generate implicit init- and exit code			       
    -emit-external-prototypes-first  emit protoypes for callbacks before foreign
                                declarations

EOF
) )


;;; Special block-variable literal type:

(define-record block-variable-literal 
  name)					; symbol


;;; Generation of random names:

(define (make-random-name . prefix)
  (string->symbol
   (sprintf "~A-~A~A"
	    (:optional prefix (gensym))
	    (current-seconds)
	    (random 1000) ) ) )


;;; Register/lookup real names:
;
; - The real-name-table contains the following mappings:
;
;     <variable-alias> -> <variable>
;     <lambda-id> -> <variable> or <variable-alias>

(define (set-real-name! name rname)
  (##sys#hash-table-set! real-name-table name rname) )

(define (real-name var . db)
  (define (resolve n)
    (let ([n2 (##sys#hash-table-ref real-name-table n)])
      (if n2
	  (or (##sys#hash-table-ref real-name-table n2)
	      n2) 
	  n) ) )
  (let ([rn (resolve var)])
    (cond [(not rn) (##sys#symbol->qualified-string var)]
	  [(pair? db)
	   (let ([db (car db)])
	     (let loop ([prev (##sys#symbol->qualified-string rn)] 
			[container (get db var 'contained-in)] )
	       (if container
		   (let ([rc (resolve container)])
		     (if (eq? rc container)
			 prev
			 (loop (sprintf "~A in ~A" prev rc)
			       (get db container 'contained-in) ) ) )
		   prev) ) ) ]
	  [else (##sys#symbol->qualified-string rn)] ) ) )

(define (real-name2 var db)
  (and-let* ([rn (##sys#hash-table-ref real-name-table var)])
    (real-name rn db) ) )

(define (display-real-name-table)
  (##sys#hash-table-for-each
   (lambda (key val)
     (printf "~S ~S~%" key val) )
   real-name-table) )

(define (source-info->string info)
  (match info
    ((file ln name)
     (let ((lns (->string ln)))
       (conc file ": " lns (make-string (max 0 (- 4 (string-length lns))) #\space) " " name) ) )
    (_ (and info (->string info))) ) )


;;; We need this for constant folding:

(define (string-null? x) (string-null? x))


;;; Dump node structure:

(define (dump-nodes n)
  (let loop ([i 0] [n n])
    (let ([class (node-class n)]
	  [params (node-parameters n)]
	  [subs (node-subexpressions n)] 
	  [ind (make-string i #\space)] 
	  [i2 (+ i 2)] )
      (printf "~%~A<~A ~S" ind class params)
      (for-each (cut loop i2 <>) subs)
      (let ([len (##sys#size n)])
	(when (fx> len 4)
	  (printf "[~S" (##sys#slot n 4))
	  (do ([i 5 (fx+ i 1)])
	      ((fx>= i len))
	    (printf " ~S" (##sys#slot n i)) )
	  (write-char #\]) ) )
      (write-char #\>) ) )
  (newline) )


;;; "#> ... <#" syntax:

(set! ##sys#user-read-hook
  (let ([old-hook ##sys#user-read-hook])
    (lambda (char port)
      (if (char=? #\> char)	       
	  (let* ((_ (read-char port))		; swallow #\>
		 (text (scan-sharp-greater-string port)))
	    `(declare (foreign-declare ,text)) )
	  (old-hook char port) ) ) ) )

(define (scan-sharp-greater-string port)
  (let ([out (open-output-string)])
    (let loop ()
      (let ([c (read-char port)])
	(cond [(eof-object? c) (quit "unexpected end of `#> ... <#' sequence")]
	      [(char=? c #\newline)
	       (newline out)
	       (loop) ]
	      [(char=? c #\<)
	       (let ([c (read-char port)])
		 (if (eqv? #\# c)
		     (get-output-string out)
		     (begin
		       (write-char #\< out)
		       (write-char c out) 
		       (loop) ) ) ) ]
	      [else
	       (write-char c out)
	       (loop) ] ) ) ) ) )


;;; Custom declarations:

(define (process-custom-declaration spec strings)
  (let* ([tag (car spec)]
	 [name (cadr spec)]
	 [fname (caddr spec)]
	 [args (cdddr spec)] 
	 [id (cons tag name)]
	 [a (assoc id custom-declare-alist)] )
    (unless a
      (let ([out (open-output-file fname)])
	(set! a (cons id out))
	(set! custom-declare-alist (cons a custom-declare-alist))
	(set! compiler-cleanup-hook
	  (let ([old compiler-cleanup-hook])
	    (lambda ()
	      (close-output-port out)
	      (old) ) ) )
	(emit-control-file-item (cons* tag name fname args)) ) )
    (for-each (cute display <> (cdr a)) strings) ) )

(define (emit-control-file-item item)
  (unless csc-control-file
    (set! csc-control-file (open-output-file (pathname-replace-extension source-filename "csc")))
    (display "#%csc\n" csc-control-file) 
    (set! compiler-cleanup-hook
      (let ([old compiler-cleanup-hook])
	(lambda ()
	  (close-output-port csc-control-file)
	  (old) ) ) ) )
  (fprintf csc-control-file "~S~%" item) )
