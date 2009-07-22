;;;; scrutinizer.scm - The CHICKEN Scheme compiler (local flow analysis)
;
; Copyright (c) 2009, The Chicken Team
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


(declare (unit scrutinizer))


(include "compiler-namespace")
(include "tweaks")


(define (d fstr . args)
  (when (##sys#fudge 13)
    (printf "[debug] ~?~%" fstr args)) )

(define-syntax d (syntax-rules () ((_ . _) (void))))


;;; Walk node tree, keeping type and binding information
;
; result specifiers:
;
;   SPEC = * | (VAL1 ...)
;   VAL = (or VAL1 ...)
;       | (struct NAME)
;       | (procedure (VAL1 ... [#!optional VALOPT1 ...] [#!rest [VAL | values]]) . RESULTS)
;       | BASIC
;       | deprecated
;   BASIC = * | string | symbol | char | number | boolean | list | pair | 
;           procedure | vector | null | eof | undefined | port | 
;           blob | noreturn | pointer | locative | fixnum | float
;   RESULTS = * 
;           | (VAL1 ...)

; global symbol properties:
;
;   ##core#type           ->  <typespec>
;   ##core#declared-type  ->  <bool>

(define-constant +fragment-max-length+ 5)
(define-constant +fragment-max-depth+ 3)

(define (scrutinize node db)
  (define (constant-result lit)
    (cond ((string? lit) 'string)
	  ((symbol? lit) 'symbol)
	  ((fixnum? lit) 'fixnum)
	  ((flonum? lit) 'float)
	  ((number? lit) 'number)	; in case...
	  ((boolean? lit) 'boolean)
	  ((list? lit) 'list)
	  ((pair? lit) 'pair)
	  ((eof-object? lit) 'eof)
	  ((vector? lit) 'vector)
	  ((and (not (##sys#immediate? lit)) ##sys#generic-structure? lit)
	   `(struct ,(##sys#slot lit 0)))
	  ((null? lit) 'null)
	  ((char? lit) 'char)
	  (else '*)))
  (define (global-result id loc)
    (cond ((##sys#get id '##core#type) =>
	   (lambda (a) 
	     (cond #;((and (get db id 'assigned)      ; remove assigned global from type db
			 (not (##sys#get id '##core#declared-type)))
		    (##sys#put! id '##core#type #f)
		    '*)
		   ((eq? a 'deprecated)
		    (report
		     loc
		     (sprintf "use of deprecated toplevel identifier `~a'" id) )
		    '*)
		   (else (list a)))))
	  (else '*)))
  (define (variable-result id e loc)
    (cond ((and (get db id 'assigned) 
		(not (##sys#get id '##core#declared-type)) )
	   '*)
	  ((assq id e) =>
	   (lambda (a)
	     (cond ((eq? 'undefined (cdr a))
		    (report 
		     loc
		     (sprintf "access to variable `~a' which has an undefined value"
			      (real-name id db)))
		    '*)
		   (else (list (cdr a))))))
	  (else (global-result id loc))))
  (define (always-true1 t)
    (cond ((and (pair? t) (eq? 'or (car t)))
	   (every always-true1 (cdr t)))
	  ((memq t '(* boolean undefined noreturn)) #f)
	  (else #t)))
  (define (always-true t loc x)
    (let ((f (always-true1 t)))
      (when f
	(report 
	 loc
	 (sprintf
	  "expected value of type boolean in conditional but were given a value of type `~a' which is always true:~%~%~a"
	  t
	  (pp-fragment x))))
      f))
  (define (typename t)
    (case t
      ((*) "anything")
      ((char) "character")
      (else
       (cond ((symbol? t) (symbol->string t))
	     ((pair? t)
	      (case (car t)
		((procedure) 
		 (if (or (string? (cadr t)) (symbol? (cadr t)))
		     (->string (cadr t))
		     (sprintf "a procedure with ~a returning ~a"
			      (argument-string (cadr t))
			      (result-string (cddr t)))))
		((or)
		 (string-intersperse
		  (map typename (cdr t))
		  " OR "))
		((struct)
		 (sprintf "a structure of type ~a" (cadr t)))
		(else (bomb "invalid type: ~a" t))))
	     (else (bomb "invalid type: ~a" t))))))
  (define (argument-string args)
    (let ((len (length args))
	  (m (multiples len)))
      (if (zero? len)
	  "zero arguments"
	  (sprintf 
	   "~a argument~a of type~a ~a"
	   len m m
	   (map typename args)))))
  (define (result-string results)
    (if (eq? '* results) 
	"an unknown number of values"
	(let ((len (length results))
	      (m (multiples len)))
	  (if (zero? len)
	      "zero values"
	      (sprintf 
	       "~a value~a of type~a ~a"
	       len m m
	       (map typename results))))))
  (define (simplify t)
    (let ((t2 (simplify1 t)))
      (d "simplify: ~a -> ~a" t t2)
      t2))
  (define (simplify1 t)
    (call/cc 
     (lambda (return)
       (if (pair? t)
	   (case (car t)
	     ((or)
	      (cond ((= 2 (length t)) (simplify (second t)))
		    ((every procedure-type? (cdr t))
		     (if (any (cut eq? 'procedure <>) (cdr t))
			 'procedure
			 (reduce
			  (lambda (t pt)
			    (let* ((name1 (and (named? t) (cadr t)))
				   (atypes1 (if name1 (third t) (second t)))
				   (rtypes1 (if name1 (cdddr t) (cddr t)))
				   (name2 (and (named? pt) (cadr pt)))
				   (atypes2 (if name2 (third pt) (second pt)))
				   (rtypes2 (if name2 (cdddr pt) (cddr pt))))
			      (append
			       '(procedure)
			       (if (and name1 name2 (eq? name1 name2)) (list name1) '())
			       (list (merge-argument-types atypes1 atypes2))
			       (merge-result-types rtypes1 rtypes2))))
			  #f
			  (cdr t))))
		    (else
		     (let* ((ts (append-map
				 (lambda (t)
				   (let ((t (simplify t)))
				     (cond ((and (pair? t) (eq? 'or (car t)))
					    (cdr t))
					   ;((eq? t 'noreturn) '())
					   ((eq? t 'undefined) (return 'undefined))
					   (else (list t)))))
				 (cdr t)))
			    (ts2 (let loop ((ts ts) (done '()))
				   (cond ((null? ts) (reverse done))
					 ((eq? '* (car ts)) (return '*))
					 ((any (cut type<=? (car ts) <>) (cdr ts))
					  (loop (cdr ts) done))
					 ((any (cut type<=? (car ts) <>) done)
					  (loop (cdr ts) done))
					 (else (loop (cdr ts) (cons (car ts) done)))))))
		       (cond ((equal? ts2 (cdr t)) t)
			     (else
			      (d "  or-simplify: ~a" ts2)
			      (simplify `(or ,@(if (any (cut eq? <> '*) ts2) '(*) ts2)))))))) )
	     ((procedure)
	      (let* ((name (and (named? t) (cadr t)))
		     (rtypes (if name (cdddr t) (cddr t))))
		(append
		 '(procedure)
		 (if name (list name) '())
		 (list (map simplify (if name (third t) (second t))))
		 (if (eq? '* rtypes)
		     '*
		     (map simplify rtypes)))))
	     (else t))
	   t))))
  (define (named? t)
    (and (pair? t) 
	 (eq? 'procedure (car t))
	 (not (or (null? (cadr t)) (pair? (cadr t))))))
  (define (rest-type r)
    (cond ((null? r) '*)
	  ((eq? 'values (car r)) '*)
	  (else (car r))))
  (define (merge-argument-types ts1 ts2) 
    (cond ((null? ts1) 
	   (cond ((null? ts2) '())
		 ((memq (car ts2) '(#!rest #!optional)) ts2)
		 (else '(#!rest))))
	  ((eq? '#!rest (car ts1))
	   (cond ((and (pair? ts2) (eq? '#!rest (car ts2)))
		  `(#!rest
		    ,(simplify 
		      `(or ,(rest-type (cdr ts1))
			   ,(rest-type (cdr ts2))))))
		 (else '(#!rest))))		;XXX giving up
	  ((eq? '#!optional (car ts1))
	   (cond ((and (pair? ts2) (eq? '#!optional (car ts2)))
		  `(#!optional 
		    ,(simplify `(or ,(cadr ts1) ,(cadr ts2)))
		    ,@(merge-argument-types (cddr ts1) (cddr ts2))))
		 (else '(#!rest))))	;XXX
	  (else (cons (simplify `(or ,(car ts1) ,(car ts2)))
		      (merge-argument-types (cdr ts1) (cdr ts2))))))
  (define (merge-result-types ts1 ts2)	;XXX possibly overly conservative
    (cond ((null? ts1) ts2)
	  ((null? ts2) ts1)
	  ((or (atom? ts1) (atom? ts2)) '*)
	  (else (cons (simplify `(or ,(car ts1) ,(car ts2)))
		      (merge-result-types (cdr ts1) (cdr ts2))))))
  (define (match t1 t2)
    (let ((m (match1 t1 t2)))
      (d "match ~a <-> ~a -> ~a" t1 t2 m)
      m))
  (define (match1 t1 t2)
    (cond ((eq? t1 t2))
	  ((eq? t1 '*))
	  ((eq? t2 '*))
	  ((eq? t1 'noreturn))
	  ((eq? t2 'noreturn))
	  ((and (eq? t1 'number) (memq t2 '(number fixnum float))))
	  ((and (eq? t2 'number) (memq t1 '(number fixnum float))))
	  ((eq? 'procedure t1) (and (pair? t2) (eq? 'procedure (car t2))))
	  ((eq? 'procedure t2) (and (pair? t1) (eq? 'procedure (car t1))))
	  ((and (pair? t1) (eq? 'or (car t1))) (any (cut match <> t2) (cdr t1)))
	  ((and (pair? t2) (eq? 'or (car t2))) (any (cut match t1 <>) (cdr t2)))
	  ((memq t1 '(pair list)) (memq t2 '(pair list)))
	  ((memq t1 '(null list)) (memq t2 '(null list)))
	  ((and (pair? t1) (pair? t2) (eq? (car t1) (car t2)))
	   (case (car t1)
	     ((procedure)
	      (let ((args1 (if (named? t1) (third t1) (second t1)))
		    (args2 (if (named? t2) (third t2) (second t2))) 
		    (results1 (if (named? t1) (cdddr t1) (cddr t1))) 
		    (results2 (if (named? t2) (cdddr t2) (cddr t2))) )
		(and (match-args args1 args2)
		     (match-results results1 results2))))
	     ((struct) (equal? t1 t2))
	     (else #f) ) )
	  (else #f)))
  (define (match-args args1 args2)
    (d "match-args: ~s <-> ~s" args1 args2)
    (define (match-rest rtype args opt)	;XXX currently ignores `opt'
      (let-values (((head tail) (break (cut eq? '#!rest <>) args)))
	(and (every (cut match rtype <>) head) ; match required args
	     (match rtype (if (pair? tail) (rest-type (cdr tail)) '*)))))
    (define (optargs a)
      (memq a '(#!rest #!optional)))
    (let loop ((args1 args1) (args2 args2) (opt1 #f) (opt2 #f))
      (d "  args ~a ~a ~a ~a" args1 args2 opt1 opt2)
      (cond ((null? args1) 
	     (or opt2
		 (null? args2)
		 (optargs (car args2))))
	    ((null? args2) 
	     (or opt1
		 (optargs (car args1))))
	    ((eq? '#!optional (car args1))
	     (loop (cdr args1) args2 #t opt2))
	    ((eq? '#!optional (car args2))
	     (loop args1 (cdr args2) opt1 #t))
	    ((eq? '#!rest (car args1))
	     (match-rest (rest-type (cdr args1)) args2 opt2))
	    ((eq? '#!rest (car args2))
	     (match-rest (rest-type (cdr args2)) args1 opt1))
	    ((match (car args1) (car args2)) (loop (cdr args1) (cdr args2) opt1 opt2))
	    (else #f))))
  (define (match-results results1 results2)
    (cond ((null? results1) (atom? results2))
	  ((eq? '* results1))
	  ((eq? '* results2))
	  ((null? results2) #f)
	  ((match (car results1) (car results2)) 
	   (match-results (cdr results1) (cdr results2)))
	  (else #f)))
  (define (type<=? t1 t2)
    (or (eq? t1 t2)
	(memq t2 '(* undefined))
	(case t2
	  ((list) (memq t1 '(null pair)))
	  ((procedure) (and (pair? t1) (eq? 'procedure (car t1))))
	  ((number) (memq t1 '(fixnum float)))
	  (else
	   (and (pair? t1) (pair? t2)
		(case (car t1)
		  ((or) (every (cut type<=? <> t2) (cdr t1)))
		  ((procedure)
		   (let ((args1 (if (pair? (cadr t1)) (cadr t1) (caddr t1)))
			 (args2 (if (pair? (cadr t2)) (cadr t2) (caddr t2)))
			 (res1 (if (pair? (cadr t1)) (cddr t1) (cdddr t1)))
			 (res2 (if (pair? (cadr t2)) (cddr t2) (cdddr t2))) )
		     (let loop1 ((args1 args1)
				 (args2 args2)
				 (m1 0) 
				 (m2 0))
		       (cond ((null? args1) 
			      (and (or (null? args2) (> m2 0))
				   (let loop2 ((res1 res1) (res2 res2))
				     (cond ((eq? '* res2) #t)
					   ((null? res2) (null? res1))
					   ((eq? '* res1) #f)
					   ((type<=? (car res1) (car res2))
					    (loop2 (cdr res1) (cdr res2)))
					   (else #f)))))
			     ((null? args2) #f)
			     ((eq? (car args1) '#!optional)
			      (loop1 (cdr args1) args2 1 m2))
			     ((eq? (car args2) '#!optional)
			      (loop1 args1 (cdr args2) m1 1))
			     ((eq? (car args1) '#!rest)
			      (loop1 (cdr args1) args2 2 m2))
			     ((eq? (car args2) '#!rest)
			      (loop1 args1 (cdr args2) m1 2))
			     ((type<=? (car args1) (car args2)) 
			      (loop1 (cdr args1) (cdr args2) m1 m2))
			     (else #f)))))))))))
  (define (multiples n)
    (if (= n 1) "" "s"))
  (define (single what tv loc)
    (if (eq? '* tv)
	'*
	(let ((n (length tv)))
	  (cond ((= 1 n) (car tv))
		((zero? n)
		 (report 
		  loc
		  (sprintf "expected ~a a single result, but were given zero results" what))
		 'undefined)
		(else
		 (report 
		  loc
		  (sprintf "expected ~a a single result, but were given ~a result~a"
			   what n (multiples n)))
		 (first tv))))))
  (define (report loc desc)
    (compiler-warning
     'scrutiny
     "~a~a" 
     (location-name loc) desc))
  (define (location-name loc)
    (define (lname loc1)
      (if loc1
	  (sprintf "procedure `~a'" (real-name loc1))
	  "unknown procedure"))
    (cond ((null? loc) "at toplevel:\n  ")
	  ((null? (cdr loc))
	   (sprintf "in toplevel ~a:\n  " (lname (car loc))))
	  (else
	   (let rec ((loc loc))
	     (if (null? (cdr loc))
		 (location-name loc)
		 (sprintf "in local ~a,\n  ~a" (lname (car loc)) (rec (cdr loc))))))))
  (define add-loc cons)
  (define (fragment x)
    (let ((x (build-expression-tree x)))
      (let walk ((x x) (d 0))
	(cond ((atom? x) x)
	      ((>= d +fragment-max-depth+) '...)
	      ((list? x)
	       (map (cute walk <> (add1 d)) (take x (min +fragment-max-length+ (length x)))))
	      (else x)))))
  (define (pp-fragment x)
    (string-chomp
     (with-output-to-string
       (lambda ()
	 (pp (fragment x))))))
  (define (call-result args e loc x params)
    (define (pname)
      (sprintf 
       "in procedure call to `~s'~a" 
       (fragment x)
       (if (and (pair? params) (pair? (cdr params)))
	   (let ((n (source-info->line (cadr params))))
	     (if (number? n)
		 (sprintf " (line ~a)" n)
		 ""))
	   "")))
    (d "call-result: ~a (~a)" args loc)
    (let* ((ptype (car args))
	   (nargs (length (cdr args)))
	   (xptype `(procedure ,(make-list nargs '*) *)))
      (when (and (not (procedure-type? ptype))
		 (not (match xptype ptype)))
	(report
	 loc
	 (sprintf
	  "expected ~a a value of type `~a', but were given a value of type `~a'"
	  (pname) 
	  xptype
	  ptype)))
      (let-values (((atypes values-rest) (procedure-argument-types ptype (length (cdr args)))))
	(d "  argument-types: ~a (~a)" atypes values-rest)
	(unless (= (length atypes) nargs)
	  (let ((alen (length atypes)))
	    (report 
	     loc
	     (sprintf
	      "expected ~a ~a argument~a, but where given ~a argument~a"
	      (pname) alen (multiples alen)
	      nargs (multiples nargs)))))
	(do ((args (cdr args) (cdr args))
	     (atypes atypes (cdr atypes))
	     (i 1 (add1 i)))
	    ((or (null? args) (null? atypes)))
	  (unless (match (car atypes) (car args))
	    (report
	     loc
	     (sprintf
	      "expected argument #~a of type `~a' ~a, but where given an argument of type `~a'"
	      i (car atypes) (pname) (car args)))))
	(let ((r (procedure-result-types ptype values-rest (cdr args))))
	  (d  "  result-types: ~a" r)
	  r))))
  (define (procedure-type? t)
    (or (eq? 'procedure t)
	(and (pair? t) 
	     (or (eq? 'procedure (car t))
		 (and (eq? 'or (car t))
		      (every procedure-type? (cdr t)))))))
  (define (procedure-argument-types t n)
    (cond ((or (memq t '(* procedure)) 
	       (not-pair? t) )
	   (values (make-list n '*) #f))
	  ((eq? 'procedure (car t))
	   (let* ((vf #f)
		  (llist
		   (let loop ((at (if (or (string? (second t)) (symbol? (second t)))
				      (third t)
				      (second t)))
			      (m n)
			      (opt #f))
		     (cond ((null? at) '())
			   ((eq? '#!optional (car at)) 
			    (loop (cdr at) m #t) )
			   ((eq? '#!rest (car at))
			    (set! vf (and (pair? (cdr at)) (eq? 'values (cadr at))))
			    (make-list m (rest-type (cdr at))))
			   ((and opt (<= m 0)) '())
			   (else (cons (car at) (loop (cdr at) (sub1 m) opt)))))))
	     (values llist vf)))
	  (else (bomb "not a procedure type" t))))
  (define (procedure-result-types t values-rest? args)
    (cond (values-rest? args)
          ((or (memq t '(* procedure))
	       (not-pair? t) )
	   '*)
	  ((eq? 'procedure (car t))
	   (call/cc
	    (lambda (return)
	      (let loop ((rt (if (or (string? (second t)) (symbol? (second t)))
				 (cdddr t)
				 (cddr t))))
		(cond ((null? rt) '())
		      ((eq? '* rt) (return '*))
		      (else (cons (car rt) (loop (cdr rt)))))))))
	  (else (bomb "not a procedure type: ~a" t))))
  (define (noreturn-type? t)
    (or (eq? 'noreturn t)
	(and (pair? t)
	     (eq? 'or (car t))
	     (any noreturn-type? (cdr t)))))
  (define (walk n e loc dest)		; returns result specifier
    (let ((subs (node-subexpressions n))
	  (params (node-parameters n)) 
	  (class (node-class n)) )
      (d "walk: ~a ~a (loc: ~a, dest: ~a)" class params loc dest)
      (let ((results
	     (case class
	       ((quote) (list (constant-result (first params))))
	       ((##core#undefined) '(*))
	       ((##core#proc) '(procedure))
	       ((##core#global-ref) (global-result (first params) loc))
	       ((##core#variable) (variable-result (first params) e loc))
	       ((if) (let ((rt (single "in conditional" (walk (first subs) e loc dest) loc)))
		       (always-true rt loc n)
		       (let ((r1 (walk (second subs) e loc dest))
			     (r2 (walk (third subs) e loc dest)))
			 (cond ((and (not (eq? r1 '*)) 
				     (not (eq? '* r2)) )
				(when (and (not (any noreturn-type? r1))
					   (not (any noreturn-type? r2))
					   (not (= (length r1) (length r2))))
				  (report 
				   loc
				   (sprintf
				    "branches in conditional expression differ in the number of results:~%~%~a"
				    (pp-fragment n))))
				(map (lambda (t1 t2) (simplify `(or ,t1 ,t2)))
				     r1 r2))
			       (else '*)))))
	       ((let)
		(let loop ((vars params) (body subs) (e2 '()))
		  (if (null? vars)
		      (walk (car body) (append e2 e) loc dest)
		      (let ((t (single 
				(sprintf "in `let' binding of `~a'" (real-name (car vars)))
				(walk (car body) e loc (car vars)) loc)))
			(loop (cdr vars) (cdr body) (alist-cons (car vars) t e2))))))
	       ((##core#lambda lambda)
		(decompose-lambda-list
		 (first params)
		 (lambda (vars argc rest)
		   (let* ((name (if dest (list dest) '()))
			  (args (append (make-list argc '*) (if rest '(#!rest) '()))) 
			  (e2 (append (map (lambda (v) (cons v '*)) 
					   (if rest (butlast vars) vars))
				      e))
			  (r (walk (first subs)
				   (if rest (alist-cons rest 'list e2) e2)
				   (add-loc dest loc)
				   #f)))
		     (list 
		      (append
		       '(procedure) 
		       name
		       (list args)
		       r))))))
	       ((set! ##core#set!)
		(let* ((var (first params))
		       (type (##sys#get var '##core#type))
		       (rt (single 
			    (sprintf "in assignment to `~a'" var)
			    (walk (first subs) e loc var)
			    loc))
		       (b (assq var e)) )
		  (when (and type (not b)
			     (not (eq? type 'deprecated))
			     (not (match type rt)))
		    (report
		     loc
		     (sprintf 
		      "assignment of value of type `~a' to toplevel variable `~a' does not match declared type `~a'"
		      rt var type)))
		  (when (and b (eq? 'undefined (cdr b)))
		    (set-cdr! b rt))
		  '(undefined)))
	       ((##core#primitive ##core#inline_ref) '*)
	       ((##core#call)
		(let* ((f (fragment n))
		       (args (map (lambda (n i)
				    (single 
				     (sprintf 
				      "in ~a of procedure call `~s'"
				      (if (zero? i)
					  "operator position"
					  (sprintf "argument #~a" i))
				      f)
				     (walk n e loc #f) loc))
				  subs (iota (length subs)))))
		  (call-result args e loc (first subs) params)))
	       ((##core#switch ##core#cond)
		(bomb "unexpected node class: ~a" class))
	       (else
		(for-each (lambda (n) (walk n e loc #f)) subs)
		'*))))
	(d "  -> ~a" results)
	results)))
  (walk (first (node-subexpressions node)) '() '() #f))

(define (load-type-database name #!optional (path (repository-path)))
  (and-let* ((dbfile (file-exists? (make-pathname path name))))
    (when verbose-mode
      (printf "loading type database ~a ...~%" dbfile))
    (for-each
     (lambda (e)
       (let* ((name (car e))
	      (old (##sys#get name '##core#type))
	      (new (cadr e)))
	 (when (and old (not (equal? old new)))
	   (compiler-warning 
	    'scrutiny
	    "type-definition `~a' for toplevel binding `~a' conflicts with previously loaded type `~a'"
	    name new old))
	 (##sys#put! name '##core#type new)))
     (read-file dbfile))))
