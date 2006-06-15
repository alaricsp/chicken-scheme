;;;; easyffi.scm
;
; Copyright (c) 2000-2006, Felix L. Winkelmann
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


#{compiler debugging-chicken used-units register-ffi-macro parse-easy-ffi ffi-include-path-list 
	   foreign-type-declaration foreign-type-table ffi-dont-include compiler-warning
	   foreign-declarations explicit-use-flag check-c-syntax no-c-syntax-checks}

(declare
  (unit easyffi)
  (disable-warning var)
  (compress-literals)
  (export parse-easy-ffi register-ffi-macro used-units foreign-declarations number-type
	  ffi-include-path-list ffi-dont-include
	  foreign-type-declaration explicit-use-flag debugging-chicken check-c-syntax
	  no-c-syntax-checks) )

(eval-when (compile) (match-error-control #:unspecified))

(include "tweaks.scm")
(include "easyffi.l.silex")

(define mutable-fields #f)
(define use-finalizers #f)
(define exception-handler #f)
(define destructor-name 'destroy)
(define pp-mode #f)
(define processed-output '())
(define macro-table '((|CHICKEN| * ())))
(define pp-conditional-stack '())
(define pp-process #t)
(define type-map '())
(define ffi-include-path-list '("."))
(define ffi-dont-include #f)
(define export-constants #f)
(define prefix #f)
(define name-substitution-rxs '())
(define name-substitution-repls '())
(define declared-types '())
(define rename-list '())
(define abstract-classes '())
(define full-specialization #f)
(define defined-enums '())
(define parsing-error quit)
(define imported-headers '())

(define (pp-token tok)
  (if pp-mode
      tok
      (quit "FFI lexer error: preprocessor-command out of context: ~S" tok) ) )

(define (lexer-error chr)
  (parsing-error "FFI lexer error: illegal character: `~c' (code ~s)" chr (char->integer chr)))


;;; Split lexed tokens into chunks

(define (chunkify)
  (let ((iparts 0))
    (let rec ([scope 0])
      (let ([chunks '()])
	(let loop ([mode #f] [tokens '()])
	  (let ([t (lexer)])
	    (case t
	      [(stop)
	       (when (not (zero? iparts))
		 (parsing-error "unbalanced `@interface'/`@implementation'") )
	       (case mode
		 ((interface implementation)
		  (parsing-error "missing `@end' declaration") ) )
	       (reverse chunks) ]
	      [(pp-end)
	       (set! chunks (cons (reverse tokens) chunks))
	       (loop #f '()) ]
	      [(pp-define pp-include pp-if pp-ifdef pp-ifndef pp-else pp-endif pp-undef pp-import
			  pp-pragma pp-error)
	       (loop 'pp (list t)) ]
	      [(close-curly)
	       (cond [(not (positive? scope)) (parsing-error "`}' out of context")]
		     [(null? tokens) (reverse chunks)]
		     [else (cons (reverse tokens) chunks)] ) ]
	      [(open-curly)
	       (let ([new (rec (add1 scope))])
		 (set! chunks (cons (append-reverse tokens `((scope . ,new))) chunks))
		 (loop #f '()) ) ]
	      [(close-paren)
	       (if (eq? mode 'declare)
		   (begin
		     (set! chunks (cons (reverse (cons 'close-paren tokens)) chunks))
		     (loop #f '()) )
		   (loop mode (cons t tokens)) ) ]
	      [(declare)
	       (loop 'declare '(declare)) ]
	      [(interface implementation)
	       (when (not (zero? iparts))
		 (parsing-error "`@interface'/`@implementation' without matching `@end'") )
	       (set! iparts (add1 iparts))
	       (loop t (list t)) ]
	      [(end)
	       (set! iparts (sub1 iparts))
	       (set! chunks (cons* '(end) (reverse tokens) chunks))
	       (loop #f '()) ]
	      [(semicolon)
	       (if mode
		   (parsing-error "unexpected semicolon")
		   (begin
		     (set! chunks (cons (reverse tokens) chunks))
		     (loop #f '()) ) ) ]
	      [else (loop mode (cons t tokens))] ) ) ) ) ) ) )


;;; Parse each chunk separately

(define (parse c)
  (match c
    [() #f]
    [('pp-else)
     (when (null? pp-conditional-stack)
       (parsing-error "unbalanced preprocessor conditionals") )
     (set! pp-process (and (not (car pp-conditional-stack)) (every identity (cdr pp-conditional-stack)))) ]
    [('pp-endif)
     (when (null? pp-conditional-stack)
       (parsing-error "unbalanced preprocessor conditionals") )
     (set! pp-conditional-stack (cdr pp-conditional-stack))
     (set! pp-process (every identity pp-conditional-stack)) ]
    [('pp-ifdef ('id name))
     (set! pp-process (and pp-process (assq (string->symbol name) macro-table)))
     (set! pp-conditional-stack (cons pp-process pp-conditional-stack)) ]
    [('pp-ifndef ('id name))
     (set! pp-process (and pp-process (not (assq (string->symbol name) macro-table))))
     (set! pp-conditional-stack (cons pp-process pp-conditional-stack)) ]
    [('pp-if . _)
     (compiler-warning 'ffi "preprocessor conditional `~A' ignored (assuming false)" c)
     (set! pp-process #f)
     (set! pp-conditional-stack (cons #f pp-conditional-stack)) ]
    [_ (when pp-process
	 (match c
	   [('pp-define ('id name))
	    (let ([s (string->symbol name)])
	      (set! macro-table (cons (list s '* '()) macro-table)) ) ]
	   [('pp-define ('id name) ('num n))
	    (let ([s (string->symbol name)])
	      (set! macro-table (cons (list s (if (exact? n) 'integer 'double) `((num ,n))) macro-table))
	      (process-constant-def s n) ) ]
	   [('pp-define ('id name) ('char c))
	    (let ([s (string->symbol name)])
	      (set! macro-table (cons (list s 'char `((char ,c))) macro-table))
	      (process-constant-def s c) ) ]
	   [('pp-define ('id name) . more)
	    (let ([t (compute-macro-type more)]
		  [s (string->symbol name)] )
	      (set! macro-table (cons (list s t more) macro-table))
	      (process-macro-def s t) ) ]
	   [('pp-undef ('id name))
	    (set! macro-table (delete (assq (string->symbol name) macro-table) eq?)) ]
	   [('pp-error msgs ...)
	    (parsing-error (string-intersperse (cons "(#error) " (map reparse-item msgs)) "")) ]
	   [('pp-include ((or 'string 'i-string) filename))
	    (unless ffi-dont-include
	      (let ([fname (resolve-ffi-include-file filename)])
		(if fname
		    (call-with-input-file fname parse-easy-ffi-rec)
		    (parsing-error "can not open include file `~A'" filename) ) ) ) ]
	   [('pp-import ((or 'string 'i-string) filename))
	    (unless ffi-dont-include
	      (let ([fname (resolve-ffi-include-file filename)])
		(if (and fname (not member fname imported-headers))
		    (call-with-input-file fname
		      (lambda (f)
			(set! imported-headers (cons fname imported-headers))
			(parse-easy-ffi-rec f) ) )
		    (parsing-error "can not open include file `~A'" filename) ) ) ) ]
	   [('pp-pragma . more) #f]
	   [('declare 'open-paren ('id decl) 'comma val 'close-paren)
	    (parse-declaration decl val) ]
	   [('declare . _)
	    (parsing-error "invalid syntax in pseudo declaration: ~S" c) ]
	   [_ (let ([cb #f] 
		    [ab #f]
		    [sp #f]
		    [dc #f]
		    [ds #f] )
		(let loop ([c (subst-macros c)])
		  (when (memq 'T debugging-chicken)
		    (pp `(TOKENS: ,c)) )
		  (match c
		    [((or 'extern 'static 'volatile 'inline) . more)
		     (loop more) ]
		    [('abstract . more)
		     (set! ab #t)
		     (loop more) ]
		    [('specialize . more)
		     (set! sp #t)
		     (loop more) ]
		    [('callback . more)
		     (set! cb #t) 
		     (loop more) ]
		    [('discard . more)
		     (set! ds #t)
		     (loop more) ]
		    [('const . more)
		     (if (memq 'open-paren more)
			 (parse-prototype c cb sp dc ds)
			 (begin
			   (set! dc #t)
			   (loop more) ) ) ]
		    [('enum ('scope more))
		     (parse-enum-def #f (subst-macros more)) ]
		    [('enum ('id name) ('scope more))
		     (parse-enum-def name (subst-macros more)) ]
		    [('class . more)
		     (parse-class-def more ab) ]
		    [((or 'union 'struct) ('id name) ('scope . more))
		     (parse-struct-def (car c) name ab (subst-macros more)) ]
		    [((or 'union 'struct) ('id name)) #f]
		    [('namespace ('id name) (scope . more))
		     (for-each parse more) ]
		    [('typedef . more)
		     (parse-typedef more) ]
		    [(and more (('id name) . _))
		     (parse-prototype more cb sp dc ds) ]
		    [more
		     (parse-prototype more cb sp dc ds)] ) ) ) ] ) ) ] ) )

(define parse-again parse)

(define parse-type-rec
  (match-lambda
    [('const . more) 
     (let-values ([(t0 more) (parse-type-rec more)])
       (values `(const ,t0) more) ) ]
    [('unsigned t 'star . more)
     (let-values ([(t0 more) (parse-type-rec (cons* 'unsigned t more))])
       (values `(pointer ,t0) more) ) ]
    [('signed t 'star . more)
     (let-values ([(t0 more) (parse-type-rec (cons* 'signed t more))])
       (values `(pointer ,t0) more) ) ]
    [(t ('op "<") . more)
     (let*-values ([(ts more) (parse-typelist more)]
		   [(t0 _) (parse-type-rec (list t))] )
       (values `(template ,t0 ,@ts) more) ) ]
    [('signed . more) (parse-type-rec more)]
    [`(unsigned fixnum . ,more) (values 'unsigned-int more)]
    [`(unsigned int . ,more)
     (values (if (eq? number-type 'fixnum) 'unsigned-int 'unsigned-integer) more)]
    [`(unsigned char . ,more) (values 'unsigned-char more)]
    [`(unsigned short int . ,more) (values 'unsigned-short more)]
    [`(unsigned long int . ,more) (values 'unsigned-long more)]
    [`(unsigned short . ,more) (values 'unsigned-short more)]
    [`(unsigned long . ,more) (values 'unsigned-long more)]
    [`(u32 . ,more) (values 'unsigned-integer32 more)]
    [`(s32 . ,more) (values 'integer32 more)]
    [`(s64 . ,more) (values 'integer64 more)]
    [`(void . ,more) (values 'void more)]
    [`(bool . ,more) (values 'bool more)]
    [`(symbol . ,more) (values 'symbol more)]
    [`(unsigned byte . ,more) (values 'unsigned-byte more)]
    [`(size_t . ,more) (values 'unsigned-integer more)]
    [`(byte . ,more) (values 'byte more)]
    [`(scheme-value . ,more) (values 'scheme-object more)]
    [`(scheme-pointer . ,more) (values 'scheme-pointer more)]
    [`(byte-vector . ,more) (values 'byte-vector more)]
    [`(fixnum . ,more) (values 'int more)]
    [`(pointer unsigned short int star . ,more) (values '(c-pointer unsigned-short) more)]
    [`(pointer unsigned long int star . ,more) (values '(c-pointer unsigned-long) more)]
    [`(pointer unsigned ,(and t (or 'char 'short 'long 'int 'byte)) star . ,more) 
     (values `(c-pointer ,(string->symbol (string-append "unsigned-" (symbol->string t)))) more) ]
    [`(pointer ,t star . ,more) (values `(c-pointer ,t) more)]
    [`(int . ,more) (values (if (eq? number-type 'fixnum) 'int 'integer) more)]
    [`(char . ,more) (values 'char more)]
    [`(short int . ,more) (values 'short more)]
    [`(long int . ,more) (values 'long more)]
    [`(short . ,more) (values 'short more)]
    [`(long . ,more) (values 'long more)]
    [`(float . ,more) (values 'float more)]
    [`(double . ,more) (values 'double more)]
    [`(number . ,more) (values 'number more)]
    [((and m (or 'union 'struct)) ('id sname) . more)
     (values `(,m ,sname) more) ]
    [('enum ('id sname) . more) (values `(enum ,sname) more)]
    [(('id t) . more)
     (let ([st (string->symbol t)])
       (cond [(assq st type-map) => (lambda (a) (values (cdr a) more))]
	     [(memq st defined-enums) (values `(enum ,t) more)]
	     [(memq st declared-types) (values st more)]
	     [else (values t more)] ) ) ]
    [x (parsing-error "invalid type specifier: ~S" x)] ) )

(define (parse-type ts #!optional io return-type discard ftype-name)
  (let-values ([(t0 more) (parse-type-rec ts)])
    (let loop ([t0 t0] [more more])
      (match more
	[('star . more)
	 (loop `(pointer ,t0) more) ]
	[(('op "&") . more)
	 (loop `(ref ,t0) more) ]
	[('open-paren 'star 'close-paren 'open-paren . more)
	 (when ftype-name (vector-set! ftype-name 0 #f))
	 (let-values ([(ts _ _ more) (parse-arglist more)])
	   (values `(function ,t0 ,ts) more) ) ]
	[('open-paren 'star ('id ftname) 'close-paren 'open-paren . more)
	 (when ftype-name (vector-set! ftype-name 0 ftname))
	 (let-values ([(ts _ _ more) (parse-arglist more)])
	   (values `(function ,t0 ,ts) more) ) ]
	[(('id _) 'open-bracket . more2)
	 (let ([a (memq 'close-bracket more2)])
	   (if a
	       (loop `(pointer ,t0) (cons (car more) (cdr a)))
	       (values (simplify-type t0 io return-type discard) more) ) ) ]
	[_ (values (simplify-type t0 io return-type discard) more)] ) ) ) )

(define (simplify-type t0 io return-type discard)
  (define (strtype) (if discard 'c-string* 'c-string))
  (define (simplify-ptr t0 t)
    (let ([st (string->symbol t)])
      (if (memq st defined-classes) 
	  `(instance ,t ,(fix-cname t))
	  t0) ) )
  (define (simplify-ref t0 t)
    (let ([st (string->symbol t)])
      (if (memq st defined-classes) 
	  `(instance-ref ,t ,(fix-cname t))
	  t0) ) )
  (cond [io t0]
	[return-type
	 (match t0
	   ['(pointer char) (strtype)]
	   ['(pointer (const char)) (strtype)]
	   [`(pointer (const ,(? string? t))) (simplify-ptr t0 t)]
	   [`(pointer ,(? string? t)) (simplify-ptr t0 t)]
	   [`(ref (const ,(? string? t))) (simplify-ref t0 t)]
	   [`(ref ,(? string? t)) (simplify-ref t0 t)]
	   [_ t0] ) ]
	[else
	 (let loop ([t1 t0])
	   (match t1
	     [`(pointer (const ,t2)) (loop `(pointer ,t2))]
	     [`(ref (const ,t2)) (loop `(ref ,t2))]
	     ['(pointer unsigned-fixnum) 'u32vector]
	     [(or '(pointer unsigned-integer)
		  '(pointer unsigned-int)
		  '(pointer unsigned-int32)
		  '(pointer unsigned-integer32)) 
	      'u32vector]
	     ['(pointer unsigned-short) 'u16vector]
	     ['(pointer unsigned-char) 'u8vector]
	     ['(pointer unsigned-byte) 'u8vector]
	     ['(pointer byte) 's8vector]
	     ['(pointer unsigned-long) 'u32vector]
	     ['(pointer fixnum) 's32vector]
	     [(or '(pointer integer)
		  '(pointer integer32)
		  '(pointer int32)
		  '(pointer int) )
	      's32vector]
	     ['(pointer short) 's16vector]
	     ['(pointer char) (strtype)]
	     ['(pointer long) 's32vector]
	     ['(pointer float) 'f32vector]
	     [`(pointer ,(or 'double 'number)) 'f64vector]
	     [`(pointer ,(? string? t)) (simplify-ptr t1 t)]
	     [`(ref ,(? string? t)) (simplify-ref t1 t)]
	     [_ t1] ) ) ] ) )

(define (parse-arglist ts)
  (let ([vars '()])
    (define (index! v i lens)
      (set! vars (append vars (list v)))
      (and-let* ([a (rassoc v lens equal?)])
	(set-cdr! a i) ) )
    (define (check-lvars lvars)
      (for-each
       (lambda (lv) 
	 (let ([name (cdr lv)])
	   (when (string? name)
	     (cond [(list-index (cut equal? name <>) vars) =>
		    (lambda (i) (set-cdr! lv i)) ]
		   [else 
		    (parsing-error "no argument named `~a' given for length indicator" name) ] ) ) ) )
       lvars) )
    (let rec ([more ts] [args '()] [inout '()] [i 0] [lens '()])
      (match more
	[('close-paren . more)
	 (check-lvars lens)
	 (values (reverse args) (reverse inout) lens more) ]
	[('dots . _)
	 (parsing-error "varargs are not supported") ]
	[_ (let ([io #f])
	     (match more
	       [((and iov (or 'in 'out 'inout)) . more2)
		(set! more more2)
		(set! io iov) ]
	       [('length 'open-paren ('id lvar) 'close-paren . more2)
		(set! more more2)
		(set! lens (alist-cons i lvar lens)) ] )
	     (let-values ([(type more) (parse-type more io #f)])
	       (match more
		 [(('id str) 'comma . more)
		  (index! str i lens)
		  (rec more (cons type args) (cons io inout) (add1 i) lens) ]
		 [(('id str) 'close-paren . more)
		  (index! str i lens)
		  (check-lvars lens)
		  (values (reverse (cons type args)) (reverse (cons io inout)) lens more) ]
		 [('comma . more) 
		  (rec more (cons type args) (cons io inout) (add1 i) lens) ]
		 [('close-paren . more)
		  (check-lvars lens)
		  (values (reverse (cons type args)) (reverse (cons io inout)) lens more) ]
		 [_ (parsing-error "bad argument list syntax: ~S" more)] ) ) ) ] ) ) ) )

(define (parse-typelist ts)
  (let rec ([more ts] [ts '()])
    (match more
      [(('op ">") . more)
       (values (reverse ts) more) ]
      [_ (let-values ([(type more) (parse-type more #f #f)])
	   (match more
	     [('comma . more)
	      (rec more (cons type ts)) ]
	     [(('op ">") . more)
	      (values (reverse (cons type ts)) more) ]
	     [_ (parsing-error "bad template type list syntax: ~S" more)] ) ) ] ) ) )

(define (subst-macros chunk)
  (let loop ([c chunk])
    (match c
      [() '()]
      [((and x ('id name)) . more)
	(let ([a (assq (string->symbol name) macro-table)])
	  (if a
	      (loop (append (third a) more))
	      (cons x (loop more)) ) ) ]
      [(x . y) (cons x (loop y))] ) ) )

(define (parse-prototype ts cb sp const discard)
  (fluid-let ([full-specialization (or sp full-specialization)])
    (let-values ([(rtype more) (parse-type ts #f #t discard)])
      (let loop ([more more])
	(match more
	  [() #f]
	  [(('id str) ('op "::") . more) #f]
	  [(('id str) 'open-paren 'void 'close-paren . more)
	   (process-prototype-def rtype (string->symbol str) '() '() '() cb)
	   (match more
	     [(('scope . _) . more) (parse-again more)]
	     [() #f]
	     [_ (parsing-error "unexpected tokens: ~S" more)] ) ]
	  [(('id str) 'open-paren . more)
	   (let-values ([(args io lvars more) (parse-arglist more)])
	     (process-prototype-def rtype (string->symbol str) args io lvars cb)
	     (match more
	       [(('scope . _) . more) (parse-again more)]
	       [() #f]
	       [_ (parsing-error "unexpected tokens: ~S" more)] ) ) ]
	  [(('id str) 'comma . more)
	   (process-variable-def rtype (string->symbol str) const)
	   (loop more) ]
	  [(('id str))
	   (process-variable-def rtype (string->symbol str) const) ]
	  [(('id str) . (or (('op "=") . _) ()))
	   (process-variable-def rtype (string->symbol str) const) ]
	  [else (parsing-error "bad prototype syntax `~A'" more)] ) ) ) ) )

(define (parse-enum-def ename ts)
  (when ename (set! defined-enums (cons (string->symbol ename) defined-enums)))
  (let loop ([ts ts] [i 0] [items '()])
    (match ts
      [('close-curly) #f]
      [_ (let-values ([(sym val more) (parse-enum-item ts i items)])
	   (let ([items (alist-cons sym val items)]
		 [i (add1 val)] )
	     (match more
	       [() (process-enum-def ename items)]
	       [('comma . more) (loop more i items)]
	       [_ (parsing-error "syntax error in enum form `~A'" more)] ) ) ) ] ) ) )

(define (parse-enum-item ts i items)
  (match ts
    [(('id name) ('op "=") ('id name2) . more)
     (cond ((assq (string->symbol name2) items)
	    => (lambda (a) (values (string->symbol name) (cdr a) more)))
	   (else (parsing-error "undefined enum value `~a'" name2)) ) ]
    [(('id name) ('op "=") ('num n) . more)
     (if (integer? n)
	 (values (string->symbol name) n more) 
	 (parsing-error "inexact enum value for `~A'" name) ) ]
    [(('id name) . more)
     (values (string->symbol name) i more) ] 
    [_ (parsing-error "invalid enum syntax `~A'" ts)] ) )

(define (parse-struct-def m sname ab ts)
  (let ([fields '()])
    (let loop ([ts ts])
      (unless (null? ts)
	(let*-values ([(mut? more) 
		       (match (car ts)
			 [('mutable . more) (values #t more)]
			 [x (values #f x)] ) ]
		      [(type more) (parse-type more #f #t)] )
	  (let loop2 ([type type] [more more])
	    (match more
	      [('star . more)
	       (loop2 (simplify-type `(pointer ,type) #f #t #f) more) ]
	      [(('id name) . more)
	       (set! fields (cons (list type (string->symbol name)) fields))
	       (process-struct-member-def m sname name type (or mut? mutable-fields))
	       (match more
		 [('comma . more) (loop2 type more)]
		 [() (loop (cdr ts))]
		 [_ (parsing-error "syntax error in struct/union member (~A): `~A'" sname more)] ) ]
	      [() (loop (cdr ts))]
	      [_ (parsing-error "syntax error in struct/union form (~A): `~A'" 
				sname more)] ) ) ) ) )
    (unless ab 
      (let ([maker (fix-name (string-append "make-" (->string sname)))]
	    [fields (reverse fields)] )
	(emit
	 `(define ,maker
	    (foreign-lambda* (pointer (,m ,sname)) ,fields
	      ,(sprintf "~A ~A *tmp_ = (~A ~A *)C_malloc(sizeof(~A ~A));~%~Areturn(tmp_);"
			m sname m sname m sname
			(string-intersperse
			 (map (lambda (f) (sprintf "tmp_->~A = ~A;~%" (cadr f) (cadr f)))
			      fields)
			 "") ) ) ) ) ) ) ) )

(define (parse-typedef ts)
  (let ([box (vector #f)])
    (let-values ([(type more) (parse-type ts #f #t #f box)])
      (let loop ([more 
		  (let ([name (vector-ref box 0)])
		    (if name
			`((id ,name))
			more) ) ]
		 [type type] )
	(match more
	  [('star . more)
	   (loop more `(pointer ,type)) ]
	  [(('id tname))
	   (set! type-map (alist-cons (string->symbol tname) 
				      (simplify-type type #f #t #f)
				      type-map)) ]
	  [_ (parsing-error "invalid typedef syntax `~A'" more)] ) ) ) ) )

(define has-constructor #f)
(define defined-classes '())

(define (parse-class-def ts ab)
  (match ts
    [(('id name)) 
     (set! defined-classes (cons (string->symbol name) defined-classes)) ]
    [(('id name) . more)
     (let ([sym (string->symbol name)])
       (set! defined-classes (cons sym defined-classes))
       (when ab (set! abstract-classes (cons sym abstract-classes))) )
     (let loop ([more more] [t '(op ":")] [bases '()])
       (if (and (pair? more) (equal? t (car more)))
	   (match more
	     [(_ (or 'public 'protected 'private) ('id bname) . more)
	      (loop more 'comma 
		    (if (memq (string->symbol bname) defined-classes)
			(cons bname bases)
			bases) ) ]
	     [(_ ('id bname) . more)
	      (loop more 'comma
		    (if (memq (string->symbol bname) defined-classes)
			(cons bname bases)
			bases) ) ]
	     [_ (parsing-error "invalid class definition for `~A': ~S" name more)] ) 
	   (match more
	     [(('scope . chunks))
	      (let ([cname (fix-cname name)]
		    [csname (string->symbol name)] )
		(process-class-def name cname bases)
		(fluid-let ([has-constructor #f])
		  (let ([exp #f])
		    (for-each
		     (lambda (chunk)
		       (let loop ([more (subst-macros chunk)])
			 (match more
			   [() #f]
			   [('public ('op ":") . more) 
			    (set! exp #t)
			    (loop more) ]
			   [((or 'private 'protected) ('op ":") . more) 
			    (set! exp #f)
			    (loop more) ]
			   [more 
			    (when exp 
			      (fluid-let ([parse-again loop])
				(parse-member-prototype name cname more #f #f) ) ) ] ) ) )
		     chunks)
		    (when (and (not has-constructor) (not (memq csname abstract-classes)))
		      (process-constructor-def name cname '() '() '()) ) ) ) ) ]
	     [_ (parsing-error "invalid class definition for `~A': ~S" name more)] ) ) ) ]
    [_ (parsing-error "invalid class definition: ~S" ts)] ) )

(define (parse-member-prototype name cname ts cb discard)
  (match ts
    [('specialize . more)
     (fluid-let ([full-specialization #t])
       (parse-member-prototype name cname more #t discard) ) ]
    [('callback . more) 
     (parse-member-prototype name cname more #t discard) ]
    [('discard . more)
     (parse-member-prototype name cname more cb #t) ]
    [((or 'explicit 'virtual) . more)
     (parse-member-prototype name cname more cb discard) ]
    [(('id name2) 'open-paren 'void 'close-paren . more)
     (if (string=? name2 name)
	 (begin
	   (process-constructor-def name cname '() '() '())
	   (set! has-constructor #t)
	   (match more
	     [(('scope . _) . more) (parse-again more)]
	     [() #f]
	     [_ (parsing-error "unexpected tokens: ~S" more)] ) )
	 (parsing-error "invalid constructor for `~A': ~S" name ts) ) ]
    [(('id name2) 'open-paren . more)
     (if (string=? name2 name)
	 (let-values ([(args io lvars more) (parse-arglist more)])
	   (process-constructor-def name cname args io lvars) 
	   (set! has-constructor #t)
	   (match more
	     [(('scope . _) . more) (parse-again more)]
	     [() #f]
	     [_ (parsing-error "unexpected tokens: ~S" more)] ) )
	 (parsing-error "invalid constructor for `~A': ~S" name ts) ) ]
    [(('op "~") ('id name2) 'open-paren . (or ('void 'close-paren . more) ('close-paren . more)))
     (if (string=? name2 name)
	 (match more
	   [(('scope . _) . more) (parse-again more)]
	   [() #f]
	   [_ (parsing-error "unexpected tokens: ~S" more)] )
	 (parsing-error "invalid destructor for `~A': ~S" name ts) ) ]
    [('static . more)
     (let-values ([(rtype more) (parse-type more #f #t)])
       (match more
	 [(('id str) 'open-paren 'void 'close-paren . more)
	    (process-prototype-def
	     rtype
	     (string->symbol (string-append name "::" str)) '() '() '() cb #f)
	    (match more
	      [(('scope . _) . more) (parse-again more)]
	      [() #f]
	      [_ (parsing-error "unexpected tokens: ~S" more)] ) ]
	 [(('id str) 'open-paren . more)
	  (let-values ([(args io lvars more) (parse-arglist more)])
	    (process-prototype-def 
	     rtype (string->symbol (string-append name "::" str)) 
	     args io lvars cb #f)
	    (match more
	      [(('scope . _) . more) (parse-again more)]
	      [() #f]
	      [_ (parsing-error "unexpected tokens: ~S" more)] ) ) ]
	 [_ (parsing-error "bad static member prototype syntax: ~S" more)] ) ) ]
    [_ (let-values ([(rtype more) (parse-type ts #f #t discard)])
	 (match more
	   [(('id str) 'open-paren 'void 'close-paren . more)
	    (process-member-prototype-def name cname rtype (string->symbol str) '() '() '() cb)
	    (parse-member-body more) ]
	   [(('id str) 'open-paren . more)
	    (let-values ([(args io lvars more) (parse-arglist more)])
	      (process-member-prototype-def name cname rtype (string->symbol str) args io lvars cb)
	      (parse-member-body more) ) ]
	   [(('id str) . (or (('op "=") . _) ()))
	    #f]				; member variables are ignored
	   [_ (parsing-error "bad member prototype syntax: ~S" more)] ) ) ] ) )

(define (parse-member-body ts)
  (let loop ([more ts])
    (match more
      [('const . more) (loop more)]
      [(('op "=") (num 0) . more) 
       (set! has-constructor #t)
       (loop more) ]
      [(('scope . _) . more) (parse-again more)]
      [() #f]
      [_ (parsing-error "unexpected tokens: ~S" more)] ) ) )

(define reparse-item 
  (match-lambda 
   ['pp-define "#define"]
   ['pp-include "#include"]
   ['pp-undef "#undef"]
   ['pp-ifndef "#ifndef"]
   ['pp-ifdef "#ifdef"]
   ['pp-if "#if"]
   ['pp-pragma "#pragma"]
   ['pp-error "#error"]
   ['pp-else "#else"]
   ['pp-endif "#endif"]
   [('id str) str]
   [('num num) num]
   [('op op) op]
   ['star "*"]
   ['open-paren "("]
   ['close-paren ")"]
   ['open-bracket "["]
   ['close-bracket "]"]
   ['open-curly "{"]
   ['close-curly "}"]
   ['fixnum "int"]
   ['comma ","]
   [('string str) (string-append "\"" str "\"")]
   [('i-string str) (string-append "<" str ">")]
   ['class "class"]
   ['protected "protected"]
   ['public "public"]
   ['private "private"]
   [c c] ) )

(define (type-union t1 t2)
  (cond [(eq? '_ t2) t1]
	[(eq? t1 t2) t1]
	[(eq? 'integer t1)
	 (case t2
	   [(double) 'double]
	   [else '*] ) ]
	[(and (eq? t1 'double) (eq? 'integer t2)) 'double]
	[else '*] ) )

(define (compute-macro-type ts)
  (let rec ([ts ts])
    (if (null? ts)
	'_
	(match (car ts)
	  [('num n) (type-union (if (exact? n) 'integer 'double) (rec (cdr ts)))]
	  [('char n) (type-union 'char (rec (cdr ts)))]
	  [('id str)
	   (let ([a (assq (string->symbol str) macro-table)])
	     (if a 
		 (type-union (second a) (rec (cdr ts)))
		 '*) ) ]
	  [_ (rec (cdr ts))] ) ) ) )

(define (emit x)
  (when (memq '|F| debugging-chicken)
    (pretty-print x) )
  (set! processed-output (cons x processed-output) ) )

(define (process-macro-def name type)
  (if (memq type '(* _))
      (compiler-warning 'ffi "can not compute macro type `~A' (ignored)" name)
      (let* ([name2 (fix-name name)]
	     [sname (->string name)] )
	(emit `(define-foreign-variable ,name2 ,type ,sname))
	(when export-constants
	  (emit `(define ,name2 ,name2))))))

(define (process-constant-def name val)
  (let ([name (fix-name name)])
    (emit `(define-constant ,name ,val))
    (when export-constants 
      (emit `(define ,name ,name)))))

(define (process-prototype-def rtype name args io lvars cb #!optional (use-prefix #t))
  (let* ([name2 (fix-name name use-prefix)]
	 [slist (gen-spec-list args io)] 
	 [vars (unzip1 slist)] )
    (emit
     (if (and full-specialization (pair? args))
	 (let ([tmp (gensym)])
	   `(begin
	      (declare (hide ,tmp))
	      (define ,tmp
		(,(if cb 'foreign-safe-lambda 'foreign-lambda)
		 ,rtype ,(->string name) ,@args) )
	      (define-method (,name2 ,@(filter-map (lambda (spec io i)
						     (and (memq io '(#f in inout))
							  (not (assq i lvars))
							  spec) )
						   slist io (iota (length slist)) ))
		,(make-inout-wrapper tmp rtype vars args io lvars) ) ) )
	 (let* ([io? (or (any identity io) (pair? lvars))]
		[fname (if io? (gensym) name2)] )
	   `(begin
	      ,@(if io? `((declare (hide ,fname))) '())
	      (define ,fname
		(,(if cb 'foreign-safe-lambda 'foreign-lambda)
		 ,rtype ,(->string name) ,@args))
	      ,@(if io?
		    (let ([inlist (filter-map (lambda (var io i)
						(and (memq io '(#f in inout)) 
						     (not (assq i lvars))
						     var) )
					      vars io (iota (length vars))) ] )
		      `((define (,name2 ,@inlist) 
			  ,(make-inout-wrapper fname rtype vars args io lvars) ) ) )
		    '() ) ) ) ) ) ) )

(define (make-inout-wrapper rname rtype vars args io lvars)
  (let ([tmp (gensym)] 
	[results (map (lambda _ (gensym)) vars)] )
    (if (or (any identity io) (pair? lvars))
	`(let-location ,(filter-map
			 (lambda (rvar var io arg)
			   (let ([pt (match arg
				       [('pointer t) t]
				       [('ref t) t]
				       [_ (if io
					      (begin
						(compiler-warning 
						 'ffi
						 "~A parameter used with non-pointer type"
						 io arg)
						arg)
					      arg) ] ) ] )
			     (case io
			       [(in inout) (list rvar pt var)]
			       [(out) (list rvar pt)] 
			       [else #f] ) ) )
			 results vars io args)
	   (let ([,tmp (,rname ,@(map
				  (lambda (rvar var io i)
				    (cond [io `(location ,rvar)]
					  [(assq i lvars) =>
					   (lambda (a)
					     (let ([i2 (cdr a)])
					       `(,(length-procedure (list-ref args i2))
						 ,(list-ref vars i2)) ) ) ]
					  [else var] ) )
				  results vars io (iota (length vars))) ) ] )
	     ,(if (any identity io)
		  `(values 
		    ,@(if (eq? rtype 'void)
			  '()
			  (list tmp) ) 
		    ,@(filter-map (lambda (rvar io) (and (memq io '(out inout)) rvar))
				  results io) )
		  tmp) ) )
	`(,rname ,@vars) ) ) )

(define (length-procedure t)
  (case t
    [(u8vector) 'u8vector-length]
    [(s8vector) 's8vector-length]
    [(u16vector) 'u16vector-length]
    [(s16vector) 's16vector-length]
    [(u32vector) 'u32vector-length]
    [(s32vector) 's32vector-length]
    [(f32vector) 'f32vector-length]
    [(f64vector) 'f64vector-length]
    [(byte-vector) 'byte-vector-length]
    [(c-string c-string*) 'string-length]
    [else (parsing-error "do not know how to compute length of foreign type argument `~s'" t)] ) )

(define (process-variable-def rtype name const)
  (let ([tmp (gensym)]
	[var (gensym)] 
	[name2 (fix-name name)] 
	[sname (->string name)] )
    (emit `(define-foreign-variable ,tmp ,rtype ,sname))
    (if const
	(emit `(define ,name2 ,tmp))
	(emit `(define (,name2 . ,var)
		 (if (pair? ,var)
		     (set! ,tmp (car ,var))
		     ,tmp) )))))

(define (process-enum-def ename items)
  (for-each
   (match-lambda
     [(name . val)
      (let ([name (fix-name name)])
	(emit `(define-constant ,name ,val))
	(when export-constants 
	  (emit `(define ,name ,name)))) ] )
   (reverse items) ) )

(define (process-struct-member-def m sname name type mut?)
  (let ([getter (fix-name (string-append (->string sname) "-" (->string name)))]
	[setter (fix-name (string-append (->string sname) "-" (->string name) "-set!"))] )
    (emit `(begin
	     (define ,getter
	       (foreign-lambda* ,type (((pointer (,m ,sname)) s))
		 ,(sprintf "return(s->~A);" name) ) )
	     ,@(if mut?
		   `((define ,setter
		       (foreign-lambda* void (((pointer (,m ,sname)) s)
					      (,type x) )
			 ,(sprintf "s->~A = x;" name) ) ) )
		   '() ) ))))

(define (process-class-def name cname basenames)
  (let ([destr (gensym)]
	[csname (string->symbol name)] 
	[bases (if (null? basenames)
		   '(<c++-object>)
		   (map (lambda (b) (fix-cname b)) (reverse basenames) ) ) ] )
    (emit
     `(begin
	(declare (hide ,destr))
	(define-class ,cname ,bases () ) ) )
    (unless (memq csname abstract-classes)
      (emit
       `(begin
	  (define ,destr (foreign-lambda void "delete " (pointer ,name)))
	  (define-method (,destructor-name (this ,cname))
	    (,destr (slot-ref this 'this)) ) )))))

(define (process-constructor-def name cname args io lvars)
  (let ([constr (gensym)]
	[finalize (and use-finalizers (not (memq (string->symbol name) abstract-classes)))] )
    (emit
     `(begin
	(declare (hide ,constr))
	(define ,constr (foreign-lambda (pointer ,name) ,(string-append "new " name) ,@args))
	(define-method (initialize (this ,cname) initargs) 
	  ;; no CALL-NEXT-METHOD here: we don't want to invoke the base-class constructor.
	  ,@(if finalize
		`((set-finalizer! this ,destructor-name))
		'() )
	  (slot-set! 
	   this 'this
	   (if (and (pair? initargs) (eq? 'this (##sys#slot initargs 0)))
	       (cadr initargs)
	       (##sys#apply
		,(if (or (any identity io) (pair? lvars))
		     (let ([vars (map (lambda _ (gensym)) args)])
		       `(lambda ,(filter-map (lambda (var io i)
					       ;;*** ___inout and ___out doesn't make sense here!
					       (and (memq io '(#f in inout)) 
						    (not (assq i lvars))
						    var))
					     vars io (iota (length vars)))
			  ,(make-inout-wrapper constr `(pointer ,name) vars args io lvars) ) )
		     constr) 
		initargs) ) ) ) ))))

(define (process-member-prototype-def name cname rtype mname args io lvars cb)
  (define (uplvars lvars)
    (map (lambda (x) (cons (add1 (car x)) (add1 (cdr x)))) lvars) )
  (let* ([stub (gensym)]
	 [this (gensym)] 
	 [slist (gen-spec-list args io)]
	 [vars (unzip1 slist)]
	 [fvars (map list args vars)] 
	 [io? (or (any identity io) (pair? lvars))] )
    (emit
     `(begin
	(declare (hide ,stub))
	(define ,stub 
	  (,(if cb 'foreign-safe-lambda* 'foreign-lambda*)
	   ,rtype (((pointer ,name) ,this) ,@fvars)
	   ,(sprintf (let ([code (if (eq? 'void rtype) 
				     "~A->~A(~A);"
				     "return(~A->~A(~A));") ] )
		       (if exception-handler
			   (sprintf "try { ~A } ~A;" code exception-handler)
			   code) )
		     this mname
		     (string-intersperse (map ->string vars) ",")) ) )
	,(if (and full-specialization (pair? args))
	     `(define-method (,(fix-name mname) (this ,cname)
			      ,@(filter-map (lambda (var io i)
					      (and (memq io '(#f in inout)) 
						   (not (assq i lvars))
						   var))
					    vars io (iota (length vars))) )
		,(make-inout-wrapper 
		  stub rtype
		  (cons '(slot-ref this 'this) vars)
		  (cons #f args)	; #f is ok, it will be ignored
		  (cons #f io)
		  (uplvars lvars)) )
	     `(define-method (,(fix-name mname) (this ,cname) . args)
		(##sys#apply 
		 ,(if io?
		      `(lambda ,(filter-map (lambda (var io i) 
					      (and (memq io '(#f in inout)) 
						   (not (assq i lvars))
						   var))
					    vars io (iota (length lvars)))
			 ,(make-inout-wrapper
			   stub rtype 
			   (cons '(slot-ref this 'this) vars)
			   (cons #f args) 
			   (cons #f io)
			   (uplvars lvars)) )
		      stub)
		 ,@(if io? '() '((slot-ref this 'this)))
		 args) ) ) ) ) ) )

(define parse-declaration
  (match-lambda*
    [("export_constants" ('id "yes"))
     (set! export-constants #t) ]
    [("export_constants" _)
     (set! export-constants #f) ]
    [("abstract" ('id cls))
     (set! abstract-classes (cons (string->symbol cls) abstract-classes)) ]
    [("class_finalizers" ('id "yes"))
     (set! use-finalizers #t) ]
    [("class_finalizers" _)
     (set! use-finalizers #f) ]
    [("destructor_name")
     (set! destructor-name 'destroy) ]
    [("destructor_name" ('string name))
     (set! destructor-name (string->symbol name)) ]
    [("exception_handler" ('string code))
     (set! exception-handler code) ]
    [("mutable_fields" ('id "yes"))
     (set! mutable-fields #t) ]
    [("mutable_fields" _)
     (set! mutable-fields #f) ]
    [("default_renaming" ('string str))
     (set! prefix str)
     (set! name-substitution-rxs (append name-substitution-rxs (list ".*[_A-Z].*")))
     (set! name-substitution-repls (append name-substitution-repls (list usual-naming-transform))) ]
    [("prefix" ('string str))
     (set! prefix str) ]
    [("prefix" (or ('id "no") 0))
     (set! prefix #f) ]
    [("scheme" ('string str))
     (let ([exp (with-input-from-string str read)])
       (emit exp) ) ]
    [("type" ('string str))
     (parse-type-declaration (string-split str ";")) ]
    [("opaque" ('string str))
     (parse-type-declaration
      (match (string-split str ";")
	[(name type)
	 (list name type
	       "(lambda (x) (##sys#block-ref x 1))"
	       (sprintf "(lambda (x) (##sys#make-structure '~a x))" name) ) ]
	[_ (parsing-error "invalid `opaque' declaration" str)] ) ) ]
    [("rename" ('string str))
     (match (string-split str ";")
       [(from to) 
	(set! rename-list (alist-cons (string->symbol from) (string->symbol to) rename-list)) ]
       [_ (parsing-error "invalid rename declaration: ~S" str)] ) ]
    [("substitute" ('string str))
     (match (string-split str ";")
       [(from to) 
	(set! name-substitution-rxs (append name-substitution-rxs (list from)))
	(set! name-substitution-repls (append name-substitution-repls (list to))) ]
       [_ (parsing-error "invalid name substitution string: ~S" str)] ) ]
    [("transform" ('string str))
     (match (string-split str ";")
       [(from to)
	(let ([tr (handle-exceptions ex (parsing-error "error in transformation expression: ~S" to)
		    (eval (safe-read-from-string to)) ) ] )
	  (unless (procedure? tr)
	    (parsing-error "transformation expression does not evaluate to procedure: ~S" to) )
	  (set! name-substitution-rxs (append name-substitution-rxs (list from)))
	  (set! name-substitution-repls (append name-substitution-repls (list tr))) ) ]
       [_ (parsing-error "invalid transformation: ~S" str)] ) ]
    [("full_specialization" ('id "yes"))
     (set! full-specialization #t) ]
    [("full_specialization" _)
     (set! full-specialization #f) ]
    [(decl _)
     (parsing-error "invalid pseudo declaration: ~S" decl) ] ) )

(define usual-naming-transform
  (let ()
    (define (downcase-string str)		; so we don't have to use srfi-13
      (let ([s2 (string-copy str)]
	    [n (string-length str)] )
	(do ([i 0 (fx+ i 1)])
	    ((fx>= i n) s2)
	  (string-set! s2 i (char-downcase (string-ref str i))) ) ) )
    (lambda (m)
      (downcase-string
       (string-translate 
	(string-substitute "([a-z])([A-Z])" "\\1-\\2" (car m) #t)
	"_" "-") ) ) ) )

(define (safe-read-from-string str)
  (handle-exceptions ex (parsing-error "can not parse expression: ~S" str)
    (with-input-from-string str read) ) )

(define (parse-type-declaration vals)
  (let rec ([vals vals])
    (match vals
      [(tname stype arg ret)
       (let ([stype (safe-read-from-string stype)]
	     [arg (and arg (safe-read-from-string arg))]
	     [ret (and ret (safe-read-from-string ret))] 
	     [stname (string->symbol tname)] )
	 (set! foreign-declarations 
	   (cons (sprintf "#define ~A ~A~%" tname (foreign-type-declaration stype "")) foreign-declarations))
	 (emit `(define-foreign-type ,stname ,stype ,@(if arg (list arg) '()) ,@(if ret (list ret) '())))
	 (##sys#hash-table-set! foreign-type-table stname stype) ; will be overwritten later
	 (set! declared-types (cons stname declared-types)) ) ]
      [(tname stype arg) (rec (list tname stype arg #f))]
      [(tname stype) (rec (list tname stype #f #f))]
      [_ (parsing-error "invalid value-syntax in type declaration: ~S" vals)] ) ) )

(define (fix-name str #!optional (use-prefix #t))
  (let ([a (assq (->symbol str) rename-list)])
    (if a 
	(cdr a)
	(let ([n1 (fold 
		   (lambda (rx repl str)
		     (if (procedure? repl)
			 (let ([m (string-match rx str)])
			   (if m (repl m) str) )
			 (string-substitute rx repl str #t) ) )
		   (->string str)
		   name-substitution-rxs
		   name-substitution-repls) ] )
	  (string->symbol
	   (strdowncase
	    (if (and use-prefix prefix)
		(string-append prefix n1)
		n1) ) ) ) ) ) )

(define (fix-cname str)
  (let ([a (assq (->symbol str) rename-list)])
    (if a 
	(cdr a)
	(string->symbol (string-append "<" (->string (fix-name str #f)) ">")) ) ) )

(define (->symbol s)
  (if (symbol? s)
      s
      (string->symbol s) ) )

(define (parse-easy-ffi text)
  (lexer-init 'string text)
  (set! processed-output '())
  (set! pp-conditional-stack '())
  (set! pp-process #t)
  (let ([chunks (chunkify)])
    (when (memq 'C debugging-chicken)
      (for-each (lambda (c) (pp `(CHUNK: ,c))) chunks) )
    (for-each parse chunks)
    (reverse processed-output) ) )

(define (parse-easy-ffi-rec port)
  (lexer-init 'port port)
  (let* ([output processed-output]
	 [chunks (chunkify)] )
    (set! processed-output '())
    (for-each parse chunks)
    (set! processed-output (append output processed-output)) ) )

(define (register-ffi-macro name)
  (set! macro-table (cons (list (string->symbol name) '* '()) macro-table)) )

(define (resolve-ffi-include-file fname)
  (find file-exists? (map (cut make-pathname <> fname) ffi-include-path-list)) )

(define (foreign-type->class ftype io)
  (let rec ([ftype ftype])
    (match ftype
      ['char '<char>]
      ['bool '<boolean>]
      ['c-string '<string>]
      [(or 'unsigned-char 'int 'unsigned-int 'short 'unsigned-short 'unsigned-int32 'int32 'integer32)
       '<exact>]
      [(or 'long 'unsigned-long 'integer32 'integer 'unsigned-integer 'unsigned-integer32 'integer64) '<integer>]
      [(or 'float 'double) '<inexact>]
      ['number '<number>]
      [('enum _) '<exact>]
      [('const t) (rec t)]
      [('function . _) '<pointer>]
      [('instance _ c) c]
      [((or 'pointer 'c-pointer 'ref) x)
       (if io
	   (rec x)
	   '<pointer>) ]
      ['u8vector '<u8vector>]
      ['s8vector '<s8vector>]
      ['u16vector '<u16vector>]
      ['s16vector '<s16vector>]
      ['u32vector '<u32vector>]
      ['s32vector '<s32vector>]
      ['f32vector '<f32vector>]
      ['f64vector '<f64vector>]
      [(? symbol?)
       (let ([a (##sys#hash-table-ref foreign-type-table ftype)])
	 (if a
	     (rec (if (vector? a) (vector-ref a 0) a))
	     '<top>) ) ] ) ) )

(define (gen-spec-list args io)
  (map (lambda (t io) (list (gensym) (foreign-type->class t io))) args io) )

(define strdowncase
  (let ([cs case-sensitive])
    (lambda (str)
      (if (cs)
	  str
	  (let ([s2 (string-copy str)]
		[len (string-length str)] )
	    (do ([i (sub1 len) (sub1 i)])
		((negative? i) s2)
	      (string-set! s2 i (char-downcase (string-ref str i))) ) ) ) ) ) )


;;; C syntax checker:

(define syntax-check-location #f)

(define (check-syntax-error text)
  (lambda (fstr . args)
    (quit #<<EOF
suspicious foreign code fragment~A:
------------------------------------------------------------
~A
------------------------------------------------------------
~?
EOF
	  (if syntax-check-location
	      (sprintf " in `~A' form" syntax-check-location)
	      "")
	  text
	  fstr
	  args) ) )

(define (check-c-syntax text . loc)
  (unless no-c-syntax-checks
    (fluid-let ([parsing-error (check-syntax-error text)]
		[syntax-check-location (:optional loc #f)] )
      (define (checkp p s)
	(cond [(null? s) (parsing-error "unbalanced parantheses - missing match to `~A'" p)]
	      [(not (eq? p (car s)))
	       (parsing-error "unbalanced parantheses - expected `~A', but found `~A'" p (car s)) ] ) )
      (define (checkpp p s)
	(cond [(null? s) (parsing-error "unbalanced parantheses - missing match to `~A'" p)]
	      [(not (equal? p (car s)))
	       (parsing-error "unbalanced preprocessor conditional - expected `~A', but found `~A'" p (car s)) ] ) )
      (lexer-init 'string text)
      (set! pp-process #t)
      (let loop ([pstack '()] [ppstack '()])
	(let ([t (lexer)])
	  (case t
	    [(stop)
	     (when (pair? pstack)
	       (parsing-error "unbalanced parentheses - missing `~A'" (car pstack)) )
	     (when (pair? ppstack)
	       (parsing-error "unbalanced preprocessor command - missing `~A'" (car ppstack)) ) ]
	    [(pp-else)
	     (checkpp "#endif" ppstack) 
	     (loop pstack ppstack) ]
	    [(pp-endif)
	     (checkpp "#endif" ppstack) 
	     (loop pstack (cdr ppstack)) ]
	    [(pp-if pp-ifdef pp-ifndef)
	     (loop pstack (cons "#endif" ppstack)) ]
	    [(open-curly)
	     (loop (cons #\} pstack) ppstack) ]
	    [(close-curly)
	     (checkp #\} pstack) 
	     (loop (cdr pstack) ppstack) ]
	    [(open-paren)
	     (loop (cons #\) pstack) ppstack) ]
	    [(close-paren)
	     (checkp #\) pstack)
	     (loop (cdr pstack) ppstack) ] 
	    [(open-bracket)
	     (loop (cons #\] pstack) ppstack) ]
	    [(close-bracket)
	     (checkp #\] pstack)
	     (loop (cdr pstack) ppstack) ] 
	    [else (loop pstack ppstack)] ) ) ) ) ) )
