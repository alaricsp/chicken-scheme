;;; c-backend.scm - C-generating backend for the CHICKEN compiler
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


(declare (unit backend))


(include "compiler-namespace")
(include "tweaks")


;;; Write atoms to output-port:

(define output #f)

(define (gen . data)
  (for-each
   (lambda (x) 
     (if (eq? #t x)
	 (newline output)
	 (display x output) ) )
   data) )

(define (gen-list lst)
  (for-each
   (lambda (x) (display x output))
   (intersperse lst #\space) ) )


;;; Unique id/prefix:

(define unique-id
  (string->c-identifier
   (sprintf "C_~X_~A_" (random #x1000000) (current-seconds)) ) )


;;; Generate target code:

(define (generate-code literals lliterals lambdas out source-file dynamic db)
  (let ()

    ;; Some helper procedures

    (define (find-lambda id)
      (or (find (lambda (ll) (eq? id (lambda-literal-id ll))) lambdas)
	  (bomb "can't find lambda" id) ) )

    (define (slashify s) (string-translate (->string s) "\\" "/"))
    (define (uncommentify s) (string-translate* (->string s) '(("*/" . "* /"))))
  
    ;; Compile a single expression
    (define (expression node temps ll)

      (define (expr n i)
	(let ((subs (node-subexpressions n))
	      (params (node-parameters n)) )
	  (case (node-class n)

	    ((##core#immediate)
	     (case (first params)
	       ((bool) (gen (if (second params) "C_SCHEME_TRUE" "C_SCHEME_FALSE")))
	       ((char) (gen "C_make_character(" (char->integer (second params)) #\)))
	       ((nil) (gen "C_SCHEME_END_OF_LIST"))
	       ((fix) (gen "C_fix(" (second params) #\)))
	       ((eof) (gen "C_SCHEME_END_OF_FILE"))
	       (else (bomb "bad immediate")) ) )

	    ((##core#literal) 
	     (let ((lit (first params)))
	       (if (vector? lit)
		   (gen "((C_word)li" (vector-ref lit 0) ")") 
		   (gen "lf[" (first params) #\])) ) )

	    ((if)
	     (gen #t "if(C_truep(")
	     (expr (car subs) i)
	     (gen ")){")
	     (expr (cadr subs) i)
	     (gen #\} #t "else{")
	     (expr (caddr subs) i)
	     (gen #\}) )

	    ((##core#proc)
	     (gen "(C_word)" (first params)) )

	    ((##core#bind) 
	     (let loop ((bs subs) (i i) (count (first params)))
	       (cond [(> count 0)
		      (gen #t #\t i #\=)
		      (expr (car bs) i)
		      (gen #\;) 
		      (loop (cdr bs) (add1 i) (sub1 count)) ]
		     [else (expr (car bs) i)] ) ) )

	    ((##core#ref) 
	     (gen "((C_word*)")
	     (expr (car subs) i)
	     (gen ")[" (+ (first params) 1) #\]) )

	    ((##core#unbox) 
	     (gen "((C_word*)")
	     (expr (car subs) i)
	     (gen ")[1]") )

	    ((##core#update_i)
	     (gen "C_set_block_item(")
	     (expr (car subs) i)
	     (gen #\, (first params) #\,)
	     (expr (cadr subs) i) 
	     (gen #\)) )

	    ((##core#update)
	     (gen "C_mutate(((C_word *)")
	     (expr (car subs) i)
	     (gen ")+" (+ (first params) 1) ",")
	     (expr (cadr subs) i) 
	     (gen #\)) )

	    ((##core#updatebox_i)
	     (gen "C_set_block_item(")
	     (expr (car subs) i)
	     (gen ",0,")
	     (expr (cadr subs) i) 
	     (gen #\)) )

	    ((##core#updatebox)
	     (gen "C_mutate(((C_word *)")
	     (expr (car subs) i)
	     (gen ")+1,")
	     (expr (cadr subs) i) 
	     (gen #\)) )

	    ((##core#closure)
	     (let ((n (first params)))
	       (gen "(*a=C_CLOSURE_TYPE|" n #\,)
	       (for-each 
		(lambda (x j)
		  (gen "a[" j "]=")
		  (expr x i)
		  (gen #\,) )
		subs (iota n 1 1) )
	       (gen "tmp=(C_word)a,a+=" (add1 n) ",tmp)") ) )

	    ((##core#box) 
	     (gen "(*a=C_VECTOR_TYPE|1,a[1]=")
	     (expr (car subs) i)
	     (gen ",tmp=(C_word)a,a+=2,tmp)") )

	    ((##core#local) (gen #\t (first params)))

	    ((##core#setlocal) 
	     (gen #\t (first params) #\=)
	     (expr (car subs) i) )

	    ((##core#global)
	     (let ((index (first params))
		   (safe (second params)) 
		   (block (third params)) )
	       (cond [block
		      (if safe
			  (gen "lf[" index "]")
			  (gen "C_retrieve2(lf[" index "]," (c-ify-string (symbol->string (fourth params))) #\)) ) ]
		     [safe (gen "*((C_word*)lf[" index "]+1)")]
		     [else (gen "C_retrieve(lf[" index "])")] ) ) )

	    ((##core#setglobal)
	     (let ((index (first params))
		   (block (second params)) 
		   (var (third params)))
	       (if block
		   (gen "C_mutate(&lf[" index "]")
		   (gen "C_mutate((C_word*)lf[" index "]+1") )
	       (gen " /* (set! " (uncommentify (symbol->string var)) " ...) */,")
	       (expr (car subs) i)
	       (gen #\)) ) )

	    ((##core#setglobal_i)
	     (let ((index (first params))
		   (block (second params)) 
		   (var (third params)) )
	       (cond [block
		      (gen "lf[" index "] /* "
			   (uncommentify (symbol->string var)) " */ =")
		      (expr (car subs) i)
		      (gen #\;) ]
		     [else
		      (gen "C_set_block_item(lf[" index "] /* "
			   (uncommentify (symbol->string var)) " */,0,")
		      (expr (car subs) i)
		      (gen #\)) ] ) ) )

	    ((##core#undefined) (gen "C_SCHEME_UNDEFINED"))

	    ((##core#call) 
	     (let* ((args (cdr subs))
		    (n (length args))
		    (nc i)
		    (nf (add1 n)) 
		    (p2 (pair? (cdr params)))
		    (name (and p2 (second params)))
		    (name-str (source-info->string name))
		    (call-id (and p2 (pair? (cddr params)) (third params))) 
		    (customizable (and call-id (fourth params)))
		    (empty-closure (and customizable (zero? (lambda-literal-closure-size (find-lambda call-id)))))
		    (fn (car subs)) )
	       (when name
		 (if emit-trace-info
		     (gen #t "C_trace(\"" (slashify name-str) "\");")
		     (gen #t "/* " (uncommentify name-str) " */") ) )
	       (cond ((eq? '##core#proc (node-class fn))
		      (let ([fpars (node-parameters fn)])
			(gen #t (first fpars) #\( nf ",0,") )
		      (expr-args args i)
		      (gen ");") )
		     (call-id
		      (cond ((and (eq? call-id (lambda-literal-id ll))
				  (lambda-literal-looping ll) )
			     (let* ([temps (lambda-literal-temporaries ll)]
				    [ts (iota n (+ temps nf) 1)] )
			       (for-each
				(lambda (arg tr)
				  (gen #t #\t tr #\=)
				  (expr arg i) 
				  (gen #\;) )
				args ts)
			       (for-each
				(lambda (from to) (gen #t #\t to "=t" from #\;))
				ts (iota n 1 1) )
			       (unless customizable (gen #t "c=" nf #\;))
			       (gen #t "goto loop;") ) )
			    (else
			     (unless empty-closure
			       (gen #t #\t nc #\=)
			       (expr fn i)
			       (gen #\;) )
			     (gen #t call-id #\()
			     (unless customizable (gen nf #\,))
			     (unless empty-closure (gen #\t nc #\,))
			     (expr-args args i)
			     (gen ");") ) ) )
		     ((and (eq? '##core#global (node-class fn))
			   (not unsafe) 
			   (not no-procedure-checks)
			   (not (first params)))
		      (let* ((gparams (node-parameters fn))
			     (index (first gparams))
			     (safe (second gparams)) 
			     (block (third gparams)) 
			     (carg #f))
			(gen #t "((C_proc" nf ")")
			(cond (block
			       (set! carg (string-append "lf[" (number->string index) "]"))
			       (if safe
				   (gen "C_retrieve_proc(" carg ")")
				   (gen "C_retrieve2_symbol_proc(" carg "," 
					(c-ify-string (symbol->string (fourth gparams))) #\)) ) )
			      (safe 
			       (set! carg 
				 (string-append "*((C_word*)lf[" (number->string index) "]+1)"))
			       (gen "C_retrieve_proc(" carg ")"))
			      (else
			       (set! carg 
				 (string-append "*((C_word*)lf[" (number->string index) "]+1)"))
			       (gen "C_retrieve_symbol_proc(lf[" index "])") ))
			(gen ")(" nf #\, carg #\,)
			(expr-args args i)
			(gen ");") ) )
		     (else
		      (gen #t #\t nc #\=)
		      (expr fn i)
		      (gen #\; #t
			   "((C_proc" nf ")")
		      (if (or unsafe no-procedure-checks (first params))
			  (gen "(void*)(*((C_word*)t" nc "+1))")
			  (gen "C_retrieve_proc(t" nc ")") )
		      (gen ")(" nf ",t" nc #\,)
		      (expr-args args i)
		      (gen ");") ) ) ) )
	  
	    ((##core#recurse) 
	     (let* ([n (length subs)]
		    [nf (add1 n)]
		    [tailcall (first params)]
		    [call-id (second params)] 
		    [empty-closure (zero? (lambda-literal-closure-size ll))] )
	       (cond (tailcall
		      (let* ([temps (lambda-literal-temporaries ll)]
			     [ts (iota n (+ temps nf) 1)] )
			(for-each
			 (lambda (arg tr)
			   (gen #t #\t tr #\=)
			   (expr arg i) 
			   (gen #\;) )
			 subs ts)
			(for-each
			 (lambda (from to) (gen #t #\t to "=t" from #\;))
			 ts (iota n 1 1) )
			(gen #t "goto loop;") ) )
		     (else
		      (gen call-id #\()
		      (unless empty-closure (gen "t0,"))
		      (expr-args subs i)
		      (gen #\)) ) ) ) )

	    ((##core#direct_call) 
	     (let* ((args (cdr subs))
		    (n (length args))
		    (nf (add1 n)) 
		    ;;(name (second params))
		    (call-id (third params))
		    (demand (fourth params))
		    (allocating (not (zero? demand)))
		    (empty-closure (zero? (lambda-literal-closure-size (find-lambda call-id))))
		    (fn (car subs)) )
	       (gen call-id #\()
	       (when allocating 
		 (gen "C_a_i(&a," demand #\))
		 (when (or (not empty-closure) (pair? args)) (gen #\,)) )
	       (unless empty-closure
		 (expr fn i)
		 (when (pair? args) (gen #\,)) )
	       (when (pair? args) (expr-args args i))
	       (gen #\)) ) )

	    ((##core#callunit)
	     ;; The code generated here does not use the extra temporary needed for standard calls, so we have
	     ;;  one unused varable:
	     (let* ((n (length subs))
		    (nf (+ n 1)) )
	       (gen #t "C_" (first params) "_toplevel(" nf ",C_SCHEME_UNDEFINED,")
	       (expr-args subs i)
	       (gen ");") ) )

	    ((##core#return)
	     (gen #t "return(")
	     (expr (first subs) i)
	     (gen ");") )

	    ((##core#inline)
	     (gen "(C_word)" (first params) #\()
	     (expr-args subs i)
	     (gen #\)) )

	    ((##core#inline_allocate)
	     (gen "(C_word)" (first params) "(&a," (length subs))
	     (if (pair? subs)
		 (begin
		   (gen #\,)
		   (expr-args subs i) ) )
	     (gen #\)) )

	    ((##core#inline_ref)
	     (gen (foreign-result-conversion (second params) "a") (first params) #\)) )

	    ((##core#inline_update)
	     (let ([t (second params)])
	       (gen #\( (first params) "=(" (foreign-type-declaration t "") #\) (foreign-argument-conversion t)) 
	       (expr (first subs) i)
	       (gen "),C_SCHEME_UNDEFINED)") ) )

	    ((##core#inline_loc_ref)
	     (let ([t (first params)])
	       (gen (foreign-result-conversion t "a") "*((" (foreign-type-declaration t "") "*)C_data_pointer(")
	       (expr (first subs) i)
	       (gen ")))") ) )

	    ((##core#inline_loc_update)
	     (let ([t (first params)])
	       (gen "((*(" (foreign-type-declaration t "") "*)C_data_pointer(")
	       (expr (first subs) i)
	       (gen "))=" (foreign-argument-conversion t))
	       (expr (second subs) i) 
	       (gen "),C_SCHEME_UNDEFINED)") ) )

	    ((##core#switch)
	     (gen #t "switch(")
	     (expr (first subs) i)
	     (gen "){")
	     (do ([j (first params) (sub1 j)]
		  [ps (cdr subs) (cddr ps)] )
		 ((zero? j)
		  (gen #t "default:")
		  (expr (car ps) i)
		  (gen #\}) )
	       (gen #t "case ")
	       (expr (car ps) i)
	       (gen #\:)
	       (expr (cadr ps) i) ) )

	    ((##core#cond)
	     (gen "(C_truep(")
	     (expr (first subs) i)
	     (gen ")?")
	     (expr (second subs) i)
	     (gen #\:)
	     (expr (third subs) i)
	     (gen #\)) )

	    (else (bomb "bad form")) ) ) )
    
      (define (expr-args args i)
	(pair-for-each
	 (lambda (xs)
	   (if (not (eq? xs args)) (gen #\,))
	   (expr (car xs) i) )
	 args) )

      (expr node temps) )
  
    (define (header)
      (define (pad0 n)
	(if (< n 10)
	    (string-append "0" (number->string n))
	    n) )
      (let* ((tm (##sys#decode-seconds (current-seconds) #f))
	     (min (vector-ref tm 1))
	     (hour (vector-ref tm 2))
	     (mday (vector-ref tm 3))
	     (mon (vector-ref tm 4))
	     (year (vector-ref tm 5)) )
	(gen "/* Generated from " source-file " by the CHICKEN compiler" #t
	     "   http://www.call-with-current-continuation.org" #t
	     "   " (+ 1900 year) #\- (pad0 (add1 mon)) #\- (pad0 mday) #\space (pad0 hour) #\: (pad0 min) #t
	     (string-intersperse
	      (map (cut string-append "   " <> "\n") 
		   (string-split (chicken-version #t) "\n") ) 
	      "")
	     "   command line: ")
	(gen-list compiler-arguments)
	(gen #t)
	(cond [unit-name (gen "   unit: " unit-name)]
	      [else 
	       (gen "   used units: ")
	       (gen-list used-units) ] )
	(gen #t "*/" #t #t "#include \"" target-include-file "\"")
	(when external-protos-first
	  (generate-foreign-callback-stub-prototypes foreign-callback-stubs) )
	(when (pair? foreign-declarations)
	  (gen #t)
	  (for-each (lambda (decl) (gen #t decl)) foreign-declarations) )
	(unless external-protos-first
	  (generate-foreign-callback-stub-prototypes foreign-callback-stubs) ) ) )
  
    (define (trailer)
      (gen #t "/* end of file */" #t) )
  
    (define (declarations)
      (let ((n (length literals)))
	(gen #t #t "static C_PTABLE_ENTRY *create_ptable(void);")
	(for-each 
	 (lambda (uu) 
	   (gen #t "C_noret_decl(C_" uu "_toplevel)"
		#t "C_externimport void C_ccall C_" uu "_toplevel(C_word c,C_word d,C_word k) C_noret;"))
	 used-units)
	(unless (zero? n)
	  (gen #t #t "static C_TLS C_word lf[" n "];") )
	(gen #t "static double C_possibly_force_alignment;")
	(do ((i 0 (add1 i))
	     (llits lliterals (cdr llits)))
	    ((null? llits))
	  (let* ((ll (##sys#lambda-info->string (car llits)))
		 (llen (string-length ll)))
	    (gen #t "static C_char C_TLS li" i "[] C_aligned={C_lihdr(" 
		 (arithmetic-shift llen -16) #\,
		 (bitwise-and #xff (arithmetic-shift llen -8)) #\,
		 (bitwise-and #xff llen)
		 #\))
	    (do ((n 0 (add1 n)))
		((>= n llen))
	      (gen #\, (char->integer (string-ref ll n))) )
	    (do ((n (- (bitwise-and #xfffff8 (+ llen 7)) llen) (sub1 n))) ; fill up with zeros to align following entry
		((zero? n))
	      (gen ",0") )
	    (gen "};")))))
  
    (define (prototypes)
      (let ([large-signatures '()])
	(gen #t)
	(for-each
	 (lambda (ll)
	   (let* ([n (lambda-literal-argument-count ll)]
		  [customizable (lambda-literal-customizable ll)] 
		  [empty-closure (and customizable (zero? (lambda-literal-closure-size ll)))]
		  [varlist (intersperse (make-variable-list (if empty-closure (sub1 n) n) "t") #\,)]
		  [id (lambda-literal-id ll)]
		  [rest (lambda-literal-rest-argument ll)]
		  [rest-mode (lambda-literal-rest-argument-mode ll)]
		  [direct (lambda-literal-direct ll)] 
		  [allocated (lambda-literal-allocated ll)] )
	     (when (>= n small-parameter-limit)
	       (set! large-signatures (lset-adjoin = large-signatures (add1 n))) )
	     (gen #t)
	     (for-each
	      (lambda (s) 
		(when (>= s small-parameter-limit)
		  (set! large-signatures (lset-adjoin = large-signatures (add1 s))) ) )
	      (lambda-literal-callee-signatures ll) )
	     (cond [(not (eq? 'toplevel id))
		    (gen "C_noret_decl(" id ")" #t)
		    (gen "static ")
		    (gen (if direct "C_word " "void "))
		    (if customizable
			(gen "C_fcall ")
			(gen "C_ccall ") )
		    (gen id) ]
		   [else
		    (let ((uname (if unit-name (string-append unit-name "_toplevel") "toplevel")))
		      (gen "C_noret_decl(C_" uname ")" #t)
		      (when emit-unsafe-marker
			(gen "C_externexport void C_dynamic_and_unsafe(void) {}" #t) )
		      (gen "C_externexport void C_ccall ")
		      (gen "C_" uname) ) ] )
	     (gen #\()
	     (unless customizable (gen "C_word c,"))
	     (when (and direct (not (zero? allocated)))
	       (gen "C_word *a")
	       (when (pair? varlist) (gen #\,)) )
	     (apply gen varlist)
	     (cond [rest
		    (gen ",...) C_noret;")
		    (if (not (eq? rest-mode 'none))
			(begin
			  (gen #t "C_noret_decl(" id ")" 
			       #t "static void C_ccall " id "r(")
			  (apply gen varlist)
			  (gen ",C_word t" (+ n 1) ") C_noret;") ) ) ]
		   [else 
		    (gen #\))
		    ;;(when customizable (gen " C_c_regparm"))
		    (unless direct (gen " C_noret"))
		    (gen #\;) ] ) ) )
	 lambdas) 
	(for-each
	 (lambda (s)
	   (gen #t "typedef void (*C_proc" s ")(C_word")
	   (for-each gen (make-list s ",C_word"))
	   (gen ") C_noret;") )
	 large-signatures) ) )
  
    (define (trampolines)
      (let ([ns '()]
	    [nsr '()] 
	    [nsrv '()] )

	(define (restore n)
	  (do ((i (- n 1) (- i 1))
	       (j 0 (+ j 1)) )
	      ((negative? i))
	    (gen #t "C_word t" i "=C_pick(" j ");") ) 
	  (gen #t "C_adjust_stack(-" n ");") )

	(define (emitter vflag)
	  (lambda (n)
	    (gen #t #t "C_noret_decl(tr" n #\r (if vflag #\v "") ")"
		 #t "static void C_fcall tr" n #\r (if vflag #\v ""))
	    (gen "(C_proc" n " k) C_regparm C_noret;")
	    (gen #t "C_regparm static void C_fcall tr" n #\r)
	    (when vflag (gen #\v))
	    (gen "(C_proc" n " k){"
		 #t "int n;"
		 #t "C_word *a,t" n #\;)
	    (restore n)
	    (gen #t "n=C_rest_count(0);")
	    (if vflag
		(gen #t "a=C_alloc(n+1);")
		(gen #t "a=C_alloc(n*3);") )
	    (gen #t #\t n "=C_restore_rest")
	    (when vflag (gen "_vector"))
	    (gen "(a,n);")
	    (gen #t "(k)(")
	    (apply gen (intersperse (make-argument-list (+ n 1) "t") #\,))
	    (gen ");}") ) )

	(for-each
	 (lambda (ll)
	   (let* ([argc (lambda-literal-argument-count ll)]
		  [rest (lambda-literal-rest-argument ll)]
		  [rest-mode (lambda-literal-rest-argument-mode ll)]
		  [id (lambda-literal-id ll)]
		  [customizable (lambda-literal-customizable ll)]
		  [empty-closure (and customizable (zero? (lambda-literal-closure-size ll)))] )
	     (when empty-closure (set! argc (sub1 argc)))
	     (unless (lambda-literal-direct ll)
	       (cond [customizable
		      (gen #t #t "C_noret_decl(tr" id ")"
			   #t "static void C_fcall tr" id "(void *dummy) C_regparm C_noret;")
		      (gen #t "C_regparm static void C_fcall tr" id "(void *dummy){")
		      (restore argc)
		      (gen #t id #\()
		      (let ([al (make-argument-list argc "t")])
			(apply gen (intersperse al #\,)) )
		      (gen ");}") ]
		     [(or rest (> (lambda-literal-allocated ll) 0) (lambda-literal-external ll))
		      (if (and rest (not (eq? rest-mode 'none)))
			  (if (eq? rest-mode 'vector)
			      (set! nsrv (lset-adjoin = nsrv argc))
			      (set! nsr (lset-adjoin = nsr argc)) ) 
			  (set! ns (lset-adjoin = ns argc)) ) ] ) ) ) )
	 lambdas)
	(for-each
	 (lambda (n)
	   (gen #t #t "C_noret_decl(tr" n ")"
		#t "static void C_fcall tr" n "(C_proc" n " k) C_regparm C_noret;")
	   (gen #t "C_regparm static void C_fcall tr" n "(C_proc" n " k){")
	   (restore n)
	   (gen #t "(k)(" n #\,)
	   (apply gen (intersperse (make-argument-list n "t") #\,))
	   (gen ");}") )
	 ns)
	(for-each (emitter #f) nsr)
	(for-each (emitter #t) nsrv) ) )
  
    (define (literal-frame)
      (do ([i 0 (add1 i)]
	   [lits literals (cdr lits)] )
	  ((null? lits))
	(gen-lit (car lits) (sprintf "lf[~s]" i)) ) )

    (define (bad-literal lit)
      (bomb "type of literal not supported" lit) )

    (define (literal-size lit)
      (cond [(immediate? lit) 0]
	    [(string? lit) 0]
	    [(number? lit) words-per-flonum]
	    [(symbol? lit) 10]		; size of symbol, and possibly a bucket
	    [(pair? lit) (+ 3 (literal-size (car lit)) (literal-size (cdr lit)))]
	    [(vector? lit) (+ 1 (vector-length lit) (reduce + 0 (map literal-size (vector->list lit))))]
	    [(block-variable-literal? lit) 0]
	    [(##sys#immediate? lit) (bad-literal lit)]
	    [(##core#inline "C_lambdainfop" lit) 0]
	    [(##sys#bytevector? lit) (+ 2 (words (##sys#size lit))) ] ; drops "permanent" property!
	    [(##sys#generic-structure? lit)
	     (let ([n (##sys#size lit)])
	       (let loop ([i 0] [s (+ 2 n)])
		 (if (>= i n)
		     s
		     (loop (add1 i) (+ s (literal-size (##sys#slot lit i)))) ) ) ) ]
	    [else (bad-literal lit)] ) )

    (define (gen-lit lit to)
      ;; we do simple immediate literals directly to avoid a function call:
      (cond ((and (fixnum? lit) (not (big-fixnum? lit)))
	     (gen #t to "=C_fix(" lit ");") )
	    ((block-variable-literal? lit))
	    ((eq? lit (void))
	     (gen #t to "=C_SCHEME_UNDEFINED;") )
	    ((boolean? lit) 
	     (gen #t to #\= (if lit "C_SCHEME_TRUE" "C_SCHEME_FALSE") #\;) )
	    ((char? lit)
	     (gen #t to "=C_make_character(" (char->integer lit) ");") )
	    ((symbol? lit)		; handled slightly specially (see C_h_intern_in)
	     (let* ([str (##sys#slot lit 1)]
		    [cstr (c-ify-string str)]
		    [len (##sys#size str)] )
	       (gen #t to "=")
	       (gen "C_h_intern(&" to #\, len #\, cstr ");") ) )
	    ((null? lit) 
	     (gen #t to "=C_SCHEME_END_OF_LIST;") )
	    ((and (not (##sys#immediate? lit))
		  (##core#inline "C_lambdainfop" lit)))
	    ((or (fixnum? lit) (not (##sys#immediate? lit)))
	     (gen #t to "=C_decode_literal(C_heaptop,")
	     (gen-string-constant (encode-literal lit))
	     (gen ");") )
	    (else (bad-literal lit))))

    (define (gen-string-constant str)
      (let* ([len (##sys#size str)]
	     [ns (fx/ len 80)]
	     [srest (modulo len 80)] )
	(do ([i ns (sub1 i)]
	     [offset 0 (+ offset 80)] )
	    ((zero? i)
	     (when (or (zero? len) (not (zero? srest)))
	       (gen (c-ify-string (string-like-substring str offset len))) ) )
	  (gen (c-ify-string (string-like-substring str offset (+ offset 80))) #t) ) ) )
 
    (define (string-like-substring s start end)
      (let* ([len (- end start)]
	     [s2 (make-string len)] )
	(##sys#copy-bytes s s2 start 0 len)
	s2) )

    (define (procedures)
      (for-each
       (lambda (ll)
	 (let* ([n (lambda-literal-argument-count ll)]
		[id (lambda-literal-id ll)]
		[rname (real-name id db)]
		[demand (lambda-literal-allocated ll)]
		[rest (lambda-literal-rest-argument ll)]
		[customizable (lambda-literal-customizable ll)]
		[empty-closure (and customizable (zero? (lambda-literal-closure-size ll)))]
		[nec (- n (if empty-closure 1 0))]
		[vlist0 (make-variable-list n "t")]
		[alist0 (make-argument-list n "t")]
		[varlist (intersperse (if empty-closure (cdr vlist0) vlist0) #\,)]
		[arglist (intersperse (if empty-closure (cdr alist0) alist0) #\,)]
		[external (lambda-literal-external ll)]
		[looping (lambda-literal-looping ll)]
		[direct (lambda-literal-direct ll)]
		[rest-mode (lambda-literal-rest-argument-mode ll)]
		[temps (lambda-literal-temporaries ll)]
		[topname (if unit-name
			     (string-append unit-name "_toplevel")
			     "toplevel") ] )
	   (when empty-closure (debugging 'o "dropping unused closure argument" id))
	   (gen #t #t)
	   (gen "/* " (cleanup rname) " */" #t)
	   (cond [(not (eq? 'toplevel id)) 
		  (gen "static ")
		  (gen (if direct "C_word " "void "))
		  (if customizable
		      (gen "C_fcall ")
		      (gen "C_ccall ") )
		  (gen id) ]
		 [else
		  (gen "static C_TLS int toplevel_initialized=0;")
		  (unless unit-name
		    (gen #t "C_main_entry_point") )
		  (gen #t "C_noret_decl(toplevel_trampoline)"
		       #t "static void C_fcall toplevel_trampoline(void *dummy) C_regparm C_noret;"
		       #t "C_regparm static void C_fcall toplevel_trampoline(void *dummy){"
		       #t "C_" topname "(2,C_SCHEME_UNDEFINED,C_restore);}"
		       #t #t "void C_ccall C_" topname) ] )
	   (gen #\()
	   (unless customizable (gen "C_word c,"))
	   (when (and direct (not (zero? demand))) 
	     (gen "C_word *a")
	     (when (pair? varlist) (gen #\,)) )
	   (apply gen varlist)
	   (when rest (gen ",..."))
	   (gen "){")
	   (when (eq? rest-mode 'none) (set! rest #f))
	   (gen #t "C_word tmp;")
	   (if rest
	       (gen #t "C_word t" n #\;) ; To hold rest-list if demand is met
	       (do ([i n (add1 i)]
		    [j (+ temps (if looping (sub1 n) 0)) (sub1 j)] )
		   ((zero? j))
		 (gen #t "C_word t" i #\;) ) )
	   (cond [(eq? 'toplevel id) 
		  (let ([ldemand (fold (lambda (lit n) (+ n (literal-size lit))) 0 literals)]
			[llen (length literals)] )
		    (gen #t "C_word *a;"
			 #t "if(toplevel_initialized) C_kontinue(t1,C_SCHEME_UNDEFINED);"
			 #t "else C_toplevel_entry(C_text(\"" topname "\"));")
		    (when disable-stack-overflow-checking
		      (gen #t "C_disable_overflow_check=1;") )
		    (unless unit-name
		      (cond [target-initial-heap-size
			     (gen #t "C_set_or_change_heap_size(" target-initial-heap-size ",1);") ]
			    [target-heap-size
			     (gen #t "C_set_or_change_heap_size(" target-heap-size ",1);"
				  #t "C_heap_size_is_fixed=1;") ] )
		      (when target-heap-growth
			(gen #t "C_heap_growth=" target-heap-growth #\;) )
		      (when target-heap-shrinkage
			(gen #t "C_heap_shrinkage=" target-heap-shrinkage #\;) )
		      (when target-stack-size
			(gen #t "C_resize_stack(" target-stack-size ");") ) )
		    (gen #t "C_check_nursery_minimum(" demand ");"
			 #t "if(!C_demand(" demand ")){"
			 #t "C_save(t1);"
			 #t "C_reclaim((void*)toplevel_trampoline,NULL);}"
			 #t "toplevel_initialized=1;")
		    (gen #t "if(!C_demand_2(" ldemand ")){"
			 #t "C_save(t1);"
			 #t "C_rereclaim2(" ldemand "*sizeof(C_word), 1);"
			 #t "t1=C_restore;}")
		    (gen #t "a=C_alloc(" demand ");")
		    (when (not (zero? llen))
		      (gen #t "C_initialize_lf(lf," llen ");")
		      (literal-frame)
		      (gen #t "C_register_lf2(lf," llen ",create_ptable());") ) ) ]
		 [rest
		  (gen #t "va_list v;")
		  (gen #t "C_word *a,c2=c;")
		  (gen #t "C_save_rest(")
		  (if (> n 0)
		      (gen #\t (- n 1))
		      (gen "c") )
		  (gen ",c2," n ");")
		  (when (and (not unsafe) (not no-argc-checks) (> n 2) (not empty-closure))
		    (gen #t "if(c<" n ") C_bad_min_argc_2(c," n ",t0);") )
		  (when insert-timer-checks (gen #t "C_check_for_interrupt;"))
		  (gen #t "if(!C_demand(c*C_SIZEOF_PAIR+" demand ")){") ]
		 [else
		  (cond [(and (not direct) (> demand 0))
			 (if looping
			     (gen #t "C_word *a;"
				  #t "loop:"
				  #t "a=C_alloc(" demand ");")
			     (gen #t "C_word ab[" demand "],*a=ab;") ) ]
			[else
			 (unless direct (gen #t "C_word *a;"))
			 (when looping (gen #t "loop:")) 
			 (when (and direct (not unsafe) (not disable-stack-overflow-checking))
			   (gen #t "C_stack_check;") ) ] )
		  (when (and external (not unsafe) (not no-argc-checks) (not customizable))
		    ;; (not customizable) implies empty-closure
		    (if (eq? rest-mode 'none)
			(when (> n 2) (gen #t "if(c<" n ") C_bad_min_argc_2(c," n ",t0);"))
			(gen #t "if(c!=" n ") C_bad_argc_2(c," n ",t0);") ) )
		  (when (and (not direct) (or external (> demand 0)))
		    (when insert-timer-checks (gen #t "C_check_for_interrupt;"))
		    (if (and looping (> demand 0))
			(gen #t "if(!C_stack_probe(a)){")
			(gen #t "if(!C_stack_probe(&a)){") ) ) ] )
	   (when (and (not (eq? 'toplevel id))
		      (not direct)
		      (or rest external (> demand 0)) )
;; 	     (cond [(> nec 1)
;; 		    (gen #t "C_adjust_stack(" nec ");")
;; 		    (do ([i (if empty-closure 1 0) (+ i 1)])
;; 			((>= i n))
;; 		      (gen #t "C_rescue(t" i #\, (- n i 1) ");") ) ]
;; 		   [(= nec 1) (gen #t "C_save(" (if empty-closure "t1" "t0") ");")] )
	     (cond [rest
		    (gen #t (if (> nec 0) "C_save_and_reclaim" "C_reclaim") "((void*)tr" n #\r)
		    (when (eq? rest-mode 'vector) (gen #\v))
		    (gen ",(void*)" id "r")
		    (when (> nec 0)
		      (gen #\, nec #\,)
		      (apply gen arglist) )
		    (gen ");}"
			 #t "else{"
			 #t "a=C_alloc((c-" n ")*3);")
		    (case rest-mode
		      [(list #f) (gen #t "t" n "=C_restore_rest(a,C_rest_count(0));")]
		      [(vector) (gen #t "t" n "=C_restore_rest_vector(a,C_rest_count(0));")] )
		    (gen #t id "r(")
		    (apply gen (intersperse (make-argument-list n "t") #\,))
		    (gen ",t" n ");}}")
		    ;; Create secondary routine (no demand-check or argument-count-parameter):
		    (gen #t #t "static void C_ccall " id "r(")
		    (apply gen varlist)
		    (gen ",C_word t" n "){")
		    (gen #t "C_word tmp;")
		    (do ([i (+ n 1) (+ i 1)]
			 [j temps (- j 1)] )
			((zero? j))
		      (gen #t "C_word t" i #\;) )
		    (when (> demand 0) (gen #t "C_word *a=C_alloc(" demand ");")) ]
		   [else 
		    (gen #t (if (> nec 0) "C_save_and_reclaim" "C_reclaim") "((void*)tr")
		    (if customizable 
			(gen id ",NULL")
			(gen n ",(void*)" id) )
		    (when (> nec 0)
		      (gen #\, nec #\,)
		      (apply gen arglist) )
		    (gen ");}") ] ) )
	   (expression
	    (lambda-literal-body ll)
	    (if rest
		(add1 n) ; One temporary is needed to hold the rest-list
		n)
	    ll)
	   (gen #\}) ) )
       lambdas) )

    (debugging 'p "code generation phase...")
    (set! output out)
    (header)
    (declarations)
    (generate-external-variables external-variables)
    (generate-foreign-stubs foreign-lambda-stubs db)
    (prototypes)
    (generate-foreign-callback-stubs foreign-callback-stubs db)
    (trampolines)
    (procedures)
    (emit-procedure-table-info lambdas source-file)
    (trailer) ) )


;;; Emit procedure table:

(define (emit-procedure-table-info lambdas sf)
  (gen #t #t "#ifdef C_ENABLE_PTABLES"
       #t "static C_PTABLE_ENTRY ptable[" (add1 (length lambdas)) "] = {")
  (do ((ll lambdas (cdr ll)))
      ((null? ll)
       (gen #t "{NULL,NULL}};") )
    (let ((id (lambda-literal-id (car ll))))
      (gen #t "{\"" id #\: (string->c-identifier sf) "\",(void*)")
      (if (eq? 'toplevel id)
	  (if unit-name
	      (gen "C_" unit-name "_toplevel},")
	      (gen "C_toplevel},") )
	  (gen id "},") ) ) )
  (gen #t "#endif")
  (gen #t #t "static C_PTABLE_ENTRY *create_ptable(void)")
  (gen "{" #t "#ifdef C_ENABLE_PTABLES"
       #t "return ptable;"
       #t "#else"
       #t "return NULL;"
       #t "#endif"
       #t "}") )


;;; Create name that is safe for C comments:

(define (cleanup s)
  (let ([s2 #f] 
	[len (string-length s)] )
    (let loop ([i 0])
      (if (>= i len)
	  (or s2 s)
	  (let ([c (string-ref s i)])
	    (if (or (char<? c #\space)
		    (char>? c #\~)
		    (and (char=? c #\*) (< i (sub1 len)) (char=? #\/ (string-ref s (add1 i)))) )
		(begin
		  (unless s2 (set! s2 (string-copy s)))
		  (string-set! s2 i #\~) )
		(when s2 (string-set! s2 i c)) ) 
	    (loop (add1 i)) ) ) ) ) )


;;; Create list of variables/parameters, interspersed with a special token:

(define (make-variable-list n prefix)
  (list-tabulate
   n
   (lambda (i) (string-append "C_word " prefix (number->string i))) ) )
  
(define (make-argument-list n prefix)
  (list-tabulate
   n
   (lambda (i) (string-append prefix (number->string i))) ) )


;;; Generate external variable declarations:

(define (generate-external-variables vars)
  (gen #t)
  (for-each
   (lambda (v)
     (let ((name (vector-ref v 0))
	   (type (vector-ref v 1))
	   (exported (vector-ref v 2)) )
       (gen #t (if exported "" "static ") (foreign-type-declaration type name) #\;) ) )
   vars) )


;;; Generate foreign stubs:

(define (generate-foreign-callback-stub-prototypes stubs)
  (for-each
   (lambda (stub)
     (gen #t)
     (generate-foreign-callback-header "C_externexport " stub)
     (gen #\;) )
   stubs) )

(define (generate-foreign-stubs stubs db)
  (for-each
   (lambda (stub)
     (let* ([id (foreign-stub-id stub)]
	    [rname (real-name2 id db)]
	    [types (foreign-stub-argument-types stub)]
	    [n (length types)]
	    [varlist (intersperse (cons "C_word C_buf" (make-variable-list n "C_a")) #\,)]
	    [rtype (foreign-stub-return-type stub)] 
	    [sname (foreign-stub-name stub)] 
	    [body (foreign-stub-body stub)]
	    [names (or (foreign-stub-argument-names stub) (make-list n #f))]
	    [rconv (foreign-result-conversion rtype "C_a")] 
	    [cps (foreign-stub-cps stub)]
	    [callback (foreign-stub-callback stub)] )
       (gen #t)
       (when rname
	 (gen #t "/* from " (cleanup rname) " */") )
       (when body
	 (gen #t "#define return(x) C_cblock C_r = (" rconv 
	      "(x))); goto C_ret; C_cblockend"))
       (if cps
	   (gen #t "C_noret_decl(" id ")"
		#t "static void C_ccall " id "(C_word C_c,C_word C_self,C_word C_k,")
	   (gen #t "static C_word C_fcall " id #\() )
       (apply gen varlist)
       (if cps
	   (gen ") C_noret;" #t "static void C_ccall " id "(C_word C_c,C_word C_self,C_word C_k,")
	   (gen ") C_regparm;" #t "C_regparm static C_word C_fcall " id #\() )
       (apply gen varlist)
       (gen "){")
       (gen #t "C_word C_r=C_SCHEME_UNDEFINED,*C_a=(C_word*)C_buf;")
       (for-each
	(lambda (type index name)
	  (gen #t 
	       (foreign-type-declaration 
		type
		(if name (symbol->string name) (sprintf "t~a" index)) )
	       "=(" (foreign-type-declaration type "") #\)
	       (foreign-argument-conversion type) "C_a" index ");") )
	types (iota n) names)
       (when callback (gen #t "int C_level=C_save_callback_continuation(&C_a,C_k);"))
       (cond [body
	      (gen #t body
		   #t "C_ret:")
	      (gen #t "#undef return" #t)
	      (cond [callback
		     (gen #t "C_k=C_restore_callback_continuation2(C_level);"
			  #t "C_kontinue(C_k,C_r);") ]
		    [cps (gen #t "C_kontinue(C_k,C_r);")]
		    [else (gen #t "return C_r;")] ) ]
	     [else
	      (if (not (eq? rtype 'void))
		  (gen #t "C_r=" rconv)
		  (gen #t) )
	      (gen sname #\()
	      (apply gen (intersperse (make-argument-list n "t") #\,))
	      (unless (eq? rtype 'void) (gen #\)))
	      (gen ");")
	      (cond [callback
		     (gen #t "C_k=C_restore_callback_continuation2(C_level);"
			  #t "C_kontinue(C_k,C_r);") ]
		    [cps (gen "C_kontinue(C_k,C_r);")]
		    [else (gen #t "return C_r;")] ) ] )
       (gen #\}) ) )
   stubs) )

(define (generate-foreign-callback-stubs stubs db)
  (for-each
   (lambda (stub)
     (let* ([id (foreign-callback-stub-id stub)]
	    [rname (real-name2 id db)]
	    [rtype (foreign-callback-stub-return-type stub)]
	    [argtypes (foreign-callback-stub-argument-types stub)]
	    [n (length argtypes)]
	    [vlist (make-argument-list n "t")] )

       (define (compute-size type var ns)
	 (case type
	   [(char int int32 short bool void unsigned-short scheme-object unsigned-char unsigned-int unsigned-int32
		  byte unsigned-byte)
	    ns]
	   [(float double c-pointer unsigned-integer unsigned-integer32 long integer integer32 unsigned-long 
		   nonnull-c-pointer number integer64 c-string-list c-string-list*)
	    (string-append ns "+3") ]
	   [(c-string c-string* unsigned-c-string unsigned-c-string unsigned-c-string*)
	    (string-append ns "+2+(" var "==NULL?1:C_bytestowords(C_strlen(" var ")))") ]
	   [(nonnull-c-string nonnull-c-string* nonnull-unsigned-c-string nonnull-unsigned-c-string* symbol)
	    (string-append ns "+2+C_bytestowords(C_strlen(" var "))") ]
	   [else
	    (cond [(and (symbol? type) (##sys#hash-table-ref foreign-type-table type)) 
		   => (lambda (t)
			(compute-size (if (vector? t) (vector-ref t 0) t) var ns) ) ]
		  [(pair? type)
		   (case (car type)
		     [(ref pointer c-pointer nonnull-pointer nonnull-c-pointer function instance 
			   nonnull-instance instance-ref)
		      (string-append ns "+3") ]
		     [(const) (compute-size (cadr type) var ns)]
		     [else ns] ) ]
		  [else ns] ) ] ) )

       (let ([sizestr (fold compute-size "0" argtypes vlist)])
	 (gen #t)
	 (when rname
	   (gen #t "/* from " (cleanup rname) " */") )
	 (generate-foreign-callback-header "" stub)
	 (gen #\{ #t "C_word x,s=" sizestr ",*a=C_alloc(s);")
	 (gen #t "C_callback_adjust_stack(a,s);") ; make sure content is below stack_bottom as well
	 (for-each
	  (lambda (v t)
	    (gen #t "x=" (foreign-result-conversion t "a") v ");"
		 #t "C_save(x);") )
	  vlist 
	  argtypes)
	 (unless (eq? 'void rtype)
	   (gen #t "return " (foreign-argument-conversion rtype)) )
	 (gen "C_callback_wrapper((void *)" id #\, n #\))
	 (unless (eq? 'void rtype) (gen #\)))
	 (gen ";}") ) ) )
   stubs) )

(define (generate-foreign-callback-header cls stub)
  (let* ([name (foreign-callback-stub-name stub)]
	 [quals (foreign-callback-stub-qualifiers stub)]
	 [rtype (foreign-callback-stub-return-type stub)]
	 [argtypes (foreign-callback-stub-argument-types stub)]
	 [n (length argtypes)]
	 [vlist (make-argument-list n "t")] )
    (gen #t cls #\space (foreign-type-declaration rtype "") quals #\space name #\()
    (pair-for-each
     (lambda (vs ts)
       (gen (foreign-type-declaration (car ts) (car vs)))
       (when (pair? (cdr vs)) (gen #\,)) )
     vlist argtypes)
    (gen #\)) ) )


;; Create type declarations

(define (foreign-type-declaration type target)
  (let ([err (lambda () (quit "illegal foreign type `~A'" type))]
	[str (lambda (ts) (string-append ts " " target))] )
    (case type
      [(scheme-object) (str "C_word")]
      [(char byte) (str "C_char")]
      [(unsigned-char unsigned-byte) (str "unsigned C_char")]
      [(unsigned-int unsigned-integer) (str "unsigned int")]
      [(unsigned-int32 unsigned-integer32) (str "C_u32")]
      [(int integer bool) (str "int")]
      [(int32 integer32) (str "C_s32")]
      [(integer64) (str "C_s64")]
      [(short) (str "short")]
      [(long) (str "long")]
      [(unsigned-short) (str "unsigned short")]
      [(unsigned-long) (str "unsigned long")]
      [(float) (str "float")]
      [(double number) (str "double")]
      ;; pointer and nonnull-pointer are DEPRECATED
      [(pointer nonnull-pointer) (str "void *")]
      [(c-pointer nonnull-c-pointer scheme-pointer nonnull-scheme-pointer) (str "void *")]
      [(c-string-list c-string-list*) "C_char **"]
      ;; byte-vector and nonnull-byte-vector are DEPRECATED
      [(byte-vector nonnull-byte-vector) (str "unsigned char *")]
      [(blob nonnull-blob u8vector nonnull-u8vector) (str "unsigned char *")]
      [(u16vector nonnull-u16vector) (str "unsigned short *")]
      [(s8vector nonnull-s8vector) (str "char *")]
      [(u32vector nonnull-u32vector) (str "unsigned int *")]
      [(s16vector nonnull-s16vector) (str "short *")]
      [(s32vector nonnull-s32vector) (str "int *")]
      [(f32vector nonnull-f32vector) (str "float *")]
      [(f64vector nonnull-f64vector) (str "double *")]
      [(nonnull-c-string c-string nonnull-c-string* c-string* symbol) 
       (str "char *")]
      [(nonnull-unsigned-c-string nonnull-unsigned-c-string* unsigned-c-string unsigned-c-string*)
       (str "unsigned char *")]
      [(void) (str "void")]
      [else
       (cond [(and (symbol? type) (##sys#hash-table-ref foreign-type-table type))
	      => (lambda (t)
		   (foreign-type-declaration (if (vector? t) (vector-ref t 0) t) target)) ]
	     [(string? type) (str type)]
	     [(list? type)
	      (let ((len (length type)))
		(cond 
		 ((and (= 2 len)
		       (memq (car type) '(pointer nonnull-pointer c-pointer 
						  nonnull-c-pointer) ) )
		  (foreign-type-declaration (cadr type) (string-append "*" target)) )
		 ((and (= 2 len)
		       (eq? 'ref (car type)))
		  (foreign-type-declaration (cadr type) (string-append "&" target)) )
		 ((and (> len 2)
		       (eq? 'template (car type)))
		  (str
		   (string-append 
		    (foreign-type-declaration (cadr type) "")
		    "<"
		    (string-intersperse
		     (map (cut foreign-type-declaration <> "") (cddr type))
		     ",")
		    "> ") ) )
		 ((and (= len 2) (eq? 'const (car type)))
		  (string-append "const " (foreign-type-declaration (cadr type) target)))
		 ((and (= len 2) (eq? 'struct (car type)))
		  (string-append "struct " (->string (cadr type)) " " target))
		 ((and (= len 2) (eq? 'union (car type)))
		  (string-append "union " (->string (cadr type)) " " target))
		 ((and (= len 2) (eq? 'enum (car type)))
		  (string-append "enum " (->string (cadr type)) " " target))
		 ((and (= len 3) (memq (car type) '(instance nonnull-instance)))
		  (string-append (->string (cadr type)) "*" target))
		 ((and (= len 3) (eq? 'instance-ref (car type)))
		  (string-append (->string (cadr type)) "&" target))
		 ((and (>= len 3) (eq? 'function (car type)))
		  (let ((rtype (cadr type))
			(argtypes (caddr type))
			(callconv (optional (cdddr type) "")))
		    (string-append
		     (foreign-type-declaration rtype "")
		     callconv
		     " (*" target ")("
		     (string-intersperse
		      (map (lambda (at)
			     (if (eq? '... at) 
				 "..."
				 (foreign-type-declaration at "") ) )
			   argtypes) 
		      ",")
		     ")" ) ) )
		 (else (err)) ) ) ]
	     [else (err)] ) ] ) ) )


;; Generate expression to convert argument from Scheme data

(define (foreign-argument-conversion type)
  (let ([err (lambda () (quit "illegal foreign argument type `~A'" type))])
    (case type
      ((scheme-object) "(")
      ((char unsigned-char) "C_character_code((C_word)")
      ((byte int unsigned-int unsigned-int32 unsigned-byte) "C_unfix(")
      ((short) "C_unfix(")
      ((unsigned-short) "(unsigned short)C_unfix(")
      ((unsigned-long) "C_num_to_unsigned_long(")
      ((double number float) "C_c_double(")
      ((integer integer32) "C_num_to_int(")
      ((integer64) "C_num_to_int64(")
      ((long) "C_num_to_long(")
      ((unsigned-integer unsigned-integer32) "C_num_to_unsigned_int(")
      ;; pointer and nonnull-pointer are DEPRECATED
      ((pointer) "C_data_pointer_or_null(")
      ((nonnull-pointer) "C_data_pointer(")
      ((scheme-pointer) "C_data_pointer_or_null(")
      ((nonnull-scheme-pointer) "C_data_pointer(")
      ((c-pointer) "C_c_pointer_or_null(")
      ((nonnull-c-pointer) "C_c_pointer_nn(")
      ((blob) "C_c_bytevector_or_null(")
      ((nonnull-blob) "C_c_bytevector(")
      ;; byte-vector and nonnull-byte-vector are DEPRECATED
      ((byte-vector) "C_c_bytevector_or_null(")
      ((nonnull-byte-vector) "C_c_bytevector(")
      ((u8vector) "C_c_u8vector_or_null(")
      ((nonnull-u8vector) "C_c_u8vector(")
      ((u16vector) "C_c_u16vector_or_null(")
      ((nonnull-u16vector) "C_c_u16vector(")
      ((u32vector) "C_c_u32vector_or_null(")
      ((nonnull-u32vector) "C_c_u32vector(")
      ((s8vector) "C_c_s8vector_or_null(")
      ((nonnull-s8vector) "C_c_s8vector(")
      ((s16vector) "C_c_s16vector_or_null(")
      ((nonnull-s16vector) "C_c_s16vector(")
      ((s32vector) "C_c_s32vector_or_null(")
      ((nonnull-s32vector) "C_c_s32vector(")
      ((f32vector) "C_c_f32vector_or_null(")
      ((nonnull-f32vector) "C_c_f32vector(")
      ((f64vector) "C_c_f64vector_or_null(")
      ((nonnull-f64vector) "C_c_f64vector(")
      ((c-string c-string* unsigned-c-string unsigned-c-string*) "C_string_or_null(")
      ((nonnull-c-string nonnull-c-string* nonnull-unsigned-c-string 
			 nonnull-unsigned-c-string* symbol) "C_c_string(")
      ((bool) "C_truep(")
      (else
       (cond [(and (symbol? type) (##sys#hash-table-ref foreign-type-table type))
	      => (lambda (t)
		   (foreign-argument-conversion (if (vector? t) (vector-ref t 0) t)) ) ]
	     [(and (list? type) (>= (length type) 2))
	      (case (car type)
	       ;; pointer and nonnull-pointer are DEPRECATED
	       ((pointer) "C_c_pointer_or_null(")
	       ((nonnull-pointer) "C_c_pointer_nn(")
	       ((c-pointer) "C_c_pointer_or_null(")
	       ((nonnull-c-pointer) "C_c_pointer_nn(")
	       ((instance) "C_c_pointer_or_null(")
	       ((nonnull-instance) "C_c_pointer_nn(")
	       ((function) "C_c_pointer_or_null(")
	       ((const) (foreign-argument-conversion (cadr type)))
	       ((enum) "C_num_to_int(")
	       ((ref)
		(string-append "*(" (foreign-type-declaration (car type) "*")
			       ")C_c_pointer_nn("))
	       ((instance-ref)
		(string-append "*(" (cadr type) "*)C_c_pointer_nn("))
	       (else (err)) ) ]
	     [else (err)] ) ) ) ) )


;; Generate suitable conversion of a result value into Scheme data
	    
(define (foreign-result-conversion type dest)
  (let ([err (lambda () (quit "illegal foreign return type `~A'" type))])
    (case type
      ((char unsigned-char) "C_make_character((C_word)")
      ((int int32) "C_fix((C_word)")
      ((unsigned-int unsigned-int32) "C_fix(C_MOST_POSITIVE_FIXNUM&(C_word)")
      ((short) "C_fix((short)")
      ((unsigned-short) "C_fix(0xffff&(C_word)")
      ((byte) "C_fix((char)")
      ((unsigned-byte) "C_fix(0xff&(C_word)")
      ((float double) (sprintf "C_flonum(&~a," dest))	;*** suboptimal for int64
      ((number) (sprintf "C_number(&~a," dest))
      ((nonnull-c-string c-string nonnull-c-pointer c-string* nonnull-c-string* 
			 unsigned-c-string unsigned-c-string* nonnull-unsigned-c-string
			 nonnull-unsigned-c-string* symbol c-string-list c-string-list*) 
       (sprintf "C_mpointer(&~a,(void*)" dest) )
      ((c-pointer) (sprintf "C_mpointer_or_false(&~a,(void*)" dest))
      ((integer integer32) (sprintf "C_int_to_num(&~a," dest))
      ((integer64) (sprintf "C_a_double_to_num(&~a," dest))
      ((unsigned-integer unsigned-integer32) (sprintf "C_unsigned_int_to_num(&~a," dest))
      ((long) (sprintf "C_long_to_num(&~a," dest))
      ((unsigned-long) (sprintf "C_unsigned_long_to_num(&~a," dest))
      ((bool) "C_mk_bool(")
      ((void scheme-object) "((C_word)")
      (else
       (cond [(and (symbol? type) (##sys#hash-table-ref foreign-type-table type))
	      => (lambda (x)
		   (foreign-result-conversion (if (vector? x) (vector-ref x 0) x) dest)) ]
	     [(and (list? type) (>= (length type) 2))
	      (case (car type)
		((nonnull-pointer nonnull-c-pointer)
		 (sprintf "C_mpointer(&~A,(void*)" dest) )
		((ref)
		 (sprintf "C_mpointer(&~A,(void*)&" dest) )
		((instance)
		 (sprintf "C_mpointer_or_false(&~A,(void*)" dest) )
		((nonnull-instance)
		 (sprintf "C_mpointer(&~A,(void*)" dest) )
		((instance-ref)
		 (sprintf "C_mpointer(&~A,(void*)&" dest) )
		((const) (foreign-result-conversion (cadr type) dest))
		((pointer c-pointer)
		 (sprintf "C_mpointer_or_false(&~a,(void*)" dest) )
		((function) (sprintf "C_mpointer(&~a,(void*)" dest))
		((enum) (sprintf "C_int_to_num(&~a," dest))
		(else (err)) ) ]
	     [else (err)] ) ) ) ) )


;;; Encoded literals as strings, to be decoded by "C_decode_literal()"
;; 
;; - everything hardcoded, using the FFI would be the ugly, but safer method.

(define (encode-literal lit)
  (define getbits
    (foreign-lambda* int ((scheme-object lit))
      "
#ifdef C_SIXTY_FOUR
return((C_header_bits(lit) >> (24 + 32)) & 0xff);
#else
return((C_header_bits(lit) >> 24) & 0xff);
#endif
") )
  (define getsize
    (foreign-lambda* int ((scheme-object lit))
      "return(C_header_size(lit));"))
  (define (encode-size n)
    ;; only handles sizes in the 24-bit range!
    (string (integer->char (bitwise-and #xff (arithmetic-shift n -16)))
	    (integer->char (bitwise-and #xff (arithmetic-shift n -8)))
	    (integer->char (bitwise-and #xff n))))
  (define (finish str)		   ; can be taken out at a later stage
    (string-append (string #\xfe) str))
  (finish
   (cond ((eq? #t lit) "\xff\x06\x01")
	 ((eq? #f lit) "\xff\x06\x00")
	 ((char? lit) (string-append "\xff\x0a" (encode-size (char->integer lit))))
	 ((null? lit) "\xff\x0e")
	 ((eof-object? lit) "\xff\x3e")
	 ((eq? (void) lit) "\xff\x1e")
	 ((fixnum? lit)
	  (if (not (big-fixnum? lit))
	      (string-append
	       "\xff\x01"
	       (string (integer->char (bitwise-and #xff (arithmetic-shift lit -24)))
		       (integer->char (bitwise-and #xff (arithmetic-shift lit -16)))
		       (integer->char (bitwise-and #xff (arithmetic-shift lit -8)))
		       (integer->char (bitwise-and #xff lit)) ) )
	      (string-append "\xff\x55" (number->string lit) "\x00") ) )
	 ((number? lit)
	  (string-append "\x55" (number->string lit) "\x00") )
	 ((symbol? lit)
	  (let ((str (##sys#slot lit 1)))
	    (string-append 
	     "\x01" 
	     (encode-size (string-length str))
	     str) ) )
	 ((##sys#immediate? lit)
	  (bomb "invalid literal - cannot encode" lit))
	 ((##core#inline "C_byteblockp" lit)
	  (##sys#string-append ; relies on the fact that ##sys#string-append doesn't check
	   (string-append
	    (string (integer->char (getbits lit)))
	    (encode-size (getsize lit)) )
	   lit) )
	 (else
	  (let ((len (getsize lit)))
	    (string-intersperse
	     (cons*
	      (string (integer->char (getbits lit)))
	      (encode-size len)
	      (list-tabulate len (lambda (i) (encode-literal (##sys#slot lit i)))))
	     ""))))) )
