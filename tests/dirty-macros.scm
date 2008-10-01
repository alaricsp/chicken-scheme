; How to write dirty R5RS macros
; http://groups.google.com/groups?selm=87oflzcdwt.fsf%40radish.petrofsky.org
; How to write seemingly unhygienic macros using syntax-rules 
; Date: 2001-11-19 01:23:33 PST 
;
; $Id: dirty-macros.scm,v 1.10 2003/08/16 02:13:32 oleg Exp oleg $

; Extract a colored identifier from a form
;    extract? SYMB BODY CONT-T CONT-F
; BODY is a form that may contain an occurence of an identifier that
; refers to the same binding occurrence as SYMB, perhaps with a different
; color. CONT-T and CONT-F are forms of the shape (K-HEAD K-IDL . K-ARGS)
; where K-IDL are K-ARGS are lists.
; If the extract? macro finds the identifier in question, it expands into
; CONT-T, to be more precise, into
;   (K-HEAD (extr-id . K-IDL) . K-ARGS)
; where extr-id is the extracted colored identifier. If the identifier
; SYMB does not occur in BODY at all, the extract macro expands into CONT-F,
; to be more precise,
;   (K-HEAD (SYMB . K-IDL) . K-ARGS)

(define-syntax m-symbol?
  (syntax-rules ()
    ((_ maybe-symbol kt kf)
     (letrec-syntax
       ((ok
	  (syntax-rules ()
	    ((_) kt)))
	 (test
	  (syntax-rules ()
	    ((_ maybe-symbol) (ok))
	    ((_ x) kf))))
       (test abracadabra)))))

(define-syntax m-symb-equal?
  (syntax-rules ()
    ((_ symb b kt kf)
	(let-syntax
	  ((symb (syntax-rules ()
		   ((_) kf)))
	   (ok (syntax-rules ()
		   ((_) kt))))
	  (let-syntax
	    ((test (syntax-rules ()
		     ((_ b) (symb))
		     ((_ x) kf))))
	    (test ok))))))

(define-syntax extract?
  (syntax-rules ()
    ((_ symb body _cont-t _cont-f)
      (letrec-syntax
	((lp
           (syntax-rules (symb)
	      ((_ d symb stack (cont-head symb-l . cont-args) cont-f)
	       (cont-head (d . symb-l) . cont-args)) ; symb has occurred
	      ((_ d (x . y) stack . rest)   ; if body is a composite form,
	       (lp x x (y . stack) . rest)) ; look inside
	      ((_ d1 d2 () cont-t (cont-head  symb-l . cont-args))
	       (cont-head (symb . symb-l) . cont-args)) ; symb does not occur
	      ((_ d1 d2 (x . y) . rest)
	       (lp x x y . rest)))))
	(lp body body () _cont-t _cont-f)))))

; (define-syntax extract?
;   (syntax-rules ()
;     ((_ symb body _cont-t _cont-f)
;       (letrec-syntax
; 	((tr
;            (syntax-rules ()
; 	      ((_ d (x . y) tail . rest)   ; if body is a composite form,
; 	       (tr x x (y . tail) . rest)) ; look inside
; 	      ((_ x y () (cont-head symb-l . cont-args) 
; 		 (cont-headf symb-lf . cont-argsf))
; 	       (m-symb-equal? symb y 
; 		 (cont-head (x . symb-l) . cont-args) ; symb has occurred
; 	       (cont-headf (symb . symb-lf) . cont-argsf)));symb does not occur
; 	     ((_ d1 d2 (x . y) (cont-head symb-l . cont-args) cont-f) 
; 	       (m-symb-equal? symb d2
; 		 (cont-head (d1 . symb-l) . cont-args) ; symb has occurred
; 		 (tr x x y (cont-head symb-l . cont-args) cont-f))))))
; 	(tr body body () _cont-t _cont-f)))))

; Extract a colored identifier from a form
;    extract SYMB BODY CONT
; BODY is a form that may contain an occurence of an identifier that
; refers to the same binding occurrence as SYMB, perhaps with a different
; color. CONT is a form of the shape (K-HEAD K-IDL . K-ARGS)
; where K-IDL are K-ARGS are S-expressions representing lists or the
; empty list.
; The extract macro expands into
;   (K-HEAD (extr-id . K-IDL) . K-ARGS)
; where extr-id is the extracted colored identifier. If symbol SYMB does
; not occur in BODY at all, extr-id is identical to SYMB.


(define-syntax extract
  (syntax-rules ()
    ((_ symb body cont)
     (extract? symb body cont cont))))

; Extract several colored identifiers from a form
;    extract* SYMB-L BODY CONT
; where SYMB-L is the list of symbols to extract, and BODY and CONT
; has the same meaning as in extract, see above.
; 
; The extract* macro expands into
;   (K-HEAD (extr-id-l . K-IDL) . K-ARGS)
; where extr-id-l is the list of extracted colored identifiers. The extraction
; itself is performed by the macro extract.

(define-syntax extract*
  (syntax-rules ()
    ((_ (symb) body cont)      ; only one symbol: use extract to do the job
     (extract symb body cont))
    ((_ _symbs _body _cont)
     (letrec-syntax
	 ((ex-aux		; extract symbol-by-symbol
	   (syntax-rules ()
	     ((_ found-symbs () body cont)
	      (reverse () found-symbs cont))
	     ((_ found-symbs (symb . symb-others) body cont)
	      (extract symb body
		       (ex-aux found-symbs symb-others body cont)))
	     ))
	  (reverse		; reverse the list of extracted symbols
	   (syntax-rules ()     ; to match the order of SYMB-L
	     ((_ res () (cont-head () . cont-args))
	      (cont-head res . cont-args))
	     ((_ res (x . tail) cont)
	      (reverse (x . res) tail cont)))))
       (ex-aux () _symbs _body _cont)))))

; Writing weakly referentially opaque macros

; A binding-capturing macro with an explicit specification
; of the captured variable
(define-syntax m1-i
  (syntax-rules ()
    ((_ i val body) (let ((i val)) body))))

(display
   (m1-i i 10 (* 1 i)))
(newline) ;==> 10

; A dirty macro m1 that extracts i from its argument and expands
; into an invocation of m1-i:

(define-syntax m1-dirty-v1
  (syntax-rules ()
    ((_ _val _body)
     (let-syntax
	 ((cont
	   (syntax-rules ()
	     ((_ (symb) val body) (let ((symb val)) body) ))))
       (extract i _body (cont () _val _body))))))

(display
   (m1-dirty-v1 10 (* 1 i))
)
(newline)

(display
   (m1-dirty-v1 10
     (m1-dirty-v1 20 (* 1 i)))
)
(newline)


; A macro that re-defines itself in its expansion
;  m1-dirty-v2 val body
; expands into
; (let ((i val)) body)
; and also re-defines itself in the scope of body.

(define-syntax m1-dirty-v2
  (syntax-rules ()
    ((_ _val _body)
     (letrec-syntax
	 ((doit		    ; it's the continuation from extract*
	   (syntax-rules () ; myself-symb i-symb are colored ids extracted
	     ((_ (myself-symb i-symb) val body)      ; from the 'body'
	      (let ((i-symb val))  ; first bind the symbol i
		(letrec-syntax	   ; now re-define oneself
		    ((myself-symb
		      (syntax-rules ()
			((_ val__ body__)
			 (extract* (myself-symb i-symb) body__
				  (doit () val__ body__))))))
		  body))))))
       (extract* (m1-dirty-v2 i) _body
		 (doit () _val _body))))))

(display "m1-dirty-v2")
(newline)
(display
   (m1-dirty-v2 10 (* 1 i))
)
(newline)
; => 10

(display
   (m1-dirty-v2 10
     (m1-dirty-v2 20 
       (m1-dirty-v2 30 (* 1 i))))
)
(newline)

(display
  (let ((i 1))
   (m1-dirty-v2 10 (* 1 i)))
)
(newline)
; => 1

; A self-perpetuating smearing let
;   (mylet ((var init)) body)
; expands into
;   (let ((var init)) body')
; where body' is body wrapped into redefinitions of mylet
; and a macro m1-dirty-v3

; This macro is closed (no free variables)
(define-syntax dirty-m-gen
  (syntax-rules ()
    ((_ name let-name _symb_ _body_)
     (let-syntax
	 ((name
	   (syntax-rules ()
	     ((_ _val _body)
	      (let-syntax
		  ((cont
		    (syntax-rules ()
		      ((_ (symb) val body) (let ((symb val)) body) ))))
		(extract _symb_ _body (cont () _val _body)))))))
       _body_))))

; (define-syntax mylet
;   (syntax-rules ()
;     ((_ ((_var _init)) _body)
;      (letrec-syntax
; 	 ((doit		    ; it's the continuation from extract*
; 	   (syntax-rules () ; myself-symb etc. are extr. colored ids extracted
; 	     ((_ (myself-symb dirty-m-symb i-symb) ((var init)) body) 
; 	      (let ((var init))     ; first do the binding
; 		(letrec-syntax	    ; now re-define oneself
; 		    ((myself-symb
; 		      (syntax-rules ()
; 			((_ ((var__ init__)) body__)
; 			 (extract* (myself-symb dirty-m-symb i-symb)
; 				   (var__ body__)
; 				  (doit () ((var__ init__)) body__))))))
; 		  (dirty-m-gen      ; re-generate the dirty macro
; 		   dirty-m-symb myself-symb i-symb
; 		  body)))))))
;        (extract* (mylet m1-dirty-v3 i) (_var _body)
; 		 (doit () ((_var _init)) _body))))))

;      (letrec-syntax 
;        ((ex
; 	 (syntax-rules ()
; 	   ((_ (mylet-symb mm-symb foo-symb) ((var init)) body)
; 	    (let ((var init))
; 	      (make-mm mm-symb foo-symb
; 		  (letrec-syntax 
; 		      ((mylet-symb
; 			(syntax-rules ()
; 			  ((_ ((var_ init_)) body_)
; 		       (extract* (mylet-symb mm-symb foo-symb) (var_ body_)
; 				  (ex () ((var_ init_)) body_))))))
; 		 body)))
; 	    ))))
;        (extract* (mylet mm foo) (_var _body)
; 		 (ex () ((_var _init)) _body))))))

; (display "m1-dirty-v3")
; (newline)
; (display
;  (mylet ((i 1))
; 	(m1-dirty-v3 10 (* 1 i)))
; )
; (newline)

; (display
;  (mylet ((i 1))
; 	(mylet ((i 10))
; 		     (m1-dirty-v3 20 (* 1 i))))
; )
; (newline)

; (display
;  (mylet ((i 1))
; 	(m1-dirty-v3 10
; 		     (m1-dirty-v3 20 (* 1 i))))
; )
; (newline)


; A macro that generates a dirty macro:
;    m1-dirty BODY
; expands into a definition of a macro
;    NAME BODY
; which in turn expands into (let ((SYMB 10)) BODY) 
; such that the binding captures any free occurences of SYMB in BODY.

; (define-syntax m1-dirty
;   (syntax-rules ()
;     ((_ _symb _body)
;      (let-syntax
; 	 ((doit
; 	   (syntax-rules ()
; 	     ((_ (symb) val body)
; 	      (let ((symb val)) body)))))
;        (extract _symb _body (doit () 10 _body))))))

; Macro: make-mm NAME SYMB BODY
; In the scope of BODY, define a macro NAME that expands into a symbol SYMB

(define-syntax make-mm
  (syntax-rules ()
    ((_ name symb body)
      (let-syntax 
	 ((name
	   (syntax-rules ()
	     ((_) symb))))
	body))))

; (define-syntax mylet
;   (syntax-rules (foo)
;     ((_ ((var init)) body)
; 	(extract foo (var)
; 	 (make-mm-in ((var init)) body)))))

; (mylet ((var init)) body)
; expands into
; (let ((var init)) body')
; where body' is the body wrapped in the re-definitions of mylet and macro mm.

(define-syntax mylet
  (syntax-rules ()
    ((_ ((_var _init)) _body)
     (letrec-syntax 
       ((doit			; The continuation from extract*
	 (syntax-rules ()       ; mylet-symb, etc. are extracted from body
	   ((_ (mylet-symb mm-symb foo-symb) ((var init)) body)
	    (let ((var init))	; bind the 'var' first
	      (make-mm mm-symb foo-symb  ; now re-generate the macro mm
		  (letrec-syntax 
		      ((mylet-symb       ; and re-define myself
			(syntax-rules ()
			  ((_ ((var_ init_)) body_)
		       (extract* (mylet-symb mm-symb foo-symb) (var_ body_)
				  (doit () ((var_ init_)) body_))))))
		 body)))
	    ))))
       (extract* (mylet mm foo) (_var _body)
		 (doit () ((_var _init)) _body))))))

(display "mylet")
(newline)
(define foo 1)
(display
 (mylet ((x 1)) (list (mm) x))
)
(newline)

(display
 (mylet ((foo 2)) (list (mm) foo))
)
(newline)

; ;(let ((foo 3)) (mylet ((foo 4)) (list foo (mm))))
;(mylet ((foo 2)) (mylet ((foo 3)) (list foo (mm))))

(display
 (mylet ((foo 3)) (mylet ((foo 4)) (mylet ((foo 5)) (list foo (mm)))))
)
(newline)

(display
 (mylet ((foo 3))
    (mylet ((thunk (lambda () (mm))))
     (mylet ((foo 4)) (list foo (mm) (thunk)))))
)
(newline)

; The following are definitions of let, let* and letrec, straight out of R5RS.
; The only difference is that the definitions use custom-bound 
; let, let*, letrec and lambda identifiers, which we explicitly pass 
; to the macros in the first argument.

(define-syntax glet		; let, straight out of R5RS
  (syntax-rules ()
    ((_ (let let* letrec lambda) ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...) val ...))
    ((_ (let let* letrec lambda) tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag
		(lambda (name ...) body1 body2 ...))) tag) val ...))))

(define-syntax glet*		; let*, straight out of R5RS
  (syntax-rules ()
    ((_ mynames () body1 body2 ...)
     (let () body1 body2 ...))
    ((_ (let let* letrec lambda)
	((name1 val1) (name2 val2) ...) body1 body2 ...)
     (let ((name1 val1)) (let* ((name2 val2) ...) body1 body2 ...)))))

; A shorter implementations of letrec, see
; "Re: Widespread bug (arguably) in letrec when an initializer returns twice"
; comp.lang.scheme, 2001-05-21 10:30:34 PST and 2001-05-21 14:56:49 PST
; http://groups.google.com/groups?selm=7eb8ac3e.0105210930.21542605%40posting.google.com
; http://groups.google.com/groups?selm=87ae468j7x.fsf%40app.dial.idiom.com

(define-syntax gletrec
  (syntax-rules ()
    ((_ (mlet let* letrec lambda) ((var init) ...) . body)
     (mlet ((var 'undefined) ...)
       (let ((temp (list init ...)))  ; the native let will do fine here
         (begin (begin (set! var (car temp)) (set! temp (cdr temp))) ...
         (let () . body)))))))

; This macro defiles its body
; It re-defines all the let-forms and the lambda, and defines
; a non-hygienic macro 'mm'. Whenever any binding is introduced,
; the let-forms, the lambdas and 'mm' are redefined.
; The redefined lambda acts as if it were infected by a virus, which
; keeps spreading within lambda's body to infect other lambda's there.

(define-syntax defile
  (syntax-rules ()
    ((_ dbody)
     (letrec-syntax
	 ((do-defile
	   (syntax-rules ()	; all the shadowed symbols
	     ((_ (let-symb let*-symb letrec-symb lambda-symb mm-symb foo-symb)
		 body-to-defile)
	      (letrec-syntax
		  ((let-symb		; R5RS definition of let
		    (syntax-rules ()
		      ((_ . args)
		       (glet (let-symb let*-symb letrec-symb lambda-symb)
			     . args))))

		   (let*-symb		; Redefinition of let*
		    (syntax-rules ()
		      ((_ . args) 
		       (glet* (let-symb let*-symb letrec-symb lambda-symb) 
			      . args))))

		   (letrec-symb		; Redefinition of letrec
		    (syntax-rules ()
		      ((_ . args) 
		       (gletrec (let-symb let*-symb letrec-symb lambda-symb) 
			      . args))))
		       
		   (lambda-symb         ; re-defined, infected lambda
		    (syntax-rules ()
		      ((_ _vars _body)
		       (letrec-syntax 
			   ((doit
			     (syntax-rules ()
			       ((_ (mylet-symb mylet*-symb myletrec-symb
					       mylambda-symb mymm-symb 
					       myfoo-symb) vars body)
				(lambda-native vars
				  (make-mm mymm-symb myfoo-symb
				    (do-defile	; proliferate in the body
				     (mylet-symb mylet*-symb myletrec-symb
						 mylambda-symb
						 mymm-symb myfoo-symb)
				     body))))))
			    (proliferate
			     (syntax-rules ()
			       ((_ dummy __vars __body)
				(extract* (let-symb let*-symb 
						    letrec-symb lambda-symb
						    mm-symb foo-symb)
					  (__vars __body)
					  (doit () __vars __body)))))
			    (stop-infection
			     (syntax-rules ()
			       ((_ dummy __vars __body)
				(lambda-native __vars __body))))
			    )
			 (extract? mm-symb _vars
			    ; true-continuation
			    (stop-infection () _vars _body)
			    ; false-cont
			    (proliferate () _vars _body))
			 ))))

		   (lambda-native	; capture the native lambda
		    (syntax-rules ()
		      ((_ . args) (lambda . args))))
		   )

		body-to-defile)))))

       (extract* (let let* letrec lambda mm foo) dbody
		 (do-defile () dbody))
       ))))


;(mylet ((foo 2)) (mylet ((x 3)) (mylet ((foo 4)) (list (mm) foo))))
(display "defile")
(display "now.\n" (current-error-port))
(newline)
(defile
  (display
   (let ((foo 2)) (list (mm) foo))
   )
)
(newline)
; ==> (2 2)

(defile
  (display
   (let ((foo 2)) (let ((foo 3)) (let ((foo 4)) (list (mm) foo))))
   )
)
(newline)
; ==> (4 4)

(defile
  (display
   (let ((foo 2))
     (let ((foo 3) (bar (list (mm) foo)))
       (list foo (mm) bar)))
   )
)
(newline)
; ==> (3 3 (2 2)) 

(defile
  (display
   (let ((foo 2))
     (list
      ((letrec ((bar (lambda () (list foo (mm))))
		(foo 3))
	bar))
      foo (mm)))))
(newline)
;==> ((3 3) 2 2) 

(defile
  (display
   (let ((foo 2))
     (let foo ((flag #t) (lst (list foo (mm))))
       (if flag ((mm) #f (list lst lst)) lst)))))
(newline)
; ==> ((2 2) (2 2)) 

(defile
  (display
  (let* ((foo 2) 
	 (i 3)
	 (foo 4) 
	 (ft (lambda () (mm))) ; will capture binding of foo to 4
	 (foo 5)
	 (ft1 (lambda (foo) (mm))) ; will  capture the arg of ft1
	 (foo 6))
    (list foo (mm) (ft) (ft1 7) '(mm))))
  )
(newline)
; ==> (6 6 4 7 (mm))

; the use of (mm) (separately-defined macro) is equivalent to the use of variable foo -- 
; (define-macro (mm) foo) -- dirty macro


; Re-defining the global let

(define-syntax dlet
  (syntax-rules ()
    ((_ new-let-symb . args) 
     ; just renaming of new-let-symbol with 'let'
     (let-syntax
	 ((ren 
	   (syntax-rules ()
	     ((_ list) (defile (let . args))))))
       (ren let1)))))


(display "dlet")
(newline)
(display
 (dlet list ((foo 2)) (list (mm) foo))
 )

; (define-syntax old-let
;   (syntax-rules ()
;     ((_ . args) (let . args))))
; (define-syntax old-let*
;   (syntax-rules ()
;     ((_ . args) (let* . args))))
; (define-syntax old-letrec
;   (syntax-rules ()
;     ((_ . args) (letrec . args))))
; (define-syntax old-lambda
;   (syntax-rules ()
;     ((_ . args) (lambda . args))))

; (define-syntax let
;    (syntax-rules ()
;      ((_ . args) (defile1 (glet (old-let old-let* old-letrec lambda) . args)))))
; ; (define-syntax let
; ;    (syntax-rules ()
; ;      ((_ . args) (defile1 (old-let . args)))))


; (define-syntax defile1
;   (syntax-rules ()
;     ((_ dbody)
;      (letrec-syntax
; 	 ((do-defile
; 	   (syntax-rules ()	; all the shadowed symbols
; 	     ((_ (let-symb let*-symb letrec-symb lambda-symb mm-symb foo-symb)
; 		 body-to-defile)
; 	      (letrec-syntax
; 		  ((let-symb		; R5RS definition of let
; 		    (syntax-rules ()
; 		      ((_ . args)
; 		       (glet (let-symb let*-symb letrec-symb lambda-symb)
; 			     . args))))

; 		   (let*-symb		; Redefinition of let*
; 		    (syntax-rules ()
; 		      ((_ . args) 
; 		       (glet* (let-symb let*-symb letrec-symb lambda-symb) 
; 			      . args))))

; 		   (letrec-symb		; Redefinition of letrec
; 		    (syntax-rules ()
; 		      ((_ . args) 
; 		       (gletrec (let-symb let*-symb letrec-symb lambda-symb) 
; 			      . args))))
		       
; 		   (lambda-symb         ; re-defined, infected lambda
; 		    (syntax-rules ()
; 		      ((_ _vars _body)
; 		       (letrec-syntax 
; 			   ((doit
; 			     (syntax-rules ()
; 			       ((_ (mylet-symb mylet*-symb myletrec-symb
; 					       mylambda-symb mymm-symb 
; 					       myfoo-symb) vars body)
; 				(lambda-native vars
; 				  (make-mm mymm-symb myfoo-symb
; 				    (do-defile	; proliferate in the body
; 				     (mylet-symb mylet*-symb myletrec-symb
; 						 mylambda-symb
; 						 mymm-symb myfoo-symb)
; 				     body)))))))
; 			 (extract* (let-symb let*-symb letrec-symb lambda-symb
; 					     mm-symb foo-symb)
; 				   (_vars _body)
; 				   (doit () _vars _body))))))

; 		   (lambda-native	; capture the native lambda
; 		    (syntax-rules ()
; 		      ((_ . args) (lambda . args))))
; 		   )

; 		body-to-defile)))))

;        (extract* (let let* letrec lambda mm foo) dbody
; 		 (do-defile () dbody))
;        ))))

; ; (define-syntax let
; ;   (syntax-rules ()
; ;     ((_ . args) (dlet let . args))))



; ; (define-syntax alet
; ;   (syntax-rules ()
; ;     ((_ . args) 
; ;      (let-syntax
; ; 	 ((doit
; ; 	   (syntax-rules ()
; ; 	      ((_ (let-symb) body) (defile1 let-symb (blet . body))))))
; ;        (extract* (blet) args (doit () args))))))


; (display "corrupted-let")
; (newline)
; (display
;  (let ((foo 2)) (list (mm) foo))
;  )

; (newline)
; (display
;    (let ((foo 2)) (let ((foo 3)) (let ((foo 4)) (list (mm) foo))))
;    )
; (newline)
; ; ==> (4 4)

; (display
;    (let ((foo 2))
;      (let ((foo 3) (bar (list (mm) foo)))
;        (list foo (mm) bar)))
;    )
; (newline)
; ; ==> (3 3 (2 2)) 

; (display
;    (let ((foo 2))
;      (list
;       ((letrec ((bar (lambda () (list foo (mm))))
; 		(foo 3))
; 	bar))
;       foo (mm))))
; (newline)
; ;==> ((3 3) 2 2) 

; (display
;    (let ((foo 2))
;      (let foo ((flag #t) (lst (list foo (mm))))
;        (if flag ((mm) #f (list lst lst)) lst))))
; (newline)
; ; ==> ((2 2) (2 2)) 

; (display
;  (let ()
;   (let* ((foo 2) 
; 	 (i 3)
; 	 (foo 4) 
; 	 (ft (lambda () (mm))) ; will capture binding of foo to 4
; 	 (foo 5)
; 	 (ft1 (lambda (foo) (mm))) ; will  capture the arg of ft1
; 	 (foo 6))
;     (list foo (mm) (ft) (ft1 7) '(mm))))
;   )
; (newline)
; ; ==> (6 6 4 7 (mm))


(define-syntax defile-what
  (syntax-rules ()
    ((_ dirty-macro-name dirty-macro-name-gen captured-symbol dbody)
     (letrec-syntax
	 ((do-defile
	   (syntax-rules ()	; all the shadowed symbols
	     ((_ (let-symb let*-symb letrec-symb lambda-symb mm-symb foo-symb)
		 body-to-defile)
	      (letrec-syntax
		  ((let-symb		; R5RS definition of let
		    (syntax-rules ()
		      ((_ . args)
		       (glet (let-symb let*-symb letrec-symb lambda-symb)
			     . args))))

		   (let*-symb		; Redefinition of let*
		    (syntax-rules ()
		      ((_ . args) 
		       (glet* (let-symb let*-symb letrec-symb lambda-symb) 
			      . args))))

		   (letrec-symb		; Redefinition of letrec
		    (syntax-rules ()
		      ((_ . args) 
		       (gletrec (let-symb let*-symb letrec-symb lambda-symb) 
			      . args))))
		       (lambda-symb         ; re-defined, infected lambda
		    (syntax-rules ()
		      ((_ _vars _body)
		       (letrec-syntax 
			   ((doit
			     (syntax-rules ()
			       ((_ (mylet-symb mylet*-symb myletrec-symb
					       mylambda-symb mymm-symb 
					       myfoo-symb) vars body)
				(lambda-native vars
				  (dirty-macro-name-gen mymm-symb myfoo-symb
				    (do-defile	; proliferate in the body
				     (mylet-symb mylet*-symb myletrec-symb
						 mylambda-symb
						 mymm-symb myfoo-symb)
				     body))))))
			    (proliferate
			     (syntax-rules ()
			       ((_ dummy __vars __body)
				(extract* (let-symb let*-symb 
						    letrec-symb lambda-symb
						    mm-symb foo-symb)
					  (__vars __body)
					  (doit () __vars __body)))))
			    (stop-infection
			     (syntax-rules ()
			       ((_ dummy __vars __body)
				(lambda-native __vars __body))))
			    )
			 (extract? mm-symb _vars
			    ; true-continuation
			    (stop-infection () _vars _body)
			    ; false-cont
			    (proliferate () _vars _body))
			 ))))

; 		   (lambda-symb         ; re-defined, infected lambda
; 		    (syntax-rules ()
; 		      ((_ _vars _body)
; 		       (letrec-syntax 
; 			   ((doit
; 			     (syntax-rules ()
; 			       ((_ (mylet-symb mylet*-symb myletrec-symb
; 					       mylambda-symb mymm-symb 
; 					       myfoo-symb) vars body)
; 				(lambda-native vars
; 				  (dirty-macro-name-gen mymm-symb myfoo-symb
; 				    (do-defile	; proliferate in the body
; 				     (mylet-symb mylet*-symb myletrec-symb
; 						 mylambda-symb
; 						 mymm-symb myfoo-symb)
; 				     body)))))))
; 			 (extract* (let-symb let*-symb letrec-symb lambda-symb
; 					     mm-symb foo-symb)
; 				   (_vars _body)
; 				   (doit () _vars _body))))))

		   (lambda-native	; capture the native lambda
		    (syntax-rules ()
		      ((_ . args) (lambda . args))))
		   )

		body-to-defile)))))

       (extract* (let let* letrec lambda dirty-macro-name captured-symbol) dbody
		 (do-defile () dbody))
       ))))


(define-syntax let-defiled-syntax
  (syntax-rules ()
    ((_ var-to-capture ((dm-name dm-body)) body)
     (let-syntax
	 ((dm-generator
	   (syntax-rules ()
	     ((_ dmg-name var-to-capture dmg-outer-body)
	      (let-syntax 
		  ((dmg-name dm-body))
		dmg-outer-body)))))
       (defile-what
	 dm-name dm-generator var-to-capture body)
       ))))

(display "defile-what") (newline)
(display
 (let-defiled-syntax
  bar ((mbar (syntax-rules () ((_ val) (+ bar val)))))
  (let ((bar 1)) (let ((bar 2)) (mbar 2))))
)
(newline)

(display "defile-what") (newline)
(display
 (let-defiled-syntax
  quux ((mquux (syntax-rules () ((_ val) (+ quux quux val)))))
  (let* ((bar 1) (quux 0) (quux 2) 
	 (lquux (lambda (x) (mquux x)))
	 (quux 3)
	 (lcquux (lambda (quux) (mquux quux)))) ; will tripple its arg
    (list (+ quux quux) (mquux 0) (lquux 2) (lcquux 5)))))
(newline)
; ==> (6 6 6 15)

; testing shadowing
(display "test shadowing") (newline)
(display
 (let-defiled-syntax
  quux ((mquux (syntax-rules () ((_ val) (+ quux quux val)))))
    (let* ((bar 1) (quux 0) (quux 2) 
	   (lquux (lambda (x) (mquux x)))
	   (mquux (lambda (val) 0))
	   (lcquux (lambda (quux) (mquux quux)))) ; will tripple its arg
      (list (+ quux quux) (mquux 0) (lquux 2) (lcquux 5)))))
(newline)
; ==> (4 0 6 0)
(display
 (let-syntax
  ((mquux (syntax-rules () ((_ val) (+ quux quux val)))))
  (let ((mquux (lambda (val) 0)))
    (let* ((bar 1) (quux 0) (quux 2) 
	   (lquux (lambda (x) (mquux x)))
	   (lcquux (lambda (quux) (mquux quux)))) ; will tripple its arg
      (list (+ quux quux) (mquux 0) (lquux 2) (lcquux 5))))))
(newline)
; ==> (4 0 0 0)

(display
 (let-defiled-syntax
  quux ((mquux (syntax-rules () ((_ val) (+ quux quux val)))))
  (let-syntax ((mquux (syntax-rules () ((_ val) 0))))
    (let* ((bar 1) (quux 0) (quux 2) 
	   (lquux (lambda (x) (mquux x)))
	   (lcquux (lambda (quux) (mquux quux)))) ; will tripple its arg
      (list (+ quux quux) (mquux 0) (lquux 2) (lcquux 5))))))
; ==> (4 0 0 0)

(display
  (defile
    (let-syntax
      ((test2
	 (syntax-rules (mm)
	   ((_ mm) 'okay)
	   ((_ x) 'wrong))))
      (list
	(test2 mm)
	(let ((foo 3)) (test2 mm))))))
(newline)

; extracting on a different sort of criteria

;   extract2 MARKER BODY CONT
; Search the body for the occurrence of the form (SYMB MARKER . REST)
; where SYMB is a symbol. MARKER is a string, boolean, or number.
; For simplicity, we don't check that SYMB
; is a symbol, but we could: see a macro m-symbol?.
; CONT is a list (K-HEAD () . K-REST)
; If we found such a form, expand into
; (K-HEAD SYMB  . K-REST)
; If we didn't find what we searched for, expand into
; (K-HEAD nai . K-REST)


(define-syntax extract2
  (syntax-rules ()
    ((_ _marker _body _cont)
      (letrec-syntax
	((lp
           (syntax-rules ()
	      ((_ (symb _marker . rest) stack (cont-head () . cont-args))
	       (cont-head symb . cont-args)) ; found
	      ((_ (x . y) stack cont)   ; if body is a composite form,
	       (lp x (y . stack) cont)) ; look inside
	      ((_ d () (cont-head () . cont-args))
	       (cont-head nai . cont-args)) ; symb does not occur
	      ((_ d (x . y) cont)
	       (lp x y cont)))))
	(lp _body () _cont)))))


(define-syntax loop
  (syntax-rules ()
    ((_ . exps)
      (let-syntax
	((cont
	   (syntax-rules ()
	     ((_ ident exps_)
	       (call-with-current-continuation
		 (lambda (k)
		   (let ((ident (lambda (dummy value) (k value))))
		     (let f ()
		       (begin 'prevent-empty-begin . exps_)
		       (f)))))))))
	(extract2 "this one" exps (cont () exps))))))


(display "loop") (newline)
(display (loop (break "this one" 'foo)))
(newline)
; ==> foo

(display "nested loop") (newline)
(display
  (loop
    (loop
      (break "this one" 'foo))
    (break "this one" 'bar)))
; ==> bar
(newline)

(display "loop: shadowing") (newline)
(display
  (let ((break (lambda (dummy x) x)))
    (loop (break "this one" 'foo))
    (break "this one" 'bar)))
(newline)

; Petrofsky:
; There are problems with writing extensions to loop.  Suppose we want
; to write loop-while, which adds a test that is checked once each time
; around the loop, and still binds an exit procedure.  We might think it
; could be written like this:

(define-syntax loop-while
  (syntax-rules ()
    ((_ test exp ...)
      (loop
	(if (not test) (break "this one" #f))
	exp ...))))

(display "loop-while") (newline)
(display
  (let ((n 0))
    (loop-while (< n 5)
      (set! n (+ n 1)))
    n))
; ==> 5
(newline)

(display
  (loop
    (let ((n 0))
      (loop-while (< n 5)
	(set! n (+ n 1))
	(if (= n 2)
	  (break "this one" 'foo)))
      (break "this one" 'bar))))
(newline)


; (define-syntax make-lambda 
;   (syntax-rules ()
;     ((_ . args) (lambda . args))))

; (define-syntax make-lambda 
;   (syntax-rules ()
;     ((_ bindings body ...)
;       (let () (define (proc . bindings) body ...)
; 	proc))))

(define-syntax make-lambda 
  (syntax-rules ()
    ((_ bindings body ...)
      (let-syntax () (define (proc . bindings) body ...)
	proc))))

(define-syntax lambda
  (syntax-rules ()
    ((_ bindings body1 body2 ...)
     (make-lambda bindings
       (display "OK") (newline)
       (begin body1 body2 ...)))))

(display "lambda-test") (newline)
(let ((p (lambda (x y z) (list x y z))))
  (display (p 1 2 3)))
(newline)

(define-syntax mm
  (syntax-rules ()
    ((_ dummy) foo)
    ((_ dummy k) (k foo))))

(define-syntax make-mm
  (syntax-rules ()
    ((_ mm foo bodies)
      (let-syntax
	((mm
	   (syntax-rules ()
	     ((_ dummy) foo)
	     ((_ dummy (kh () . kargs)) (kh foo . kargs)))))
	. bodies))))

(define-syntax recolor
  (syntax-rules ()
    ((_ from to bodies . rest)
      (let-syntax 
	((ren
	   (syntax-rules ()
	     ((_ from) bodies))))
	(ren to)))))

(define-syntax nai
  (syntax-rules ()
    ((_ dummy (kh () . kargs)) (kh nai . kargs))))

(define-syntax lambda
  (syntax-rules ()
    ((_ bindings . bodies)
      (letrec-syntax
	((test
	   (syntax-rules ()
	     ((_ symb exp _kt _kf)
	       (letrec-syntax
		 ((loop
		    (syntax-rules (symb)
		      ((_ d () kt kf) kf)
		      ((_ (s . r1) (symb . r2) (kh () . kargs) kf)
			(kh s . kargs))
		      ((_ d (x . rest) kt kf) (loop rest rest kt kf)))))
		 (loop exp exp _kt _kf)))))
	 (doit
	   (syntax-rules ()
	     ((_ foo orig-foo bindings_ bodies_)
	       (extract2 "mm" bodies_
		 (cont () foo orig-foo bindings_ bodies_)))))
	 (cont
	   (syntax-rules ()
	     ((_ mm bindings_ bodies_)
	      (mm dummy (cont2 () mm bindings_ bodies_)))))
	  (cont2
	    (syntax-rules ()
	      ((_ xxx foo mm bindings_ bodies_)
	       (test foo bindings_
		 (cont3 () mm bindings_ bodies_)
		 (make-lambda bindings_ bodies_)))))
	  (cont3
	    (syntax-rules ()
	      ((_ foo mm bindings_ bodies_)
		(make-lambda bindings_
		  (make-mm mm foo
		    bodies_))))))
	(extract2 "mm" bodies
	  (cont () bindings bodies))))))


; (define-syntax let
;   (syntax-rules ()
;     ((_ ((v i)) . bodies)
;       ((lambda (v) . bodies) i))))

(define-syntax let*
  (syntax-rules ()
    ((_ () . bodies) (begin . bodies))
    ((_ ((v i) . rest) . bodies)
      ((lambda (v) (let* rest . bodies)) i))))
;    ((_ . args) (glet* (let let* letrec lambda) . args))))

;(display (let* ((foo 2)) (list foo (mm "mm"))))

; (display
;   (let* ((foo 2) 
; 	  (i 3)
; 	  (foo 4) 
; 	  (ft (lambda () (mm "mm"))) ; will capture binding of foo to 4
; 	  (foo 5)
; 	  (ft1 (lambda (foo) (mm "mm"))) ; will  capture the arg of ft1
; 	  (foo 6))
;     (list foo (mm "mm") (ft) (ft1 7) '(mm "mm"))))
(newline)
; ==> (6 6 4 7 (mm))


