;;;; collect cross-module inlining candidates


(define (node->sexpr n #!optional (port (current-output-port)))
  (let walk ((n n))
    `(,(node-class n)
      ,(node-parameters n)
      ,@(map walk (node-subexpressions n)))))


(define (collect-cmi-candidates node db)
  (let ((collected '()))
    (define (exported? var)
      (and (##compiler#get db var 'global)
	   (or (memq var ##compiler#export-list)
	       (not (memq var ##compiler#block-globals)))))
    (define (walk n e le dest)
      (let ((params (node-parameters n))
	    (subs (node-subexpressions n)))
	(case (node-class n)
	  ((##core#variable) 
	   (let ((var (car params)))
	     (cond ((memq var e) #t)
		   ((memq var le) 1)	; references lexical
		   ((exported? var) #t)
		   (else #f))))
	  ((let)
	   (and (walk (car subs) e le #f)
		(walk (cadr subs) (cons (car params) e) le dest)))
	  ((##core#lambda)
	   (##compiler#decompose-lambda-list
	    (third params)
	    (lambda (vars argc rest)
	      (cond ((walk (car subs) vars e #f) =>
		     (lambda (r)
		       ;; if lambda doesn't refer to outer lexicals, collect
		       (when (and dest (not (eq? 1 r)))
			 (set! collected (alist-cons dest n collected)))
		       #t))
		    (else #f)))))
	  ((set!)
	   (let ((var (car params)))
	     (walk (car subs) e le (and (exported? var) var))))
	  ((##core#callunit) #f)
	  ((if)
	   (and (walk (first subs) e le #f)
		(walk (second subs) e le dest)
		(walk (third subs) e le dest)))
	  (else (every (cut walk <> e le #f) subs)))))
    (walk node '() '() #f)
    (for-each 
     (lambda (p)
       (display "#,")
       (pp `(node ,(car p) ,(node->sexpr (cdr p))))
       (newline))
     collected)))

(user-post-optimization-pass collect-cmi-candidates)
