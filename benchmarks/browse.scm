;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File:         browse.sc
;;; Description:  The BROWSE benchmark from the Gabriel tests
;;; Author:       Richard Gabriel
;;; Created:      8-Apr-85
;;; Modified:     14-Jun-85 18:44:49 (Bob Shaw)
;;;               16-Aug-87 (Will Clinger)
;;;               22-Jan-88 (Will Clinger)
;;;               24-Mar-96 (Qobi)
;;;               31-Mar-98 (Qobi)
;;;               26-Mar-00 (flw)
;;; Language:     Scheme (but see notes below)
;;; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Note:  This benchmark has been run only in implementations in which
;;; the empty list is the same as #f, and may not work if that is not true.
;;; Note:  This benchmark uses property lists.  The procedures that must
;;; be supplied are get and put, where (put x y z) is equivalent to Common
;;; Lisp's (setf (get x y) z).
;;; Note:  The Common Lisp version assumes that eq works on characters,
;;; which is not a portable assumption but is true in most implementations.
;;; This translation makes the same assumption about eq?.
;;; Note:  The gensym procedure was left as in Common Lisp.  Most Scheme
;;; implementations have something similar internally.
;;; Note:  The original benchmark took the car or cdr of the empty list
;;; 14,600 times.  Before explicit tests were added to protect the offending
;;; calls to car and cdr, MacScheme was spending a quarter of its run time
;;; in the exception handler recovering from those errors.

; 11/07/00 - felix:
;
; - Renamed 'match' to 'bmatch', because there exists a macro-definition of
;   'match' already.

;;; The next few definitions should be omitted if the Scheme implementation
;;; already provides them.

(define (append! x y)
  (if (null? x)
      y
      (do ((a x b) (b (cdr x) (cdr b))) ((null? b) (set-cdr! a y) x))))

(define (copy-tree x)
  (if (not (pair? x)) x (cons (copy-tree (car x)) (copy-tree (cdr x)))))


;;; BROWSE -- Benchmark to create and browse through
;;; an AI-like data base of units.

;;; n is # of symbols
;;; m is maximum amount of stuff on the plist
;;; npats is the number of basic patterns on the unit
;;; ipats is the instantiated copies of the patterns

(define *rand* 21)

(define (init n m npats ipats)
  (let ((ipats (copy-tree ipats)))
    (do ((p ipats (cdr p))) ((null? (cdr p)) (set-cdr! p ipats)))
    (do ((n n (- n 1))
	 (i m (cond ((zero? i) m) (else (- i 1))))
	 (name (gensym) (gensym))
	 (a '()))
	((= n 0) a)
      (set! a (cons name a))
      (do ((i i (- i 1))) ((zero? i)) (put name (gensym) #f))
      (put name
	   'pattern
	   (do ((i npats (- i 1)) (ipats ipats (cdr ipats)) (a '()))
	       ((zero? i) a)
	     (set! a (cons (car ipats) a))))
      (do ((j (- m i) (- j 1))) ((zero? j)) (put name (gensym) #f)))))

(define (browse-random)
  (set! *rand* (remainder (* *rand* 17) 251))
  *rand*)

(define (randomize l)
  (do ((a '())) ((null? l) a)
   (let ((n (remainder (browse-random) (length l))))
    (cond ((zero? n)
	   (set! a (cons (car l) a))
	   (set! l (cdr l))
	   l)
	  (else (do ((n n (- n 1)) (x l (cdr x)))
		  ((= n 1)
		   (set! a (cons (cadr x) a))
		   (set-cdr! x (cddr x))
		   x)))))))

(define (bmatch pat dat alist)
  (cond ((null? pat) (null? dat))
	((null? dat) #f)		;Qobi: used to depend on () being false
	((or (eq? (car pat) '?) (eq? (car pat) (car dat)))
	 (bmatch (cdr pat) (cdr dat) alist))
	((eq? (car pat) '*)
	 (or (bmatch (cdr pat) dat alist)
	     (bmatch (cdr pat) (cdr dat) alist)
	     (bmatch pat (cdr dat) alist)))
	(else (cond ((not (pair? (car pat)))
		     (cond ((eq? (string-ref (symbol->string (car pat)) 0) #\?)
			    (let ((val (assv (car pat) alist)))
			     (cond (val (bmatch (cons (cdr val) (cdr pat))
					       dat alist))
				   (else (bmatch (cdr pat)
						(cdr dat)
						(cons (cons (car pat)
							    (car dat))
						      alist))))))
			   ((eq? (string-ref (symbol->string (car pat)) 0) #\*)
			    (let ((val (assv (car pat) alist)))
			     (cond (val (bmatch (append (cdr val) (cdr pat))
					       dat alist))
				   (else
				    (do ((l '()
					    (append! l
						     (cons (if (null? d)
							       '()
							       (car d))
							   '())))
					 (e (cons '() dat) (cdr e))
					 (d dat (if (null? d) '() (cdr d))))
				      ((or (null? e)
					   (bmatch (cdr pat)
						  d
						  (cons (cons (car pat) l)
							alist)))
				       (if (null? e) #f #t)))))))
			   ;; Qobi: used to depend of missing ELSE returning #F
			   (else #f)))
		    (else (and (pair? (car dat))
			       (bmatch (car pat) (car dat) alist)
			       (bmatch (cdr pat) (cdr dat) alist)))))))

(define (browse)
  (investigate (randomize (init 100 10 4 '((a a a b b b b a a a a a b b a a a)
					   (a a b b b b a a (a a) (b b))
					   (a a a b (b a) b a b a))))
	       '((*a ?b *b ?b a *a a *b *a)
		 (*a *b *b *a (*a) (*b))
		 (? ? * (b a) * ? ?))))

(define (investigate units pats)
  (do ((units units (cdr units))) ((null? units))
   (do ((pats pats (cdr pats))) ((null? pats))
    (do ((p (get (car units) 'pattern) (cdr p))) ((null? p))
     (bmatch (car pats) (car p) '())))))


(time (browse))
