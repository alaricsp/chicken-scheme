; Mode: Scheme
;
;
; **********************************************************************
; Copyright (c) 1992 Xerox Corporation.  
; All Rights Reserved.  
;
; Use, reproduction, and preparation of derivative works are permitted.
; Any copy of this software or of any derivative work must include the
; above copyright notice of Xerox Corporation, this paragraph and the
; one after it.  Any distribution of this software or derivative works
; must comply with all applicable United States export control laws.
;
; This software is made available AS IS, and XEROX CORPORATION DISCLAIMS
; ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
; PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
; LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
; EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
; NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGES.
; **********************************************************************
;
; Some simple examples of using Tiny CLOS and its MOP.
; 
; Much of this stuff corresponds to stuff in AMOP (The Art of the
; Metaobject Protocol).
;
; [felix] Changed to reflect Chicken's syntax


(declare (uses tinyclos))

(define getl
    (lambda (initargs name . not-found)
      (letrec ((scan (lambda (tail)
		       (cond ((null? tail)
			      (if (pair? not-found)
				  (car not-found)
				  (error "GETL couldn't find" name)))
			     ((eq? (car tail) name) (cadr tail))
			     (else (scan (cddr tail)))))))
	(scan initargs))))

(define filter-in
    (lambda (f l)
      (cond ((null? l) '())
	    ((f (car l)) (cons (car l) (filter-in f (cdr l))))
	    (else (filter-in f (cdr l))))))


;***
;
; A simple class, just an instance of <class>.  Note that we are using
; make and <class> rather than make-class to make it.  See Section 2.4
; of AMOP for more on this.
;
;

(define-class <pos> () (x y))

(define-method (initialize (pos <pos>) initargs)
  (call-next-method)
  (initialize-slots pos initargs))

(define p1 (make <pos> 'x 1 'y 2))
(define p2 (make <pos> 'x 3 'y 5))


;***
;
; Another way of writing that class definition, that achives better
; `encapsulation' by using slot names that are unique keys, rather
; than symbols.
;
;

(define <pos> #f)

(let ((x (vector 'x))
      (y (vector 'y)))

  (set! <pos> (make <class> 'name '<pos> 'direct-supers (list <object>) 'direct-slots (list x y)))

  (define-method (pos-x (pos <pos>)) (slot-ref pos x))
  (define-method (pos-y (pos <pos>)) (slot-ref pos y))

  (define-method (move (pos <pos>) new-x new-y)
    (slot-set! pos x new-x)
    (slot-set! pos y new-y))

  (define-method (initialize (pos <pos>) initargs)
    (move pos (getl initargs 'x 0) (getl initargs 'y 0)))
  )


(define p3 (make <pos> 'x 1 'y 2))
(define p4 (make <pos> 'x 3 'y 5))


;***
;
; Class allocated slots.
;
; In Scheme, this extension isn't worth a whole lot, but what the hell.
;
;

(define-class <class-slots-class> (<class>) ())

(define-method (compute-getter-and-setter (class <class-slots-class>) slot allocator)
  (if (not (memq ':class-allocation slot))
      (call-next-method)
      (let ((cell '()))
	(values (lambda (o) cell)
		(lambda (o new) (set! cell new) new)))))


;
; Here's a silly program that uses class allocated slots.
;
;
(define-class <ship> () (name (all-ships :class-allocation)) <class-slots-class>)

(define-method (initialize (ship <ship>) initargs)
  (call-next-method)
  (initialize-slots ship initargs)
  (slot-set! ship
	     'all-ships
	     (cons ship (slot-ref ship 'all-ships))))

(define-method (siblings (ship <ship>))
  (remove ship (slot-ref ship 'all-ships)))

(define s1 (make <ship> 'name 's1))
(define s2 (make <ship> 'name 's2))
(define s3 (make <ship> 'name 's3))

(assert (= 3 (length (slot-ref s1 'all-ships))))


;***
;
; Here's a class of class that allocates some slots dynamically.
;
; It has a layered protocol (dynamic-slot?) that decides whether a given
; slot should be dynamically allocated.  This makes it easy to define a
; subclass that allocates all its slots dynamically.
;
;
(define-class <dynamic-class> (<class>) (alist-g-n-s))

(define-method (dynamic-slot? (class <dynamic-class>) slot)
  (memq ':dynamic-allocation (cdr slot)))

(define alist-getter-and-setter
  (lambda (dynamic-class allocator)
    (let ((old (slot-ref dynamic-class 'alist-g-n-s)))
      (if (eq? old (void))
	  (let ([new (call-with-values (lambda () (allocator (lambda () (void)))) cons)])
	    (slot-set! dynamic-class 'alist-g-n-s new)
	    new)
	  old))))

(define-method (compute-getter-and-setter (class <dynamic-class>) slot allocator)
  (if (not (dynamic-slot? class slot))
      (call-next-method)
      (let* ((name (car slot))
	     (g-n-s (alist-getter-and-setter class allocator))
	     (alist-getter (car g-n-s))
	     (alist-setter (cdr g-n-s)))
	(values (lambda (o)
		  (let ((entry (assq name  (alist-getter o))))
		    (if (not entry)
			#f
			(cdr entry))))
		(lambda (o new)
		  (let* ((alist (alist-getter o))
			 (entry (assq name alist)))
		    (if (not entry)
			(alist-setter o
				      (cons (cons name new) alist))
			(set-cdr! entry new))
		    new))))))

(define-class <all-dynamic-class> (<dynamic-class>) ())

(define-method (dynamic-slot? (class <all-dynamic-class>) slot) #t)
	    


;
; A silly program that uses this.
;
;

(define-class <person> () (name age address) <all-dynamic-class>)

(define-method (initialize (person <person>) initargs)
  (initialize-slots person initargs))

(define person1 (make <person> 'name 'sally))
(define person2 (make <person> 'name 'betty))
(define person3 (make <person> 'name 'sue))


;***
;
; A ``database'' class that stores slots externally.
;
;

(define-class <db-class> (<class>) (id-g-n-s))

(define id-getter-and-setter
  (lambda (db-class allocator)
    (let ((old (slot-ref db-class 'id-g-n-s)))
      (if (eq? old (void))
	  (let ((new (call-with-values (lambda () (allocator db-allocate-id)) cons)))
	    (slot-set! class 'id-g-n-s new)
	    new)
	  old))))

(define-method (compute-getter-and-setter (class <db-class>) slot allocator)
  (let* ((id-g-n-s (id-getter-and-setter class allocator))
	 (id-getter (car id-g-n-s))
	 (id-setter (cdr id-g-n-s))
	 (slot-name (car slot)))
    (values (lambda (o)
	      (db-lookup (id-getter o) slot-name)) 
	    (lambda (o new)
	      (db-store  (id-getter o) slot-name new)))))


;***
;
; A kind of generic that supports around methods.
;
;
(define make-around-generic
    (lambda () (make <around-generic>)))

(define make-around-method
    (lambda (specializers procedure)
      (make <around-method>
	    'specializers specializers
	    'procedure procedure)))


(define <around-generic> (make <entity-class>
			   'direct-supers (list <generic>)))

(define <around-method> (make <class>
			  'direct-supers (list <method>)))


(define-method (around-method? (x <method>)) #f)
(define-method (around-method? (x <around-method>)) #t)

(define-method (compute-methods (generic <around-generic>))
  (let ((normal-compute-methods (call-next-method)))
    (lambda (args)
      (let ((normal-methods (normal-compute-methods args)))
	(append
	 (filter-in around-method?
		    normal-methods)
	 (filter-in (lambda (m) (not (around-method? m)))
		    normal-methods))))))


;
; And a simple example of using it.
;
;

(define-class <baz> () ())
(define-class <bar> (<baz>) ())
(define-class <foo> (<bar>) ())

(define test-around
  (lambda (generic)
    (add-method generic
      (make-method        (list <foo>)
	(lambda (cnm x) (cons 'foo (cnm)))))

    (add-method generic
      (make-around-method (list <bar>)
			  (lambda (cnm x) (cons 'bar (cnm)))))

    (add-method generic
      (make-method        (list <baz>)
	(lambda (cnm x) '(baz))))

    (generic (make <foo>))))

(assert (equal? (test-around (make-generic))        '(foo bar baz)))
(assert (equal? (test-around (make-around-generic)) '(bar foo baz)))
