;;;; tinyclos.scm - A port of Kiczales TinyCLOS to Chicken - felix
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
;
; [felix] I have stolen several optimizations from Eli Barzilay's Swindle code - thanks anyway!
       

#{tinyclos
  every1 every2 getl filter-in ensure-generic add-global-method
  compute-std-cpl top-sort std-tie-breaker build-transitive-closure build-constraints
  %allocate-instance %allocate-entity get-field set-field! lookup-slot-info the-slots-of-a-class
  getters-n-setters-for-class generic-invocation-generics applicable? more-specific? 
  make-primitive-class entity-tag method-cache-tag method-cache-lookup make-method-cache
  hash-arg-list method-caching-enabled make-instance-from-pointer
  quick-getl symbol-vector}


(include "parameters")

(declare
  (unit tinyclos)
  (uses extras)
  (usual-integrations)
  (run-time-macros)
  (fixnum) 
  (disable-interrupts)
  (hide every1 every2 every2-eq? method-caching-enabled method-cache-tag symbol-vector ##tinyclos#slot-ref
	symbol-vector getl make-method-cache set-field! method-cache-lookup %allocate-entity
	hash-arg-list std-tie-breaker build-constraints %allocate-instance 
	get-field ensure-generic ##tinyclos#slot-set! lookup-slot-info
	getters-n-setters-for-class the-slots-of-a-class make-primitive-class quick-getl
	build-transitive-closure applicable? more-specific? method-cache-tag top-sort
	filter-in) )

(cond-expand
 ((not unsafe)
  (declare (emit-exports "tinyclos.exports")) )
 (else) )

(cond-expand
 [paranoia]
 [else
  (declare
    (no-bound-checks)
    (no-procedure-checks-for-usual-bindings)
    (bound-to-procedure
     every1 every2 getl filter-in ensure-generic add-global-method gensym make make-generic
     make-method compute-apply-methods compute-methods add-method class-of compute-method-morre-specific?
     call-next-method ##sys#symbol->string compute-cpl compute-slots compute-getter-and-setter
     fprintf slot-ref subclass? slot-set! allocate-instance compute-apply-generic class-slots
     class-direct-supers port? input-port? ##sys#bytevector?
     compute-std-cpl top-sort std-tie-breaker build-transitive-closure build-constraints
     %allocate-instance %allocate-entity get-field set-field! lookup-slot-info
     applicable? more-specific? ##sys#error ##sys#signal-hook
     make-primitive-class method-cache-lookup make-method-cache
     method-cache-update! hash-arg-list
     quick-getl) ) ] )


;
; A very simple CLOS-like language, embedded in Scheme, with a simple
; MOP.  The features of the default base language are:
;
;   * Classes, with instance slots, but no slot options.
;   * Multiple-inheritance.
;   * Generic functions with multi-methods and class specializers only.
;   * Primary methods and call-next-method; no other method combination.
;   * Uses Scheme's lexical scoping facilities as the class and generic
;     function naming mechanism.  Another way of saying this is that
;     class, generic function and methods are first-class (meta)objects.
;
; While the MOP is simple, it is essentially equal in power to both MOPs
; in AMOP.  This implementation is not at all optimized, but the MOP is
; designed so that it can be optimized.  In fact, this MOP allows better
; optimization of slot access extenstions than those in AMOP.
; 
;
;
; In addition to calling a generic, the entry points to the default base
; language are:
;
;   (MAKE-CLASS list-of-superclasses list-of-slot-names)
;   (MAKE-GENERIC)
;   (MAKE-METHOD list-of-specializers procedure)
;   (ADD-METHOD generic method)
;
;   (MAKE class . initargs)
;   (INITIALIZE instance initargs)            ;Add methods to this,
;                                             ;don't call it directly.
;   
;   (SLOT-REF  object slot-name)
;   (SLOT-SET! object slot-name new-value)
;
;
; So, for example, one might do:
;
;   (define <position> (make-class (list <object>) (list 'x 'y)))
;   (add-method initialize
;       (make-method (list <position>)
;         (lambda (call-next-method pos initargs)
;           (for-each (lambda (initarg-name slot-name)
;                       (##tinyclos#slot-set! pos
;                                  slot-name
;                                  (getl initargs initarg-name 0)))
;                     '(x y)
;                     '(x y)))))
;
;   (set! p1 (make <position> 'x 1 'y 3))
;
;
;
; NOTE!  Do not use EQUAL? to compare objects!  Use EQ? or some hand
;        written procedure.  Objects have a pointer to their class,
;        and classes are circular structures, and ...
;
;
;
; The introspective part of the MOP looks like the following.  Note that
; these are ordinary procedures, not generics.
;
;   CLASS-OF
;
;   CLASS-DIRECT-SUPERS
;   CLASS-DIRECT-SLOTS
;   CLASS-CPL
;   CLASS-SLOTS
;
;   GENERIC-METHODS
;
;   METHOD-SPECIALIZERS
;   METHOD-PROCEDURE
;
;
; The intercessory protocol looks like (generics in uppercase):
;
;   make                        
;     ALLOCATE-INSTANCE
;     INITIALIZE                   (really a base-level generic)
;
;   class initialization
;     COMPUTE-CPL
;     COMPUTE-SLOTS
;     COMPUTE-GETTER-AND-SETTER
;
;   add-method                     (Notice this is not a generic!)
;     COMPUTE-APPLY-GENERIC
;       COMPUTE-METHODS
;         COMPUTE-METHOD-MORE-SPECIFIC?
;       COMPUTE-APPLY-METHODS
;


(register-feature! 'tinyclos)


;;; Macros

(define-macro (define-class name supers slots . meta)
  (##sys#check-syntax 'define-class name 'symbol)
  (##sys#check-syntax 'define-class supers '#(_ 0))
  (##sys#check-syntax 'define-class slots '#(_ 0))
  (##sys#check-syntax 'define-class meta '#(_ 0 1))
  `(##core#set! ,name
     (make ,(if (pair? meta) (##sys#slot meta 0) '<class>)
       'name ',name
       'direct-supers (list ,@(if (null? supers) '(<object>) supers))
       'direct-slots (list ,@(map (lambda (s) `',s) slots)) ) ) )

(define-macro (define-generic name . class)
  (##sys#check-syntax 'define-generic name 'symbol)
  `(define ,name (make ,(:optional class '<generic>) 'name ',name)) )

(define-macro (define-method head . body)
  (##sys#check-syntax 'define-method head '(symbol . _))
  (##sys#check-syntax 'define-method body '#(_ 1))
  (let gather ([args (##sys#slot head 1)]
	       [specs '()]
	       [vars '()] )
    (if (or (not (pair? args)) 
	    (memq (car args) '(#!optional #!key #!rest)) )
	(let ([name (##sys#slot head 0)])
	  `(##core#set! ,name
			(##tinyclos#add-global-method
			 (##core#global-ref ,name)
			 ',name
			 (list ,@(reverse specs))
			 (##core#named-lambda ,name (call-next-method ,@(reverse vars) ,@args) ,@body) ) ) )
	(let ([arg (##sys#slot args 0)])
	  (gather (##sys#slot args 1)
		  (cons (if (pair? arg) (cadr arg) '<top>) specs)
		  (cons (if (pair? arg) (car arg) arg) vars) ) ) ) ) )


;;; Support code

(define (every1 test lst)
  (let loop ([lst lst])
    (or (null? lst)
	(and (test (##sys#slot lst 0))
	     (loop (##sys#slot lst 1)) ) ) ) )

(define (every2 test list1 list2)
  (let loop ([list1 list1] [list2 list2])
    (or (null? list1)
	(null? list2)
	(and (test (##sys#slot list1 0) (##sys#slot list2 0))
	     (loop (##sys#slot list1 1) (##sys#slot list2 1)) ) ) ) )

(define quick-getl
  (foreign-lambda* scheme-object ((scheme-object initargs) (scheme-object name) (scheme-object def)) "
    while(initargs != C_SCHEME_END_OF_LIST) {
      if(name == C_block_item(initargs, 0)) {
        if((initargs = C_block_item(initargs, 1)) == C_SCHEME_END_OF_LIST) return(def);
        else return(C_block_item(initargs, 0));
      }
      initargs = C_block_item(initargs, 1);
    }
    return(def);") )

(define getl
  (lambda (initargs name . not-found)
    (letrec ((scan (lambda (tail)
		     (cond ((null? tail)
			    (if (pair? not-found)
				(car not-found)
				(##sys#error 'getl "couldn't find item" name initargs)))
			   ((eq? (car tail) name) (cadr tail))
			   (else (scan (cddr tail)))))))
      (scan initargs))))

(define (filter-in f l)
  (if (null? l)
      '()
      (let ([h (##sys#slot l 0)]
	    [r (##sys#slot l 1)] )
	(if (f h)
	    (cons h (filter-in f r))
	    (filter-in f r) ) ) ) )


;;; Method cache support code:

(define method-caching-enabled #f)
(define method-cache-tag #f)

(define (make-method-cache)
  (cons method-cache-tag (make-vector (arithmetic-shift method-cache-size 1) #f)) )

(define method-cache-lookup
  (foreign-lambda* scheme-object ((scheme-object mcache) (scheme-object hash) (scheme-object classes)) "
    C_word v = C_block_item(mcache, 1);
    C_word clist, x, y;
    int free_index = -1;
    int i = ((C_unfix(hash) & (C_METHOD_CACHE_SIZE - 1)) << 1) & 0xffff,
        i0, i2;
    for(i0 = i;; i = i2) {
      clist = C_block_item(v, i);
      if(clist != C_SCHEME_FALSE) {
        x = classes;
        y = clist;
        while(x != C_SCHEME_END_OF_LIST && y != C_SCHEME_END_OF_LIST) {
          if(C_block_item(x, 0) != C_block_item(y, 0)) goto mismatch;
          else {
            x = C_block_item(x, 1);
            y = C_block_item(y, 1);
          }
        }
        if (x == C_SCHEME_END_OF_LIST && y == C_SCHEME_END_OF_LIST)
          return(C_block_item(v, i + 1));
        else
          goto mismatch;
      }
      else if(free_index == -1) free_index = i;
    mismatch:
      i2 = (i + 2) & ((C_METHOD_CACHE_SIZE << 1) - 1);
      if(i2 == i0) return(free_index == -1 ? C_SCHEME_FALSE : C_fix(free_index));
    }") )


;
; A simple topological sort.
;
; This is a fairly modified version of code I originally got from Anurag
; Mendhekar <anurag@moose.cs.indiana.edu>.
;

(define compute-std-cpl
    (lambda (c get-direct-supers)
      (top-sort ((build-transitive-closure get-direct-supers) c)
		((build-constraints get-direct-supers) c)
		(std-tie-breaker get-direct-supers))))


(define top-sort
    (lambda (elements constraints tie-breaker)
      (let loop ((elements    elements)
		 (constraints constraints)
		 (result      '()))
	(if (null? elements)
	    result
	    (let ((can-go-in-now
		    (filter-in
		      (lambda (x)
			(every1 (lambda (constraint)
				 (or (not (eq? (cadr constraint) x))
				     (memq (car constraint) result)))
			       constraints))
		      elements)))
	      (if (null? can-go-in-now)
		  (##sys#error 'top-sort "invalid constraints")
		  (let ((choice (if (null? (cdr can-go-in-now))
				    (car can-go-in-now)
				    (tie-breaker result
						 can-go-in-now))))
		    (loop
		      (filter-in (lambda (x) (not (eq? x choice)))
			         elements)
		     ;(filter-in (lambda (x) (not (eq? (cadr x) choice)))
		     ;           constraints)
		      constraints
		      (append result (list choice))))))))))

(define std-tie-breaker
    (lambda (get-supers)
      (lambda (partial-cpl min-elts)
	(let loop ((pcpl (reverse partial-cpl)))
	     (let ((current-elt (car pcpl)))
	       (let ((ds-of-ce (get-supers current-elt)))
		 (let ((common (filter-in (lambda (x)
					    (memq x ds-of-ce))
					  min-elts)))
		   (if (null? common)
		       (if (null? (cdr pcpl))
			   (##sys#error 'std-tie-breaker "nothing valid")
			   (loop (cdr pcpl)))
		       (car common)))))))))


(define build-transitive-closure
    (lambda (get-follow-ons)
      (lambda (x)
	(let track ((result '())
		    (pending (list x)))
	     (if (null? pending)
		 result
		 (let ((next (car pending)))
		   (if (memq next result)
		       (track result (cdr pending))
		       (track (cons next result)
			      (append (get-follow-ons next)
				      (cdr pending))))))))))

(define build-constraints
  (lambda (get-follow-ons)
    (lambda (x)
      (let loop ((elements ((build-transitive-closure get-follow-ons) x))
		 (this-one '())
		 (result '()))
	   (if (or (null? this-one) (null? (cdr this-one)))
	       (if (null? elements)
		   result
		   (loop (cdr elements)
			 (cons (car elements)
			       (get-follow-ons (car elements)))
			 result))
	       (loop elements
		     (cdr this-one)
		     (cons (list (car this-one) (cadr this-one))
			   result)))))))

;
; Then, we need to build what, in a more real implementation, would be
; the interface to the memory subsystem: instances and entities.  The
; former are used for instances of instances of <class>; the latter
; are used for instances of instances of <entity-class>.  In this MOP,
; none of this is visible to base- or MOP-level programmers.
;
; (One might consider rewriting the definition of instances using
; define-record or something.  It doesn't turn out to make it much
; simpler, at least not to me.  It also breaks the nice parallelism
; with entities.)
;

;
; instances
;
;

(define (%allocate-instance class nfields)
  (let ((instance (make-vector (+ nfields 3) #f)))
    (##core#inline "C_vector_to_structure" instance)
    (##sys#setslot instance 0 'instance)
    (##sys#setslot instance 1 class)
    instance))

(define-inline (%instance? x) (##sys#structure? x 'instance))
(define-inline (%instance-class instance) (##sys#slot instance 1))
(define-inline (%set-instance-class! instance new-value) (##sys#setslot instance 1 new-value))
(define-inline (%instance-ref instance index) (##sys#slot instance (+ index 3)))
(define-inline (%instance-set! instance index new-value) (##sys#setslot instance (+ index 3) new-value))
(define-inline (%instance-cache-ref instance) (##sys#slot instance 2))
(define-inline (%instance-cache-set! instance x) (##sys#setslot instance 2 x))

(define-record-printer (instance x out)
  (print-object x out) )

(define entity-tag (gensym))

(let ([default-proc
	(lambda args
	  (##sys#error '%allocate-entity "called entity without first setting proc"))] )
  (set! %allocate-entity 
    (lambda (class nfields name)
      (letrec ((vector (make-vector (+ nfields 5) #f))
	       (closure (lambda args (apply (##sys#slot vector 1) args))))
	(##core#inline "C_vector_to_structure" vector)
	(##sys#setslot vector 0 'entity)
	(##sys#setslot vector 1 default-proc)
	(##sys#setslot vector 2 class)
	(##sys#setslot vector 3 name)
	;; slot #4 is cache (defaults to #f)
	(let ([len (##sys#size closure)])
	  (let* ([len2 (+ len 2)]
		 [p2 (make-vector len2)] )
	    (do ([i 1 (+ i 1)])
		((>= i len)
		 (##sys#setslot p2 i vector)
		 (##sys#setslot p2 (add1 i) entity-tag)
		 (##core#inline "C_vector_to_closure" p2)
		 (##core#inline "C_copy_pointer" closure p2)
		 p2)
	      (##sys#setslot p2 i (##sys#slot closure i)) ) ) ) ) ) ) )

(define-inline (%entity? x)
  (and (procedure? x)
       (let ([len (##sys#size x)])
	 (and (> len 3)
	      (eq? entity-tag (##sys#slot x (- len 1))) ) ) ) )

(define-inline (%entity-class closure)
  (##sys#slot (##sys#slot closure (- (##sys#size closure) 2)) 2) )

(define-inline (%set-entity-proc! closure proc)
  (##sys#setslot (##sys#slot closure (- (##sys#size closure) 2)) 1 proc) )

(define-inline (%entity-cache-ref closure)
  (##sys#slot (##sys#slot closure (- (##sys#size closure) 2)) 4) )
	
(define-inline (%entity-cache-set! closure x)
  (##sys#setslot (##sys#slot closure (- (##sys#size closure) 2)) 4 x) )

(define-inline (%entity-name closure)
  (##sys#slot (##sys#slot closure (- (##sys#size closure) 2)) 3) )

(define-inline (%set-entity-name! closure x)
  (##sys#setslot (##sys#slot closure (- (##sys#size closure) 2)) 3 x) )

(define-inline (%entity-ref closure index)
  (##sys#slot (##sys#slot closure (- (##sys#size closure) 2)) (+ index 5)) )
		  
(define-inline (%entity-set! closure index new-value)
  (##sys#setslot (##sys#slot closure (- (##sys#size closure) 2)) (+ index 5) new-value) )

(define-record-printer (entity x out)
  (print-object x out) )


;;; Compute class ID from object:

(define symbol-vector (vector 'instance entity-tag))

(define hash-arg-list
  (foreign-lambda* unsigned-int ((scheme-object args) (scheme-object svector)) "
    C_word tag, h, x;
    int n, i, j, len = 0;
    for(i = 0; args != C_SCHEME_END_OF_LIST; args = C_block_item(args, 1)) {
      x = C_block_item(args, 0);
      if(C_immediatep(x)) {
        switch(x) {
          case C_SCHEME_END_OF_LIST: i += 1; break;
          case C_SCHEME_TRUE:
          case C_SCHEME_FALSE: i += 3; break;
          case C_SCHEME_END_OF_FILE: i += 7; break;
          case C_SCHEME_UNDEFINED: i += 5; break;
          default:
            if(x & C_FIXNUM_BIT) i += 2;
            else i += 4;
        }
      }
      else {
        h = C_header_bits(x);
        switch(h) {
        case C_STRUCTURE_TYPE:
          tag = C_block_item(x, 0);
          if(tag == C_block_item(svector, 0)) { /* instance */
            if((tag = C_block_item(C_block_item(x, 1), 2)) != C_SCHEME_FALSE) i += C_unfix(tag);
            else i += C_header_size(x) << 4;
          }
          else i += 17;
          break;
        case C_CLOSURE_TYPE:
          n = C_header_size(x);
          if(n > 3 && C_block_item(svector, 1) == C_block_item(x, n - 1)) {
            if((tag = C_block_item(C_block_item(C_block_item(x, n - 2), 2), 2)) != C_SCHEME_FALSE) i += C_unfix(tag);
            else i += 13;
          }
          break;
        case C_SYMBOL_TYPE: i += 8; break;
        case C_BYTEVECTOR_TYPE: i += 16; break;
        case C_VECTOR_TYPE: i += 9; break;
        case C_PAIR_TYPE: i += 10; break;
        case C_FLONUM_TYPE: i += 11; break;
        case C_STRING_TYPE: i += 12; break;
        case C_PORT_TYPE: i += C_block_item(x, 1) ? 15 : 14; break;
        default: i += 255;
        }
      }
      ++len;
    }
    return((i + len) & (C_METHOD_CACHE_SIZE - 1));") )


;
; These next three, plus %allocate-instance and %allocate-entity, are
; the normal interface, from the rest of the code, to the low-level
; memory system.  One thing to take note of is that the protocol does
; not allow the user to add low-level instance representations.  I
; have never seen a way to make that work.
;
; Note that this implementation of class-of assumes the name of a the
; primitive classes that are set up later.
; 
(define (class-of x)
  (cond [(null? x)       <null>]
	[(fixnum? x)     <exact>]
	[(boolean? x)    <boolean>]
	[(char? x)       <char>]
	[(eq? x (##core#undefined)) <void>]
	[(eof-object? x) <end-of-file>]
	[(%instance? x)  (%instance-class x)]
	[(%entity? x)    (%entity-class x)]
	[(symbol? x)     <symbol>]
	[(vector? x)     <vector>]
	[(pair? x)       <pair>]
	[(number? x)     (if (integer? x) <integer> <inexact>)]
	[(string? x)     <string>]
	[(procedure? x)  <procedure>]
	[(port? x) (if (input-port? x) <input-port> <output-port>)]
	[(##core#inline "C_pointerp" x) <pointer>]
	[(##core#inline "C_taggedpointerp" x) <tagged-pointer>]
	[(##core#inline "C_swigpointerp" x) <swig-pointer>]
	[(##core#inline "C_locativep" x) <locative>]
	[(##sys#bytevector? x) <byte-vector>]
	[(##sys#generic-structure? x)
	 (case (##sys#slot x 0)
	   [(environment) <environment>]
	   [(array) <array>]
	   [(hash-table) <hash-table>]
	   [(queue) <queue>]
	   [(condition) <condition>]
	   [(condition-variable) <condition-variable>]
	   [(char-set) <char-set>]
	   [(time) <time>]
	   [(lock) <lock>]
	   [(mmap) <mmap>]
	   [(promise) <promise>]
	   [(u8vector) <u8vector>]
	   [(s8vector) <s8vector>]
	   [(u16vector) <u16vector>]
	   [(s16vector) <s16vector>]
	   [(u32vector) <u32vector>]
	   [(s32vector) <s32vector>]
	   [(f32vector) <f32vector>]
	   [(f64vector) <f64vector>]
	   [(tcp-listener) <tcp-listener>]
	   [(thread) <thread>]
	   [(mutex) <mutex>]
	   [(continuation) <continuation>]
	   [(read-table) <read-table>]
	   [(regexp) <regexp>]
	   [else <structure>] ) ]
	[else (##sys#error 'class-of "can not compute class of primitive object" x)] ) )

(define get-field
  (lambda (object field)
    (cond ((%instance? object) (%instance-ref object field))
	  ((%entity?   object) (%entity-ref   object field))
	  (else
	   (##sys#signal-hook #:type-error 'get-field "can only get-field of instances and entities" object)))))

(define set-field!
  (lambda (object field new-value)
    (cond ((%instance? object) (%instance-set! object field new-value))
	  ((%entity?   object) (%entity-set!   object field new-value))
	  (else
	   (##sys#signal-hook #:type-error 'set-field! "can only set-field! of instances and entities" object)))))




;
; Now we can get down to business.  First, we initialize the braid.
;
; For Bootstrapping, we define an early version of MAKE.  It will be
; changed to the real version later on.  String search for ``set! make''.
;

(randomize)

(define make
  (lambda (class . initargs)
    (cond ((or (eq? class <class>)
	       (eq? class <entity-class>))
	   (let* ((new (%allocate-instance
			class
			(length the-slots-of-a-class)))
		  (dsupers (quick-getl initargs 'direct-supers '()))
		  (name (quick-getl initargs 'name "(anonymous)"))
		  (dslots  (map list
				(getl initargs 'direct-slots  '())))
		  (cpl     (let loop ((sups dsupers)
				      (so-far (list new)))
			     (if (null? sups)
				 (reverse so-far)
				 (loop (class-direct-supers
					(car sups))
				       (cons (car sups)
					     so-far)))))
		  (slots (apply append
				dslots
				(map class-direct-slots
				     (cdr cpl))))
		  (nfields 0)
		  (field-initializers '())
		  (allocator
		   (lambda (init)
		     (let ((f nfields))
		       (set! nfields (+ nfields 1))
		       (set! field-initializers
			 (cons init field-initializers))
		       (values (lambda (o)   (get-field  o f))
			       (lambda (o n) (set-field! o f n))))))
		  (getters-n-setters
		   (map (lambda (s)
			  (cons (car s)
				(call-with-values (lambda () (allocator (lambda () (##core#undefined)))) cons) ) )
			slots)))
	     (##tinyclos#slot-set! new 'direct-supers      dsupers)
	     (##tinyclos#slot-set! new 'direct-slots       dslots)
	     (##tinyclos#slot-set! new 'cpl                cpl)
	     (##tinyclos#slot-set! new 'slots              slots)
	     (##tinyclos#slot-set! new 'nfields            nfields)
	     (##tinyclos#slot-set! new 'field-initializers (reverse
							    field-initializers))
	     (##tinyclos#slot-set! new 'getters-n-setters  getters-n-setters)
	     (##tinyclos#slot-set! new 'name name)
	     (%instance-cache-set! new (##core#inline "C_random_fixnum" #x10000))
	     new))
	  ((eq? class <generic>)
	   (let ([new (%allocate-entity 
		       class
		       (length (class-slots class))
		       (quick-getl initargs 'name "(unnamed)") ) ] )
	     (##tinyclos#slot-set! new 'methods '())
	     new))
	  ((eq? class <method>)
	   (let ((new (%allocate-instance
		       class
		       (length (class-slots class)))))
	     (##tinyclos#slot-set! new
				   'specializers
				   (getl initargs 'specializers))
	     (##tinyclos#slot-set! new
				   'procedure
				   (getl initargs 'procedure))
	     new)))))


;
; These are the real versions of slot-ref and slot-set!.  Because of the
; way the new slot access protocol works, with no generic call in line,
; they can be defined up front like this.  Cool eh?
;
;

(define (##tinyclos#slot-ref object slot-name)
  (if (and (%instance? object) (%instance-cache-ref object)) ; if true, then this is an instance of <class>, with no additional slots
      (%instance-ref
       object
       (case slot-name
	 [(getters-n-setters) 6]
	 [(direct-supers) 0]
	 [(direct-slots) 1]
	 [(cpl) 2]
	 [(slots) 3]
	 [(nfields) 4]
	 [(field-initializers) 5]
	 [(name) 7]
	 [else (##sys#error "huh?" slot-name)] ) )
      (let* ((info   (lookup-slot-info (class-of object) slot-name))
	     (getter (##sys#slot info 0)))
	(getter object))) )

(define ##tinyclos#slot-set!
  (lambda (object slot-name new-value)
    (let* ((info   (lookup-slot-info (class-of object) slot-name))
	   (setter (##sys#slot info 1)) )
      (setter object new-value))))

(define slot-ref (getter-with-setter ##tinyclos#slot-ref ##sys#tinyclos#slot-set!))
(define slot-set! ##tinyclos#slot-set!)

(define lookup-slot-info
  (lambda (class slot-name)
    (let* ((getters-n-setters
	    (if (eq? class <class>)	;* This grounds out
		getters-n-setters-for-class ;* the slot-ref tower.
		(##tinyclos#slot-ref class 'getters-n-setters)))
	   (entry (assq slot-name getters-n-setters)))
      (if (not entry)
	  (##sys#error "no slot in instances of class" slot-name class)
	  (##sys#slot entry 1) ) ) ) )


;
; Given that the early version of MAKE is allowed to call accessors on
; class metaobjects, the definitions for them come here, before the
; actual class definitions, which are coming up right afterwards.
;
;
(define class-direct-slots
    (lambda (class) (##tinyclos#slot-ref class 'direct-slots)))
(define class-direct-supers
    (lambda (class) (##tinyclos#slot-ref class 'direct-supers)))
(define class-slots
    (lambda (class) (##tinyclos#slot-ref class 'slots)))

(define (class-name class) (##tinyclos#slot-ref class 'name))

(define generic-methods
    (lambda (generic) (##tinyclos#slot-ref generic 'methods)))

(define method-specializers
    (lambda (method) (##tinyclos#slot-ref method 'specializers)))
(define method-procedure
    (lambda (method) (##tinyclos#slot-ref method 'procedure)))

(define (class-cpl class)
  (##tinyclos#slot-ref class 'cpl) )


;;; Inline procedures inside this module only:

(eval-when (compile)
  (define-macro (class-direct-slots class) `(##tinyclos#slot-ref ,class 'direct-slots))
  (define-macro (class-direct-supers class) `(##tinyclos#slot-ref ,class 'direct-supers))
  (define-macro (class-slots class) `(##tinyclos#slot-ref ,class 'slots))
  (define-macro (class-name class) `(##tinyclos#slot-ref ,class 'name))
  (define-macro (generic-methods generic) `(##tinyclos#slot-ref ,generic 'methods))
  (define-macro (method-specializers method) `(##tinyclos#slot-ref ,method 'specializers))
  (define-macro (method-procedure method) `(##tinyclos#slot-ref ,method 'procedure))
  (define-macro (class-cpl class) `(##tinyclos#slot-ref ,class 'cpl)) )

;
; The next 7 clusters define the 6 initial classes.  It takes 7 to 6
; because the first and fourth both contribute to <class>.
;
(define the-slots-of-a-class		;
  '(direct-supers			;(class ...)        
    direct-slots			;((name . options) ...)
    cpl					;(class ...) 
    slots				;((name . options) ...) 
    nfields				;an integer
    field-initializers			;(proc ...)
    getters-n-setters			;((slot-name getter . setter) ...)
    name) )				;name

(define getters-n-setters-for-class       ;see lookup-slot-info
  (let loop ([lst the-slots-of-a-class] [i 0])
    (if (null? lst)
	'()
	(cons
	 (cons (##sys#slot lst 0)
	       (cons (lambda (o)   (%instance-ref  o i))
		     (lambda (o n) (%instance-set! o i n)) ) )
	 (loop (##sys#slot lst 1) (add1 i)) ) ) ) )

(define <class> (%allocate-instance #f (length the-slots-of-a-class)))
(%set-instance-class! <class> <class>)

(define <top>          (make <class>
			 'name "top"
			     'direct-supers '()
			     'direct-slots  '()))

(define <object>       (make <class>
			 'name "object"
			     'direct-supers (list <top>)
			     'direct-slots  '()))

;
; This cluster, together with the first cluster above that defines
; <class> and sets its class, have the effect of:
;
;   (define <class>
;     (make <class>
;           'direct-supers (list <object>)
;           'direct-slots  (list 'direct-supers ...)))
;
(%instance-set! <class> 0 (list <object>))                  ;d supers
(%instance-set! <class> 1 (map list the-slots-of-a-class))  ;d slots
(%instance-set! <class> 2 (list <class> <object> <top>))    ;cpl
(%instance-set! <class> 3 (map list the-slots-of-a-class))  ;slots
(%instance-set! <class> 4 (length the-slots-of-a-class))    ;nfields
(%instance-set! <class> 5 (map (lambda (s)                  ;field-ini..
				 (lambda () (##core#undefined)))
			       the-slots-of-a-class))
(%instance-set! <class> 6 '())
(%instance-set! <class> 7 'class)
(%instance-cache-set! <class> (##core#inline "C_random_fixnum" #x10000))


(define <procedure-class> (make <class>
			    'name "procedure-class"
				'direct-supers (list <class>)
				'direct-slots  '()))

(define <entity-class>    (make <class>
			    'name "entity-class"
			        'direct-supers (list <procedure-class>)
			        'direct-slots  '()))

(define <generic>         (make <entity-class>
			    'name "generic"
			        'direct-supers (list <object>)
			        'direct-slots  (list 'methods)))

(define <method>          (make <class>
			    'name "method"
			        'direct-supers (list <object>)
			        'direct-slots  (list 'specializers
						     'procedure)))



;
; These are the convenient syntax we expose to the base-level user.
;
;
(define make-class
    (lambda (direct-supers direct-slots)
      (make <class>
	'name "(anonymous)"
	    'direct-supers direct-supers
	    'direct-slots  direct-slots)))

(define make-generic
  (lambda name
    (make <generic> 'name (:optional name "(unnamed)")) ) )

(define make-method
    (lambda (specializers procedure)
      (make <method>
	    'specializers specializers
	    'procedure    procedure)))



;
; The initialization protocol
;
(define initialize (make-generic "initialize"))
	    

;
; The instance structure protocol.
;
(define allocate-instance (make-generic "allocate-instance"))
(define compute-getter-and-setter (make-generic "compute-getter-and-setter"))


;
; The class initialization protocol.
;
(define compute-cpl (make-generic "compute-cpl"))
(define compute-slots (make-generic "compute-slots"))

;
; The generic invocation protocol.
;
(define compute-apply-generic         (make-generic "compute-apply-generic"))
(define compute-methods               (make-generic "compute-methods"))
(define compute-method-more-specific? (make-generic "compute-method-more-specific?"))
(define compute-apply-methods         (make-generic "compute-apply-methods"))




;
; The next thing to do is bootstrap generic functions.
; 
(define generic-invocation-generics (list compute-apply-generic
					  compute-methods
					  compute-method-more-specific?
					  compute-apply-methods))

(define add-method
  (lambda (generic method)
    (##tinyclos#slot-set!
     generic
     'methods
     (let* ([ms1 (method-specializers method)]
	    [l1 (length ms1)] )
       (let filter-in-method ([methods (##tinyclos#slot-ref generic 'methods)])
	 (if (null? methods)
	     (list method)
	     (let* ([mm (##sys#slot methods 0)]
		    [ms2 (method-specializers mm)]
		    [l2 (length ms2)])
	       (cond ((> l1 l2)
		      (cons mm (filter-in-method (##sys#slot methods 1))))
		     ((< l1 l2)
		      (cons method methods))
		     (else
		      (let check-method ([ms1 ms1]
					 [ms2 ms2])
			(cond ((and (null? ms1) (null? ms2))
			       (cons method (##sys#slot methods 1))) ;; skip the method already in the generic
			      ((eq? (##sys#slot ms1 0) (##sys#slot ms2 0))
			       (check-method (##sys#slot ms1 1) (##sys#slot ms2 1)))
			      (else
			       (cons mm (filter-in-method (##sys#slot methods 1)))))))))))))
    (if (memq generic generic-invocation-generics)
	(set! method-cache-tag (vector))
	(%entity-cache-set! generic #f) )
    (%set-entity-proc! generic (compute-apply-generic generic))))

;
; Adding a method calls COMPUTE-APPLY-GENERIC, the result of which calls
; the other generics in the generic invocation protocol.  Two, related,
; problems come up.  A chicken and egg problem and a infinite regress
; problem.
;
; In order to add our first method to COMPUTE-APPLY-GENERIC, we need
; something sitting there, so it can be called.  The first definition
; below does that.
; 
; Then, the second definition solves both the infinite regress and the
; not having enough of the protocol around to build itself problem the
; same way: it special cases invocation of generics in the invocation
; protocol.
;
;

(%set-entity-proc! compute-apply-generic
     (lambda (generic)             ;The ONE time this is called
				   ;it doesn't get cnm.
       (lambda args
	 (apply (method-procedure (car (generic-methods generic)))
		#f args)))) ;But, the ONE time it is run,
				   ;it needs to pass a dummy
				   ;value for cnm!

(add-method compute-apply-generic
  (make-method (list <generic>)
    (lambda (call-next-method generic)
      (lambda args
	(let ([mc (%entity-cache-ref generic)])
	  (when (or (not mc) (not (eq? method-cache-tag (##sys#slot mc 0))))
	    (set! mc (make-method-cache))
	    (%entity-cache-set! generic mc) )
	  (let* ([classes (and method-caching-enabled (map class-of args))]
		 [key (and classes (hash-arg-list args symbol-vector))]
		 [e (and classes (method-cache-lookup mc key classes))] )
;	    (unless (##sys#immediate? e) (print (%entity-name generic) ": " key))
	    (if (not (##sys#immediate? e))
		(e args)
		(let ([cam 
		       (if (and (memq generic generic-invocation-generics)
				(memq (car args) generic-invocation-generics))
			   (let ([proc 
				  (method-procedure
				    ; select the first method of one argument
				   (let lp ([lis (generic-methods generic)])
				     (if (null? lis)
				       (##sys#error "Unable to find original compute-apply-generic")
				       (if (= (length (method-specializers (##sys#slot lis 0))) 1)
					 (##sys#slot lis 0)
					 (lp (##sys#slot lis 1)))))) ] )
			     (lambda (args) (apply proc #f args)) )
			   (let ([x (compute-apply-methods generic)]
				 [y ((compute-methods generic) args)] )
			     (lambda (args) (x y args)) ) ) ] )
		  (when (and e method-caching-enabled)
		    (let ([v (##sys#slot mc 1)])
		      (##sys#setslot v e classes)
		      (##sys#setslot v (add1 e) cam) ) )
		  (cam args) ) ) ) ) ) ) ) )

(add-method compute-methods
  (make-method (list <generic>)
    (lambda (call-next-method generic)
      (lambda (args)
	(let ([applicable
	       (filter-in (lambda (method)
                            (let check-applicable ([list1 (method-specializers method)]
                                                   [list2 args])
                              (cond ((null? list1) #t)
                                    ((null? list2) #f)
                                    (else
                                      (and (applicable? (##sys#slot list1 0) (##sys#slot list2 0))
                                           (check-applicable (##sys#slot list1 1) (##sys#slot list2 1)))))))
			  (generic-methods generic) ) ] )
	  (if (or (null? applicable) (null? (##sys#slot applicable 1))) 
	      applicable
	      (let ([cmms (compute-method-more-specific? generic)])
		(sort applicable (lambda (m1 m2) (cmms m1 m2 args))) ) ) ) ) ) ) )

(add-method compute-method-more-specific?
  (make-method (list <generic>)
    (lambda (call-next-method generic)
      (lambda (m1 m2 args)
	(let loop ((specls1 (method-specializers m1))
		   (specls2 (method-specializers m2))
		   (args args))
	  (cond-expand
	   [unsafe
	    (let ((c1  (##sys#slot specls1 0))
		  (c2  (##sys#slot specls2 0))
		  (arg (##sys#slot args 0)))
	      (if (eq? c1 c2)
		  (loop (##sys#slot specls1 1)
			(##sys#slot specls2 1)
			(##sys#slot args 1))
		  (more-specific? c1 c2 arg))) ] 
	   [else
	    (cond ((and (null? specls1) (null? specls2))
		   (##sys#error "two methods are equally specific" generic))
		  ;((or (null? specls1) (null? specls2))
		  ; (##sys#error "two methods have different number of specializers" generic))
                  ((null? specls1) #f)
                  ((null? specls2) #t)
		  ((null? args)
		   (##sys#error "fewer arguments than specializers" generic))
		  (else
		   (let ((c1  (##sys#slot specls1 0))
			 (c2  (##sys#slot specls2 0))
			 (arg (##sys#slot args 0)))
		     (if (eq? c1 c2)
			 (loop (##sys#slot specls1 1)
			       (##sys#slot specls2 1)
			       (##sys#slot args 1))
			 (more-specific? c1 c2 arg)))) ) ] ) ) ) ) ) )

(add-method compute-apply-methods
  (make-method (list <generic>)
    (lambda (call-next-method generic)
      (lambda (methods args)
	(letrec ((one-step
		  (lambda (tail)
		    (lambda ()
		      (cond-expand 
		       [unsafe]
		       [else
			(when (null? tail)
			  (##sys#error "call-next-method: no methods left" generic) ) ] )
		      (apply (method-procedure (##sys#slot tail 0))
			     (one-step (##sys#slot tail 1)) args)))))
	  ((one-step methods)))))))

(define applicable?
  (lambda (c arg)
    (memq c (class-cpl (class-of arg)))))

(define more-specific?
  (lambda (c1 c2 arg)
    (memq c2 (memq c1 (class-cpl (class-of arg))))))

(add-method initialize
  (make-method (list <top>)
    (lambda (call-next-method object initargs) 
      (##sys#error "can not initialize object" object) ) ) )

(add-method initialize
  (make-method (list <object>)
    (lambda (call-next-method object initargs) object)))

(add-method initialize
  (make-method (list <class>)
    (lambda (call-next-method class initargs)
      (call-next-method)
      (##tinyclos#slot-set! 
       class
       'direct-supers
       (quick-getl initargs 'direct-supers '()))
      (let* ([dslots
	      (map (lambda (s)
		     (if (pair? s) s (list s)))
		   (quick-getl initargs 'direct-slots  '()))] 
	     [name (quick-getl initargs 'name "(anonymous)")] 
	     [namestr 
	      (cond [(symbol? name) (##sys#symbol->string name)]
		    [(string? name) name]
		    [else (##sys#signal-hook #:type-error 'initialize "invalid class name" name)] ) ] 
	     [namestrlen (##sys#size namestr)] )
	(##tinyclos#slot-set! class 'direct-slots dslots)
	(##tinyclos#slot-set! class 'cpl (compute-cpl class))
	(##tinyclos#slot-set!
	 class 'name
	 (if (and (> namestrlen 0)
		  (char=? #\< (string-ref namestr 0))
		  (char=? #\> (string-ref namestr (sub1 namestrlen))) )
	     (substring namestr 1 (sub1 namestrlen))
	     namestr) )
	(let ([slots (compute-slots class)])
	  (##tinyclos#slot-set! class 'slots slots)
	  (when (= basic-class-instance-size (##sys#size class))
	    (%instance-cache-set! class (##core#inline "C_random_fixnum" #x10000)) )
	  (let* ([nfields 0]
		 [field-initializers '()]
		 [allocator
		  (lambda (init)
		    (let ((f nfields))
		      (set! nfields (+ nfields 1))
		      (set! field-initializers (cons init field-initializers))
		      (values (lambda (o)   (get-field  o f))
			      (lambda (o n) (set-field! o f n)))))]
		 [getters-n-setters
		  (map (lambda (slot)
			 (cons (##sys#slot slot 0)
			       (call-with-values (lambda () (compute-getter-and-setter class slot allocator)) cons) ) )
		       slots) ] )
	    (##tinyclos#slot-set! class 'nfields nfields)
	    (##tinyclos#slot-set! class 'field-initializers (reverse field-initializers))
	    (##tinyclos#slot-set! class 'getters-n-setters getters-n-setters))))) ) )

(add-method initialize
  (make-method (list <generic>)
    (lambda (call-next-method generic initargs)
      (call-next-method)
      (unless (%entity? generic)
	(##sys#error 'initialize "generic is not an entity") )
      (##tinyclos#slot-set! generic 'methods '())
      (%set-entity-name! generic (quick-getl initargs 'name "(unnamed)"))
      (%set-entity-proc! generic (lambda args (##sys#error "has no methods" generic))))))

(add-method initialize
  (make-method (list <method>)
    (lambda (call-next-method method initargs)
      (call-next-method)
      (##tinyclos#slot-set! method 'specializers (getl initargs 'specializers))
      (##tinyclos#slot-set! method 'procedure    (getl initargs 'procedure)))))

(add-method allocate-instance
  (make-method (list <class>)
    (lambda (call-next-method class)
      (let* ((field-initializers (##tinyclos#slot-ref class 'field-initializers))
	     (new (%allocate-instance
		   class
		   (length field-initializers))))
	(let loop ((n 0)
		   (inits field-initializers))
	  (if (not (null? inits))
	      (begin
		(%instance-set! new n ((##sys#slot inits 0)))
		(loop (+ n 1)
		      (##sys#slot inits 1)))
	      new))))))

(add-method allocate-instance
  (make-method (list <entity-class>)
    (lambda (call-next-method class)
      (let* ([field-initializers (##tinyclos#slot-ref class 'field-initializers)]
	     [new (%allocate-entity
		   class
		   (length field-initializers)
		   "(unnamed)") ] )
	(let loop ((n 0)
		   (inits field-initializers))
	  (if (not (null? inits))
	      (begin
		(%entity-set! new n ((##sys#slot inits 0)))
		(loop (+ n 1)
		      (##sys#slot inits 1)))
	      new))))))

(add-method compute-cpl
    (make-method (list <class>)
      (lambda (call-next-method class)
	(compute-std-cpl class class-direct-supers))))

(add-method compute-slots
  (make-method (list <class>)
    (lambda (call-next-method class)
      (let collect ((to-process (apply append
				       (map class-direct-slots
					    (class-cpl class))))
		    (result '()))
	(if (null? to-process)
	    (reverse result)
	    (let* ((current (##sys#slot to-process 0))
		   (name (##sys#slot current 0))
		   (others '())
		   (remaining-to-process
		    (filter-in (lambda (o)
				 (if (eq? (##sys#slot o 0) name)
				     (begin
				       (set! others (cons o others))
				       #f)
				     #t))
			       (##sys#slot to-process 1))))
	      (collect remaining-to-process
		       (cons (append current
				     (apply append (map (lambda (x) (##sys#slot x 1)) others)))
			     result))))))))

(add-method compute-getter-and-setter
  (make-method (list <class>)
    (lambda (call-next-method class slot allocator)
      (allocator (lambda () (##core#undefined))))))

;
; Now everything works, both generic functions and classes, so we can
; turn on the real MAKE.
;
;
(set! make
  (lambda (class . initargs)
    (let ((instance (allocate-instance class)))
      (initialize instance initargs)
      instance)))

;
; Now define what CLOS calls `built in' classes.
;
;
(define <primitive-class>
    (make <class>
      'name "primitive-class"
	  'direct-supers (list <class>)
	  'direct-slots  '()))

(define <primitive>
  (make <class> 'direct-supers (list <top>)
                'direct-slots  '()
                'name          "primitive"))

(define (make-primitive-class name . sclasses)
  (make <primitive-class>
        'direct-supers (if (null? sclasses) (list <primitive>) sclasses)
        'direct-slots  '()
        'name          name))

(add-method initialize
  (make-method (list <primitive>)
    (lambda (call-next-method object initargs) object)))

(define <void>      (make-primitive-class "void"))
(define <null>      (make-primitive-class "null"))
(define <boolean>   (make-primitive-class "boolean"))
(define <symbol>    (make-primitive-class "symbol"))
(define <char>      (make-primitive-class "char"))
(define <vector>    (make-primitive-class "vector"))
(define <pair>      (make-primitive-class "pair"))
(define <number>    (make-primitive-class "number"))
(define <integer>    (make-primitive-class "integer" <number>))
(define <exact>    (make-primitive-class "exact" <integer>))
(define <inexact>    (make-primitive-class "inexact" <number>))
(define <string>    (make-primitive-class "string"))
(define <port>      (make-primitive-class "port"))
(define <input-port> (make <class> 'name "input-port" 'direct-supers (list <port>) 'direct-slots '()))
(define <output-port> (make <class> 'name "output-port" 'direct-supers (list <port>) 'direct-slots '()))
(define <byte-vector>      (make-primitive-class "byte-vector"))
(define <structure>      (make-primitive-class "structure"))
(define <procedure> (make-primitive-class "procedure" <procedure-class>))
(define <end-of-file> (make-primitive-class "end-of-file"))
(define <environment> (make-primitive-class "environment" <structure>))
(define <hash-table> (make-primitive-class "hash-table" <structure>))
(define <promise> (make-primitive-class "promise" <structure>))
(define <queue> (make-primitive-class "queue" <structure>))
(define <condition> (make-primitive-class "condition" <structure>))
(define <char-set> (make-primitive-class "char-set" <structure>))
(define <time> (make-primitive-class "time" <structure>))
(define <lock> (make-primitive-class "lock" <structure>))
(define <mmap> (make-primitive-class "mmap" <structure>))
(define <array> (make-primitive-class "array" <structure>))
(define <u8vector> (make-primitive-class "u8vector" <byte-vector>))
(define <s8vector> (make-primitive-class "s8vector" <byte-vector>))
(define <u16vector> (make-primitive-class "u16vector" <byte-vector>))
(define <s16vector> (make-primitive-class "s16vector" <byte-vector>))
(define <u32vector> (make-primitive-class "u32vector" <byte-vector>))
(define <s32vector> (make-primitive-class "s32vector" <byte-vector>))
(define <f32vector> (make-primitive-class "f32vector" <byte-vector>))
(define <f64vector> (make-primitive-class "f64vector" <byte-vector>))
(define <pointer> (make-primitive-class "pointer"))
(define <tagged-pointer> (make-primitive-class "tagged-pointer" <pointer>))
(define <swig-pointer> (make-primitive-class "swig-pointer" <pointer>))
(define <locative> (make-primitive-class "locative"))
(define <tcp-listener> (make-primitive-class "tcp-listener" <structure>))
(define <thread> (make-primitive-class "thread" <structure>))
(define <mutex> (make-primitive-class "mutex" <structure>))
(define <regexp> (make-primitive-class "regexp" <structure>))
(define <condition-variable> (make-primitive-class "condition-variable" <structure>))
(define <read-table> (make-primitive-class "read-table" <structure>))
(define <c++-object> (make <class> 'name "c++-object" 'direct-supers (list <object>) 'direct-slots '(this)))

(set! method-caching-enabled #t)


;;; Utilities:

(define initialize-slots
  (let ([not-there (gensym)])
    (lambda (object initargs)
      (##sys#check-list initargs 'initialize-slots)
      (for-each
       (lambda (slot)
	 (let* ([name (car slot)]
		[value  (quick-getl initargs name not-there)] )
	   (unless (eq? value not-there)
	     (slot-set! object name value))))
       (class-slots (class-of object))))))

(define print-object (make-generic "print-object"))
(define describe-object (make-generic "describe-object"))

(add-method print-object
  (make-method (list <object>)
    (lambda (call-next-method x . port)
      (fprintf (:optional port ##sys#standard-output) "#<~A>" (class-name (class-of x))) ) ) )

(add-method print-object
  (make-method (list <primitive>)
    (lambda (call-next-method x . port)
      (write x (:optional port ##sys#standard-output)) ) ) )

(add-method print-object
  (make-method (list <class>)
    (lambda (call-next-method x . port)
      (fprintf (:optional port ##sys#standard-output) "#<class ~A>" (class-name x)) ) ) )

(add-method print-object
  (make-method (list <generic>)
    (lambda (call-next-method x . port)
      (fprintf (:optional port ##sys#standard-output) "#<generic ~A>" (%entity-name x)) ) ) )

(add-method describe-object
  (make-method (list <object>) 
    (lambda (call-next-method x . port)
      (let ([class (class-of x)]
	    [port (:optional port ##sys#standard-output)] )
	(fprintf port "instance of class ~A:~%" (class-name class))
	(for-each 
	 (lambda (s)
	   (let ([slot (car s)])
	     (fprintf port " ~S\t-> ~S~%" slot (slot-ref x slot)) ) )
	 (class-slots class) ) ) ) ) )

(add-method describe-object
  (make-method (list <class>) 
    (lambda (call-next-method x . port)
      (fprintf (:optional port ##sys#standard-output) "class ~A~%" (class-name x)) ) ) )

(add-method describe-object
  (make-method (list <generic>) 
    (lambda (call-next-method x . port)
      (fprintf (:optional port ##sys#standard-output) "generic ~A~%" (%entity-name x)) ) ) )

(define ensure-generic
  (let ([make-generic make-generic])
    (lambda (x sym)
      (if (%entity? x)
	  x
	  (make-generic (##sys#symbol->string sym)) ) ) ) )

(define add-global-method
  (let ([make-method make-method]
	[add-method add-method] )
    (lambda (val sym specializers proc)
      (let ([g (ensure-generic val sym)])
	(add-method g (make-method specializers proc))
	g) ) ) )

(define (instance? x)
  (or (%instance? x) (%entity? x)) )

(define (subclass? x y)
  (if (memq y (compute-cpl x)) #t #f) )

(define (instance-of? x class)
  (let ([cl (class-of x)])
    (or (eq? cl class)
	(subclass? cl class) ) ) )

(define (make-instance-from-pointer ptr cls)
  (and ptr 
       (not (##sys#null-pointer? ptr))
       (make cls 'this ptr) ) )

(add-method initialize
  (make-method (list <c++-object>)
    (lambda (call-next-method obj initargs)
      (when (and (pair? initargs) (eq? 'this (##sys#slot initargs 0)))
	(##tinyclos#slot-set! obj 'this (cadr initargs)) ) ) ) )
