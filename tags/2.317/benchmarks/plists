;;; (flw): property-lists for CHICKEN -*- Scheme -*-
;
; this is very ugly and probably not even faster than an a-list.

(declare
 (always-bound plists:plist-table ##sys#hash-table-ref ##sys#hash-table-set! ##sys#setslot ##sys#check-symbol) )

(define plists:plist-table (make-vector 997 '()))

(define get
  (lambda (key prop)
    (let ((plist (##sys#hash-table-ref plists:plist-table key)))
      (or (and plist
	       (let ((a (##core#inline "C_i_assq" prop plist)))
		 (and a (##sys#slot a 1)) ) )
	  '() ) ) ) )

(define put
  (lambda (key prop val)
    (let ((plist (##sys#hash-table-ref plists:plist-table key)))
      (if plist
	  (let ((a (##core#inline "C_i_assq" prop plist)))
	    (if a
		(##sys#setslot a 1 val)
		(##sys#setslot 
		 plist 1
		 (##core#inline_allocate 
		  ("C_a_i_cons" 3)
		  (##core#inline_allocate ("C_a_i_cons" 3) prop val)
		  (##sys#slot plist 1) ) ) ) )
	  (##sys#hash-table-set! 
	   plists:plist-table key
	   (##core#inline_allocate 
	    ("C_a_i_cons" 3)
	    (##core#inline_allocate ("C_a_i_cons" 3) prop val)
	    '() ) ) ) ) ) )
