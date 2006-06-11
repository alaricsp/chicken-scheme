;;;; partition.scm - The CHICKEN Scheme compiler (partitioning)
;
; Copyright (c) 2000-2006, Felix L. Winkelmann
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
;
;   Redistributions of source code must retain the above copyright
;   notice, this list of conditions and the following
;   disclaimer. Redistributions in binary form must reproduce the
;   above copyright notice, this list of conditions and the following
;   disclaimer in the documentation and/or other materials provided
;   with the distribution. Neither the name of the author nor the
;   names of its contributors may be used to endorse or promote
;   products derived from this software without specific prior written
;   permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
; COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGE.
;
; Send bugs, suggestions and ideas to: 
;
; felix@call-with-current-continuation.org
;
; Felix L. Winkelmann
; Unter den Gleichen 1
; 37130 Gleichen
; Germany

#{compiler debugging debugging-chicken split-level partition-fm}

(declare
  (unit partition)
  (uses extras support tinyclos)
  (disable-warning var)
  (export partition-fm make-graph$ graph$-add-cell! make-graph-cell$ 
	  graph$-get graph-cell$-add-edge! graph$->list 
	  graph*id graph*color graph*color-set! graph*partition 
	  graph*partition-move! graph*neighbours graph-cell$$-info
	  graph*gain  graph*cost graph*balance split-level
	  graph-cell$-add-undirected-edge!
	  graph-cell-info$$-inregion-set!
	  graph$-length graph*partition))

;; split-level:
;;
;; 0 - Exit after first iteration (quickest)
;; 1 - Exit when cost does not decrease by at least one-half
;; 2 - Exit when cost does not change 
(define split-level 1)

;;; CLASS dlist$ ***********************
;;;
;; A dlist$ is a header (a pair whose car is the head and the cdr is
;; the tail) followed by a 0..n sequence of dlist items, where each
;; dlist item knows the preceding and following items.  That's right,
;; it is just a doubly-linked list, which is useful because it allows
;; items to be deleted from lists in constant time.
;;; ************************************

(define-record dlist-item$ value left right)
(define (make-dlist$) (cons #f #f))
;; For internal use.  Make the dlist only one dlist item.
(define (dlist$-one! dlist dlistitem)
  (set-car! dlist dlistitem)
  (set-cdr! dlist dlistitem))
;; Is the dlist empty?
(define (dlist$-null? dlist)
  (not (car dlist)))
;; Deleted bucket item?
(define (dlist-item$-deleted? dlistitem)
  (eq? 'REMOVED (dlist-item$-left dlistitem)))
;; Adds value to tail, and returns a dlist item
(define (dlist$-add-tail! dlist value)
  (let* [(oldtail (cdr dlist))
         (newtail (make-dlist-item$ value oldtail #f))]
    (cond
      (oldtail
        (dlist-item$-right-set! oldtail newtail)
        (set-cdr! dlist newtail))
      (else
        (dlist$-one! dlist newtail)))
    newtail))
;; Removes the first item from dlist, and returns the item (not the
;; dlist-item).
(define (dlist$-first! dlist)
  (let [(dlistitem (car dlist))]
    (or dlistitem (error "The dlist is empty; cannot get first!"))
    (dlist$-remove! dlist dlistitem)
    (dlist-item$-value dlistitem)))
;; Removes dlist item from dlist
(define (dlist$-remove! dlist dlistitem)
  (let* [(left (dlist-item$-left dlistitem))
         (right (dlist-item$-right dlistitem))]
  (cond
    [(not left)
     ;; head position
     (set-car! dlist right)
     (if right
         (dlist-item$-left-set! right #f)
         ;; special case of empty dlist
         (set-cdr! dlist #f))]
    [(not right)
     ;; tail position
     (set-cdr! dlist left)
     (dlist-item$-right-set! left #f)]
    [else
      ;; middle positions
      (dlist-item$-left-set! right left)
      (dlist-item$-right-set! left right)
      ])
  (dlist-item$-left-set! dlistitem 'REMOVED)
  (dlist-item$-right-set! dlistitem 'REMOVED)))
;; List operations
(define (dlist$-map func dlist)
  (let loop [(n (car dlist))]
    (if n
        (cons
          (func (dlist-item$-value n))
          (loop (dlist-item$-right n)))
        '())))
(define (dlist$-length dlist)
  (let loop [(n (car dlist)) (c 0)]
    (if n
        (loop (dlist-item$-right n) (+ c 1))
        c)))
(define (dlist$-count pred dlist)
  (let loop [(n (car dlist)) (c 0)]
    (if n
        (loop
          (dlist-item$-right n)
          (if (pred (dlist-item$-value n)) (+ c 1) c))
        c)))
(define (dlist$-find pred dlist)
  (let loop [(n (car dlist))]
    (cond
      [(not n) #f]
      [(and n (pred (dlist-item$-value n)))
       (dlist-item$-value n)]
      [else
        (loop (dlist-item$-right n))])))
(define (dlist$->list dlist)
  (dlist$-map identity dlist))

;;; CLASS bucket$ ***********************
;; A bucket$ is essentially a hashtable, with a series of buckets to
;; hold hash-equivalent items, and whose hash values (called gain
;; values) are calculated by a gain procedure (GAINPROC).  It is
;; different because it allows these gain values to change from time
;; to time, unlike a hashtable where the hash value is static.  By
;; calling bucket$-recalc!, an item can move from one gain bucket to
;; another.
;;; *************************************

(define-record bucket$$ db gainproc keyproc)
(define-record bucket-item$ gain dlistitem)

;; Construct a bucket$.  GAINPROC is a one-arg procedure that
;; calculates the gain of a bucket item.
(define (make-bucket$ GAINPROC KEYPROC)
  (make-bucket$$ (make-hash-table eq?) GAINPROC KEYPROC))

;; Gets the current gain of a bucket-item$
(define (bucket$-gain bucketitem)
  (bucket-item$-gain bucketitem))

;; Gets the value of a bucket-item$
(define (bucket$-value bucketitem)
  (dlist-item$-value (bucket-item$-dlistitem bucketitem)))

;; Sets the value of bucket-item$.  It is your responsibility to all
;; bucket$-recalc on all affected bucket-item$.
(define (bucket$-value-set! bucketitem value)
  (dlist-item$-value-set! (bucket-item$-dlistitem bucketitem) value))

;; Adds a bucket item to a bucket.  Returns a bucket-item$.
(define (bucket$-add! bucket item)
  (let* [(db (bucket$$-db bucket))
         (gain ((bucket$$-gainproc bucket) item))
         (dl (hash-table-ref/default db gain #f))]
    (cond
      [(not dl)
       (set! dl (make-dlist$))
       (hash-table-set! db gain dl)])
    (make-bucket-item$ gain (dlist$-add-tail! dl item))))

;; Recalculates the gain of a bucket-item$, and shifts the item into
;; the appropriate bucket.  If the bucketitem has already been
;; deleted.  Returns a new dlist-item$.
(define (bucket$-recalc! bucket bucketitem)
  (let* [(db (bucket$$-db bucket))
         (oldgain (bucket-item$-gain bucketitem))
         (value (bucket$-value bucketitem))
         (gain ((bucket$$-gainproc bucket) value))
         (dl (hash-table-ref db oldgain))
         (dli (bucket-item$-dlistitem bucketitem))]
    (and (dlist-item$-deleted? dli)
         (error "recalc! cannot occur on a deleted bucket item"))
    (dlist$-remove! dl dli)
    (bucket$-add! bucket value)))

;; Removes the largest item (by gain) from the bucket.  Returns a pair; the
;; car is the item (not bucket-item$), and the cdr is the gain.
(define (bucket$-remove-largest! bucket)
  (let [(n #f)
        (db (bucket$$-db bucket))]
    (hash-table-walk
     db
      (lambda (gain dlist)
        (and (or (not n) (> gain n)) ;; is larger or not set
             (not (dlist$-null? dlist)) ;; is not empty dlist
             (set! n gain))) );; this is now largest
    (or n (error "No items remain in bucket to remove-largest!"))
    (cons (dlist$-first! (hash-table-ref db n)) n)))
  
;;; CLASS graph$ ***********************
;; Container for a graph G(V,E), where V are vertices (called cells)
;; and E are edges (each edge forms a relationship between two, and
;; only two, vertices).
;;; ************************************

; the cell-info might change app to app
(define-record graph-cell-info$$ id color partition inregion)
(define-record graph-cell$$ info vertices)
(define-record graph$$ cells num-cells)

(define (make-graph-cell$ id)
  (make-graph-cell$$ (make-graph-cell-info$$ id #f #f #t) (make-dlist$)))

;; Add directed edge between cell1 and cell2
(define (graph-cell$-add-edge! cell1 cell2)
  (dlist$-add-tail! (graph-cell$$-vertices cell1) cell2))

;; Add undirected edge between cell1 and cell2
(define (graph-cell$-add-undirected-edge! cell1 cell2)
  (dlist$-add-tail! (graph-cell$$-vertices cell1) cell2)
  (dlist$-add-tail! (graph-cell$$-vertices cell2) cell1))

;; Make a graph$.  (make-graph$ [PRED [SIZE]])
(define (make-graph$ . rest)
  (make-graph$$ (apply make-hash-table rest) 0))

;; Add a cell (vertex) to graph
(define (graph$-add-cell! graph cell)
  (graph$$-num-cells-set! graph (+ 1 (graph$$-num-cells graph)))
  (hash-table-set! (graph$$-cells graph)
    (graph-cell-info$$-id (graph-cell$$-info cell)) cell))

;; Get a cell from graph
(define (graph$-get graph id)
  (hash-table-ref/default (graph$$-cells graph) id #f))

;; List operations
(define (graph$-length graph)
  (graph$$-num-cells graph))
(define (graph$->list graph)
  (map cdr (hash-table->alist (graph$$-cells graph))))
(define (graph$-for-each proc graph)
  (for-each
    proc
    (graph$->list graph)))

;;; *******************************

(define (graph*id cell)
  (graph-cell-info$$-id (graph-cell$$-info cell)))
(define (graph*color cell)
  (graph-cell-info$$-color (graph-cell$$-info cell)))
(define (graph*color-set! cell color)
  (graph-cell-info$$-color-set! (graph-cell$$-info cell) color))
(define (graph*partition cell)
  (graph-cell-info$$-partition (graph-cell$$-info cell)))
(define (graph*partition-move! cell partition)
  (graph-cell-info$$-partition-set! (graph-cell$$-info cell) partition))
(define (graph*neighbours cell)
  (filter
   (lambda (c)
     (graph-cell-info$$-inregion (graph-cell$$-info c)))
   (dlist$->list (graph-cell$$-vertices cell))))
;; gain of a cell move within an undirected graph
(define (graph*gain cell)
  ;; gain is positive if beneficial to go to partition=#t
  (let* [(vertices (graph-cell$$-vertices cell))
         (total (dlist$-length vertices))
         (p1 (graph-cell-info$$-partition (graph-cell$$-info cell)))
         ;; what is edge cost now?
         (now (dlist$-count
                (lambda (c2)
                  (let [(p2 (graph-cell-info$$-partition
                              (graph-cell$$-info c2)))]
                    (not (eq? p1 p2))))
                vertices))
         ;; what is edge cost later, when moved?
         (later (- total now))
         ;; if cell to be moved is starts at partition=#f (p1=#f)
         ;; and ends up in partition=#t, then a lower edge cost
         ;; transition (that is, later < now) corresponds to a
         ;; positive gain
                                        ;(gain (if p1 (- later now) (- now later)))
         
         ;; if cell is moved and causes increase in solution cost
         ;; (that is, later > now), then the gain is negative
         (gain (- now later))]
    gain))
;; cost of a undirected graph
(define (graph*cost graph)
  (let [(t 0)]
    ;; count all edges from one partition to another (the edge
    ;; cut). note that this will double count if undirected edges,
    ;; since both edges A->B and B->A will be counted.
    (graph$-for-each
      (lambda (c1)
        (let [(p1 (graph-cell-info$$-partition (graph-cell$$-info c1)))]
          (set! t (+ t (dlist$-count
                         (lambda (c2)
                           (let [(p2 (graph-cell-info$$-partition
                                       (graph-cell$$-info c2)))]
                             (not (eq? p1 p2))))
                         (graph-cell$$-vertices c1))))))
      graph)
    (quotient t 2)))
;; how much more do you have in the larger partition compared to the
;; smaller partition?  can range as an integer from 0 to 100, with 0
;; being perfectly balanced and 100 being perfectly imbalanced.
;;
;; the argument (weight) is the desired weighting of the partition.
;; for example, if w=(cons 3 7), then the perfect balance is when 30%
;; of the cost is for those within partition #f, while 70% are within
;; partition #t.  normally you would want '(1 . 1)
;;
;; you can, and should, pass in the number in partition #f and the
;; number in partition #t by using n#f and n#t.  if you want to be
;; very fast, set them to non-negative integers; otherwise, leave them
;; #f otherwise.
(define (graph*balance graph num-cells weight n#f n#t)
  (let ([b 0]
	[for-each (if (graph$$? graph) graph$-for-each for-each)])
    (cond
     [(and n#f n#t)
      (set! b (- (* (car weight) n#t) (* (cdr weight) n#f)))]
     [else
      (for-each
       (lambda (c)
	 (set! b
	       (+ b
		  (if (graph-cell-info$$-partition (graph-cell$$-info c))
		      (car weight)
		      (- (cdr weight))))))
       graph)])
    (quotient (* (abs b) 100) 
	      (* (max (car weight) (cdr weight)) 
		 num-cells))))

;;; *******************
;;; Fiduccia-Mattheyses
;;;
;;; Bipartitioning of a graph or hypergraph, minimizing a cost
;;; function, subject to a balancing criterion.  Is a NP-Complete
;;; problem, so this method is a fast heuristic.
;;;
;;; http://www.cs.caltech.edu/~andre/courses/CS294S97/notes/day15/day15.html
;;;
;;; http://www.microelectronic.e-technik.tu-darmstadt.de/lectures/summer/rse/english/download/Uebung/2.2/fiduccia_alg.pdf
;;;
;;; *******************

;;; Graph requirements:
;;
;; Directed or undirected graph g.
;;
;; (graph:vertex n g) should be O(1).
;;
;; (graph:adjacent-vertices-l vd g) should be O(E/V), where E/V is the
;; maximum number of out-edges in any vertex, and the constant factor
;; should be as low as possible, since this is heavily used.  The
;; fastest edge list for adjacency list graphs are -vector, followed
;; by -slist, -list, -set and lastly -hash-set.
;;
;; (partition-fm ...)
;;    Should never fail to find a bipartition, although it might be a
;;    bad partition if the balance criterion is too rigid.
;;
;; You will want to (randomize N) before calling this method, so you
;; can have repeatable partitions.
;;
;; cells - Scheme list of cells to partition.  You do not need to use
;; a list of graph-cell$; you may use whatever you want, as long as
;; you define the appropriate procedures listed below
;;
;; id - procedure of 1-arg that returns the unique "id" of its
;; argument (cell), #f for no id
;;
;; color - procedure of 1-arg that returns the "color" of its
;; argument (cell), #f for no color
;;
;; color-set! - procedure of 2-arg that will "color" its first
;; argument with the second argument
;;
;; partition - procedure of 1-arg that returns the partition (either
;; #t or #f) that the first argument (cell) belongs to
;;
;; partition-move! - procedure of 2-arg that places the first argument
;; (cell) into one of the two partitions specified by the second
;; argument (#t or #f).
;;
;; neighbours - procedure of 1-arg that returns a list of the
;; adjacent/neighbour cells of the first argument (cell)
;;
;; gain - procedure of 1-arg that calculates the gain of the first
;; argument (cell).  See section 1.2 of
;; http://www.gigascale.org/pubs/2/alenex.pdf.  Basically, the gain is
;; positive if it reduces the solution cost if the cell switched
;; partitions, and negative if it increases the solution cost.
;;
;; cost - procedure of 0-arg that calculates the cost of the current
;; partition for 'cells'.  This solution cost is usually the edge
;; cost, although it may be anything.
;;
;; balance - procedure of 1-arg that calculates the balance criterion.
;; the smaller the number, the more balanced the partition is.  must
;; return an *integer*. it is best to use the range [0..100], and let
;; 0 mean perfectly balanced while 100 is completely imbalanced.  the
;; 1-arg is just the weight, which is described below.
;;
;; weight - a pair in the form (X . Y).  If X=3 and Y=7, then
;; perfectly balanced would mean that 3/10 of the cost is in partition
;; #f, and 7/10 of the cost is in partition #t.
;;
;; criterion - a non-negative integer that is the maximum that the
;; balance may be.  if you use the range [0..100] for balance, then the
;; range for criterion would be [0..100], and you might use
;; criterion=35 to allow significant imbalances.
(define partition-fm-check-error #f) ;; #t will take a lot of CPU
(define-record partition-fmv cost balance cell)
(define (partition-fm cells
          id color color-set!
          partition partition-move!
          neighbours gain cost balance weight criterion)
  (debugging 'P "Fiduccia-Mattheyses bipartitioning" weight)
  ;; RANDOMLY PARTITION INTO TWO HALVES
  (let* [(L (length cells))
	 (L1 (quotient (* (car weight) L) (+ (car weight) (cdr weight))))
	 (LS (shuffle cells))]
    (let loop1 ([l LS] [n 0])
      (cond 
       [(null? l)]
       [(< n L1)
	(partition-move! (car l) #f)
	(loop1 (cdr l) (add1 n))]
       [else
	(partition-move! (car l) #t)
	(loop1 (cdr l) (add1 n))]))
    ;; REPEAT UNTIL NO UPDATES.  Modification: repeat until the
    ;; overall cost does not decrease
    (let loop1 ([initial-cost (cost)])
      (let
          [(initial-partition (map partition cells))
	   (current-cost #f)
	   (n#f #f)
	   (n#t #f)
           (bucket (make-bucket$
                     gain
                     (lambda (cell)
                       (graph-cell-info$$-id (graph-cell$$-info cell)))))
           (costs #f)
           (lop #f)]
        (when (debugging 'P  "  Repeat until no updates")
          (debugging 'Q "    initial-partition "
            (if (< L 10) initial-partition (append (take initial-partition 10) "...")))
          (debugging 'Q "    initial-id        "
            (if (< L 10) (map id cells) (append (take (map id cells) 10) "...")))
          (debugging 'P  "    initial-cost      " initial-cost))
	(set! n#f (count not initial-partition))
	(set! n#t (- L n#f))
	(set! current-cost initial-cost)
        ;; START WITH ALL CELLS FREE
        ;; [add all cells to gain bucket]
        (for-each
          (lambda (c)
            (color-set! c (bucket$-add! bucket c)))
          cells)

        (set! costs
          ;; REPEAT UNTIL NO CELLS FREE
          (let loop2 [(cells-free L)]
            (cond
              [(= cells-free 0) '()]
              [else
                (let [(largest #f)]
                            
                  ;; MOVE CELL WITH LARGEST GAIN
                  (set! largest (bucket$-remove-largest! bucket))
                  (debugging 'R "    Move cell with largest gain")
                  (debugging 'R "      gain " (cdr largest))
		  (set! current-cost (- current-cost (cdr largest)))
		  (set! largest (car largest))
		  (cond
		   [(partition largest)
		    (set! n#f (add1 n#f))
		    (set! n#t (sub1 n#t))]
		   [else
		    (set! n#t (add1 n#t))
		    (set! n#f (sub1 n#f))])
                  (partition-move! largest
                    (not (partition largest)))
                  (debugging 'R "      id   "
                    (graph-cell-info$$-id (graph-cell$$-info largest)))
                            
                  ;; UPDATE COSTS OF NEIGHBOURS
                  (for-each
                    (lambda (c)
                      (let [(bi (color c))];; bucket item
                        (and bi;; if unlocked
                             (let*                                 
				 ([oldg (bucket$-gain bi)]
                                  ;; get new bucket item upon
				  ;; recalculation
				  [binew (bucket$-recalc! bucket bi)])
                               (debugging 'R
                                 "    Update neighbour gain " oldg " to " (bucket$-gain binew))
                               ;; make sure cell is updated with new
                               ;; bucket item
                               (color-set! c binew)))))
                    (neighbours largest))
                            
                  ;; LOCK CELL IN PLACE. implicitly done by
                  ;; bucket$-remove-largest.  need just to remove
                  ;; bucket item as color so cost is not updated next
                  ;; iteration
                  (color-set! largest #f)
                            
                  ;; NOTE CURRENT COST
                  (when (debugging 'R "    Note current cost")
                    (let ([c1 (cost)])
                      (debugging 'R "      (cost)                    " c1)
                      (debugging 'R "      current-cost              " current-cost)
                      (debugging 'R "      (balance weight n#f n#t)  " (balance weight n#f n#t))))
                  (and (memq 'P debugging-chicken)
                       partition-fm-check-error
                       (not (= (cost) current-cost))
                       (error "Bug found where (cost) does not equal current-cost"))
                  (cons
                    ;; store cost, balance and which cell moved
                    (make-partition-fmv current-cost 
					(balance weight n#f n#t)
					largest)
                    ;; end REPEAT UNTIL NO CELLS FREE
                    (loop2 (- cells-free 1))))])))
               
        ;; PICK LEAST COST POINT IN PREVIOUS SEQUENCE AND USE AS
        ;; PARTITION
        
        ;; find local optimal point (lop)
        (for-each
	 (lambda (fmv)
	   (and
	    ;; better than initial cost
	    (< (partition-fmv-cost fmv) initial-cost)
	    ;; least cost
	    (or (not lop)
		(<
		 (partition-fmv-cost fmv)
		 (partition-fmv-cost lop)))
	    ;; balance criterion
	    (<= (partition-fmv-balance fmv) criterion)
	    ;; good!
	    (set! lop fmv)))
	 costs)
        
        ;; reset to initial partition
        (let reset [(cells cells) (initial initial-partition)]
          (cond
            [(not (null? cells))
             (partition-move! (car cells) (car initial))
             (reset (cdr cells) (cdr initial))]))
        (when (debugging 'Q "    Pick least cost point")
          (cond
            [lop
              (debugging 'Q "      cost,id = "
                (partition-fmv-cost lop) ","
                (graph-cell-info$$-id (graph-cell$$-info (partition-fmv-cell lop))))]
            [else
              (debugging 'Q "      no least cost point")]))
        
        ;; apply moves until we get to least cost point
        (and lop
             (let apply-moves [(costs costs)]
               (let [(cell (partition-fmv-cell (car costs)))
                     (cost (partition-fmv-cost (car costs)))]                 
                 (cond
                   [(null? costs)]
                   [else
                     (partition-move!
                       cell
                       (not (partition cell)))
                     (or (eq? lop (car costs))
                         (apply-moves (cdr costs)))]))))        
        ;; end REPEAT UNTIL NO UPDATES
        (unless (zero? initial-cost)
          (let ([c (cost)]
                [l split-level])
            (cond
              [(zero? l)]
              [(= l 1)
               (when (>= (quotient initial-cost c) 2)
                 (loop1 c))]
              [else
                (unless (= initial-cost c)
                  (loop1 c))])))))
    ))
