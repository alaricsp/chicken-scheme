
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adapted from http://okmij.org/ftp/Scheme/keyword-arg-macro.txt
;; Currently fails in Gauche.
;; A more commented version is available at
;; http://mumble.net/~campbell/scheme/syn-param.scm

(define-syntax let-keyword-form
  (syntax-rules ()
    ((let-keyword-form
      ((labeled-arg-macro-name
        (positional-form-name (arg-name . arg-default) ...)))
      . body)
     (letrec-syntax
         ((labeled-arg-macro-name
           (syntax-rules ()
             ((labeled-arg-macro-name . keyword-val-pairs)
              (letrec-syntax
                  ((find
                    (syntax-rules (<- arg-name ...)
                      ((find kvp k-args (arg-name . default) arg-name <- val
                             . others) ; found arg-name among keyword-val-pairs
                       (next kvp val . k-args)) ...
                      ((find kvp k-args key arg-no-match-name <- val . others)
                       (find kvp k-args key . others))
                      ((find kvp k-args (arg-name default)) ; default must be here
                       (next kvp default . k-args)) ...
                      ))
                   (next               ; pack the continuation to find
                    (syntax-rules ()
                      ((next kvp val vals key . keys)
                       (find kvp ((val . vals) . keys) key . kvp))
                      ((next kvp val vals) ; processed all arg-descriptors
                       (rev-apply (val) vals))))
                   (match-positionals
                    (syntax-rules (<-)
                      ((match-positionals () res . rest)
                       (rev-apply () res))
                      ((match-positionals args (val . vals) name <- value . rest)
                       (next (name <- value . rest) val vals . args))
                      ((match-positionals args (val . vals))
                       (next () val vals . args))
                      ((match-positionals (arg1 . args) res pos-arg . rest)
                       (match-positionals args (pos-arg . res) . rest))))
                   (rev-apply
                    (syntax-rules ()
                      ((rev-apply form (x . xs))
                       (rev-apply (x . form) xs))
                      ((rev-apply form ()) form))))
                (match-positionals ((arg-name . arg-default) ...)
                                   (positional-form-name)
                                   . keyword-val-pairs)
                )))))
       . body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax loop
  (syntax-rules ()
    ;; unnamed, implicit recursion
    ((loop (vars ...) body ...)
     (%loop tmp-loop () () () () () (vars ...) body ... (tmp-loop)))
    ;; named, explicit recursion
    ((loop name (vars ...) body ...)
     (%loop name () () () () () (vars ...) body ...))))

;; Main LOOP macro. Separate the variables from the iterator and
;; parameters, then walk through each parameter expanding the
;; bindings, and build the final form.

(define-syntax %loop
  (syntax-rules (=> <-)
    ;; automatic iteration
    ((_ name l v c r f ((var1 <- iterator source ...) rest ...) . body)
     (iterator ((var1) (source ...)) %loop-next name l v c r f (rest ...) . body))
    ((_ name l v c r f ((var1 var2 <- iterator source ...) rest ...) . body)
     (iterator ((var1 var2) (source ...)) %loop-next name l v c r f (rest ...) . body))
    ((_ name l v c r f ((var1 var2 var3 <- iterator source ...) rest ...) . body)
     (iterator ((var1 var2 var3) (source ...)) %loop-next name l v c r f (rest ...) . body))
    ((_ name l v c r f ((var1 var2 var3 var4 <- iterator source ...) rest ...) . body)
     (iterator ((var1 var2 var3 var4) (source ...)) %loop-next name l v c r f (rest ...) . body))
    ;; do equivalents, with optional guards
    ((_ name l (vars ...) (checks ...) r f ((var init step guard) rest ...) . body)
     (%loop name l (vars ... (var init step)) (checks ... (guard var)) r f (rest ...) . body))
    ((_ name l (vars ...) c r f ((var init step) rest ...) . body)
     (%loop name l (vars ... (var init step)) c r f (rest ...) . body))
    ((_ name l (vars ...) c r f ((var init) rest ...) . body)
     (%loop name l (vars ... (var init var)) c r f (rest ...) . body))
    ;; specify a default done?
    ((_ name l v c r f ())
     (%loop name l v c r f () (#f #f)))
    ((_ name l v c r f () () . body)
     (%loop name l v c r f () (#f #f) . body))
    ;; final expansion
    ((_ name (lets ...) ((var init step) ...) (checks ...) (refs ...) (finals ...) ()
        => result
        . body)
     (let* (lets ...)
       (letrec ((tmp (lambda (var ...)
                       (if (or checks ...)
                         (let-keyword-form ((name (tmp (var step) ...)))
                            (match-let (finals ...)
                              result))
                         (match-let (refs ...)
                           (let-keyword-form ((name (tmp (var step) ...)))
                             (if #f #f)
                             . body))))))
         (tmp init ...))))
    ;; unspecified return value case
    ((_ name (lets ...) ((var init step) ...) (checks ...) (refs ...) (finals ...) ()
        . body)
     (%loop name (lets ...) ((var init step) ...) (checks ...) (refs ...) (finals ...) ()
            => (if #f #f) . body))
    ))

(define-syntax %loop-next
  (syntax-rules ()
    ((_ (new-lets ...) (new-vars ...) (new-checks ...) (new-refs ...) (new-finals ...)
        name (lets ...) (vars ...) (checks ...) (refs ...) (finals ...)
        . rest)
     (%loop name (lets ... new-lets ...) (vars ... new-vars ...)
                 (checks ... new-checks ...) (refs ... new-refs ...)
                 (finals ... new-finals ...)
        . rest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Iterators

;; Each gets passed two lists, those items left of the <- and those to
;; the right, followed by a NEXT and REST continuation.

;; Should finish with
;;
;;  (next (outer-vars ...) (cursor-vars ...) (done?-tests ...)
;;        (loop-vars ...) (final-vars ...) . rest)
;;
;;  OUTER-VARS: bound once outside the loop in a LET*
;;  CURSOR-VARS: DO-style bindings of the form (name init update)
;;  DONE?-TESTS: possibly empty list of forms that terminate the loop on #t
;;  LOOP-VARS: inner variables, updated in parallel after the cursors
;;  FINAL-VARS: final variables, bound only in the => result

(define-syntax in-list                  ; called just "IN" in ITER
  (syntax-rules ()
    ((in-list ((var) source) next . rest)
     (in-list ((var cursor) source) next . rest))
    ((in-list ((var cursor) source) next . rest)
     (in-list ((var cursor succ) source) next . rest))
    ((in-list ((var cursor succ) (source)) next . rest)
     (in-list ((var cursor succ) (source cdr)) next . rest))
    ((in-list ((var cursor succ) (source step)) next . rest)
     (in-list ((var cursor succ) (source step null?)) next . rest))
    ((in-list ((var cursor succ) (source step done?)) next . rest)
     (next ()                              ; outer let bindings
           ((cursor source succ))          ; iterator, init, step
           ((done? cursor))                ; finish tests for iterator vars
           ((var (car cursor))             ; step variables and values
            (succ (step cursor)))
           ()                              ; final result bindings
           . rest))))

;; Iterator from Taylor R. Campbell.  If you know the number of lists
;; ahead of time it's much more efficient to iterate over each one
;; separately.
(define-syntax in-lists
  (syntax-rules ()
    ((in-lists ((elts) lol) next . rest)
     (in-lists ((elts pairs) lol) next . rest))
    ((in-lists ((elts pairs) lol) next . rest)
     (in-lists ((elts pairs succ) lol) next . rest))
    ((in-lists ((elts pairs succ) (lol)) next . rest)
     (in-lists ((elts pairs succ) (lol cdr)) next . rest))
    ((in-lists ((elts pairs succ) (lol)) next . rest)
     (in-lists ((elts pairs succ) (lol cdr)) next . rest))
    ((in-lists ((elts pairs succ) (lol step)) next . rest)
     (in-lists ((elts pairs succ) (lol step null?)) next . rest))
    ((in-lists ((elts pairs succ) (lol step done?)) next . rest)
     (next ()
           ((pairs lol succ))
           ((let lp ((ls pairs)) ; yes, an in-lined ANY
              (and (pair? ls) (or (done? (car ls)) (lp (cdr ls))))))
           ((elts (map car pairs))
            (succ (map step pairs)))
           ()
           . rest))
    ))

(define-syntax define-in-indexed
  (syntax-rules ()
    ((define-in-indexed in-type in-type-reverse length ref)
     (begin
       (define-syntax in-type
         (syntax-rules ()
           ((in-type ls next . rest)
            (%in-indexed >= + 0 (length tmp) ref tmp ls next . rest))))
       (define-syntax in-type-reverse
         (syntax-rules ()
           ((in-type-reverse ls next . rest)
            (%in-indexed < - (- (length tmp) 1) 0 ref tmp ls next . rest))))
       ))))

(define-in-indexed in-string in-string-reverse string-length string-ref)
(define-in-indexed in-vector in-vector-reverse vector-length vector-ref)
(define-in-indexed in-u8vector in-u8vector-reverse u8vector-length u8vector-ref)
(define-in-indexed in-s8vector in-s8vector-reverse s8vector-length s8vector-ref)
(define-in-indexed in-u16vector in-u16vector-reverse u16vector-length u16vector-ref)
(define-in-indexed in-s16vector in-s16vector-reverse s16vector-length s16vector-ref)
(define-in-indexed in-u32vector in-u32vector-reverse u32vector-length u32vector-ref)
(define-in-indexed in-s32vector in-s32vector-reverse s32vector-length s32vector-ref)
(define-in-indexed in-f32vector in-f32vector-reverse f32vector-length f32vector-ref)
(define-in-indexed in-f64vector in-f64vector-reverse f64vector-length f64vector-ref)

;; helper for the above string and vector iterators
(define-syntax %in-indexed
  (syntax-rules ()
    ;;   cmp inc start end ref
    ((%in-indexed ge + s e r tmp-vec ((var) (vec ...)) next . rest)
     (%in-indexed ge + s e r tmp-vec ((var vec-index) (vec ...)) next . rest))
    ((%in-indexed ge + s e r tmp-vec ((var index) (vec)) next . rest)
     (%in-indexed ge + s e r tmp-vec ((var index) (vec s e 1)) next . rest))
    ((%in-indexed ge + s e r tmp-vec ((var index) (vec from)) next . rest)
     (%in-indexed ge + s e r tmp-vec ((var index) (vec from e 1)) next . rest))
    ((%in-indexed ge + s e r tmp-vec ((var index) (vec from to)) next . rest)
     (%in-indexed ge + s e r tmp-vec ((var index) (vec from to 1)) next . rest))
    ((%in-indexed ge + s e r tmp-vec ((var index) (vec from to step)) next . rest)
     (next ((tmp-vec vec) (end to))
           ((index from (+ index step)))
           ((ge index end))
           ((var (r tmp-vec index)))
           ()
       . rest))
    ))

(define-syntax in-port
  (syntax-rules ()
    ((in-port ((var) source) next . rest)
     (in-port ((var p) source) next . rest))
    ((in-port ((var p) ()) next . rest)
     (in-port ((var p) ((current-input-port))) next . rest))
    ((in-port ((var p) (port)) next . rest)
     (in-port ((var p) (port read-char)) next . rest))
    ((in-port ((var p) (port read-char)) next . rest)
     (in-port ((var p) (port read-char eof-object?)) next . rest))
    ((in-port ((var p) (port reader eof?)) next . rest)
     (next ((p port) (r reader) (e? eof?))
           ((var (r p) (r p)))
           ((e? var))
           ()
           ()
       . rest))))

(define-syntax in-file
  (syntax-rules ()
    ((in-file ((var) source) next . rest)
     (in-file ((var p) source) next . rest))
    ((in-file ((var p) (file)) next . rest)
     (in-file ((var p) (file read-char)) next . rest))
    ((in-file ((var p) (file reader)) next . rest)
     (in-file ((var p) (file reader eof-object?)) next . rest))
    ((in-file ((var p) (file reader eof?)) next . rest)
     (next ((p (open-input-file file)) (r reader) (e? eof?))
           ((var (r p) (r p)))
           ((e? var))
           ()
           ((dummy (close-input-port p)))
       . rest))))

;; XXXX Consider a keyword approach such as Taylor uses.

(define-syntax in-range
  (syntax-rules ()
    ((in-range ((var) ()) next . rest)
     (next () ((var 0 (+ var 1))) () () . rest))
    ((in-range ((var) (to)) next . rest)
     (next () ((var 0 to)) () () . rest))
    ((in-range ((var) (from to)) next . rest)
     (in-range ((var) (from to 1)) next . rest))
    ((in-range ((var) (from to step)) next . rest)
     (next ((tmp-to to))
           ((var from (+ var step)))
           ((>= var tmp-to))
           ()
           ()
       . rest))))

(define-syntax in-range-reverse
  (syntax-rules ()
    ((in-range ((var) ()) next . rest)
     (next () ((var 0 (- var 1))) () () . rest))
    ((in-range ((var) (to)) next . rest)
     (next () ((var 0 to)) () () . rest))
    ((in-range ((var) (from to)) next . rest)
     (in-range ((var) (from to 1)) next . rest))
    ((in-range ((var) (from to step)) next . rest)
     (next ((tmp-to to))
           ((var from (- var step)))
           ((<= var tmp-to))
           ()
           ()
       . rest))))

;; XXXX A generalized accumulator, possibly not worth the effort.

(define-syntax collecting
  (syntax-rules ()
    ((collecting ((var) source) next . rest)
     (collecting ((var cursor) source) next . rest))
    ((collecting ((var cursor) (source)) next . rest)
     (collecting ((var cursor) (source cons)) next . rest))
    ((collecting ((var cursor) (source kons)) next . rest)
     (collecting ((var cursor) (source kons reverse)) next . rest))
    ((collecting ((var cursor) (source kons final)) next . rest)
     (next ((tmp-kons kons))
           ((cursor '() (tmp-kons source cursor)))
           ()
           ()
           ((var (final cursor)))
       . rest))))

;; XXXX should these be loop variables or body variables?

(define-syntax in-random
  (syntax-rules ()
    ((in-random ((var) ()) next . rest) ; XXXX consider in-random-real
     (next ((MAX_RAND (+ (expt 2 29) (- (expt 2 29) 1))))
           ((var (/ (random MAX_RAND) MAX_RAND)
                 (/ (random MAX_RAND) MAX_RAND)))
           ()
           ()
           . rest))
    ((in-random ((var) (n)) next . rest)
     (next ((tmp-n n))
           ((var (random tmp-n) (random tmp-n)))
           ()
           ()
           ()
        . rest))
    ((in-random ((var) (n lo)) next . rest)
     (next ((tmp-n n) (tmp-lo lo))
           ((var (+ tmp-lo (random tmp-n))
                 (+ tmp-lo (random tmp-n))))
           ()
           ()
           ()
       . rest))
    ))

;; takes either a list or vector

(define-syntax in-random-element
  (syntax-rules ()
    ((in-random-element ((var) (source)) next . rest)
     (next ((tmp-source source)
            (tmp-vec (if (pair? tmp-source)
                       (list->vector tmp-source)
                       tmp-source))
            (tmp-len (vector-length tmp-vec)))
           ((var (vector-ref tmp-vec (random tmp-len))
                 (vector-ref tmp-vec (random tmp-len))))
           ()
           ()
           ()
           . rest))))

;; XXXX document this and explain what the hell it's doing :)
(define-syntax in-permutations
  (syntax-rules ()
    ((in-permutations ((var) source) next . rest)
     (in-permutations ((var p) source) next . rest))
    ((in-permutations ((var p) (set)) next . rest)
     (in-permutations ((var p) (set #f)) next . rest))
    ((in-permutations ((var p) (set len)) next . rest)
     (next
      ((tmp-set set))
      ((p
        (let ((tmp-len (or len (length tmp-set))))
          (let lp ((i 0) (ls tmp-set) (res '()))
            (if (= i tmp-len)
              res
              (lp (+ i 1) (cdr ls) (cons (cons ls '()) res)))))
        (and (pair? p)
             (let lp ((ls p) (count 0))
               (if (pair? (cdaar ls))
                 (let lp2 ((i count)
                           (ls2 (append (reverse (cdar ls))
                                        (cons (caaar ls) (cddaar ls))))
                           (res (cons (cons (cdaar ls)
                                            (cons (caaar ls) (cdar ls)))
                                      (cdr ls))))
                   (if (zero? i)
                     res
                     (lp2 (- i 1) (cdr ls2) (cons (cons ls2 '()) res))))
                 (and (pair? (cdr ls)) (lp (cdr ls) (+ count 1))))))))
      ((not p))
      ((var
        (let lp ((ls p) (res '()))
          (if (null? ls) res (lp (cdr ls) (cons (caaar ls) res))))))
      ()
      . rest))
    ))

(define-syntax in-combinations
  (syntax-rules ()
    ((in-combinations ((var) x) next . rest)
     (in-combinations ((var p) x) next . rest))
    ;; all 2^len combinations
    ((in-combinations ((var p) (set)) next . rest)
     (next
      ((tmp-vec (list->vector set))
       (tmp-len (vector-length tmp-vec))
       (tmp-limit (expt 2 tmp-len)))
      ((p 0 (+ p 1)))
      ((>= p tmp-limit))
      ((var
        (let lp ((p p) (i 0) (res '()))
          (cond
            ((zero? p) (reverse res))
            ((odd? p)
             (lp (arithmetic-shift p -1)
                 (+ i 1)
                 (cons (vector-ref tmp-vec i) res)))
            (else (lp (arithmetic-shift p -1) (+ i 1) res))))))
      ()
      . rest))
    ;; all C(n,k) combinations of length k
    ((in-combinations ((var p) (set len)) next . rest)
     (next
      ((tmp-len len))
      ((p
        (let lp ((i 0) (ls set) (res '()))
          (if (= i tmp-len)
            res
            (lp (+ i 1) (cdr ls) (cons ls res))))
        (and (pair? p)
             (if (and (pair? (car p)) (pair? (cdar p)))
               (cons (cdar p) (cdr p))
               (let lp ((ls (cdr p)) (count 1))
                 (and (pair? ls)
                      (if (> (length (cdar ls)) count)
                        (let lp2 ((i count)
                                  (ls2 (cddar ls))
                                  (res (cons (cdar ls) (cdr ls))))
                          (if (zero? i)
                            res
                            (lp2 (- i 1) (cdr ls2) (cons ls2 res))))
                        (lp (cdr ls) (+ count 1)))))))))
      ((not p))
      ((var
        (let lp ((ls p) (res '()))
          (if (null? ls) res (lp (cdr ls) (cons (caar ls) res))))))
      ()
      . rest))
    ))

(define-syntax in-cartesian-product
  (syntax-rules ()
    ((in-cartesian-product ((var) (lol-src)) next . rest)
     (in-cartesian-product ((var p) (lol-src)) next . rest))
    ;; all NxMx... joins
    ((in-cartesian-product ((var x) (lol-src)) next . rest)
     (next
      ((lol lol-src))
      ((x (and (pair? lol)
               (cons (reverse lol) (reverse (cdr lol))))
          (let lp ((p (car x)) (ls (cdr x)) (rev '()))
            (cond
              ((pair? (cdar p))
               (cons (append (reverse rev)
                             (cons (cdar p) (cdr p)))
                     (cdr x)))
              ((pair? (cdr p))
               (lp (cdr p) (cdr ls) (cons (car ls) rev)))
              (else
               #f)))))
      ((not x))
      ((var (let lp ((ls (car x)) (res '()))
              (if (null? ls) res (lp (cdr ls) (cons (caar ls) res))))))
      ()
      . rest))
    ))

;; Chicken-specific implementation using internal knowledge of the
;; vector+alist representation.  The ##sys#slot form will cause most
;; other implementations to choke, so comment this out if needed.

(define-syntax in-hash-table
  (syntax-rules ()
    ((in-hash-table ((key val) (table)) next . rest)
     (next ((tmp-vec (##sys#slot table 1))
            (end (vector-length tmp-vec))
            (next-pair-bucket
             (lambda (start)
               (let lp ((i start))
                 (and (< i end)
                      (let ((x (vector-ref tmp-vec i)))
                        (if (pair? x)
                          i
                          (lp (+ i 1))))))))
            (first-bucket (next-pair-bucket 0)))
           ((bucket first-bucket
                    (if (and (pair? cell) (pair? (cdr cell)))
                      bucket
                      (next-pair-bucket (+ bucket 1))))
            (cell (and first-bucket (vector-ref tmp-vec first-bucket))
                  (if (and (pair? cell) (pair? (cdr cell)))
                    (cdr cell)
                    (let ((i (next-pair-bucket (+ bucket 1))))
                      (and i (vector-ref tmp-vec i))))))
           ((not bucket))
           ((key (caar cell))
            (val (cdar cell)))
           ()
       . rest))
    ))

;; Portable R5RS + SRFI-69 version.

;; (define-syntax in-hash-table
;;   (syntax-rules ()
;;     ((in-hash-table ((key val) (table)) next . rest)
;;      (next ((tmp-table table)
;;             (start-cursor
;;              (call-with-current-continuation
;;                (lambda (return)
;;                  (hash-table-walk
;;                   table
;;                   (lambda (k v)
;;                     (call-with-current-continuation
;;                       (lambda (inside)
;;                         (return
;;                          (lambda (sym)
;;                            (cond
;;                              ((eq? sym 'key) k)
;;                              ((eq? sym 'value) v)
;;                              ((eq? sym 'next) (inside #t))
;;                              ((eq? sym 'end?) #f))))))))
;;                  (lambda (sym)
;;                    (if (eq? sym 'end?)
;;                      #t
;;                      (error "past end of hash table")))))))
;;            ((tmp-cursor start-cursor (tmp-cursor 'next)))
;;            ((tmp-cursor 'end?))
;;            ((key (tmp-cursor 'key))
;;             (val (tmp-cursor 'value)))
;;            ()
;;            . rest))
;;     ))

