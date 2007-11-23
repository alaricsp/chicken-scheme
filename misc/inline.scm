;;; this assumes that :
;;;    a) nothing has been evaluated yet
;;;    b) basic syntactical correctness has been assured (so a list l starting
;;;       with 'define-inline will have the procedure-name as (caadr l) and
;;;       arity for all procedure calls is correct)
;;;    c) alpha substitution has occurred so all named symbols are guaranteed
;;;       unique across all procedures
;;;    d) optional, keyword, and rest arguments are not allowed for inline
;;;       procedures (although it should be possible to add them)

;; beginning of the pass
;; takes the ordered quoted list of all top-level statements
;; ends by calling either
;;    inline-pass:final with the input list (if no inline procedures exist) and
;;        null, or
;;    inline-pass:graph-inline with two lists, the inline procedures (with some
;;        metadata) and all non-inline-procedure statements.
(define (inline-pass:start qlst)
    (let find-inline ((q   qlst)   ; quoted top-level statements
                      (i   0)      ; index of inline procedure for later steps
                      (l   '())    ; inline procedures
                      (r   '()))   ; non-inline statements
        (cond ((null? q)
                  (if (= 0 i)
                      (inline-pass:final (reverse r) '())
                      (inline-pass:graph-inline i (reverse l) (reverse r))))
              ((and (list? (car q)) (eq? 'define-inline (caar q)))
                  (find-inline
                      (cdr q)
                      (+ 1 i)
                      (cons (cons (caadar q)
                                  (vector i 0 (cddar q) (cdadar q)))
                            l)
                      r))
              (else
                  (find-inline (cdr q) i l (cons (car q) r))))))


;; walks through a list
;; takes a list, an index vector, and the metadata inline list from above
;; ends by returning the (possibly modified) vector
(define (inline-pass:walk l v ilst)
    (let walk ((l   l)
               (t   0))
        (cond ((null? l)
                  v)
              ((list? (car l))
                  (cond ((null? (car l))
                            (walk (cdr l) t))
                        ((eq? 'quote (caar l))
                            (or (= 0 t)
                                (walk (cdar l) 3))
                            (walk (cdr l) t))
                        ((eq? 'quasiquote (caar l))
                            (walk (cdar l) 2)
                            (walk (cdr l) t))
                        ((or (eq? 'unquote (caar l))
                             (eq? 'unquote-splicing (caar l)))
                            (walk (cdar l) 1)
                            (walk (cdr l) t))
                        (else
                            (walk (car l) t)
                            (walk (cdr l) t))))
              ((pair? (car l))
                  (walk (unfold not-pair? car cdr (car l) list) t)
                  (walk (cdr l) t))
              ((vector? (car l))
                  (walk (vector->list (car l)) t)
                  (walk (cdr l) t))
              ((not (symbol? (car l)))
                  (walk (cdr l) t))
              ((> t 1)
                  (walk (cdr l) t))
              ((alist-ref (car l) ilst) =>
                  (lambda (d)
                      (vector-set! v (vector-ref d 0) #t)
                      (walk (cdr l) t)))
              (else
                  (walk (cdr l) t)))))


;; builds a graph of calls to inline procedures from inline procedures
;; takes the inline-list-length, inline metadata list, and other statements
;; ends by calling inline-pass:simplify1 with the graph and input args
(define (inline-pass:graph-inline i ilst rlst)
    (inline-pass:simplify1
        (map
            (lambda (iv)
                (cons (car iv)
                      (inline-pass:walk
                          (vector-ref (cdr iv) 3)
                          (make-vector i #f)
                          ilst)))
            ilst)
        i ilst rlst))


;; simplifies direct self-call, no further inline, and only-self cases
;; takes the graph, inline list length, inline metadata list, and statements
;; ends by calling either:
;;    inline-pass:simplify2 with the further inline, no-further-but-self inline,
;;        graph, inline length, all inline, and other statements, or
;;    inline-pass:final with the statements and inlines
(define (inline-pass:simplify1 g i ilst rlst)
    (for-each
        (lambda (x)
            (and (vector-ref (cdr x) (car x))
                 (vector-set! (cdr (list-ref ilst (car x))) 1 1)))
        g)
    (let simple ((h   g)      ; graph
                 (l   ilst)   ; inline metadata
                 (r   '())    ; no further inlines (except possibly self)
                 (s   '()))   ; further inlining
        (cond ((null? h)
                  (if (null? s)
                      (inline-pass:final rlst r)
                      (inline-pass:simplify2 s r g i ilst rlst)))
              ((every (lambda (x i) (or (= i (caar h)) (not x)))
                      (vector->list (cdar h)) (iota i))
                  (simple (cdr h) (cdr l) (cons (car l) r) s))
              (else
                  (simple (cdr h) (cdr l) r (cons (car l) s))))))

;; substitutes in inlined procedures
;; takes the procedure in which to do the substitution (as a list) and the
;;     list of inlined procedures with metadata
;; ends with the new procedure-as-list
;; note: there are four distinct cases -
;;       1) inline procedure in application position, no self call :
;;          becomes a (begin ...) with the arguments set locally
;;       2) inline procedure in application position, with self call :
;;          becomes a (let <name> (vars ...) ...) 
;;       3) inline procedure not in application position, no self call :
;;          becomes a (lambda (arglist) ...)
;;       4) inline procedure not in application position, with self call :
;;          becomes a (lambda (arglist) (let <name> (vars ...) ...) with new
;;          symbols generated for arglist
(define (inline-pass:subst1 l ilst)
    (let walk ((l   l)
               (t   0))
        (cond ((null? l)
                  l)
              ((vector? l)
                  (list->vector (walk (vector->list l) t)))
              ((symbol? l)
                  (cond ((> t 1)
                            l)
                        ((alist-ref l ilst) =>
                            (lambda (d)
                                (if (= 1 (vector-ref d 1))
                                    (let* ((a   (map
                                                    (lambda (x) (gensym 'ia))
                                                    (vector-ref d 2)))
                                           (m   (map
                                                    (lambda (a x) (list a x))
                                                    (vector-ref d 2) a)))
                                        `(lambda ,a (let ,l ,m
                                                         ,@(vector-ref d 3))))
                                    `(lambda ,(vector-ref d 2)
                                        ,@(vector-ref d 3)))))
                        (else
                            l)))
              ((not (pair? l))
                  l)
              ((list? (car l))
                  (cond ((null? (car l))
                            (cons (car l) (walk (cdr l) t)))
                        ((not (symbol? (caar l)))
                            (cons (walk (car l) t) (walk (cdr l) t)))
                        ((eq? 'quote (caar l))
                            (if (= t 0)
                                (cons (car l) (walk (cdr l) t))
                                (cons `(quote ,(walk (cadr l) 3))
                                      (walk (cdr l) t))))
                        ((eq? 'quasiquote (caar l))
                            (cons `(quasiquote ,(walk (cadr l) 2))
                                  (walk (cdr l) t)))
                        ((or (eq? 'unquote (caar l))
                             (eq? 'unquote-splicing (caar l)))
                            (cons `(,(caar l) ,(walk (cadr l) 1))
                                  (walk (cdr l) t)))
                        ((> t 1)
                            (cons (walk (car l) t) (walk (cdr l) t)))
                        ((alist-ref (caar l) ilst) =>
                            (lambda (d)
                                (cons
                                    (if (= 1 (vector-ref d 1))
                                        (let ((m   (map
                                                       (lambda (a x) (list a x))
                                                       (vector-ref d 2)
                                                       (walk (cdar l) t))))
                                            `(let ,(caar l) ,m
                                                  ,@(vector-ref d 3)))
                                        `(begin
                                            ,@(map
                                                  (lambda (a x)
                                                      `(set-local! ,a ,x))
                                                  (vector-ref d 2)
                                                  (walk (cdar l) t))
                                            ,@(vector-ref d 3)))
                                    (walk (cdr l) t))))
                        (else
                            (cons (walk (car l) t) (walk (cdr l) t)))))
              ((pair? (car l))
                  (cons (cons (walk (caar l) t) (walk (cdar l) t))
                        (walk (cdr l) t)))
              ((vector? (car l))
                  (cons (list->vector (walk (vector->list (car l)) t))
                        (walk (cdr l) t)))
              ((not (symbol? (car l)))
                  (cons (car l) (walk (cdr l) t)))
              ((> t 1)
                  (cons (car l) (walk (cdr l) t)))
              ((alist-ref (car l) ilst) =>
                  (lambda (d)
                      (cons
                          (if (= 1 (vector-ref d 1))
                              (let* ((a   (map
                                              (lambda (x) (gensym 'ia))
                                              (vector-ref d 2)))
                                     (m   (map
                                              (lambda (a x) (list a x))
                                              (vector-ref d 2) a)))
                                  `(lambda ,a (let ,(car l) ,m
                                                  ,@(vector-ref d 3))))
                              `(lambda ,(vector-ref d 2) ,@(vector-ref d 3)))
                          (walk (cdr l) t))))
              (else
                  (cons (car l) (walk (cdr l) t))))))


;; substitutes in inlined procedures with further processing
;; takes the procedure in which to do the substitution (as a list), the
;;     list of inlined procedures with metadata, and a list of procedures to
;;     not treat as inline
;; ends with the new procedure-as-list
;; note: there are four distinct cases -
;;       1) inline procedure in application position, no self call :
;;          becomes a (begin ...) with the arguments set locally
;;       2) inline procedure in application position, with self call :
;;          becomes a (let <name> (vars ...) ...) 
;;       3) inline procedure not in application position, no self call :
;;          becomes a (lambda (arglist) ...)
;;       4) inline procedure not in application position, with self call :
;;          becomes a (lambda (arglist) (let <name> (vars ...) ...) with new
;;          symbols generated for arglist
(define (inline-pass:subst2 l ilst nof)
    (let walk ((l   l)
               (n   nof)
               (t   0))
        (cond ((null? l)
                  l)
              ((vector? l)
                  (list->vector (walk (vector->list l) t n)))
              ((symbol? l)
                  (cond ((> t 1)
                            l)
                        ((memq l n) =>
                            (lambda (m)
                                (let ((d   (alist-ref l ilst)))
                                    (if (= 1 (vector-ref d 1))
                                        l
                                        (begin
                                            (vector-set! d 1 1)
                                            (if (= 1 (length m))
                                                l
                                                (walk l t (cdr m))))))))
                        ((alist-ref l ilst) =>
                            (lambda (d)
                                (if (= 1 (vector-ref d 1))
                                    (let* ((a   (map
                                                    (lambda (x) (gensym 'ia))
                                                    (vector-ref d 2)))
                                           (m   (map
                                                    (lambda (a x) (list a x))
                                                    (vector-ref d 2) a)))
                                        `(lambda ,a (let ,l ,m
                                            ,@(walk (vector-ref d 3) t
                                                    (cons l n)))))
                                    `(lambda ,(vector-ref d 2)
                                        ,@(walk (vector-ref d 3) t
                                                (cons l n))))))
                        (else
                            l)))
              ((not (pair? l))
                  l)
              ((list? (car l))
                  (cond ((null? (car l))
                            (cons (car l) (walk (cdr l) t n)))
                        ((not (symbol? (caar l)))
                            (cons (walk (car l) t n) (walk (cdr l) t n)))
                        ((eq? 'quote (caar l))
                            (if (= t 0)
                                (cons (car l) (walk (cdr l) t n))
                                (cons `(quote ,(walk (cadr l) 3 n))
                                      (walk (cdr l) t n))))
                        ((eq? 'quasiquote (caar l))
                            (cons `(quasiquote ,(walk (cadr l) 2 n))
                                  (walk (cdr l) t n)))
                        ((or (eq? 'unquote (caar l))
                             (eq? 'unquote-splicing (caar l)))
                            (cons `(,(caar l) ,(walk (cadr l) 1 n))
                                  (walk (cdr l) t n)))
                        ((> t 1)
                            (cons (walk (car l) t n) (walk (cdr l) t n)))
                        ((memq (caar l) n) =>
                            (lambda (m)
                                (let ((d   (alist-ref (caar l) ilst)))
                                    (if (= 1 (vector-ref d 1))
                                        (cons (cons (caar l)
                                                    (walk (cdar l) t n))
                                              (walk (cdr l) t n))
                                        (begin
                                            (vector-set! d 1 1)
                                            (if (= 1 (length m))
                                                (cons (cons (caar l)
                                                            (walk (cdar l) t n))
                                                      (walk (cdr l) t n))
                                                (walk l t
                                                      (cdr m))))))))
                        ((alist-ref (caar l) ilst) =>
                            (lambda (d)
                                (cons
                                    (if (= 1 (vector-ref d 1))
                                        (let ((m   (map
                                                       (lambda (a x) (list a x))
                                                       (vector-ref d 2)
                                                       (walk (cdar l) t
                                                           (cons (caar l) n)))))
                                            `(let ,(caar l) ,m
                                                  ,@(walk (vector-ref d 3) t
                                                          (cons (caar l) n))))
                                        `(begin
                                            ,@(map
                                                  (lambda (a x)
                                                      `(set-local! ,a ,x))
                                                  (vector-ref d 2)
                                                  (walk (cdar l) t
                                                      (cons (caar l) n)))
                                            ,@(walk (vector-ref d 3) t
                                                    (cons (caar l) n))))
                                    (walk (cdr l) t n))))
                        (else
                            (cons (walk (car l) t n) (walk (cdr l) t n)))))
              ((pair? (car l))
                  (cons (cons (walk (caar l) t n) (walk (cdar l) t n))
                        (walk (cdr l) t n)))
              ((vector? (car l))
                  (cons (list->vector (walk (vector->list (car l)) t n))
                        (walk (cdr l) t n)))
              ((not (symbol? (car l)))
                  (cons (car l) (walk (cdr l) t n)))
              ((> t 1)
                  (cons (car l) (walk (cdr l) t)))
              ((memq (car l) n) =>
                  (lambda (m)
                      (let ((d   (alist-ref (car l) ilst)))
                          (if (= 1 (vector-ref d 1))
                              (cons (car l) (walk (cdr l) t n))
                              (begin
                                  (vector-set! d 1 1)
                                  (if (= 1 (length m))
                                      (cons (car l) (walk (cdr l) t n))
                                      (walk l t (cdr m))))))))
              ((alist-ref (car l) ilst) =>
                  (lambda (d)
                      (cons
                          (if (= 1 (vector-ref d 1))
                              (let* ((a   (map
                                              (lambda (x) (gensym 'ia))
                                              (vector-ref d 2)))
                                     (m   (map
                                              (lambda (a x) (list a x))
                                              (vector-ref d 2) a)))
                                  `(lambda ,a (let ,l ,m 
                                      ,@(walk (vector-ref d 3) t
                                              (cons (car l) n)))))
                              `(lambda ,(vector-ref d 2) 
                                  ,@(walk (vector-ref d 3) t (cons (car l) n))))
                          (walk (cdr l) t n))))
              (else
                  (cons (car l) (walk (cdr l) t n))))))

;; finds which inlined procedures are called from non-inlined procedures
;; performs substitutions for all inline procedures
;; takes the further inline procedures, no further inline procedures, graph,
;;     inlined procedures list, and statements list
;; ends by calling inline-pass:final with the statements and inline procedures
;;     ready for substitution
(define (inline-pass:simplify2 fur nof g ilst rlst)
    (for-each
        (lambda (x)
            (vector-set! (cdr x) 3
                (inline-pass:subst1 (vector-ref (cdr x) 3) nof)))
        fur)
    (let ((v   (inline-pass:walk rlst (make-vector i #f) fur)))
        (for-each
            (lambda (x)
                (vector-set! (cdr x) 3
                    (inline-pass:subst2 (vector-ref (cdr x) 3) ilst
                        (list (car x)))))
            (vector-fold
                (lambda (i r x)
                    (if x
                        (cons (list-ref ilst i) r)
                        r))
                '() v))
        (inline-pass:final rlst ilst)))


;; inlines all procedures
;; takes the list of statements and the list of inline procedures with metadata
;; returns the list of statements with all procedures inlined
(define (inline-pass:final rlst ilst)
    (if (null? ilst)
        rlst
        (inline-pass:subst1 rlst ilst)))

