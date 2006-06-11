;;;; pregexp.scm - Portable regular expression library using Dorai Sitaram's PREGEXP package

;Portable regular expressions for Scheme
;Dorai Sitaram
;http://www.ccs.neu.edu/~dorai
;ds26 AT gte.com
;Oct 2, 1999

; (With additions by Felix Winkelmann)


(cond-expand [(not chicken-compile-shared) (declare (unit regex))] [else])

(declare
  (fixnum)
  (disable-interrupts)
  (usual-integrations)
  (hide pregexp-reverse! pregexp-read-pattern pregexp-read-branch
	pregexp-read-piece pregexp-read-escaped-number pregexp-read-escaped-char
	pregexp-read-posix-char-class pregexp-read-cluster-type pregexp-read-subpattern
	pregexp-wrap-quantifier-if-any pregexp-read-nums pregexp-invert-char-list
	pregexp-read-char-list pregexp-string-match pregexp-char-word?
	pregexp-at-word-boundary? pregexp-check-if-in-char-class? pregexp-list-ref
	pregexp-match-positions-aux pregexp-replace-aux
	pregexp-match pregexp-match-positions pregexp-split pregexp-replace pregexp-replace*) )

(cond-expand
 [paranoia]
 [else
  (declare
    (no-bound-checks)
    (no-procedure-checks-for-usual-bindings)
    (bound-to-procedure
     ##sys#check-string ##sys#check-exact ##sys#make-pointer ##sys#cons ##sys#size ##sys#slot) ) ] )

(cond-expand
 [unsafe
  (eval-when (compile)
    (define-macro (##sys#check-structure . _) '(##core#undefined))
    (define-macro (##sys#check-range . _) '(##core#undefined))
    (define-macro (##sys#check-pair . _) '(##core#undefined))
    (define-macro (##sys#check-list . _) '(##core#undefined))
    (define-macro (##sys#check-symbol . _) '(##core#undefined))
    (define-macro (##sys#check-string . _) '(##core#undefined))
    (define-macro (##sys#check-char . _) '(##core#undefined))
    (define-macro (##sys#check-exact . _) '(##core#undefined))
    (define-macro (##sys#check-port . _) '(##core#undefined))
    (define-macro (##sys#check-number . _) '(##core#undefined))
    (define-macro (##sys#check-byte-vector . _) '(##core#undefined)) ) ]
 [else
  (declare (emit-exports "pregexp.exports"))] )

(register-feature! 'regex 'pregexp)


(define-constant *pregexp-comment-char* #\#)
(define-constant *pregexp-return-char* #\return)
(define-constant *pregexp-tab-char* #\tab)

(define *pregexp-space-sensitive?* #t)

(define pregexp-reverse!
  ;the useful reverse! isn't R5RS
  (lambda (s)
    (let loop ((s s) (r '()))
      (if (null? s) r
          (let ((d (cdr s)))
            (set-cdr! s r)
            (loop d s))))))

(define (pregexp-error . args)
  (apply ##sys#error "pregexp-error: " args) )

(define pregexp-read-pattern
  (lambda (s i n)
    (if (>= i n)
        (list
          (list ':or (list ':seq)) i)
        (let loop ((branches '()) (i i))
          (if (or (>= i n)
                  (char=? (string-ref s i) #\)))
              (list (cons ':or (pregexp-reverse! branches)) i)
              (let ((vv (pregexp-read-branch
                          s
                          (if (char=? (string-ref s i) #\|) (+ i 1) i) n)))
                (loop (cons (car vv) branches) (cadr vv))))))))

(define pregexp-read-branch
  (lambda (s i n)
    (let loop ((pieces '()) (i i))
      (cond ((>= i n)
             (list (cons ':seq (pregexp-reverse! pieces)) i))
            ((let ((c (string-ref s i)))
               (or (char=? c #\|)
                   (char=? c #\))))
             (list (cons ':seq (pregexp-reverse! pieces)) i))
            (else (let ((vv (pregexp-read-piece s i n)))
                    (loop (cons (car vv) pieces) (cadr vv))))))))

(define pregexp-read-piece
  (lambda (s i n)
    (let ((c (string-ref s i)))
      (case c
        ((#\^) (list ':bos (+ i 1)))
        ((#\$) (list ':eos (+ i 1)))
        ((#\.) (pregexp-wrap-quantifier-if-any
                 (list ':any (+ i 1)) s n))
        ((#\[) (pregexp-wrap-quantifier-if-any
                 (case (string-ref s (+ i 1))
                   ((#\^)
                    (let ((vv (pregexp-read-char-list s (+ i 2) n)))
                      (list (list ':neg-char (car vv)) (cadr vv))))
                   (else (pregexp-read-char-list s (+ i 1) n)))
                 s n))
        ((#\()
         (pregexp-wrap-quantifier-if-any
           (pregexp-read-subpattern s (+ i 1) n) s n))
        ((#\\)
         (pregexp-wrap-quantifier-if-any
           (cond ((pregexp-read-escaped-number s i n) =>
                  (lambda (num-i)
                    (list (list ':backref (car num-i)) (cadr num-i))))
                 ((pregexp-read-escaped-char s i n) =>
                  (lambda (char-i)
                    (list (car char-i) (cadr char-i))))
                 (else (pregexp-error 'pregexp-read-piece "backslash")))
           s n))
        (else
          (if (or *pregexp-space-sensitive?*
                  (and (not (char-whitespace? c))
                       (not (char=? c *pregexp-comment-char*))))
              (pregexp-wrap-quantifier-if-any
                (list c (+ i 1)) s n)
              (let loop ((i i) (in-comment? #f))
                (if (>= i n) (list ':empty i)
                    (let ((c (string-ref s i)))
                      (cond (in-comment?
                              (loop (+ i 1)
                                (not (char=? c #\newline))))
                            ((char-whitespace? c)
                             (loop (+ i 1) #f))
                            ((char=? c *pregexp-comment-char*)
                             (loop (+ i 1) #t))
                            (else (list ':empty i))))))))))))

(define pregexp-read-escaped-number
  (lambda (s i n)
    ; s[i] = \
    (and (< (+ i 1) n) ;must have at least something following \
         (let ((c (string-ref s (+ i 1))))
           (and (char-numeric? c)
                (let loop ((i (+ i 2)) (r (list c)))
                  (if (>= i n)
                      (list (string->number
                              (list->string (pregexp-reverse! r))) i)
                      (let ((c (string-ref s i)))
                        (if (char-numeric? c)
                            (loop (+ i 1) (cons c r))
                            (list (string->number
                                    (list->string (pregexp-reverse! r)))
                              i))))))))))

(define pregexp-read-escaped-char
  (lambda (s i n)
    ; s[i] = \
    (and (< (+ i 1) n)
         (let ((c (string-ref s (+ i 1))))
           (case c
             ((#\b) (list ':wbdry (+ i 2)))
             ((#\B) (list ':not-wbdry (+ i 2)))
             ((#\d) (list ':digit (+ i 2)))
             ((#\D) (list '(:neg-char :digit) (+ i 2)))
             ((#\n) (list #\newline (+ i 2)))
             ((#\r) (list *pregexp-return-char* (+ i 2)))
             ((#\s) (list ':space (+ i 2)))
             ((#\S) (list '(:neg-char :space) (+ i 2)))
             ((#\t) (list *pregexp-tab-char* (+ i 2)))
             ((#\w) (list ':word (+ i 2)))
             ((#\W) (list '(:neg-char :word) (+ i 2)))
             (else (list c (+ i 2))))))))

(define pregexp-read-posix-char-class
  (lambda (s i n)
    ; lbrack, colon already read
    (let ((neg? #f))
      (let loop ((i i) (r (list #\:)))
        (if (>= i n)
            (pregexp-error 'pregexp-read-posix-char-class)
            (let ((c (string-ref s i)))
              (cond ((char=? c #\^)
                     (set! neg? #t)
                     (loop (+ i 1) r))
                    ((char-alphabetic? c)
                     (loop (+ i 1) (cons c r)))
                    ((char=? c #\:)
                     (if (or (>= (+ i 1) n)
                             (not (char=? (string-ref s (+ i 1)) #\])))
                         (pregexp-error 'pregexp-read-posix-char-class)
                         (let ((posix-class
                                 (string->symbol
                                   (list->string (pregexp-reverse! r)))))
                           (list (if neg? (list ':neg-char posix-class)
                                     posix-class)
                             (+ i 2)))))
                    (else
                      (pregexp-error 'pregexp-read-posix-char-class)))))))))

(define pregexp-read-cluster-type
  (lambda (s i n)
    ; s[i-1] = left-paren
    (let ((c (string-ref s i)))
      (case c
        ((#\?)
         (let ((i (+ i 1)))
           (case (string-ref s i)
             ((#\:) (list '() (+ i 1)))
             ((#\=) (list '(:lookahead) (+ i 1)))
             ((#\!) (list '(:neg-lookahead) (+ i 1)))
             ((#\>) (list '(:no-backtrack) (+ i 1)))
             ((#\<)
              (list (case (string-ref s (+ i 1))
                      ((#\=) '(:lookbehind))
                      ((#\!) '(:neg-lookbehind))
                      (else (pregexp-error 'pregexp-read-cluster-type)))
                (+ i 2)))
             (else (let loop ((i i) (r '()) (inv? #f))
                     (let ((c (string-ref s i)))
                       (case c
                         ((#\-) (loop (+ i 1) r #t))
                         ((#\i) (loop (+ i 1)
                                  (cons (if inv? ':case-sensitive
                                            ':case-insensitive) r) #f))
                         ((#\x)
                          (set! *pregexp-space-sensitive?* inv?)
                          (loop (+ i 1) r #f))
                         ((#\:) (list r (+ i 1)))
                         (else (pregexp-error 'pregexp-read-cluster-type)))))))))
        (else (list '(:sub) i))))))
 
(define pregexp-read-subpattern
  (lambda (s i n)
    (let* ((remember-space-sensitive? *pregexp-space-sensitive?*)
           (ctyp-i (pregexp-read-cluster-type s i n))
           (ctyp (car ctyp-i))
           (i (cadr ctyp-i))
           (vv (pregexp-read-pattern s i n)))
      (set! *pregexp-space-sensitive?* remember-space-sensitive?)
      (let ((vv-re (car vv))
            (vv-i (cadr vv)))
        (if (and (< vv-i n)
                 (char=? (string-ref s vv-i) 
                   #\)))
            (list
              (let loop ((ctyp ctyp) (re vv-re))
                (if (null? ctyp) re
                    (loop (cdr ctyp)
                      (list (car ctyp) re))))
              (+ vv-i 1))
            (pregexp-error 'pregexp-read-subpattern))))))

(define pregexp-wrap-quantifier-if-any
  (lambda (vv s n)
    (let ((re (car vv)))
      (let loop ((i (cadr vv)))
        (if (>= i n) vv
            (let ((c (string-ref s i)))
              (if (and (char-whitespace? c) (not *pregexp-space-sensitive?*))
                  (loop (+ i 1))
                  (case c
                    ((#\* #\+ #\? #\{)
                     (let* ((new-re (list ':between 'minimal?
                                      'at-least 'at-most re))
                            (new-vv (list new-re 'next-i)))
                       (case c
                         ((#\*) (set-car! (cddr new-re) 0)
                          (set-car! (cdddr new-re) #f))
                         ((#\+) (set-car! (cddr new-re) 1)
                          (set-car! (cdddr new-re) #f))
                         ((#\?) (set-car! (cddr new-re) 0)
                          (set-car! (cdddr new-re) 1))
                         ((#\{) (let ((pq (pregexp-read-nums s (+ i 1) n)))
                                  (if (not pq)
                                      (pregexp-error 
                                        'pregexp-wrap-quantifier-if-any
                                        "left bracket must be followed by number"))
                                  (set-car! (cddr new-re) (car pq))
                                  (set-car! (cdddr new-re) (cadr pq))
                                  (set! i (caddr pq)))))
                       (let loop ((i (+ i 1)))
                         (if (>= i n)
                             (begin (set-car! (cdr new-re) #f)
                               (set-car! (cdr new-vv) i))
                             (let ((c (string-ref s i)))
                               (cond ((and (char-whitespace? c)
                                           (not *pregexp-space-sensitive?*))
                                      (loop (+ i 1)))
                                     ((char=? c #\?)
                                      (set-car! (cdr new-re) #t)
                                      (set-car! (cdr new-vv) (+ i 1)))
                                     (else (set-car! (cdr new-re) #f)
                                       (set-car! (cdr new-vv) i))))))
                       new-vv))
                    (else vv)))))))))

;

(define pregexp-read-nums
  (lambda (s i n)
    ; s[i-1] = {
    ; returns (p q k) where s[k] = }
    (let loop ((p '()) (q '()) (k i) (reading 1))
      (if (>= k n) (pregexp-error 'pregexp-read-nums))
      (let ((c (string-ref s k)))
        (cond ((char-numeric? c)
               (if (= reading 1)
                   (loop (cons c p) q (+ k 1) 1)
                   (loop p (cons c q) (+ k 1) 2)))
              ((and (char-whitespace? c) (not *pregexp-space-sensitive?*))
               (loop p q (+ k 1) reading))
              ((and (char=? c #\,) (= reading 1))
               (loop p q (+ k 1) 2))
              ((char=? c #\})
               (let ((p (string->number (list->string (pregexp-reverse! p))))
                     (q (string->number (list->string (pregexp-reverse! q)))))
                 (cond ((and (not p) (= reading 1)) (list 0 #f k))
                       ((= reading 1) (list p p k))
                       (else (list p q k)))))
              (else #f))))))

(define pregexp-invert-char-list
  (lambda (vv)
    (set-car! (car vv) ':none-of-chars)
    vv))

;

(define pregexp-read-char-list
  (lambda (s i n)
    (let loop ((r '()) (i i))
      (if (>= i n)
          (pregexp-error 'pregexp-read-char-list
                         "character class ended too soon")
          (let ((c (string-ref s i)))
            (case c
              ((#\]) (if (null? r)
                         (loop (cons c r) (+ i 1))
                         (list (cons ':one-of-chars (pregexp-reverse! r)) 
                               (+ i 1))))
              ((#\\)
               (let ((char-i (pregexp-read-escaped-char s i n)))
                 (if char-i (loop (cons (car char-i) r) (cadr char-i))
                     (pregexp-error 'pregexp-read-char-list "backslash"))))
              ((#\-) (let ((c-prev (car r)))
                       (if (char? c-prev)
                           (loop (cons (list ':char-range c-prev
                                             (string-ref s (+ i 1))) (cdr r))
                                 (+ i 2))
                           (loop (cons c r) (+ i 1)))))
              ((#\[) (if (char=? (string-ref s (+ i 1)) #\:)
                         (let ((posix-char-class-i
                                 (pregexp-read-posix-char-class s (+ i 2) n)))
                           (loop (cons (car posix-char-class-i) r)
                                 (cadr posix-char-class-i)))
                         (loop (cons c r) (+ i 1))))
              (else (loop (cons c r) (+ i 1)))))))))

;

(define pregexp-string-match
  (lambda (s1 s i n sk fk)
    (let ((n1 (string-length s1)))
      (if (> n1 n) (fk)
          (let loop ((j 0) (k i))
            (cond ((>= j n1) (sk k))
                  ((>= k n) (fk))
                  ((char=? (string-ref s1 j) (string-ref s k))
                   (loop (+ j 1) (+ k 1)))
                  (else (fk))))))))

(define pregexp-char-word?
  (lambda (c)
    ;too restrictive for Scheme but this
    ;is what \w is in most regexp notations
    (or (char-alphabetic? c)
        (char-numeric? c)
        (char=? c #\_))))

(define pregexp-at-word-boundary?
  (lambda (s i n)
    (or (= i 0) (>= i n)
        (let ((c/i (string-ref s i))
              (c/i-1 (string-ref s (- i 1))))
          (let ((c/i/w? (pregexp-check-if-in-char-class?
                          c/i ':word))
                (c/i-1/w? (pregexp-check-if-in-char-class?
                            c/i-1 ':word)))
            (or (and c/i/w? (not c/i-1/w?))
                (and (not c/i/w?) c/i-1/w?)))))))

(define pregexp-check-if-in-char-class?
  (lambda (c char-class)
    (case char-class
      ((:any) (not (char=? c #\newline)))
      ;
      ((:alnum) (or (char-alphabetic? c) (char-numeric? c)))
      ((:alpha) (char-alphabetic? c))
      ((:ascii) (< (char->integer c) 128))
      ((:blank) (or (char=? c #\space) (char=? c *pregexp-tab-char*)))
      ((:cntrl) (< (char->integer c) 32))
      ((:digit) (char-numeric? c))
      ((:graph) (and (>= (char->integer c) 32)
                     (not (char-whitespace? c))))
      ((:lower) (char-lower-case? c))
      ((:print) (>= (char->integer c) 32))
      ((:punct) (and (>= (char->integer c) 32)
                     (not (char-whitespace? c))
                     (not (char-alphabetic? c))
                     (not (char-numeric? c))))
      ((:space) (char-whitespace? c))
      ((:upper) (char-upper-case? c))
      ((:word) (or (char-alphabetic? c)
                   (char-numeric? c)
                   (char=? c #\_)))
      ((:xdigit) (or (char-numeric? c)
                     (char-ci=? c #\a) (char-ci=? c #\b)
                     (char-ci=? c #\c) (char-ci=? c #\d)
                     (char-ci=? c #\e) (char-ci=? c #\f)))
      (else (pregexp-error 'pregexp-check-if-in-char-class?)))))

(define pregexp-list-ref
  (lambda (s i)
    ;like list-ref but returns #f if index is
    ;out of bounds
    (let loop ((s s) (k 0))
      (cond ((null? s) #f)
            ((= k i) (car s))
            (else (loop (cdr s) (+ k 1)))))))

;re is a compiled regexp.  It's a list that can't be
;nil.  pregexp-match-positions-aux returns a 2-elt list whose
;car is the string-index following the matched
;portion and whose cadr contains the submatches.
;The proc returns false if there's no match.

;Am spelling loop- as loup- because these shouldn't
;be translated into CL loops by scm2cl (although
;they are tail-recursive in Scheme)

(define pregexp-match-positions-aux
  (lambda (re s start n i)
    (let ((case-sensitive? #t))
      (let sub ((re re) (i i) (backrefs '()) (sk list) (fk (lambda () #f)))
        ;(printf "sub ~s ~s~%" i re)
        (cond ((eqv? re ':bos)
               (if (= i start) (sk i backrefs) (fk)))
              ((eqv? re ':eos)
               (if (>= i n) (sk i backrefs) (fk)))
              ((eqv? re ':empty)
               (sk i backrefs))
              ((eqv? re ':wbdry)
               (if (pregexp-at-word-boundary? s i n)
                   (sk i backrefs)
                   (fk)))
              ((eqv? re ':not-wbdry)
               (if (pregexp-at-word-boundary? s i n)
                   (fk)
                   (sk i backrefs)))
              ((and (char? re) (< i n))
               (if ((if case-sensitive? char=? char-ci=?)
                    (string-ref s i) re)
                   (sk (+ i 1) backrefs) (fk)))
              ((and (not (pair? re)) (< i n))
               (if (pregexp-check-if-in-char-class?
                     (string-ref s i) re)
                   (sk (+ i 1) backrefs) (fk)))
              ((and (pair? re) (eqv? (car re) ':char-range) (< i n))
               (let ((c (string-ref s i)))
                 (if (let ((c< (if case-sensitive? char<=? char-ci<=?)))
                       (and (c< (cadr re) c)
                            (c< c (caddr re))))
                     (sk (+ i 1) backrefs) (fk))))
              ((pair? re)
               (case (car re)
                 ((:char-range)
                  (if (>= i n) (fk) (pregexp-error 'pregexp-match-positions-aux)))
                 ((:one-of-chars)
                  (if (>= i n) (fk)
                      (let loup-one-of-chars ((chars (cdr re)))
                        (if (null? chars) (fk)
                            (sub (car chars) i backrefs sk
                              (lambda ()
                                (loup-one-of-chars (cdr chars))))))))
                 ((:neg-char)
                  (if (>= i n) (fk)
                      (sub (cadr re) i backrefs
                        (lambda (i1 backrefs1) (fk))
                        (lambda () (sk (+ i 1) backrefs)))))
                 ((:seq)
                  (let loup-seq ((res (cdr re)) (i i) (backrefs backrefs))
                    (if (null? res) (sk i backrefs)
                        (sub (car res) i backrefs
                          (lambda (i1 backrefs1)
                            (loup-seq (cdr res) i1 backrefs1))
                          fk))))
                 ((:or)
                  (let loup-or ((res (cdr re)))
                    (if (null? res) (fk)
                        (sub (car res) i backrefs
                          (lambda (i1 backrefs1)
                            (or (sk i1 backrefs1)
                                (loup-or (cdr res))))
                          (lambda () (loup-or (cdr res)))))))
                 ((:backref)
                  (let ((backref (pregexp-list-ref backrefs (cadr re))))
                    (if backref
                        (pregexp-string-match
                          (substring s (car backref) (cdr backref))
                          s i n (lambda (i) (sk i backrefs)) fk)
                        (sk i backrefs))))
                 ((:sub)
                  (let* ((sub-backref (cons i i))
                         (backrefs (append backrefs (list sub-backref))))
                    (sub (cadr re) i backrefs
                      (lambda (i1 backrefs1)
                        (set-cdr! sub-backref i1)
                        (sk i1 backrefs1)) fk)))
                 ((:lookahead)
                  (let ((found-it?
                          (sub (cadr re) i backrefs
                            list (lambda () #f))))
                    (if found-it? (sk i backrefs) (fk))))
                 ((:neg-lookahead)
                  (let ((found-it?
                          (sub (cadr re) i backrefs
                            list (lambda () #f))))
                    (if found-it? (fk) (sk i backrefs))))
                 ((:lookbehind)
                  (let ((n-actual n)) (set! n i)
                    (let ((found-it?
                            (sub (list ':seq '(:between #f 0 #f :any)
                                   (cadr re) ':eos) 0 backrefs
                              list (lambda () #f))))
                      (set! n n-actual)
                      (if found-it? (sk i backrefs) (fk)))))
                 ((:neg-lookbehind)
                  (let ((n-actual n)) (set! n i)
                    (let ((found-it?
                            (sub (list ':seq '(:between #f 0 #f :any)
                                   (cadr re) ':eos) 0 backrefs
                              list (lambda () #f))))
                      (set! n n-actual)
                      (if found-it? (fk) (sk i backrefs)))))
                 ((:no-backtrack)
                  (let ((found-it? (sub (cadr re) i backrefs
                                     list (lambda () #f))))
                    (if found-it?
                        (sk (car found-it?) (cadr found-it?))
                        (fk))))
                 ((:case-sensitive :case-insensitive)
                  (let ((old case-sensitive?))
                    (set! case-sensitive?
                      (eqv? (car re) ':case-sensitive))
                    (sub (cadr re) i backrefs
                      (lambda (i1 backrefs1)
                        (set! case-sensitive? old)
                        (sk i1 backrefs1))
                      (lambda ()
                        (set! case-sensitive? old)
                        (fk)))))
                 ((:between)
                  (let* ((maximal? (not (cadr re)))
                         (p (caddr re)) (q (cadddr re))
                         (re (car (cddddr re)))
                         (subpat? (and (pair? re) (eqv? (car re) ':sub))))
                    (let loup-p ((k 0) (i i) (cbackrefs 'no-match-yet))
                      (if (< k p)
                          (sub re i backrefs
                            (lambda (i1 backrefs1)
                              (loup-p (+ k 1) i1 backrefs1))
                            fk)
                          (let ((q (and q (- q p))))
                            (let loup-q ((k 0) (i i) (cbackrefs cbackrefs))
                              (let ((fk (lambda ()
                                          (sk i (if (eqv? cbackrefs
                                                      'no-match-yet)
                                                    (if subpat?
                                                        (append backrefs
                                                          (list #f))
                                                        backrefs)
                                                    cbackrefs)))))
                                (if (and q (>= k q)) (fk)
                                    (if maximal?
                                        (sub re i backrefs
                                          (lambda (i1 backrefs1)
                                            (or (loup-q (+ k 1) i1 backrefs1)
                                                (fk)))
                                          fk)
                                        (or (fk)
                                            (sub re i backrefs
                                              (lambda (i1 backrefs1)
                                                (loup-q (+ k 1) i1 backrefs1))
                                              fk)))))))))))
                 (else (pregexp-error 'pregexp-match-positions-aux))))
              ((>= i n) (fk))
              (else (pregexp-error 'pregexp-match-positions-aux)))))))

(define pregexp-replace-aux
  (lambda (str ins n backrefs)
    (let loop ((i 0) (r ""))
      (if (>= i n) r
          (let ((c (string-ref ins i)))
            (if (char=? c #\\)
                (let* ((br-i (pregexp-read-escaped-number ins i n))
                       (br (if br-i (car br-i)
                               (if (char=? (string-ref ins (+ i 1)) #\&) 0
                                   #f)))
                       (i (if br-i (cadr br-i)
                              (if br (+ i 2)
                                  (+ i 1)))))
                  (if (not br)
                      (let ((c2 (string-ref ins i)))
                        (loop (+ i 1)
                          (if (char=? c2 #\$) r
                              (string-append r (string c2)))))
                      (loop i
                        (let ((backref (pregexp-list-ref backrefs br)))
                          (if backref
                              (string-append r
                                (substring str (car backref) (cdr backref)))
                              r)))))
                (loop (+ i 1) (string-append r (string c)))))))))

(define (extract-bit-field size position n)
  (fxand (fxnot (arithmetic-shift -1 size))
	 (arithmetic-shift n (- position))))

(define utf8-start-byte->length
  (let ((table '#(
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 0x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 1x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 2x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 3x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 4x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 5x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 6x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 7x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 8x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; 9x
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; ax
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 ; bx
2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ; cx
2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 ; dx
3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 ; ex
4 4 4 4 4 4 4 4 5 5 5 5 6 6 0 0 ; fx
)))
    (lambda (i) (vector-ref table i))))

(define (string-ref-at-byte s byte)
  (let* ((c (string-ref s byte))
         (ci (char->integer c))
         (len (utf8-start-byte->length ci)))
    (if (<= len 1)
      c
      (let ((end (+ byte len)))
        (if (> end (string-length s))
          (##sys#error 'regexp "utf8 trailing char overflow" s byte)
          (let loop ((i (+ byte 1)) (res (extract-bit-field (- 7 len) 0 c)))
            (if (= i end)
              (integer->char res)
              (loop (+ i 1)
                    (fxior
                     (fxshl res 6)
                     (fxand #b00111111
			    (char->integer (string-ref s i))))))))))))

(define-constant pat-utf8
  (let ((pat-ascii "[\x01-\x7F]")
        (pat-utf8-2 "[\xC2-\xDF][\x80-\xC1]")
        (pat-utf8-3 "[\xE0-\xEF][\x80-\xC1]{2}"))
    (reverse
     (string->list
      (string-append "(?:" pat-ascii "|" pat-utf8-2 "|" pat-utf8-3 ")")))))

(define (utf8-pattern->byte-pattern s)
  (let ((size (string-length s)))
    (define (scan i res)
      (if (= i size)
        (list->string (reverse res))
        (let ((c (string-ref s i)))
          (case c
            ((#\.) ; XXXX optimize the .+ cases
             (if (and (< (+ i 1) size) (eqv? #\* (string-ref s (+ i 1))))
               (scan (+ i 2) (cons #\* (cons #\. res)))
               (scan (+ i 1) (append pat-utf8 res))))
            ((#\\)
             (scan (+ i 2) (cons (string-ref s (+ i 1)) (cons #\\ res))))
            ((#\[) (class (+ i 1) '() #t res))
            (else (scan (+ i 1) (cons c res)))))))
    (define (class->group ls)
      (let loop ((ls ls) (acc '()))
        (if (null? ls)
          (string->list
           (string-append "(?:" (string-intersperse acc "|") ")"))
          (let ((c (car ls)) (rst (cdr ls)))
            (if (and (pair? rst) (eqv? #\- (car rst)))
              (let ((end (cadr rst)))
                (if (or (> (char->integer c) 128)
                        (> (char->integer end) 128))
                  (##sys#error 'regexp "full unicode classes not supported" s)
                  (loop (cddr rst)
                        (cons (string-append "[" (char->string end)
                                             "-" (char->string c) "]")
                              acc))))
              (loop (cdr ls) (cons (char->string c) acc)))))))
    (define (class i acc ascii? res)
      (if (= i size)
        (##sys#error 'regexp "incomplete character class" s)
        (let ((c (string-ref s i)))
          (case c
            ((#\])
             (if ascii?
               (scan (+ i 1) (cons #\] (append acc (cons #\[ res))))
               (scan (+ i 1) (append (reverse (class->group acc)) res))))
            ((#\\)
             (let ((next (string-ref s (+ i 1))))
               (if (< (char->integer next) 128)
                 (class (+ i 2) (cons next (cons #\\ acc)) ascii? res)
                 (class (+ i 1) (cons #\\ acc) ascii? res))))
            (else
             (if (< (char->integer c) 128)
               (class (+ i 1) (cons c acc) ascii? res)
               (class (+ i (utf8-start-byte->length (char->integer c)))
                      (cons (string-ref-at-byte s i) acc)
                      #f res)))))))
    (scan 0 '())))

(define %pregexp
  (lambda (s)
    (set! *pregexp-space-sensitive?* #t) ;in case it got corrupted
    (list ':sub (car (pregexp-read-pattern s 0 (string-length s))))))

(define (pregexp s . o)
  (%pregexp
   (if (null? o)
     s
     (let ((s1 (if (car o) (string-append "(?i:" s ")") s))
           (o1 (cdr o)))
       (if (null? o1)
         s1
         (let ((s2 (if (car o1) (string-append "(?x:" s1 ")") s1))
               (o2 (cdr o1)))
           (if (and (pair? o2) (car o2))
             (utf8-pattern->byte-pattern s2)
             s2)))))))

(define regexp pregexp)
(define (regexp? x) (and (pair? x) (eq? ':sub (##sys#slot x 0))))

(define pregexp-match-positions
  (lambda (pat str . opt-args)
    (let* ((pat (if (string? pat) (pregexp pat) pat))
           (start (if (null? opt-args) 0
                      (let ((start (car opt-args)))
                        (set! opt-args (cdr opt-args))
                        start)))
           (end (if (null? opt-args) (string-length str)
                    (car opt-args))))
      (let loop ((i start))
        (and (<= i end)
             (let ((vv (pregexp-match-positions-aux pat str start end i)))
               (if vv
                   (cadr vv)
                   (loop (+ i 1)))))))))

(define pregexp-match
  (lambda (pat str . opt-args)
    (let ((ix-prs (apply pregexp-match-positions pat str opt-args)))
      (and ix-prs
           (map
             (lambda (ix-pr)
               (and ix-pr
                    (substring str (car ix-pr) (cdr ix-pr))))
             ix-prs)))))

(define pregexp-split
  (lambda (pat str)
    ;split str into substrings, using pat as delimiter
    (let ((n (string-length str)))
      (let loop ((i 0) (r '()) (picked-up-one-undelimited-char? #f))
        (cond ((>= i n) (pregexp-reverse! r))
              ((pregexp-match-positions pat str i n)
               =>
               (lambda (y)
                 (let ((jk (car y)))
                   (let ((j (car jk)) (k (cdr jk)))
                     ;(printf "j = ~a; k = ~a; i = ~a~n" j k i)
                     (cond ((= j k)
                            ;(printf "producing ~s~n" (substring str i (+ j 1)))
                            (loop (+ k 1) 
                                  (cons (substring str i (+ j 1)) r) #t))
                           ((and (= j i) picked-up-one-undelimited-char?)
                            (loop k r #f))
                           (else
                             ;(printf "producing ~s~n" (substring str i j))
                             (loop k (cons (substring str i j) r) #f)))))))
              (else (loop n (cons (substring str i n) r) #f)))))))

(define pregexp-replace
  (lambda (pat str ins)
    (let* ((n (string-length str))
           (pp (pregexp-match-positions pat str 0 n)))
      (if (not pp) str
          (let ((ins-len (string-length ins))
                (m-i (caar pp))
                (m-n (cdar pp)))
            (string-append
              (substring str 0 m-i)
              (pregexp-replace-aux str ins ins-len pp)
              (substring str m-n n)))))))

(define pregexp-replace*
  (lambda (pat str ins)
    (let ((pat (if (string? pat) (pregexp pat) pat))
          (n (string-length str))
          (ins-len (string-length ins)))
      (let loop ((i 0) (r ""))
        (let ((pp (pregexp-match-positions pat str i n)))
          (if pp
              (loop (cdar pp)
                (string-append r
                  (substring str i (caar pp))
                  (pregexp-replace-aux str ins ins-len pp)))
              (string-append r
                (substring str i n))))))))
    
;eof

;;; Chicken API:

(let ([string-append string-append])
  (define (prep op rx str start)
    (apply op
	   (if (string? rx)
	       (string-append "^" rx "$") 
	       `(:seq :bos ,rx :eos) )
	   str start) )
  (define (prep2 op rx str start)
    (apply op rx str start) )
  (set! string-match (lambda (rx str . start) (prep pregexp-match rx str start)))
  (set! string-match-positions
    (lambda (rx str . start)
      (let ([r (prep pregexp-match-positions rx str start)])
	(and r (map (lambda (p) (list (car p) (cdr p))) r) ) ) ) )
  (set! string-search (lambda (rx str . start) (prep2 pregexp-match rx str start)))
  (set! string-search-positions
    (lambda (rx str . start)
      (let ([r (prep2 pregexp-match-positions rx str start)])
	(and r (map (lambda (p) (list (car p) (cdr p))) r) ) ) ) ) )


(include "regex-common.scm")
