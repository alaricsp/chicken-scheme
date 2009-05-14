;;;; irregex.scm -- IrRegular Expressions
;;
;; Copyright (c) 2005-2008 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; At this moment there was a loud ring at the bell, and I could
;; hear Mrs. Hudson, our landlady, raising her voice in a wail of
;; expostulation and dismay.
;;
;; "By heaven, Holmes," I said, half rising, "I believe that
;; they are really after us."
;;
;; "No, it's not quite so bad as that.  It is the unofficial
;; force, -- the Baker Street irregulars."

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; History
;;
;; 0.6.2: 2008/07/26 - minor bugfixes, allow global disabling of utf8 mode,
;;                     friendlier error messages in parsing, \Q..\E support
;; 0.6.1: 2008/07/21 - added utf8 mode, more utils, bugfixes
;;   0.6: 2008/05/01 - most of PCRE supported
;;   0.5: 2008/04/24 - fully portable R4RS, many PCRE features implemented
;;   0.4: 2008/04/17 - rewriting NFA to use efficient closure compilation,
;;                     normal strings only, but all of the spencer tests pass
;;   0.3: 2008/03/10 - adding DFA converter (normal strings only)
;;   0.2: 2005/09/27 - adding irregex-opt (like elisp's regexp-opt) utility
;;   0.1: 2005/08/18 - simple NFA interpreter over abstract chunked strings

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define irregex-tag '*irregex-tag*)

(define (make-irregex dfa dfa/search dfa/extract nfa flags
                      submatches lengths names)
  (vector irregex-tag dfa dfa/search dfa/extract nfa flags
          submatches lengths names))

(define (irregex? obj)
  (and (vector? obj)
       (= 9 (vector-length obj))
       (eq? irregex-tag (vector-ref obj 0))))

(define (irregex-dfa x) (vector-ref x 1))
(define (irregex-dfa/search x) (vector-ref x 2))
(define (irregex-dfa/extract x) (vector-ref x 3))
(define (irregex-nfa x) (vector-ref x 4))
(define (irregex-flags x) (vector-ref x 5))
(define (irregex-submatches x) (vector-ref x 6))
(define (irregex-lengths x) (vector-ref x 7))
(define (irregex-names x) (vector-ref x 8))

(define (irregex-new-matches irx)
  (make-irregex-match #f (irregex-submatches irx) (irregex-names irx)))
(define (irregex-reset-matches! m)
  (do ((i (- (vector-length m) 1) (- i 1)))
      ((<= i 3) m)
    (vector-set! m i #f)))

(define irregex-match-tag '*irregex-match-tag*)

(define (irregex-match-data? obj)
  (and (vector? obj)
       (>= (vector-length obj) 5)
       (eq? irregex-match-tag (vector-ref obj 0))))

(define (make-irregex-match str count names)
  (let ((res (make-vector (+ (* 2 (+ 1 count)) 3) #f)))
    (vector-set! res 0 irregex-match-tag)
    (vector-set! res 1 str)
    (vector-set! res 2 names)
    res))

(define (irregex-match-num-submatches m)
  (- (quotient (- (vector-length m) 3) 2) 1))

(define (irregex-match-string m)
  (vector-ref m 1))
(define (irregex-match-names m)
  (vector-ref m 2))
(define (irregex-match-string-set! m str)
  (vector-set! m 1 str))

(define (irregex-match-start-index m n)
  (vector-ref m (+ 3 (* n 2))))
(define (irregex-match-end-index m n)
  (vector-ref m (+ 4 (* n 2))))

(define (irregex-match-start-index-set! m n start)
  (vector-set! m (+ 3 (* n 2)) start))
(define (irregex-match-end-index-set! m n end)
  (vector-set! m (+ 4 (* n 2)) end))

(define (irregex-match-index m opt)
  (if (pair? opt)
      (cond ((number? (car opt)) (car opt))
            ((assq (car opt) (irregex-match-names m)) => cdr)
            (else (error "unknown match name" (car opt))))
      0))

(define (irregex-match-valid-index? m n)
  (and (< (+ 3 (* n 2)) (vector-length m))
       (vector-ref m (+ 4 (* n 2)))))

(define (irregex-match-substring m . opt)
  (let ((n (irregex-match-index m opt)))
    (and (irregex-match-valid-index? m n)
         (substring (irregex-match-string m)
                    (vector-ref m (+ 3 (* n 2)))
                    (vector-ref m (+ 4 (* n 2)))))))

(define (irregex-match-start m . opt)
  (let ((n (irregex-match-index m opt)))
    (and (irregex-match-valid-index? m n)
         (vector-ref m (+ 3 (* n 2))))))

(define (irregex-match-end m . opt)
  (irregex-match-valid-index? m (irregex-match-index m opt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string utilities

;;;; Unicode version (skip surrogates)
(define *all-chars*
  `(/ ,(integer->char 0) ,(integer->char #xD7FF)
      ,(integer->char #xE000) ,(integer->char #x10FFFF)))

;;;; ASCII version, offset to not assume 0-255
;; (define *all-chars* `(/ ,(integer->char (- (char->integer #\space) 32)) ,(integer->char (+ (char->integer #\space) 223))))

;; set to #f to ignore even an explicit request for utf8 handling
(define *allow-utf8-mode?* #t)

;; (define *named-char-properties* '())

(define (string-scan-char str c . o)
  (let ((end (string-length str)))
    (let scan ((i (if (pair? o) (car o) 0)))
      (cond ((= i end) #f)
            ((eqv? c (string-ref str i)) i)
            (else (scan (+ i 1)))))))

(define (string-scan-char-escape str c . o)
  (let ((end (string-length str)))
    (let scan ((i (if (pair? o) (car o) 0)))
      (cond ((= i end) #f)
            ((eqv? c (string-ref str i)) i)
            ((eqv? c #\\) (scan (+ i 2)))
            (else (scan (+ i 1)))))))

(define (string-scan-pred str pred . o)
  (let ((end (string-length str)))
    (let scan ((i (if (pair? o) (car o) 0)))
      (cond ((= i end) #f)
            ((pred (string-ref str i)) i)
            (else (scan (+ i 1)))))))

(define (string-split-char str c)
  (let ((end (string-length str)))
    (let lp ((i 0) (from 0) (res '()))
      (define (collect) (cons (substring str from i) res))
      (cond ((>= i end) (reverse (collect)))
            ((eqv? c (string-ref str i)) (lp (+ i 1) (+ i 1) (collect)))
            (else (lp (+ i 1) from res))))))

(define (char-alphanumeric? c)
  (or (char-alphabetic? c) (char-numeric? c)))

;; SRFI-13 extracts

(define (%%string-copy! to tstart from fstart fend)
  (do ((i fstart (+ i 1))
       (j tstart (+ j 1)))
      ((>= i fend))
    (string-set! to j (string-ref from i))))

(define (string-cat-reverse string-list)
  (string-cat-reverse/aux
   (fold (lambda (s a) (+ (string-length s) a)) 0 string-list)
   string-list))

(define (string-cat-reverse/aux len string-list)
  (let ((res (make-string len)))
    (let lp ((i len) (ls string-list))
      (if (pair? ls)
	  (let* ((s (car ls))
		 (slen (string-length s))
		 (i (- i slen)))
	    (%%string-copy! res i s 0 slen)
	    (lp i (cdr ls)))))
    res))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list utilities

;; like the one-arg IOTA case
(define (zero-to n)
  (if (<= n 0)
      '()
      (let lp ((i (- n 1)) (res '()))
        (if (zero? i) (cons 0 res) (lp (- i 1) (cons i res))))))

;; take the head of list FROM up to but not including TO, which must
;; be a tail of the list
(define (take-up-to from to)
  (let lp ((ls from) (res '()))
    (if (and (pair? ls) (not (eq? ls to)))
        (lp (cdr ls) (cons (car ls) res))
        (reverse res))))

;; SRFI-1 extracts (simplified 1-ary versions)

(define (find pred ls)
  (cond ((find-tail pred ls) => car)
	(else #f)))

(define (find-tail pred ls)
  (let lp ((ls ls))
    (cond ((null? ls) #f)
          ((pred (car ls)) ls)
          (else (lp (cdr ls))))))

(define (last ls)
  (if (not (pair? ls))
      (error "can't take last of empty list" ls)
      (let lp ((ls ls))
        (if (pair? (cdr ls))
            (lp (cdr ls))
            (car ls)))))

(define (any pred ls)
  (and (pair? ls)
       (let lp ((head (car ls)) (tail (cdr ls)))
         (if (null? tail)
             (pred head)
             (or (pred head) (lp (car tail) (cdr tail)))))))

(define (every pred ls)
  (or (null? ls)
      (let lp ((head (car ls))  (tail (cdr ls)))
        (if (null? tail)
            (pred head)
            (and (pred head) (lp (car tail) (cdr tail)))))))

(define (fold kons knil ls)
  (let lp ((ls ls) (res knil))
    (if (null? ls)
        res
        (lp (cdr ls) (kons (car ls) res)))))

(define (filter pred ls)
  (let lp ((ls ls) (res '()))
    (if (null? ls)
        (reverse res)
        (lp (cdr ls) (if (pred (car ls)) (cons (car ls) res) res)))))

(define (remove pred ls)
  (let lp ((ls ls) (res '()))
    (if (null? ls)
        (reverse res)
        (lp (cdr ls) (if (pred (car ls)) res (cons (car ls) res))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flags

(define (bit-shr n i)
  (quotient n (expt 2 i)))

(define (bit-shl n i)
  (* n (expt 2 i)))

(define (bit-not n) (- #xFFFF n))

(define (bit-ior a b)
  (cond
   ((zero? a) b)
   ((zero? b) a)
   (else
    (+ (if (or (odd? a) (odd? b)) 1 0)
       (* 2 (bit-ior (quotient a 2) (quotient b 2)))))))

(define (bit-and a b)
  (cond
   ((zero? a) 0)
   ((zero? b) 0)
   (else
    (+ (if (and (odd? a) (odd? b)) 1 0)
       (* 2 (bit-and (quotient a 2) (quotient b 2)))))))

(define (flag-set? flags i)
  (= i (bit-and flags i)))
(define (flag-join a b)
  (if b (bit-ior a b) a))
(define (flag-clear a b)
  (bit-and a (bit-not b)))

(define ~none 0)
(define ~searcher? 1)
(define ~consumer? 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parsing

(define ~save? 1)
(define ~case-insensitive? 2)
(define ~multi-line? 4)
(define ~single-line? 8)
(define ~ignore-space? 16)
(define ~utf8? 32)

(define (symbol-list->flags ls)
  (let lp ((ls ls) (res ~none))
    (if (not (pair? ls))
        res
        (lp (cdr ls)
            (flag-join
             res
             (case (car ls)
               ((i ci case-insensitive) ~case-insensitive?)
               ((m multi-line) ~multi-line?)
               ((s single-line) ~single-line?)
               ((x ignore-space) ~ignore-space?)
               ((u utf8) ~utf8?)
               (else #f)))))))

(define (string->sre str . o)
  (let ((end (string-length str))
        (flags (symbol-list->flags o)))

    (let lp ((i 0) (from 0) (flags flags) (res '()) (st '()))

      ;; handle case sensitivity at the literal char/string level
      (define (cased-char ch)
        (if (and (flag-set? flags ~case-insensitive?)
                 (char-alphabetic? ch))
            `(or ,ch ,(char-altcase ch))
            ch))
      (define (cased-string str)
        (if (flag-set? flags ~case-insensitive?)
            (sre-sequence (map cased-char (string->list str)))
            str))
      ;; accumulate the substring from..i as literal text
      (define (collect)
        (if (= i from) res (cons (cased-string (substring str from i)) res)))
      ;; like collect but breaks off the last single character when
      ;; collecting literal data, as the argument to ?/*/+ etc.
      (define (collect/single)
        (let* ((utf8? (flag-set? flags ~utf8?))
               (j (if (and utf8? (> i 1))
                      (utf8-backup-to-initial-char str (- i 1))
                      (- i 1))))
          (cond
           ((< j from)
            res)
           (else
            (let ((c (cased-char (if utf8?
                                     (utf8-string-ref str j (- i j) )
                                     (string-ref str j)))))
              (cond
               ((= j from)
                (cons c res))
               (else
                (cons c
                      (cons (cased-string (substring str from j))
                            res)))))))))
      ;; collects for use as a result, reversing and grouping OR
      ;; terms, and some ugly tweaking of `function-like' groups and
      ;; conditionals
      (define (collect/terms)
        (let* ((ls (collect))
               (func
                (and (pair? ls)
                     (memq (last ls)
                           '(atomic if look-ahead neg-look-ahead
                                    look-behind neg-look-behind submatch-named
                                    w/utf8 w/noutf8))))
               (prefix (if (and func (eq? 'submatch-named (car func)))
                           (list 'submatch-named (cadr (reverse ls)))
                           (and func (list (car func)))))
               (ls (if func
                       (if (eq? 'submatch-named (car func))
                           (reverse (cddr (reverse ls)))
                           (reverse (cdr (reverse ls))))
                       ls)))
          (let lp ((ls ls) (term '()) (res '()))
            (define (shift)
              (cons (sre-sequence term) res))
            (cond
             ((null? ls)
              (let* ((res (sre-alternate (shift)))
                     (res (if (flag-set? flags ~save?)
                              (list 'submatch res)
                              res)))
                (if prefix
                    (if (eq? 'if (car prefix))
                        (cond
                         ((not (pair? res))
                          'epsilon)
                         ((memq (car res)
                                '(look-ahead neg-look-ahead
                                             look-behind neg-look-behind))
                          res)
                         ((eq? 'seq (car res))
                          `(if ,(cadr res)
                               ,(if (pair? (cdr res))
                                    (sre-sequence (cddr res))
                                    'epsilon)))
                         (else
                          `(if ,(cadadr res)
                               ,(if (pair? (cdr res))
                                    (sre-sequence (cddadr res))
                                    'epsilon)
                               ,(sre-alternate
                                 (if (pair? (cdr res)) (cddr res) '())))))
                        `(,@prefix ,res))
                    res)))
             ((eq? 'or (car ls)) (lp (cdr ls) '() (shift)))
             (else (lp (cdr ls) (cons (car ls) term) res))))))
      (define (save)
        (cons (cons flags (collect)) st))

      ;; main parsing
      (if (>= i end)
          (if (pair? st)
              (error "unterminated parenthesis in regexp" str)
              (collect/terms))
          (let ((c (string-ref str i)))
            (case c
              ((#\.)
               (lp (+ i 1) (+ i 1) flags
                   (cons (if (flag-set? flags ~single-line?) 'any 'nonl)
                         (collect))
                   st))
              ((#\?)
               (let ((res (collect/single)))
                 (if (null? res)
                     (error "? can't follow empty sre" str res)
                     (let ((x (car res)))
                       (lp (+ i 1)
                           (+ i 1)
                           flags
                           (cons
                            (if (pair? x)
                                (case (car x)
                                  ((*)  `(*? ,@(cdr x)))
                                  ((+)  `(**? 1 #f ,@(cdr x)))
                                  ((?)  `(?? ,@(cdr x)))
                                  ((**) `(**? ,@(cdr x)))
                                  ((=)  `(**? ,(cadr x) ,@(cdr x)))
                                  ((>=)  `(**? ,(cadr x) #f ,@(cddr x)))
                                  (else `(? ,x)))
                                `(? ,x))
                            (cdr res))
                           st)))))
              ((#\+ #\*)
               (let* ((res (collect/single))
                      (x (car res))
                      (op (string->symbol (string c))))
                 (cond
                  ((sre-repeater? x)
                   (error "duplicate repetition (e.g. **) in sre" str res))
                  ((sre-empty? x)
                   (error "can't repeat empty sre (e.g. ()*)" str res))
                  (else
                   (lp (+ i 1) (+ i 1) flags
                       (cons (list op x) (cdr res))
                       st)))))
              ((#\()
               (cond
                ((>= (+ i 1) end)
                 (error "unterminated parenthesis in regexp" str))
                ((not (eqv? #\? (string-ref str (+ i 1))))
                 (lp (+ i 1) (+ i 1) (flag-join flags ~save?) '() (save)))
                ((>= (+ i 2) end)
                 (error "unterminated parenthesis in regexp" str))
                (else
                 (case (string-ref str (+ i 2))
                   ((#\#)
                    (let ((j (string-scan-char str #\) (+ i 3))))
                      (lp (+ j i) (+ j 1) flags (collect) st)))
                   ((#\:)
                    (lp (+ i 3) (+ i 3) (flag-clear flags ~save?) '() (save)))
                   ((#\=)
                    (lp (+ i 3) (+ i 3) (flag-clear flags ~save?)
                        '(look-ahead) (save)))
                   ((#\!)
                    (lp (+ i 3) (+ i 3) (flag-clear flags ~save?)
                        '(neg-look-ahead) (save)))
                   ((#\<)
                    (cond
                     ((>= (+ i 3) end)
                      (error "unterminated parenthesis in regexp" str))
                     (else
                      (case (string-ref str (+ i 3))
                        ((#\=)
                         (lp (+ i 4) (+ i 4) (flag-clear flags ~save?)
                             '(look-behind) (save)))
                        ((#\!)
                         (lp (+ i 4) (+ i 4) (flag-clear flags ~save?)
                             '(neg-look-behind) (save)))
                        (else
                         (let ((j (and (char-alphabetic?
                                        (string-ref str (+ i 3)))
                                       (string-scan-char str #\> (+ i 4)))))
                           (if j
                               (lp (+ j 1) (+ j 1) (flag-clear flags ~save?)
                                   `(,(string->symbol (substring str (+ i 3) j))
                                     submatch-named)
                                   (save))
                               (error "invalid (?< sequence" str))))))))
                   ((#\>)
                    (lp (+ i 3) (+ i 3) (flag-clear flags ~save?)
                        '(atomic) (save)))
                   ;;((#\' #\P) ; named subpatterns
                   ;; )
                   ;;((#\R) ; recursion
                   ;; )
                   ((#\()
                    (cond
                     ((>= (+ i 3) end)
                      (error "unterminated parenthesis in regexp" str))
                     ((char-numeric? (string-ref str (+ i 3)))
                      (let* ((j (string-scan-char str #\) (+ i 3)))
                             (n (string->number (substring str (+ i 3) j))))
                        (if (not n)
                            (error "invalid conditional reference" str)
                            (lp (+ j 1) (+ j 1) (flag-clear flags ~save?)
                                `(,n if) (save)))))
                     ((char-alphabetic? (string-ref str (+ i 3)))
                      (let* ((j (string-scan-char str #\) (+ i 3)))
                             (s (string->symbol (substring str (+ i 3) j))))
                        (lp (+ j 1) (+ j 1) (flag-clear flags ~save?)
                            `(,s if) (save))))
                     (else
                      (lp (+ i 2) (+ i 2) (flag-clear flags ~save?)
                          '(if) (save)))))
                   ((#\{)
                    (error "unsupported Perl-style cluster" str))
                   (else
                    (let ((old-flags flags))
                      (let lp2 ((j (+ i 2)) (flags flags) (invert? #f))
                        (define (join x)
                          ((if invert? flag-clear flag-join) flags x))
                        (define (new-res res)
                          (let ((before (flag-set? old-flags ~utf8?))
                                (after (flag-set? flags ~utf8?)))
                            (if (eq? before after)
                                res
                                (cons (if after 'w/utf8 'w/noutf8) res))))
                        (cond
                         ((>= j end)
                          (error "incomplete cluster" str i))
                         (else
                          (case (string-ref str j)
                            ((#\i)
                             (lp2 (+ j 1) (join ~case-insensitive?) invert?))
                            ((#\m)
                             (lp2 (+ j 1) (join ~multi-line?) invert?))
                            ((#\x)
                             (lp2 (+ j 1) (join ~ignore-space?) invert?))
                            ((#\u)
                             (if *allow-utf8-mode?*
                                 (lp2 (+ j 1) (join ~utf8?) invert?)
                                 (lp2 (+ j 1) flags invert?)))
                            ((#\-)
                             (lp2 (+ j 1) flags (not invert?)))
                            ((#\))
                             (lp (+ j 1) (+ j 1) flags (new-res (collect))
                                 st))
                            ((#\:)
                             (lp (+ j 1) (+ j 1) flags (new-res '())
                                 (cons (cons old-flags (collect)) st)))
                            (else
                             (error "unknown regex cluster modifier" str)
                             )))))))))))
              ((#\))
               (if (null? st)
                   (error "too many )'s in regexp" str)
                   (lp (+ i 1)
                       (+ i 1)
                       (caar st)
                       (cons (collect/terms) (cdar st))
                       (cdr st))))
              ((#\[)
               (apply
                (lambda (sre j)
                  (lp (+ j 1) (+ j 1) flags (cons sre (collect)) st))
                (string-parse-cset str (+ i 1) flags)))
              ((#\{)
               (if (or (>= (+ i 1) end)
                       (not (or (char-numeric? (string-ref str (+ i 1)))
                                (eqv? #\, (string-ref str (+ i 1))))))
                   (lp (+ i 1) from flags res st)
                   (let* ((res (collect/single))
                          (x (car res))
                          (tail (cdr res))
                          (j (string-scan-char str #\} (+ i 1)))
                          (s2 (string-split-char (substring str (+ i 1) j) #\,))
                          (n (or (string->number (car s2)) 0))
                          (m (and (pair? (cdr s2)) (string->number (cadr s2)))))
                     (cond
                      ((null? (cdr s2))
                       (lp (+ j 1) (+ j 1) flags `((= ,n ,x) ,@tail) st))
                      (m
                       (lp (+ j 1) (+ j 1) flags `((** ,n ,m ,x) ,@tail) st))
                      (else
                       (lp (+ j 1) (+ j 1) flags `((>= ,n ,x) ,@tail) st)
                       )))))
              ((#\\)
               (cond
                ((>= (+ i 1) end)
                 (error "incomplete escape sequence" str))
                (else
                 (let ((c (string-ref str (+ i 1))))
                   (case c
                     ((#\d)
                      (lp (+ i 2) (+ i 2) flags `(numeric ,@(collect)) st))
                     ((#\D)
                      (lp (+ i 2) (+ i 2) flags `((~ numeric) ,@(collect)) st))
                     ((#\s)
                      (lp (+ i 2) (+ i 2) flags `(space ,@(collect)) st))
                     ((#\S)
                      (lp (+ i 2) (+ i 2) flags `((~ space) ,@(collect)) st))
                     ((#\w)
                      (lp (+ i 2) (+ i 2) flags
                          `((or alphanumeric ("_")) ,@(collect)) st))
                     ((#\W)
                      (lp (+ i 2) (+ i 2) flags
                          `((~ (or alphanumeric ("_"))) ,@(collect)) st))
                     ((#\b)
                      (lp (+ i 2) (+ i 2) flags
                          `((or bow eow) ,@(collect)) st))
                     ((#\B)
                      (lp (+ i 2) (+ i 2) flags `(nwb ,@(collect)) st))
                     ((#\A)
                      (lp (+ i 2) (+ i 2) flags `(bos ,@(collect)) st))
                     ((#\Z)
                      (lp (+ i 2) (+ i 2) flags
                          `((? #\newline) eos ,@(collect)) st))
                     ((#\z)
                      (lp (+ i 2) (+ i 2) flags `(eos ,@(collect)) st))
                     ((#\R)
                      (lp (+ i 2) (+ i 2) flags `(newline ,@(collect)) st))
                     ((#\K)
                      (lp (+ i 2) (+ i 2) flags `(reset ,@(collect)) st))
                     ;; these two are from Emacs and TRE, but not PCRE
                     ((#\<)
                      (lp (+ i 2) (+ i 2) flags `(bow ,@(collect)) st))
                     ((#\>)
                      (lp (+ i 2) (+ i 2) flags `(eow ,@(collect)) st))
                     ((#\x)
                      (apply
                       (lambda (ch j)
                         (lp (+ j 1) (+ j 1) flags `(,ch ,@(collect)) st))
                       (string-parse-hex-escape str (+ i 2) end)))
                     ((#\k)
                      (let ((c (string-ref str (+ i 2))))
                        (if (not (memv c '(#\< #\{ #\')))
                            (error "bad \\k usage, expected \\k<...>" str)
                            (let* ((terminal (char-mirror c))
                                   (j (string-scan-char str terminal (+ i 2)))
                                   (s (and j (substring str (+ i 3) j)))
                                   (backref
                                    (if (flag-set? flags ~case-insensitive?)
                                        'backref-ci
                                        'backref)))
                              (if (not j)
                                  (error "interminated named backref" str)
                                  (lp (+ j 1) (+ j 1) flags
                                      `((,backref ,(string->symbol s))
                                        ,@(collect))
                                      st))))))
                     ((#\Q)  ;; \Q..\E escapes
                      (let ((res (collect)))
                        (let lp2 ((j (+ i 2)))
                          (cond
                           ((>= j end)
                            (lp j (+ i 2) flags res st))
                           ((eqv? #\\ (string-ref str j))
                            (cond
                             ((>= (+ j 1) end)
                              (lp (+ j 1) (+ i 2) flags res st))
                             ((eqv? #\E (string-ref str (+ j 1)))
                              (lp (+ j 2) (+ j 2) flags
                                  (cons (substring str (+ i 2) j) res) st))
                             (else
                              (lp2 (+ j 2)))))
                           (else
                            (lp2 (+ j 1)))))))
                     ;;((#\p)  ; XXXX unicode properties
                     ;; )
                     ;;((#\P)
                     ;; )
                     (else
                      (cond
                       ((char-numeric? c)
                        (let* ((j (or (string-scan-pred
                                       str
                                       (lambda (c) (not (char-numeric? c)))
                                       (+ i 2))
                                      end))
                               (backref
                                (if (flag-set? flags ~case-insensitive?)
                                    'backref-ci
                                    'backref))
                               (res `((,backref ,(string->number
                                                  (substring str (+ i 1) j)))
                                      ,@(collect))))
                          (lp j j flags res st)))
                       ((char-alphabetic? c)
                        (let ((cell (assv c posix-escape-sequences)))
                          (if cell
                              (lp (+ i 2) (+ i 2) flags
                                  (cons (cdr cell) (collect)) st)
                              (error "unknown escape sequence" str c))))
                       (else
                        (lp (+ i 2) (+ i 1) flags (collect) st)))))))))
              ((#\|)
               (lp (+ i 1) (+ i 1) flags (cons 'or (collect)) st))
              ((#\^)
               (let ((sym (if (flag-set? flags ~multi-line?) 'bol 'bos)))
                 (lp (+ i 1) (+ i 1) flags (cons sym (collect)) st)))
              ((#\$)
               (let ((sym (if (flag-set? flags ~multi-line?) 'eol 'eos)))
                 (lp (+ i 1) (+ i 1) flags (cons sym (collect)) st)))
              ((#\space)
               (if (flag-set? flags ~ignore-space?)
                   (lp (+ i 1) (+ i 1) flags (collect) st)
                   (lp (+ i 1) from flags res st)))
              ((#\#)
               (if (flag-set? flags ~ignore-space?)
                   (let ((j (or (string-scan-char str #\newline (+ i 1))
                                (- end 1))))
                     (lp (+ j 1) (+ j 1) flags (collect) st))
                   (lp (+ i 1) from flags res st)))
              (else
               (lp (+ i 1) from flags res st))))))))

(define posix-escape-sequences
  `((#\n . #\newline)
    (#\r . ,(integer->char (+ (char->integer #\newline) 3)))
    (#\t . ,(integer->char (- (char->integer #\newline) 1)))
    (#\a . ,(integer->char (- (char->integer #\newline) 3)))
    (#\e . ,(integer->char (+ (char->integer #\newline) #x11)))
    (#\f . ,(integer->char (+ (char->integer #\newline) 2)))
    ))

(define (char-altcase c)
  (if (char-upper-case? c) (char-downcase c) (char-upcase c)))

(define (char-mirror c)
  (case c ((#\<) #\>) ((#\{) #\}) ((#\() #\)) ((#\[) #\]) (else c)))

(define (string-parse-hex-escape str i end)
  (cond
   ((>= i end)
    (error "incomplete hex escape" str i))
   ((eqv? #\{ (string-ref str i))
    (let ((j (string-scan-char-escape str #\} (+ i 1))))
      (if (not j)
          (error "incomplete hex brace escape" str i)
          (let* ((s (substring str (+ i 1) j))
                 (n (string->number s 16)))
            (if n
                (list (integer->char n) j)
                (error "bad hex brace escape" s))))))
   ((>= (+ i 1) end)
    (error "incomplete hex escape" str i))
   (else
    (let* ((s (substring str i (+ i 2)))
           (n (string->number s 16)))
      (if n
          (list (integer->char n) (+ i 2))
          (error "bad hex escape" s))))))

(define (string-parse-cset str start flags)
  (let ((end (string-length str))
        (invert? (eqv? #\^ (string-ref str start)))
        (utf8? (flag-set? flags ~utf8?)))
    (define (go i chars ranges)
      (if (>= i end)
          (error "incomplete char set")
          (let ((c (string-ref str i)))
            (case c
              ((#\])
               (if (and (null? chars) (null? ranges))
                   (go (+ i 1) (cons #\] chars) ranges)
                   (let ((ci? (flag-set? flags ~case-insensitive?))
                         (hi-chars (if utf8? (filter high-char? chars) '()))
                         (chars (if utf8? (remove high-char? chars) chars)))
                     (list
                      ((lambda (res)
                         (if invert? (cons '~ res) (sre-alternate res)))
                       (append
                        hi-chars
                        (if (pair? chars)
                            (list
                             (list (list->string
                                    ((if ci?
                                         cset-case-insensitive
                                         (lambda (x) x))
                                     (reverse chars)))))
                            '())
                        (if (pair? ranges)
                            (let ((res (if ci?
                                           (cset-case-insensitive
                                            (reverse ranges))
                                           (reverse ranges))))
                              (list (cons '/ (alist->plist res))))
                            '())))
                      i))))
              ((#\-)
               (cond
                ((or (= i start)
                     (and (= i (+ start 1)) (eqv? #\^ (string-ref str start)))
                     (eqv? #\] (string-ref str (+ i 1))))
                 (go (+ i 1) (cons c chars) ranges))
                ((null? chars)
                 (error "bad char-set"))
                (else
                 (let* ((c1 (car chars))
                        (c2 (string-ref str (+ i 1))))
                   (apply
                    (lambda (c2 j)
                      (if (char<? c2 c1)
                          (error "inverted range in char-set" c1 c2)
                          (go j (cdr chars) (cons (cons c1 c2) ranges))))
                    (cond
                     ((and (eqv? #\\ c2) (assv c2 posix-escape-sequences))
                      => (lambda (x) (list (cdr x) (+ i 3))))
                     ((and (eqv? #\\ c2)
                           (eqv? (string-ref str (+ i 2)) #\x))
                      (string-parse-hex-escape str (+ i 3) end))
                     ((and utf8? (<= #x80 (char->integer c2) #xFF))
                      (let ((len (utf8-start-char->length c2)))
                        (list (utf8-string-ref str (+ i 1) len) (+ i 1 len))))
                     (else
                      (list c2 (+ i 2)))))))))
              ((#\[)
               (let* ((inv? (eqv? #\^ (string-ref str (+ i 1))))
                      (i2 (if inv? (+ i 2) (+ i 1))))
                 (case (string-ref str i2)
                   ((#\:)
                    (let ((j (string-scan-char str #\: (+ i2 1))))
                      (if (or (not j) (not (eqv? #\] (string-ref str (+ j 1)))))
                          (error "incomplete character class" str)
                          (let* ((cset (sre->cset
                                        (string->symbol
                                         (substring str (+ i2 1) j))))
                                 (cset (if inv? (cset-complement cset) cset)))
                            (go (+ j 2)
                                (append (filter char? cset) chars)
                                (append (filter pair? cset) ranges))))))
                   ((#\= #\.)
                    (error "collating sequences not supported" str))
                   (else
                    (go (+ i 1) (cons #\[ chars) ranges)))))
              ((#\\)
               (let ((c (string-ref str (+ i 1))))
                 (case c
                   ((#\d #\D #\s #\S #\w #\W)
                    (let ((cset (sre->cset (string->sre (string #\\ c)))))
                      (go (+ i 2)
                          (append (filter char? cset) chars)
                          (append (filter pair? cset) ranges))))
                   ((#\x)
                    (apply
                     (lambda (ch j)
                       (go j (cons ch chars) ranges))
                     (string-parse-hex-escape str (+ i 2) end)))
                   (else
                    (let ((c (cond ((assv c posix-escape-sequences) => cdr)
                                   (else c))))
                      (go (+ i 2)
                          (cons (string-ref str (+ i 1)) (cons c chars))
                          ranges))))))
              (else
               (if (and utf8? (<= #x80 (char->integer c) #xFF))
                   (let ((len (utf8-start-char->length c)))
                     (go (+ i len)
                         (cons (utf8-string-ref str i len) chars)
                         ranges))
                   (go (+ i 1) (cons c chars) ranges)))))))
    (if invert?
        (go (+ start 1)
            (if (flag-set? flags ~multi-line?) '(#\newline) '())
            '())
        (go start '() '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utf8 utilities

;; Here are some hairy optimizations that need to be documented
;; better.  Thanks to these, we never do any utf8 processing once the
;; regexp is compiled.

;; two chars: ab..ef
;;            a[b..xFF]|[b-d][x80..xFF]|e[x80..xFF]

;; three chars: abc..ghi
;;              ab[c..xFF]|a[d..xFF][x80..xFF]|
;;              [b..f][x80..xFF][x80..xFF]|
;;              g[x80..g][x80..xFF]|gh[x80..i]

;; four chars: abcd..ghij
;;             abc[d..xFF]|ab[d..xFF][x80..xFF]|a[c..xFF][x80..xFF][x80..xFF]|
;;             [b..f][x80..xFF][x80..xFF][x80..xFF]|
;;             g[x80..g][x80..xFF][x80..xFF]|gh[x80..h][x80..xFF]|ghi[x80..j]

(define (high-char? c) (<= #x80 (char->integer c)))

;; number of total bytes in a utf8 char given the 1st byte

(define utf8-start-char->length
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
    (lambda (c) (vector-ref table (char->integer c)))))

(define (utf8-string-ref str i len)
  (define (byte n) (char->integer (string-ref str n)))
  (case len
    ((1) ; shouldn't happen in this module
     (string-ref str i))
    ((2)
     (integer->char
      (+ (bit-shl (bit-and (byte i) #b00011111) 6)
         (bit-and (byte (+ i 1)) #b00111111))))
    ((3)
     (integer->char
      (+ (bit-shl (bit-and (byte i) #b00001111) 12)
         (bit-shl (bit-and (byte (+ i 1)) #b00111111) 6)
         (bit-and (byte (+ i 2)) #b00111111))))
    ((4)
     (integer->char
      (+ (bit-shl (bit-and (byte i) #b00000111) 18)
         (bit-shl (bit-and (byte (+ i 1)) #b00111111) 12)
         (bit-shl (bit-and (byte (+ i 2)) #b00111111) 6)
         (bit-and (byte (+ i 3)) #b00111111))))
    (else
     (error "invalid utf8 length" str len i))))

(define (utf8-backup-to-initial-char str i)
  (let lp ((i i))
    (if (= i 0)
        0
        (let ((c (char->integer (string-ref str i))))
          (if (or (< c #x80) (>= c #xC0))
              i
              (lp (- i 1)))))))

(define (utf8-lowest-digit-of-length len)
  (case len
    ((1) 0) ((2) #xC0) ((3) #xE0) ((4) #xF0)
    (else (error "invalid utf8 length" len))))

(define (utf8-highest-digit-of-length len)
  (case len
    ((1) #x7F) ((2) #xDF) ((3) #xEF) ((4) #xF7)
    (else (error "invalid utf8 length" len))))

(define (char->utf8-list c)
  (let ((i (char->integer c)))
    (cond
     ((<= i #x7F) (list i))
     ((<= i #x7FF)
      (list (bit-ior #b11000000 (bit-shr i 6))
            (bit-ior #b10000000 (bit-and i #b111111))))
     ((<= i #xFFFF)
      (list (bit-ior #b11100000 (bit-shr i 12))
            (bit-ior #b10000000 (bit-and (bit-shr i 6) #b111111))
            (bit-ior #b10000000 (bit-and i #b111111))))
     ((<= i #x1FFFFF)
      (list (bit-ior #b11110000 (bit-shr i 18))
            (bit-ior #b10000000 (bit-and (bit-shr i 12) #b111111))
            (bit-ior #b10000000 (bit-and (bit-shr i 6) #b111111))
            (bit-ior #b10000000 (bit-and i #b111111))))
     (else (error "unicode codepoint out of range:" i)))))

(define (unicode-range->utf8-pattern lo hi)
  (let ((lo-ls (char->utf8-list lo))
        (hi-ls (char->utf8-list hi)))
    (if (not (= (length lo-ls) (length hi-ls)))
        (sre-alternate (list (unicode-range-climb-digits lo-ls hi-ls)
                             (unicode-range-up-to hi-ls)))
        (let lp ((lo-ls lo-ls) (hi-ls hi-ls))
          (cond
           ((null? lo-ls)
            '())
           ((= (car lo-ls) (car hi-ls))
            (sre-sequence
             (list (integer->char (car lo-ls))
                   (lp (cdr lo-ls) (cdr hi-ls)))))
           ((= (+ (car lo-ls) 1) (car hi-ls))
            (sre-alternate (list (unicode-range-up-from lo-ls)
                                 (unicode-range-up-to hi-ls))))
           (else
            (sre-alternate (list (unicode-range-up-from lo-ls)
                                 (unicode-range-middle lo-ls hi-ls)
                                 (unicode-range-up-to hi-ls)))))))))

(define (unicode-range-helper one ls prefix res)
  (if (null? ls)
      res
      (unicode-range-helper
       one
       (cdr ls)
       (cons (car ls) prefix)
       (cons (sre-sequence
              `(,@(map integer->char prefix)
                ,(one (car ls))
                ,@(map (lambda (_)
                         `(/ ,(integer->char #x80)
                             ,(integer->char #xFF)))
                       (cdr ls))))
             res))))

(define (unicode-range-up-from lo-ls)
  (sre-sequence
   (list (integer->char (car lo-ls))
         (sre-alternate
          (unicode-range-helper
           (lambda (c)
             `(/ ,(integer->char (+ (car lo-ls) 1)) ,(integer->char #xFF)))
           (cdr (reverse (cdr lo-ls)))
           '()
           (list
            (sre-sequence
             (append
              (map integer->char (reverse (cdr (reverse (cdr lo-ls)))))
              `((/ ,(integer->char (last lo-ls))
                   ,(integer->char #xFF)))))))))))

(define (unicode-range-up-to hi-ls)
  (sre-sequence
   (list (integer->char (car hi-ls))
         (sre-alternate
          (unicode-range-helper
           (lambda (c)
             `(/ ,(integer->char #x80) ,(integer->char (- (car hi-ls) 1))))
           (cdr (reverse (cdr hi-ls)))
           '()
           (list
            (sre-sequence
             (append
              (map integer->char (reverse (cdr (reverse (cdr hi-ls)))))
              `((/ ,(integer->char #x80)
                   ,(integer->char (last hi-ls))))))))))))

(define (unicode-range-climb-digits lo-ls hi-ls)
  (let ((lo-len (length lo-ls)))
    (sre-alternate
     (append
      (list
       (sre-sequence
        (cons `(/ ,(integer->char (car lo-ls))
                  ,(integer->char (if (<= (car lo-ls) #x7F) #x7F #xFF)))
              (map (lambda (_)
                     `(/ ,(integer->char #x80) ,(integer->char #xFF)))
                   (cdr lo-ls)))))
      (map
       (lambda (i)
         (sre-sequence
          (cons
           `(/ ,(integer->char (utf8-lowest-digit-of-length (+ i lo-len 1)))
               ,(integer->char (utf8-highest-digit-of-length (+ i lo-len 1))))
           (map (lambda (_)
                  `(/ ,(integer->char #x80) ,(integer->char #xFF)))
                (zero-to (+ i lo-len))))))
       (zero-to (- (length hi-ls) lo-len 1)))
      (list
       (sre-sequence
        (cons `(/ ,(integer->char
                    (utf8-lowest-digit-of-length
                     (utf8-start-char->length
                      (integer->char (- (car hi-ls) 1)))))
                  ,(integer->char (- (car hi-ls) 1)))
              (map (lambda (_)
                     `(/ ,(integer->char #x80) ,(integer->char #xFF)))
                   (cdr hi-ls)))))))))

(define (unicode-range-middle lo-ls hi-ls)
  (let ((lo (integer->char (+ (car lo-ls) 1)))
        (hi (integer->char (- (car hi-ls) 1))))
    (sre-sequence
     (cons (if (char=? lo hi) lo `(/ ,lo ,hi))
           (map (lambda (_) `(/ ,(integer->char #x80) ,(integer->char #xFF)))
                (cdr lo-ls))))))

(define (cset->utf8-pattern cset)
  (let lp ((ls cset) (alts '()) (lo-cset '()))
    (cond
     ((null? ls)
      (sre-alternate (append (reverse alts)
                             (if (null? lo-cset)
                                 '()
                                 (list (cons '/ (reverse lo-cset)))))))
     ((char? (car ls))
      (if (high-char? (car ls))
          (lp (cdr ls) (cons (car ls) alts) lo-cset)
          (lp (cdr ls) alts (cons (car ls) lo-cset))))
     (else
      (if (or (high-char? (caar ls))  (high-char? (cdar ls)))
          (lp (cdr ls)
              (cons (unicode-range->utf8-pattern (caar ls) (cdar ls)) alts)
              lo-cset)
          (lp (cdr ls) alts (cons (cdar ls) (cons (caar ls) lo-cset))))))))

(define (sre-adjust-utf8 sre flags)
  (let adjust ((sre sre)
               (utf8? (flag-set? flags ~utf8?))
               (ci? (flag-set? flags ~case-insensitive?)))
    (define (rec sre) (adjust sre utf8? ci?))
    (cond
     ((pair? sre)
      (case (car sre)
        ((w/utf8) (adjust (sre-sequence (cdr sre)) #t ci?))
        ((w/noutf8) (adjust (sre-sequence (cdr sre)) #f ci?))
        ((w/case)
         (cons (car sre) (map (lambda (s) (adjust s utf8? #f)) (cdr sre))))
        ((w/nocase)
         (cons (car sre) (map (lambda (s) (adjust s utf8? #t)) (cdr sre))))
        ((/ ~ & -)
         (if (not utf8?)
             sre
             (let ((cset (sre->cset sre ci?)))
               (if (any (lambda (x)
                          (if (pair? x)
                              (or (high-char? (car x)) (high-char? (cdr x)))
                              (high-char? x)))
                        cset)
                   (if ci?
                       (list 'w/case (cset->utf8-pattern cset))
                       (cset->utf8-pattern cset))
                   sre))))
        ((*)
         (case (sre-sequence (cdr sre))
           ;; special case optimization: .* w/utf8 == .* w/noutf8
           ((any) '(* any))
           ((nonl) '(* nonl))
           (else (cons '* (map rec (cdr sre))))))
        (else
         (cons (car sre) (map rec (cdr sre))))))
     (else
      (case sre
        ((any) 'utf8-any)
        ((nonl) 'utf8-nonl)
        (else
         (if (and utf8? (char? sre) (high-char? sre))
             (sre-sequence (map integer->char (char->utf8-list sre)))
             sre)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compilation

(define (irregex x . o)
  (cond
   ((irregex? x) x)
   ((string? x) (apply string->irregex x o))
   (else (apply sre->irregex x o))))

(define (string->irregex str . o)
  (apply sre->irregex (apply string->sre str o) o))

(define (sre->irregex sre . o)
  (let* ((pat-flags (symbol-list->flags o))
         (sre (if *allow-utf8-mode?*
                  (sre-adjust-utf8 sre pat-flags)
                  sre))
         (searcher? (sre-searcher? sre))
         (sre-dfa (if searcher? (sre-remove-initial-bos sre) sre))
         (dfa-limit (cond ((memq 'small o) 1) ((memq 'fast o) 50) (else 10)))
         (dfa/search
          (if searcher?
              #t
              (cond ((sre->nfa `(seq (* any) ,sre-dfa) pat-flags)
                     => (lambda (nfa) (nfa->dfa nfa (* dfa-limit (length nfa)))))
                    (else #f))))
         (dfa (cond ((and dfa/search (sre->nfa sre-dfa pat-flags))
                     => (lambda (nfa) (nfa->dfa nfa (* dfa-limit (length nfa)))))
                    (else #f)))
         (extractor (and dfa dfa/search (sre-match-extractor sre-dfa)))
         (submatches (sre-count-submatches sre-dfa))
         (names (sre-names sre-dfa 1 '()))
         (lens (sre-length-ranges sre-dfa names))
         (flags (flag-join
                 (flag-join ~none (and searcher? ~searcher?))
                 (and (sre-consumer? sre) ~consumer?))))
    (cond
     (dfa
      (make-irregex dfa dfa/search extractor #f flags submatches lens names))
     (else
      (let ((f (sre->procedure sre pat-flags names)))
        (make-irregex #f #f #f f flags submatches lens names))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sre analysis

;; returns #t if the sre can ever be empty
(define (sre-empty? sre)
  (if (pair? sre)
      (case (car sre)
        ((* ? look-ahead look-behind neg-look-ahead neg-look-behind) #t)
        ((**) (or (not (number? (cadr sre))) (zero? (cadr sre))))
        ((or) (any sre-empty? (cdr sre)))
        ((: seq submatch + atomic) (every sre-empty? (cdr sre)))
        (else #f))
      (memq sre '(epsilon bos eos bol eol bow eow commit))))

(define (sre-any? sre)
  (or (eq? sre 'any)
      (and (pair? sre)
           (case (car sre)
             ((seq : submatch)
              (and (pair? (cdr sre)) (null? (cddr sre)) (sre-any? (cadr sre))))
             ((or) (every sre-any? (cdr sre)))
             (else #f)))))

(define (sre-repeater? sre)
  (and (pair? sre)
       (or (memq (car sre) '(* +))
           (and (memq (car sre) '(submatch seq :))
                (pair? (cdr sre))
                (null? (cddr sre))
                (sre-repeater? (cadr sre))))))

(define (sre-searcher? sre)
  (if (pair? sre)
      (case (car sre)
        ((* +) (sre-any? (sre-sequence (cdr sre))))
        ((seq : submatch) (and (pair? (cdr sre)) (sre-searcher? (cadr sre))))
        ((or) (every sre-searcher? (cdr sre)))
        (else #f))
      (eq? 'bos sre)))

(define (sre-consumer? sre)
  (if (pair? sre)
      (case (car sre)
        ((* +) (sre-any? (sre-sequence (cdr sre))))
        ((seq : submatch) (and (pair? (cdr sre)) (sre-consumer? (last sre))))
        ((or) (every sre-consumer? (cdr sre)))
        (else #f))
      (eq? 'eos sre)))

(define (sre-has-submatchs? sre)
  (and (pair? sre)
       (or (eq? 'submatch (car sre))
           (any sre-has-submatchs? (cdr sre)))))

(define (sre-count-submatches sre)
  (let count ((sre sre) (sum 0))
    (if (pair? sre)
        (fold count
              (+ sum (case (car sre)
                       ((submatch submatch-named) 1)
                       ((dsm) (+ (cadr sre) (caddr sre)))
                       (else 0)))
              (cdr sre))
        sum)))

(define (sre-length-ranges sre . o)
  (let ((names (if (pair? o) (car o) (sre-names sre 1 '())))
        (sublens (make-vector (+ 1 (sre-count-submatches sre)) #f)))
    (vector-set!
     sublens
     0
     (let lp ((sre sre) (n 1) (lo 0) (hi 0) (return cons))
       (define (grow i) (return (+ lo i) (and hi (+ hi i))))
       (cond
        ((pair? sre)
         (if (string? (car sre))
             (grow 1)
             (case (car sre)
               ((/ ~ & -)
                (grow 1))
               ((posix-string)
                (lp (string->sre (cadr sre)) n lo hi return))
               ((seq : w/case w/nocase atomic)
                (let lp2 ((ls (cdr sre)) (n n) (lo2 0) (hi2 0))
                  (if (null? ls)
                      (return (+ lo lo2) (and hi hi2 (+ hi hi2)))
                      (lp (car ls) n 0 0
                          (lambda (lo3 hi3)
                            (lp2 (cdr ls)
                                 (+ n (sre-count-submatches (car ls)))
                                 (+ lo2 lo3)
                                 (and hi2 hi3 (+ hi2 hi3))))))))
               ((or)
                (let lp2 ((ls (cdr sre)) (n n) (lo2 #f) (hi2 0))
                  (if (null? ls)
                      (return (+ lo lo2) (and hi hi2 (+ hi hi2)))
                      (lp (car ls) n 0 0
                          (lambda (lo3 hi3)
                            (lp2 (cdr ls)
                                 (+ n (sre-count-submatches (car ls)))
                                 (if lo2 (min lo2 lo3) lo3)
                                 (and hi2 hi3 (max hi2 hi3))))))))
               ((if)
                (cond
                 ((or (null? (cdr sre)) (null? (cddr sre)))
                  (return lo hi))
                 (else
                  (let ((n1 (sre-count-submatches (car sre)))
                        (n2 (sre-count-submatches (cadr sre))))
                    (lp (if (or (number? (cadr sre)) (symbol? (cadr sre)))
                            'epsilon
                            (cadr sre))
                        n lo hi
                        (lambda (lo2 hi2)
                          (lp (caddr sre) (+ n n1) 0 0
                              (lambda (lo3 hi3)
                                (lp (if (pair? (cdddr sre))
                                        (cadddr sre)
                                        'epsilon)
                                    (+ n n1 n2) 0 0
                                    (lambda (lo4 hi4)
                                      (return (+ lo2 (min lo3 lo4))
                                              (and hi2 hi3 hi4
                                                   (+ hi2 (max hi3 hi4))
                                                   ))))))))))))
               ((dsm)
                (lp (sre-sequence (cdddr sre)) (+ n (cadr sre)) lo hi return))
               ((submatch submatch-named)
                (lp (sre-sequence
                     (if (eq? 'submatch (car sre)) (cdr sre) (cddr sre)))
                    (+ n 1) lo hi
                    (lambda (lo2 hi2)
                      (vector-set! sublens n (cons lo2 hi2))
                      (return lo2 hi2))))
               ((backref backref-ci)
                (let ((n (cond
                          ((number? (cadr sre)) (cadr sre))
                          ((assq (cadr sre) names) => cdr)
                          (else (error "unknown backreference" (cadr sre))))))
                  (cond
                   ((or (not (integer? n))
                        (not (< 0 n (vector-length sublens))))
                    (error "sre-length: invalid backreference" sre))
                   ((not (vector-ref sublens n))
                    (error "sre-length: invalid forward backreference" sre))
                   (else
                    (let ((lo2 (car (vector-ref sublens n)))
                          (hi2 (cdr (vector-ref sublens n))))
                      (return (+ lo lo2) (and hi hi2 (+ hi hi2))))))))
               ((* *?)
                (lp (sre-sequence (cdr sre)) n lo hi (lambda (lo hi) #f))
                (return lo #f))
               ((** **?)
                (cond
                 ((or (and (number? (cadr sre))
                           (number? (caddr sre))
                           (> (cadr sre) (caddr sre)))
                      (and (not (cadr sre)) (caddr sre)))
                  (return lo hi))
                 (else
                  (if (caddr sre)
                      (lp (sre-sequence (cdddr sre)) n 0 0
                          (lambda (lo2 hi2)
                            (return (+ lo (* (cadr sre) lo2))
                                    (and hi hi2 (+ hi (* (caddr sre) hi2))))))
                      (lp (sre-sequence (cdddr sre)) n 0 0
                          (lambda (lo2 hi2)
                            (return (+ lo (* (cadr sre) lo2)) #f)))))))
               ((+)
                (lp (sre-sequence (cdr sre)) n lo hi
                    (lambda (lo2 hi2)
                      (return (+ lo lo2) #f))))
               ((? ??)
                (lp (sre-sequence (cdr sre)) n lo hi
                    (lambda (lo2 hi2)
                      (return lo (and hi hi2 (+ hi hi2))))))
               ((= =? >= >=?)
                (lp `(** ,(cadr sre)
                         ,(if (memq (car sre) '(>= >=?)) #f (cadr sre))
                         ,@(cddr sre))
                    n lo hi return))
               ((look-ahead neg-look-ahead look-behind neg-look-behind)
                (return lo hi))
               (else
                (error "sre-length-ranges: unknown sre operator" sre)))))
        ((char? sre)
         (grow 1))
        ((string? sre)
         (grow (string-length sre)))
        ((memq sre '(any nonl))
         (grow 1))
        ((memq sre '(epsilon bos eos bol eol bow eow nwb commit))
         (return lo hi))
        (else
         (let ((cell (assq sre sre-named-definitions)))
           (if cell
               (lp (cdr cell) n lo hi return)
               (error "sre-length-ranges: unknown sre" sre)))))))
    sublens))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sre manipulation

;; build a (seq ls ...) sre from a list
(define (sre-sequence ls)
  (cond
   ((null? ls) 'epsilon)
   ((null? (cdr ls)) (car ls))
   (else (cons 'seq ls))))

;; build a (or ls ...) sre from a list
(define (sre-alternate ls)
  (cond
   ((null? ls) 'epsilon)
   ((null? (cdr ls)) (car ls))
   (else (cons 'or ls))))

;; returns an equivalent SRE without any match information
(define (sre-strip-submatches sre)
  (if (not (pair? sre))
      sre
      (case (car sre)
        ((submatch) (sre-strip-submatches (sre-sequence (cdr sre))))
        ((dsm) (sre-strip-submatches (sre-sequence (cdddr sre))))
        (else (map sre-strip-submatches sre)))))

;; given a char-set list of chars and strings, flattens them into
;; chars only
(define (sre-flatten-ranges ls)
  (let lp ((ls ls) (res '()))
    (cond
     ((null? ls)
      (reverse res))
     ((string? (car ls))
      (lp (append (string->list (car ls)) (cdr ls)) res))
     (else
      (lp (cdr ls) (cons (car ls) res))))))

(define (sre-names sre n names)
  (if (not (pair? sre))
      names
      (case (car sre)
        ((submatch)
         (sre-names (sre-sequence (cdr sre)) (+ n 1) names))
        ((submatch-named)
         (sre-names (sre-sequence (cddr sre))
                    (+ n 1)
                    (cons (cons (cadr sre) n) names)))
        ((dsm)
         (sre-names (sre-sequence (cdddr sre)) (+ n (cadr sre)) names))
        ((seq : or * + ? *? ?? w/case w/nocase atomic
              look-ahead look-behind neg-look-ahead neg-look-behind)
         (sre-sequence-names (cdr sre) n names))
        ((= >=)
         (sre-sequence-names (cddr sre) n names))
        ((** **?)
         (sre-sequence-names (cdddr sre) n names))
        (else
         names))))

(define (sre-sequence-names ls n names)
  (if (null? ls)
      names
      (sre-sequence-names (cdr ls)
                          (+ n (sre-count-submatches (car ls)))
                          (sre-names (car ls) n names))))

(define (sre-remove-initial-bos sre)
  (cond
   ((pair? sre)
    (case (car sre)
      ((seq : submatch * +)
       (cond
        ((not (pair? (cdr sre)))
         sre)
        ((eq? 'bos (cadr sre))
         (cons (car sre) (cddr sre)))
        (else
         (cons (car sre)
               (cons (sre-remove-initial-bos (cadr sre)) (cddr sre))))))
      ((or)
       (sre-alternate (map sre-remove-initial-bos (cdr sre))))
      (else
       sre)))
   (else
    sre)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; matching

(define (irregex-search x str . o)
  (let ((irx (irregex x)))
    (let ((start (if (pair? o) (car o) 0))
          (end   (if (and (pair? o) (pair? (cdr o)))
		     (cadr o) (string-length str)))
          (matches (irregex-new-matches irx)))
      (irregex-match-string-set! matches str)
      (irregex-search/matches irx str start end matches))))

;; internal routine, can be used in loops to avoid reallocating the
;; match vector
(define (irregex-search/matches irx str start end matches)
  (cond
   ((irregex-dfa irx)
    (cond
     ((flag-set? (irregex-flags irx) ~searcher?)
      (let ((m-end (dfa-match/longest (irregex-dfa irx) str start end)))
        (cond
         (m-end
          (irregex-match-start-index-set! matches 0 start)
          (irregex-match-end-index-set! matches 0 m-end)
          ((irregex-dfa/extract irx) str start m-end matches)
          matches)
         (else
          #f))))
     (else
      (let ((first-match
             (dfa-match/shortest (irregex-dfa/search irx) str start end)))
        (and
         first-match
         (let* ((lo+hi (vector-ref (irregex-lengths irx) 0))
                (m-start (if (cdr lo+hi)
                             (max start (- first-match (cdr lo+hi)))
                             start))
                (m-limit (- first-match (car lo+hi)))
                (dfa (irregex-dfa irx)))
           (let lp ((m-start m-start))
             (and (<= m-start m-limit)
                  (let ((m-end (dfa-match/longest dfa str m-start end)))
                    (cond
                     (m-end
                      (irregex-match-start-index-set! matches 0 m-start)
                      (irregex-match-end-index-set! matches 0 m-end)
                      ((irregex-dfa/extract irx) str m-start m-end matches)
                      matches)
                     (else
                      (lp (+ m-start 1)))))))))))))
   (else
    (let ((matcher (irregex-nfa irx)))
      (let lp ((start start))
        (and (<= start end)
             (let ((i (matcher str start matches (lambda () #f))))
               (cond
                (i
                 (irregex-match-start-index-set! matches 0 start)
                 (irregex-match-end-index-set! matches 0 i)
                 matches)
                (else
                 (lp (+ start 1)))))))))))

(define (irregex-match irx str)
  (let* ((irx (irregex irx))
         (matches (irregex-new-matches irx))
         (start 0)
         (end (string-length str)))
    (irregex-match-string-set! matches str)
    (cond
     ((irregex-dfa irx)
      (let ((m-end (dfa-match/longest (irregex-dfa irx) str start end)))
        (cond
         ((equal? m-end end)
          (irregex-match-start-index-set! matches 0 start)
          (irregex-match-end-index-set! matches 0 m-end)
          ((irregex-dfa/extract irx) str start m-end matches)
          matches)
         (else
          #f))))
     (else
      (let* ((matcher (irregex-nfa irx))
             (i (matcher str start matches (lambda () #f))))
        (cond
         ((equal? i end)
          (irregex-match-start-index-set! matches 0 start)
          (irregex-match-end-index-set! matches 0 i)
          matches)
         (else
          #f)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DFA matching

;; inline these
(define (dfa-init-state dfa)
  (vector-ref dfa 0))
(define (dfa-next-state dfa node)
  (vector-ref dfa (cdr node)))
(define (dfa-final-state? dfa state)
  (car state))

;; this searches for the first end index for which a match is possible
(define (dfa-match/shortest dfa str start end)
  (let lp ((i start) (state (dfa-init-state dfa)))
    (if (dfa-final-state? dfa state)
        i
        (and (< i end)
             (let* ((ch (string-ref str i))
                    (next (find (lambda (x)
                                  (or (eqv? ch (car x))
                                      (and (pair? (car x))
                                           (char<=? (caar x) ch)
                                           (char<=? ch (cdar x)))))
                                (cdr state))))
               (and next (lp (+ i 1) (dfa-next-state dfa next))))))))

;; this finds the longest match starting at a given index
(define (dfa-match/longest dfa str start end)
  (let lp ((i start)
           (state (dfa-init-state dfa))
           (res (and (dfa-final-state? dfa (dfa-init-state dfa)) start)))
    (if (>= i end)
        res
        (let* ((ch (string-ref str i))
               (cell (find (lambda (x)
                             (or (eqv? ch (car x))
                                 (and (pair? (car x))
                                      (char<=? (caar x) ch)
                                      (char<=? ch (cdar x)))))
                           (cdr state))))
          (if cell
              (let ((next (dfa-next-state dfa cell)))
                (lp (+ i 1)
                    next
                    (if (dfa-final-state? dfa next) (+ i 1) res)))
              res)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SRE->NFA compilation
;;
;; An NFA state is a numbered node with a list of patter->number
;; transitions, where pattern is either a character, (lo . hi)
;; character range, or epsilon (indicating an empty transition).
;; There may be duplicate characters and overlapping ranges - since
;; it's an NFA we process it by considering all possible transitions.

(define sre-named-definitions
  `((any . ,*all-chars*)
    (nonl . (- ,*all-chars* (,(string #\newline))))
    (alphabetic . (/ #\a #\z #\A #\Z))
    (alpha . alphabetic)
    (alphanumeric . (/ #\a #\z #\A #\Z #\0 #\9))
    (alphanum . alphanumeric)
    (alnum . alphanumeric)
    (lower-case . (/ #\a #\z))
    (lower . lower-case)
    (upper-case . (/ #\A #\Z))
    (upper . upper-case)
    (numeric . (/ #\0 #\9))
    (num . numeric)
    (digit . numeric)
    (punctuation . (or #\! #\" #\# #\% #\& #\' #\( #\) #\* #\, #\- #\.
                       #\/ #\: #\; #\? #\@ #\[ #\\ #\] #\_ #\{ #\}))
    (punct . punctuation)
    (graphic
     . (or alphanumeric punctuation #\$ #\+ #\< #\= #\> #\^ #\` #\| #\~))
    (graph . graphic)
    (blank . (or #\space ,(integer->char (- (char->integer #\space) 23))))
    (whitespace . (or blank #\newline))
    (space . whitespace)
    (white . whitespace)
    (printing or graphic whitespace)
    (print . printing)
    ;; XXXX we assume a (possibly shifted) ASCII-based ordering
    (control . (/ ,(integer->char (- (char->integer #\space) 32))
                  ,(integer->char (- (char->integer #\space) 1))))
    (cntrl . control)
    (hex-digit . (or numeric (/ #\a #\f #\A #\F)))
    (xdigit . hex-digit)
    (ascii . (/ ,(integer->char (- (char->integer #\space) 32))
                ,(integer->char (+ (char->integer #\space) 95))))
    (ascii-nonl . (/ ,(integer->char (- (char->integer #\space) 32))
                     ,(integer->char (- (char->integer #\newline) 1))
                     ,(integer->char (+ (char->integer #\newline) 1))
                     ,(integer->char (+ (char->integer #\space) 95))))
    (newline . (or (seq ,(integer->char (+ (char->integer #\newline) 3))
                        #\newline)
                   (/ #\newline
                      ,(integer->char (+ (char->integer #\newline) 3)))))

    ;; ... it's really annoying to support scheme48
    (word . (seq bow (+ (or alphanumeric #\_)) eow))
    (utf8-tail-char . (/ ,(integer->char (+ (char->integer #\space) #x60))
                         ,(integer->char (+ (char->integer #\space) #xA1))))
    (utf8-2-char . (seq (/ ,(integer->char (+ (char->integer #\space) #xA2))
                           ,(integer->char (+ (char->integer #\space) #xBF)))
                        utf8-tail-char))
    (utf8-3-char . (seq (/ ,(integer->char (+ (char->integer #\space) #xC0))
                           ,(integer->char (+ (char->integer #\space) #xCF)))
                        utf8-tail-char
                        utf8-tail-char))
    (utf8-4-char . (seq (/ ,(integer->char (+ (char->integer #\space) #xD0))
                           ,(integer->char (+ (char->integer #\space) #xD7)))
                        utf8-tail-char
                        utf8-tail-char
                        utf8-tail-char))
    (utf8-any . (or ascii utf8-2-char utf8-3-char utf8-4-char))
    (utf8-nonl . (or ascii-nonl utf8-2-char utf8-3-char utf8-4-char))
    ))

;; Compile and return the list of NFA states.  The start state will be
;; at the head of the list, and all remaining states will be in
;; descending numeric order, with state 0 being the unique accepting
;; state.
(define (sre->nfa sre . o)
  ;; we loop over an implicit sequence list
  (let lp ((ls (list sre))
           (n 1)
           (flags (if (pair? o) (car o) ~none))
           (next (list (list 0))))
    (define (new-state-number state)
      (max n (+ 1 (caar state))))
    (define (extend-state next . trans)
      (and next
           (cons (cons (new-state-number next)
                       (map (lambda (x) (cons x (caar next))) trans))
                 next)))
    (if (null? ls)
        next
        (cond
         ((string? (car ls))
          ;; process literal strings a char at a time
          (lp (append (string->list (car ls)) (cdr ls)) n flags next))
         ((eq? 'epsilon (car ls))
          ;; chars and epsilons go directly into the transition table
          (extend-state (lp (cdr ls) n flags next) (car ls)))
         ((char? (car ls))
          (let ((alt (char-altcase (car ls))))
            (if (and (flag-set? flags ~case-insensitive?)
                     (not (eqv? (car ls) alt)))
                (extend-state (lp (cdr ls) n flags next) (car ls) alt)
                (extend-state (lp (cdr ls) n flags next) (car ls)))))
         ((symbol? (car ls))
          (let ((cell (assq (car ls) sre-named-definitions)))
            (and cell (lp (cons (cdr cell) (cdr ls)) n flags next))))
         ((pair? (car ls))
          (cond
           ((string? (caar ls))
            ;; enumerated character set
            (lp (cons (sre-alternate (string->list (caar ls))) (cdr ls))
                n
                flags
                next))
           (else
            (case (caar ls)
              ((seq :)
               ;; for an explicit sequence, just append to the list
               (lp (append (cdar ls) (cdr ls)) n flags next))
              ((w/case w/nocase w/utf8 w/noutf8)
               (let* ((next (lp (cdr ls) n flags next))
                      (flags ((if (memq (caar ls) '(w/case w/utf8))
                                  flag-clear
                                  flag-join)
                              flags
                              (if (memq (caar ls) '(w/case w/nocase))
                                  ~case-insensitive?
                                  ~utf8?))))
                 (and next (lp (cdar ls) (new-state-number next) flags next))))
              ((/ - & ~) 
               (let ((ranges (sre->cset (car ls)
                                        (flag-set? flags ~case-insensitive?))))
                 (case (length ranges)
                   ((1)
                    (extend-state (lp (cdr ls) n flags next) (car ranges)))
                   (else
                    (let ((next (lp (cdr ls) n flags next)))
                      (and
                       next
                       (lp (list (sre-alternate
                                  (map (lambda (x) (if (pair? x)
                                                  (list '/ (car x) (cdr x))
                                                  x))
                                       ranges)))
                           (new-state-number next)
                           (flag-clear flags ~case-insensitive?)
                           next)))))))
              ((or)
               (let* ((next (lp (cdr ls) n flags next))
                      (b (and next
                              (lp (list (sre-alternate (cddar ls)))
                                  (new-state-number next)
                                  flags
                                  next)))
                      (a (and b (lp (list (cadar ls))
                                    (new-state-number b)
                                    flags
                                    next))))
                 ;; compile both branches and insert epsilon
                 ;; transitions to either
                 (and a
                      `((,(new-state-number a)
                         (epsilon . ,(caar a))
                         (epsilon . ,(caar b)))
                        ,@(take-up-to a next)
                        ,@b))))
              ((?)
               (let ((next (lp (cdr ls) n flags next)))
                 ;; insert an epsilon transition directly to next
                 (and
                  next
                  (let ((a (lp (cdar ls) (new-state-number next) flags next)))
                    (cond
                     (a
                      (set-cdr! (car a) `((epsilon . ,(caar next)) ,@(cdar a)))
                      a)
                     (else
                      #f))))))
              ((+ *)
               (let ((next (lp (cdr ls) n flags next)))
                 (and
                  next
                  (let* ((new (lp '(epsilon)
                                  (new-state-number next)
                                  flags
                                  next))
                         (a (lp (cdar ls) (new-state-number new) flags new)))
                    (and
                     a
                     (begin
                       ;; for *, insert an epsilon transition as in ? above
                       (if (eq? '* (caar ls))
                           (set-cdr! (car a)
                                     `((epsilon . ,(caar new)) ,@(cdar a))))
                       ;; for both, insert a loop back to self
                       (set-cdr! (car new)
                                 `((epsilon . ,(caar a)) ,@(cdar new)))
                       a))))))
              ((submatch submatch-named)
               ;; ignore submatches altogether
               (lp (cons (sre-sequence (cdar ls)) (cdr ls)) n flags next))
              (else
               #f)))))
         (else
          #f)))))

;; We don't really want to use this, we use the closure compilation
;; below instead, but this is included for reference and testing the
;; sre->nfa conversion.

;; (define (nfa-match nfa str)
;;   (let lp ((ls (string->list str)) (state (car nfa)) (epsilons '()))
;;     (if (null? ls)
;;         (zero? (car state))
;;         (any (lambda (m)
;;                (if (eq? 'epsilon (car m))
;;                    (and (not (memv (cdr m) epsilons))
;;                         (lp ls (assv (cdr m) nfa) (cons (cdr m) epsilons)))
;;                    (and (or (eqv? (car m) (car ls))
;;                             (and (pair? (car m))
;;                                  (char<=? (caar m) (car ls))
;;                                  (char<=? (car ls) (cdar m))))
;;                         (lp (cdr ls) (assv (cdr m) nfa) '()))))
;;              (cdr state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NFA->DFA compilation
;;
;; During processing, the DFA is a list of the form:
;;
;;   ((NFA-states ...) accepting-state? transitions ...)
;;
;; where the transitions are as in the NFA, except there are no
;; epsilons, duplicate characters or overlapping char-set ranges, and
;; the states moved to are closures (sets of NFA states).  Multiple
;; DFA states may be accepting states.

(define (nfa->dfa nfa . o)
  (let ((max-states (and (pair? o) (car o))))
    (let lp ((ls (list (nfa-closure nfa (list (caar nfa)))))
             (i 0)
             (res '()))
      (cond
       ((null? ls)
        (dfa-renumber (reverse res)))
       ((assoc (car ls) res)
        (lp (cdr ls) i res))
       (else
        (let* ((states (car ls))
               (trans (nfa-state-transitions nfa states))
               (accept? (and (memv 0 states) #t)))
          (and (or (not max-states) (< (+ i 1) max-states))
               (lp (append (map cdr trans) (cdr ls))
                   (+ i 1)
                   `((,states ,accept? ,@trans) ,@res)))))))))

;; When the conversion is complete we renumber the DFA sets-of-states
;; in order and convert the result to a vector for fast lookup.
(define (dfa-renumber dfa)
  (let ((states (map cons (map car dfa) (zero-to (length dfa)))))
    (define (renumber state)
      (cdr (assoc state states)))
    (list->vector
     (map
      (lambda (node)
        (cons (cadr node)
              (map (lambda (x) (cons (car x) (renumber (cdr x))))
                   (cddr node)))) 
      dfa))))

;; Extract all distinct characters or ranges and the potential states
;; they can transition to from a given set of states.  Any ranges that
;; would overlap with distinct characters are split accordingly.
(define (nfa-state-transitions nfa states)
  (let lp ((trans '())   ;; list of (char . state) or ((char . char) . state)
           (ls states)   ;; list of integers (remaining state numbers)
           (res '()))    ;; (char state ...) or ((char . char) state ...)
    (cond
     ((null? trans)
      (if (null? ls)
          (map (lambda (x) (cons (car x) (nfa-closure nfa (cdr x))))
               res)
          (let ((node (assv (car ls) nfa)))
            (lp (if node (cdr node) '()) (cdr ls) res))))
     ((eq? 'epsilon (caar trans))
      (lp (cdr trans) ls res))
     (else
      (lp (cdr trans) ls (nfa-join-transitions! res (car trans)))))))

(define (nfa-join-transitions! existing new)
  (define (join ls elt state)
    (if (not elt)
        ls
        (nfa-join-transitions! ls (cons elt state))))
  (cond
   ((char? (car new))
    (let ((ch (car new)))
      (let lp ((ls existing) (res '()))
        (cond
         ((null? ls)
          ;; done, just cons this on to the original list
          (cons (list ch (cdr new)) existing))
         ((eqv? ch (caar ls))
          ;; add a new state to an existing char
          (set-cdr! (car ls) (insert-sorted (cdr new) (cdar ls)))
          existing)
         ((and (pair? (caar ls))
               (char<=? (caaar ls) ch)
               (char<=? ch (cdaar ls)))
          ;; split a range
          (apply
           (lambda (left right)
             (cons (cons ch (insert-sorted (cdr new) (cdar ls)))
                   (append (if left (list (cons left (cdar ls))) '())
                           (if right (list (cons right (cdar ls))) '())
                           res
                           (cdr ls))))
           (split-char-range (caar ls) (car new))))
         (else
          ;; keep looking
          (lp (cdr ls) (cons (car ls) res)))))))
   (else
    (let ((lo (caar new))
          (hi (cdar new)))
      (let lp ((ls existing) (res '()))
        (cond
         ((null? ls)
          (cons (list (car new) (cdr new)) existing))
         ((and (char? (caar ls)) (char<=? lo (caar ls)) (char<=? (caar ls) hi))
          ;; range enclosing a character
          (apply
           (lambda (left right)
             (set-cdr! (car ls) (insert-sorted (cdr new) (cdar ls)))
             (join (join existing left (cdr new)) right (cdr new)))
           (split-char-range (car new) (caar ls))))
         ((and (pair? (caar ls))
               (or (and (char<=? (caaar ls) hi) (char<=? lo (cdaar ls)))
                   (and (char<=? hi (caaar ls)) (char<=? (cdaar ls) lo))))
          ;; overlapping ranges
          (apply
           (lambda (left1 left2 same right1 right2)
             (let ((old-states (cdar ls)))
               (set-car! (car ls) same)
               (set-cdr! (car ls) (insert-sorted (cdr new) old-states))
               (let* ((res (if right1
                               (cons (cons right1 old-states) existing)
                               existing))
                      (res (if right2 (cons (cons right2 old-states) res) res)))
                 (join (join res left1 (cdr new)) left2 (cdr new)))))
           (intersect-char-ranges (car new) (caar ls))))
         (else
          (lp (cdr ls) (cons (car ls) res)))))))))

(define (char-range c1 c2)
  (if (eqv? c1 c2) c1 (cons c1 c2)))

;; assumes ch is included in the range
(define (split-char-range range ch)
  (list
   (and (not (eqv? ch (car range)))
        (char-range (car range) (integer->char (- (char->integer ch) 1))))
   (and (not (eqv? ch (cdr range)))
        (char-range (integer->char (+ (char->integer ch) 1)) (cdr range)))))

;; returns (possibly #f) char ranges:
;;    a-only-1  a-only-2  a-and-b  b-only-1  b-only-2
(define (intersect-char-ranges a b)
  (if (char>? (car a) (car b))
      (reverse (intersect-char-ranges b a))
      (let ((a-lo (car a))
            (a-hi (cdr a))
            (b-lo (car b))
            (b-hi (cdr b)))
        (list
         (and (char<? a-lo b-lo)
              (char-range a-lo (integer->char (- (char->integer b-lo) 1))))
         (and (char>? a-hi b-hi)
              (char-range (integer->char (+ (char->integer b-hi) 1)) a-hi))
         (char-range b-lo (if (char<? b-hi a-hi) b-hi a-hi))
         #f
         (and (char>? b-hi a-hi)
              (char-range (integer->char (+ (char->integer a-hi) 1)) b-hi))))))

;; The `closure' of a list of NFA states - all states that can be
;; reached from any of them using any number of epsilon transitions.
(define (nfa-closure nfa states)
  (let lp ((ls states)
           (res '()))
    (cond
     ((null? ls)
      res)
     ((memv (car ls) res)
      (lp (cdr ls) res))
     (else
      (lp (append (map cdr
                       (filter (lambda (trans) (eq? 'epsilon (car trans)))
                               (cdr (assv (car ls) nfa))))
                  (cdr ls))
          (insert-sorted (car ls) res))))))

;; insert an integer uniquely into a sorted list
(define (insert-sorted n ls)
  (cond
   ((null? ls)
    (cons n '()))
   ((<= n (car ls))
    (if (= n (car ls))
        ls
        (cons n ls)))
   (else
    (cons (car ls) (insert-sorted n (cdr ls))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DFAs don't give us match information, so once we match and
;; determine the start and end, we need to recursively break the
;; problem into smaller DFAs to get each submatch.
;;
;; See http://compilers.iecc.com/comparch/article/07-10-026

(define (sre-match-extractor sre)
  (let lp ((sre sre) (n 1) (submatch-deps? #f))
    (cond
     ((not (sre-has-submatchs? sre))
      (if (not submatch-deps?)
          (lambda (str i j matches) j)
          (let ((dfa (nfa->dfa (sre->nfa sre))))
            (lambda (str i j matches)
              (dfa-match/longest dfa str i j)))))
     ((pair? sre)
      (case (car sre)
        ((: seq)
         (let* ((right (sre-sequence (cddr sre)))
                (match-left (lp (cadr sre) n #t))
                (match-right
                 (lp right (+ n (sre-count-submatches (cadr sre))) #t)))
           (lambda (str i j matches)
             (let lp ((k j) (best #f))
               (if (< k i)
                   best
                   (let* ((middle (match-left str i k matches))
                          (end (and middle
                                    (eqv? middle k)
                                    (match-right str middle j matches))))
                     (if (eqv? end j)
                         end
                         (lp (- k 1)
                             (if (or (not best) (and end (> end best)))
                                 end
                                 best)))))))))
        ((or)
         (let* ((rest (sre-alternate (cddr sre)))
                (match-first
                 (lp (cadr sre) n #t))
                (match-rest
                 (lp rest
                     (+ n (sre-count-submatches (cadr sre)))
                     submatch-deps?)))
           (lambda (str i j matches)
             (let ((k (match-first str i j matches)))
               (if (eqv? k j)
                   k
                   (match-rest str i j matches))))))
        ((* +)
         (letrec ((match-once
                   (lp (sre-sequence (cdr sre)) n #t))
                  (match-all
                   (lambda (str i j matches)
                     (let ((k (match-once str i j matches)))
                       (if (and k (< i k))
                           (match-all str k j matches)
                           i)))))
           (if (eq? '* (car sre))
               match-all
               (lambda (str i j matches)
                 (let ((k (match-once str i j matches)))
                   (and k
                        (match-all str k j matches)))))))
        ((?)
         (let ((match-once (lp (sre-sequence (cdr sre)) n #t)))
           (lambda (str i j matches)
             (let ((k (match-once str i j matches)))
               (or k i)))))
        ((submatch)
         (let ((match-one
                (lp (sre-sequence (cdr sre)) (+ n 1) #t)))
           (lambda (str i j matches)
             (let ((res (match-one str i j matches)))
               (cond
                ((number? res)
                 (irregex-match-start-index-set! matches n i)
                 (irregex-match-end-index-set! matches n res)))
               res))))
        (else
         (error "unknown regexp operator" (car sre)))))
     (else
      (error "unknown regexp" sre)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; closure compilation - we use this for non-regular expressions
;; instead of an interpreted NFA matcher

(define (sre->procedure sre . o)
  (define names
    (if (and (pair? o) (pair? (cdr o))) (cadr o) (sre-names sre 1 '())))
  (let lp ((sre sre)
           (n 1)
           (flags (if (pair? o) (car o) ~none))
           (next (lambda (str i matches fail) i)))
    (define (rec sre) (lp sre n flags next))
    (cond
     ((pair? sre)
      (if (string? (car sre))
          (sre-cset->procedure
           (sre->cset (car sre) (flag-set? flags ~case-insensitive?))
           next)
          (case (car sre)
            ((~ - & /)
             (sre-cset->procedure
              (sre->cset sre (flag-set? flags ~case-insensitive?))
              next))
            ((or)
             (case (length (cdr sre))
               ((0) (lambda (str i matches fail) (fail)))
               ((1) (rec (cadr sre)))
               (else
                (let* ((first (rec (cadr sre)))
                       (rest (lp (sre-alternate (cddr sre))
                                 (+ n (sre-count-submatches (cadr sre)))
                                 flags
                                 next)))
                  (lambda (str i matches fail)
                    (first str i matches (lambda () (rest str i matches fail))))))))
            ((w/case)
             (lp (sre-sequence (cdr sre))
                 n
                 (flag-clear flags ~case-insensitive?)
                 next))
            ((w/nocase)
             (lp (sre-sequence (cdr sre))
                 n
                 (flag-join flags ~case-insensitive?)
                 next))
            ((w/utf8)
             (lp (sre-sequence (cdr sre)) n (flag-join flags ~utf8?) next))
            ((w/noutf8)
             (lp (sre-sequence (cdr sre)) n (flag-clear flags ~utf8?) next))
            ((seq :)
             (case (length (cdr sre))
               ((0) next)
               ((1) (rec (cadr sre)))
               (else
                (let ((rest (lp (sre-sequence (cddr sre))
                                (+ n (sre-count-submatches (cadr sre)))
                                flags
                                next)))
                  (lp (cadr sre) n flags rest)))))
            ((?)
             (let ((body (rec (sre-sequence (cdr sre)))))
               (lambda (str i matches fail)
                 (body str i matches (lambda () (next str i matches fail))))))
            ((??)
             (let ((body (rec (sre-sequence (cdr sre)))))
               (lambda (str i matches fail)
                 (next str i matches (lambda () (body str i matches fail))))))
            ((*)
             (cond
              ((sre-empty? (sre-sequence (cdr sre)))
               (error "invalid sre: empty *" sre))
              (else
               (letrec ((body
                         (lp (sre-sequence (cdr sre))
                             n
                             flags
                             (lambda (str i matches fail)
                               (body str
                                     i
                                     matches
                                     (lambda () (next str i matches fail)))))))
                 (lambda (str i matches fail)
                   (body str i matches (lambda () (next str i matches fail))))))))
            ((*?)
             (cond
              ((sre-empty? (sre-sequence (cdr sre)))
               (error "invalid sre: empty *?" sre))
              (else
               (letrec ((body
                         (lp (sre-sequence (cdr sre))
                             n
                             flags
                             (lambda (str i matches fail)
                               (next str
                                     i
                                     matches
                                     (lambda () (body str i matches fail)))))))
                 (lambda (str i matches fail)
                   (next str i matches (lambda () (body str i matches fail))))))))
            ((+)
             (lp (sre-sequence (cdr sre))
                 n
                 flags
                 (rec (list '* (sre-sequence (cdr sre))))))
            ((=)
             (rec `(** ,(cadr sre) ,(cadr sre) ,@(cddr sre))))
            ((>=)
             (rec `(** ,(cadr sre) #f ,@(cddr sre))))
            ((** **?)
             (cond
              ((or (and (number? (cadr sre))
                        (number? (caddr sre))
                        (> (cadr sre) (caddr sre)))
                   (and (not (cadr sre)) (caddr sre)))
               (lambda (str i matches fail) (fail)))
              (else
               (let* ((from (cadr sre))
                      (to (caddr sre))
                      (? (if (eq? '** (car sre)) '? '??))
                      (* (if (eq? '** (car sre)) '* '*?))
                      (sre (sre-sequence (cdddr sre)))
                      (x-sre (sre-strip-submatches sre))
                      (next (if to
                                (if (= from to)
                                    next
                                    (fold (lambda (x next)
                                            (lp `(,? ,sre) n flags next))
                                          next
                                          (zero-to (- to from))))
                                (rec `(,* ,sre)))))
                 (if (zero? from)
                     next
                     (lp `(seq ,@(map (lambda (x) x-sre) (zero-to (- from 1)))
                               ,sre)
                         n
                         flags
                         next))))))
            ((word)
             (rec `(seq bow ,@(cdr sre) eow)))
            ((word+)
             (rec `(seq bow (+ (& (or alphanumeric "_")
                                  (or ,@(cdr sre)))) eow)))
            ((posix-string)
             (rec (string->sre (cadr sre))))
            ((look-ahead)
             (let ((check
                    (lp (sre-sequence (cdr sre))
                        n
                        flags
                        (lambda (str i matches fail) i))))
               (lambda (str i matches fail)
                 (if (check str i matches (lambda () #f))
                     (next str i matches fail)
                     (fail)))))
            ((neg-look-ahead)
             (let ((check
                    (lp (sre-sequence (cdr sre))
                        n
                        flags
                        (lambda (str i matches fail) i))))
               (lambda (str i matches fail)
                 (if (check str i matches (lambda () #f))
                     (fail)
                     (next str i matches fail)))))
            ((look-behind)
             (let ((check
                    (lp (sre-sequence (cons '(* any) (cdr sre)))
                        n
                        flags
                        (lambda (str i matches fail) i))))
               (lambda (str i matches fail)
                 (if (eqv? i (check (substring str 0 i) 0 matches (lambda () #f)))
                     (next str i matches fail)
                     (fail)))))
            ((neg-look-behind)
             (let ((check
                    (lp (sre-sequence (cons '(* any) (cdr sre)))
                        n
                        flags
                        (lambda (str i matches fail) i))))
               (lambda (str i matches fail)
                 (if (eqv? i (check (substring str 0 i) 0 matches (lambda () #f)))
                     (fail)
                     (next str i matches fail)))))
            ((atomic)
             (let ((once
                    (lp (sre-sequence (cdr sre))
                        n
                        flags
                        (lambda (str i matches fail) i))))
               (lambda (str i matches fail)
                 (let ((j (once str i matches (lambda () #f))))
                   (if j
                       (next str j matches fail)
                       (fail))))))
            ((if)
             (let* ((test-submatches (sre-count-submatches (cadr sre)))
                    (pass (lp (caddr sre) flags (+ n test-submatches) next))
                    (fail (if (pair? (cdddr sre))
                              (lp (cadddr sre)
                                  (+ n test-submatches
                                     (sre-count-submatches (caddr sre)))
                                  flags
                                  next)
                              (lambda (str i matches fail) (fail)))))
               (cond
                ((or (number? (cadr sre)) (symbol? (cadr sre)))
                 (let ((index
                        (if (symbol? (cadr sre))
                            (cond
                             ((assq (cadr sre) names) => cdr)
                             (else
                              (error "unknown named backref in SRE IF" sre)))
                            (cadr sre))))
                   (lambda (str i matches fail2)
                     (if (irregex-match-end-index matches index)
                         (pass str i matches fail2)
                         (fail str i matches fail2)))))
                (else
                 (let ((test (lp (cadr sre) n flags pass)))
                   (lambda (str i matches fail2)
                     (test str i matches (lambda () (fail str i matches fail2)))
                     ))))))
            ((backref backref-ci)
             (let ((n (cond ((number? (cadr sre)) (cadr sre))
                            ((assq (cadr sre) names) => cdr)
                            (else (error "unknown backreference" (cadr sre)))))
                   (compare (if (or (eq? (car sre) 'backref-ci)
                                    (flag-set? flags ~case-insensitive?))
                                string-ci=?
                                string=?)))
               (lambda (str i matches fail)
                 (let ((s (irregex-match-substring matches n)))
                   (if (not s)
                       (fail)
                       (let ((j (+ i (string-length s))))
                         (if (and (<= j (string-length str))
                                  (compare s (substring str i j)))
                             (next str j matches fail)
                             (fail))))))))
            ((dsm)
             (lp (sre-sequence (cdddr sre)) (+ n (cadr sre)) flags next))
            ((submatch)
             (let ((body
                    (lp (sre-sequence (cdr sre))
                        (+ n 1)
                        flags
                        (lambda (str i matches fail)
                          (let ((old (irregex-match-end-index matches n)))
                            (irregex-match-end-index-set! matches n i)
                            (next str i matches
                                  (lambda ()
                                    (irregex-match-end-index-set! matches n old)
                                    (fail))))))))
               (lambda (str i matches fail)
                 (let ((old (irregex-match-start-index matches n)))
                   (irregex-match-start-index-set! matches n i)
                   (body str i matches
                         (lambda ()
                           (irregex-match-start-index-set! matches n old)
                           (fail)))))))
            ((submatch-named)
             (rec `(submatch ,@(cddr sre))))
            (else
             (error "unknown regexp operator" sre)))))
     ((symbol? sre)
      (case sre
        ((any)
         (lambda (str i matches fail)
           (if (< i (string-length str))
               (next str (+ i 1) matches fail)
               (fail))))
        ((nonl)
         (lambda (str i matches fail)
           (if (and (< i (string-length str))
                    (not (eqv? #\newline (string-ref str i))))
               (next str (+ i 1) matches fail)
               (fail))))
        ((bos)
         (lambda (str i matches fail)
           (if (zero? i) (next str i matches fail) (fail))))
        ((bol)
         (lambda (str i matches fail)
           (if (or (zero? i) (eqv? #\newline (string-ref str (- i 1))))
               (next str i matches fail)
               (fail))))
        ((bow)
         (lambda (str i matches fail)
           (if (and (or (zero? i)
                        (not (char-alphanumeric? (string-ref str (- i 1)))))
                    (< i (string-length str))
                    (char-alphanumeric? (string-ref str i)))
               (next str i matches fail)
               (fail))))
        ((eos)
         (lambda (str i matches fail)
           (if (>= i (string-length str)) (next str i matches fail) (fail))))
        ((eol)
         (lambda (str i matches fail)
           (if (or (>= i (string-length str))
                   (eqv? #\newline (string-ref str i)))
               (next str i matches fail)
               (fail))))
        ((eow)
         (lambda (str i matches fail)
           (if (and (or (>= i (string-length str))
                        (not (char-alphanumeric? (string-ref str i))))
                    (> i 0)
                    (char-alphanumeric? (string-ref str (- i 1))))
               (next str i matches fail)
               (fail))))
        ((nwb)  ;; non-word-boundary
         (lambda (str i matches fail)
           (if (and (not (zero? i))
                    (< i (string-length str))
                    (if (char-alphanumeric? (string-ref str (- i 1)))
                        (char-alphanumeric? (string-ref str i))
                        (not (char-alphanumeric? (string-ref str i)))))
               (next str i matches fail)
               (fail))))
        ((epsilon)
         next)
        (else
         (let ((cell (assq sre sre-named-definitions)))
           (if cell
               (rec (cdr cell))
               (error "unknown regexp" sre))))))
     ((char? sre)
      (if (flag-set? flags ~case-insensitive?)
          (lambda (str i matches fail)
            (if (and (< i (string-length str))
                     (char-ci=? sre (string-ref str i)))
                (next str (+ i 1) matches fail)
                (fail)))
          (lambda (str i matches fail)
            (if (and (< i (string-length str))
                     (eqv? sre (string-ref str i)))
                (next str (+ i 1) matches fail)
                (fail)))))
     ((string? sre)
      (rec (sre-sequence (string->list sre))))
     (else
      (error "unknown regexp" sre)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple character sets as lists of ranges, as used in the NFA/DFA
;; compilation.  This is not especially efficient, but is portable and
;; scalable for any range of character sets.

(define (sre-cset->procedure cset next)
  (lambda (str i matches fail)
    (if (and (< i (string-length str))
             (cset-contains? cset (string-ref str i)))
        (next str (+ i 1) matches fail)
        (fail))))

(define (plist->alist ls)
  (let lp ((ls ls) (res '()))
    (if (null? ls)
        (reverse res)
        (lp (cddr ls) (cons (cons (car ls) (cadr ls)) res)))))

(define (alist->plist ls)
  (let lp ((ls ls) (res '()))
    (if (null? ls)
        (reverse res)
        (lp (cdr ls) (cons (cdar ls) (cons (caar ls) res))))))

(define (sre->cset sre . o)
  (let lp ((sre sre) (ci? (and (pair? o) (car o))))
    (define (rec sre) (lp sre ci?))
    (cond
     ((pair? sre)
      (if (string? (car sre))
          (if ci?
              (cset-case-insensitive (string->list (car sre)))
              (string->list (car sre)))
          (case (car sre)
            ((~)
             (cset-complement
              (fold cset-union (rec (cadr sre)) (map rec (cddr sre)))))
            ((&)
             (fold cset-intersection (rec (cadr sre)) (map rec (cddr sre))))
            ((-)
             (fold (lambda (x res) (cset-difference res x))
                   (rec (cadr sre))
                   (map rec (cddr sre))))
            ((/)
             (let ((res (plist->alist (sre-flatten-ranges (cdr sre)))))
               (if ci?
                   (cset-case-insensitive res)
                   res)))
            ((or)
             (fold cset-union (rec (cadr sre)) (map rec (cddr sre))))
            ((w/case)
             (lp (sre-alternate (cdr sre)) #f))
            ((w/nocase)
             (lp (sre-alternate (cdr sre)) #t))
            (else
             (error "not a valid sre char-set operator" sre)))))
     ((char? sre) (rec (list (string sre))))
     ((string? sre) (rec (list sre)))
     (else
      (let ((cell (assq sre sre-named-definitions)))
        (if cell
            (rec (cdr cell))
            (error "not a valid sre char-set" sre)))))))

;;;; another debugging utility
;; (define (cset->sre cset)
;;   (let lp ((ls cset) (chars '()) (ranges '()))
;;     (cond
;;      ((null? ls)
;;       (sre-alternate
;;        (append
;;         (if (pair? chars) (list (list (list->string chars))) '())
;;         (if (pair? ranges) (list (cons '/ (alist->plist ranges))) '()))))
;;      ((char? (car ls)) (lp (cdr ls) (cons (car ls) chars) ranges))
;;      (else (lp (cdr ls) chars (cons (car ls) ranges))))))

(define (cset-contains? cset ch)
  (find (lambda (x)
          (or (eqv? x ch)
              (and (pair? x) (char<=? (car x) ch) (char<=? ch (cdr x)))))
        cset))

(define (cset-range x)
  (if (char? x) (cons x x) x))

(define (char-ranges-overlap? a b)
  (if (pair? a)
      (if (pair? b)
          (or (and (char<=? (car a) (cdr b)) (char<=? (car b) (cdr a)))
              (and (char<=? (cdr b) (car a)) (char<=? (cdr a) (car b))))
          (and (char<=? (car a) b) (char<=? b (cdr a))))
      (if (pair? b)
          (char-ranges-overlap? b a)
          (eqv? a b))))

(define (char-ranges-union a b)
  (cons (if (char<=? (car a) (car b)) (car a) (car b))
        (if (char>=? (cdr a) (cdr b)) (cdr a) (cdr b))))

(define (cset-union a b)
  (cond ((null? b) a)
        ((find-tail (lambda (x) (char-ranges-overlap? x (car b))) a)
         => (lambda (ls)
              (cset-union
               (cset-union (append (take-up-to a ls) (cdr ls))
                           (list (char-ranges-union (cset-range (car ls))
                                                    (cset-range (car b)))))
               (cdr b))))
        (else (cset-union (cons (car b) a) (cdr b)))))

(define (cset-difference a b)
  (cond ((null? b) a)
        ((not (car b)) (cset-difference a (cdr b)))
        ((find-tail (lambda (x) (char-ranges-overlap? x (car b))) a)
         => (lambda (ls)
              (apply
               (lambda (left1 left2 same right1 right2)
                 (let* ((a (append (take-up-to a ls) (cdr ls)))
                        (a (if left1 (cons left1 a) a))
                        (a (if left2 (cons left2 a) a))
                        (b (if right1 (cset-union b (list right1)) b))
                        (b (if right2 (cset-union b (list right2)) b)))
                   (cset-difference a b)))
               (intersect-char-ranges (cset-range (car ls))
                                      (cset-range (car b))))))
        (else (cset-difference a (cdr b)))))

(define (cset-intersection a b)
  (let intersect ((a a) (b b) (res '()))
    (cond ((null? b) res)
          ((find-tail (lambda (x) (char-ranges-overlap? x (car b))) a)
           => (lambda (ls)
                (apply
                 (lambda (left1 left2 same right1 right2)
                   (let* ((a (append (take-up-to a ls) (cdr ls)))
                          (a (if left1 (cons left1 a) a))
                          (a (if left2 (cons left2 a) a))
                          (b (if right1 (cset-union b (list right1)) b))
                          (b (if right2 (cset-union b (list right2)) b)))
                     (intersect a b (cset-union res (list same)))))
                 (intersect-char-ranges (cset-range (car ls))
                                        (cset-range (car b))))))
          (else (intersect a (cdr b) res)))))

(define (cset-complement a)
  (cset-difference (sre->cset *all-chars*) a))

(define (cset-case-insensitive a)
  (let lp ((ls a) (res '()))
    (cond ((null? ls) (reverse res))
          ((and (char? (car ls)) (char-alphabetic? (car ls)))
           (let ((c2 (char-altcase (car ls)))
                 (res (cons (car ls) res)))
             (lp (cdr ls) (if (cset-contains? res c2) res (cons c2 res)))))
          ((and (pair? (car ls))
                (char-alphabetic? (caar ls))
                (char-alphabetic? (cdar ls)))
           (lp (cdr ls)
               (cset-union (cset-union res (list (car ls)))
                           (list (cons (char-altcase (caar ls))
                                       (char-altcase (cdar ls)))))))
          (else (lp (cdr ls) (cset-union res (list (car ls))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; match and replace utilities

(define (irregex-fold irx kons knil str . o)
  (let* ((irx (irregex irx))
         (matches (irregex-new-matches irx))
         (finish (if (pair? o) (car o) (lambda (i acc) acc)))
         (start (if (and (pair? o) (pair? (cdr o))) (cadr o) 0))
         (end (if (and (pair? o) (pair? (cdr o)) (pair? (cddr o)))
                  (caddr o)
                  (string-length str))))
    (irregex-match-string-set! matches str)
    (let lp ((i start) (acc knil))
      (if (>= i end)
          (finish i acc)
          (let ((m (irregex-search/matches irx str i end matches)))
            (if (not m)
                (finish i acc)
                (let* ((end (irregex-match-end m 0))
                       (acc (kons i m acc)))
                  (irregex-reset-matches! matches)
                  (lp end acc))))))))

(define (irregex-replace irx str . o)
  (let ((m (irregex-search (irregex irx) str)))
    (and
     m
     (string-cat-reverse
      (cons (substring str (irregex-match-end m 0) (string-length str))
            (append (irregex-apply-match m o)
                    (list (substring str 0 (irregex-match-start m 0)))))))))

(define (irregex-replace/all irx str . o)
  (irregex-fold
   irx
   (lambda (i m acc)
     (let ((m-start (irregex-match-start m 0)))
       (append (irregex-apply-match m o)
               (if (= i m-start)
                   acc
                   (cons (substring str i m-start) acc)))))
   '()
   str
   (lambda (i acc)
     (let ((end (string-length str)))
       (string-cat-reverse (if (= i end)
                               acc
                               (cons (substring str i end) acc)))))))

(define (irregex-apply-match m ls)
  (let lp ((ls ls) (res '()))
    (if (null? ls)
        res
        (cond
         ((integer? (car ls))
          (lp (cdr ls)
              (cons (or (irregex-match-substring m (car ls)) "") res)))
         ((procedure? (car ls))
          (lp (cdr ls) (cons ((car ls) m) res)))
         ((symbol? (car ls))
          (case (car ls)
            ((pre)
             (lp (cdr ls)
                 (cons (substring (irregex-match-string m)
                                  0
                                  (irregex-match-start m 0))
                       res)))
            ((post)
             (lp (cdr ls)
                 (cons (substring (irregex-match-string m)
                                  (irregex-match-end m 0)
                                  (string-length (irregex-match-string m)))
                       res)))
            (else (error "unknown match replacement" (car ls)))))
         (else
          (lp (cdr ls) (cons (car ls) res)))))))
