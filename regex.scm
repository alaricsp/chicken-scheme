;;;; regex.scm - Unit for using the PCRE regex package
;
; Copyright (c) 2000-2007, Felix L. Winkelmann
; Copyright (c) 2008, The Chicken Team
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following
; conditions are met:
;
;   Redistributions of source code must retain the above copyright notice, this list of conditions and the following
;     disclaimer.
;   Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
;     disclaimer in the documentation and/or other materials provided with the distribution.
;   Neither the name of the author nor the names of its contributors may be used to endorse or promote
;     products derived from this software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
; AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
; POSSIBILITY OF SUCH DAMAGE.


(cond-expand
 [chicken-compile-shared]
 [else (declare (unit regex))] )

(declare
  (usual-integrations)
  (disable-interrupts)
  (generic) ; PCRE options use lotsa bits
  (disable-warning var)
  (bound-to-procedure
    ;; Forward reference
    regex-chardef-table? make-anchored-pattern
    ;; Imports
    get-output-string open-output-string
    string->list list->string string-length string-ref substring make-string string-append
    reverse list-ref
    char=? char-alphabetic? char-numeric? char->integer
    set-finalizer!
    ##sys#pointer?
    ##sys#slot ##sys#setslot ##sys#size
    ##sys#make-structure ##sys#structure?
    ##sys#error ##sys#signal-hook
    ##sys#substring ##sys#fragments->string ##sys#make-c-string ##sys#string-append
    ##sys#write-char-0 )
  (export
    regex-chardef-table? regex-chardef-table
    regexp? regexp regexp*
    regexp-optimize
    make-anchored-pattern
    string-match string-match-positions string-search string-search-positions
    string-split-fields string-substitute string-substitute*
    glob? glob->regexp
    grep
    regexp-escape ) )

(cond-expand
 [paranoia]
 [else
  (declare
    (no-bound-checks)
    (no-procedure-checks-for-usual-bindings) ) ] )

(include "unsafe-declarations.scm")

(cond-expand
 ((not unsafe)
  (define (##sys#check-chardef-table x loc)
    (unless (regex-chardef-table? x)
      (##sys#error loc "invalid character definition tables structure" x) ) )
  (declare
    (bound-to-procedure
      ;; Imports
      ##sys#check-string ##sys#check-list ##sys#check-exact ##sys#check-vector
      ##sys#check-structure ##sys#check-symbol ##sys#check-blob ##sys#check-integer )
    (export
      ##sys#check-chardef-table )))
 (else))


;;;

#>#include "pcre.h"<#


;;;

(register-feature! 'regex 'pcre)


;;; From unit lolevel:

(define-inline (%tag-pointer ptr tag)
  (let ([tp (##sys#make-tagged-pointer tag)])
    (##core#inline "C_copy_pointer" ptr tp)
    tp ) )

(define-inline (%tagged-pointer? x tag)
  (and (##core#inline "C_blockp" x)
       (##core#inline "C_taggedpointerp" x)
       (eq? tag (##sys#slot x 1)) ) )


;;; PCRE Types:

(define-foreign-type pcre (c-pointer "pcre"))
(define-foreign-type nonnull-pcre (nonnull-c-pointer "pcre"))

(define-foreign-type pcre_extra (c-pointer "pcre_extra"))
(define-foreign-type nonnull-pcre_extra (nonnull-c-pointer "pcre_extra"))

(define-foreign-variable PCRE_CASELESS unsigned-integer)
(define-foreign-variable PCRE_EXTENDED unsigned-integer)
(define-foreign-variable PCRE_UTF8 unsigned-integer)
(define-foreign-variable PCRE_MULTILINE unsigned-integer)
(define-foreign-variable PCRE_DOTALL unsigned-integer)
(define-foreign-variable PCRE_ANCHORED unsigned-integer)
(define-foreign-variable PCRE_DOLLAR_ENDONLY unsigned-integer)
(define-foreign-variable PCRE_EXTRA unsigned-integer)
(define-foreign-variable PCRE_NOTBOL unsigned-integer)
(define-foreign-variable PCRE_NOTEOL unsigned-integer)
(define-foreign-variable PCRE_UNGREEDY unsigned-integer)
(define-foreign-variable PCRE_NOTEMPTY unsigned-integer)
(define-foreign-variable PCRE_NO_AUTO_CAPTURE unsigned-integer)
(define-foreign-variable PCRE_NO_UTF8_CHECK unsigned-integer)
(define-foreign-variable PCRE_AUTO_CALLOUT unsigned-integer)
(define-foreign-variable PCRE_PARTIAL unsigned-integer)
(define-foreign-variable PCRE_DFA_SHORTEST unsigned-integer)
(define-foreign-variable PCRE_DFA_RESTART unsigned-integer)
(define-foreign-variable PCRE_FIRSTLINE unsigned-integer)
(define-foreign-variable PCRE_DUPNAMES unsigned-integer)
(define-foreign-variable PCRE_NEWLINE_CR unsigned-integer)
(define-foreign-variable PCRE_NEWLINE_LF unsigned-integer)
(define-foreign-variable PCRE_NEWLINE_CRLF unsigned-integer)
(define-foreign-variable PCRE_NEWLINE_ANY unsigned-integer)
(define-foreign-variable PCRE_NEWLINE_ANYCRLF unsigned-integer)
(define-foreign-variable PCRE_BSR_ANYCRLF unsigned-integer)
(define-foreign-variable PCRE_BSR_UNICODE unsigned-integer)

(declare (hide pcre-option->number))

(define (pcre-option->number opt)
  (case opt
    ((caseless)             PCRE_CASELESS)
    ((multiline)            PCRE_MULTILINE)
    ((dotall)               PCRE_DOTALL)
    ((extended)             PCRE_EXTENDED)
    ((anchored)             PCRE_ANCHORED)
    ((dollar-endonly)       PCRE_DOLLAR_ENDONLY)
    ((extra)                PCRE_EXTRA)
    ((notbol)               PCRE_NOTBOL)
    ((noteol)               PCRE_NOTEOL)
    ((ungreedy)             PCRE_UNGREEDY)
    ((notempty)             PCRE_NOTEMPTY)
    ((utf8)                 PCRE_UTF8)
    ((no-auto-capture)      PCRE_NO_AUTO_CAPTURE)
    ((no-utf8-check)        PCRE_NO_UTF8_CHECK)
    ((auto-callout)         PCRE_AUTO_CALLOUT)
    ((partial)              PCRE_PARTIAL)
    ((dfa-shortest)         PCRE_DFA_SHORTEST)
    ((dfa-restart)          PCRE_DFA_RESTART)
    ((firstline)            PCRE_FIRSTLINE)
    ((dupnames)             PCRE_DUPNAMES)
    ((newline-cr)           PCRE_NEWLINE_CR)
    ((newline-lf)           PCRE_NEWLINE_LF)
    ((newline-crlf)         PCRE_NEWLINE_CRLF)
    ((newline-any)          PCRE_NEWLINE_ANY)
    ((newline-anycrlf)      PCRE_NEWLINE_ANYCRLF)
    ((bsr-anycrlf)          PCRE_BSR_ANYCRLF)
    ((bsr-unicode)          PCRE_BSR_UNICODE) ) )


;;; The regexp structure primitives:

(define re-finalizer
  (foreign-lambda void "pcre_free" c-pointer) )

(define-inline (%make-regexp code)
  (set-finalizer! code re-finalizer)
  (##sys#make-structure 'regexp code #f 0) )

(define-inline (%regexp? x)
  (##sys#structure? x 'regexp) )

(define-inline (%regexp-code rx)
  (##sys#slot rx 1) )

(define-inline (%regexp-extra rx)
  (##sys#slot rx 2) )

(define-inline (%regexp-options rx)
  (##sys#slot rx 3) )

(define-inline (%regexp-extra-set! rx extra)
  (when extra (set-finalizer! extra re-finalizer))
  (##sys#setslot rx 2 extra) )

(define-inline (%regexp-options-set! rx options)
  (##sys#setslot rx 3 options) )


;;; Character Definition Tables:

;; The minimum necessary to handle chardef table parameters.

;;

(define (regex-chardef-table? x)
  (%tagged-pointer? x 'chardef-table) )

;; Get a character definitions tables structure for the current locale.

(define regex-chardef-table
  (let ([re-maketables
          (foreign-lambda* (c-pointer unsigned-char) ()
            "return (pcre_maketables ());")]
        [re-make-chardef-table-type
          (lambda (tables)
            (%tag-pointer tables 'chardef-table) ) ] )
    (lambda (#!optional tables)
      ; Using this to type tag a ref is a bit of a hack but beats
      ; having another public variable.
      (if tables
          ; then existing reference so just tag it
          (if (##sys#pointer? tables)
              (re-make-chardef-table-type tables)
              (##sys#signal-hook #:type-error 'regex-chardef-table
               "bad argument type - not a pointer" tables) )
          ; else make a new chardef tables
          (let ([tables (re-maketables)])
            (if tables
                (let ([tables (re-make-chardef-table-type tables)])
                  (set-finalizer! tables re-finalizer)
                  tables )
                (##sys#error-hook 6 'regex-chardef-table) ) ) ) ) ) )


;;; Regexp record:

(define (regexp? x)
  (%regexp? x) )


;;; PCRE errors:

#>
static const char *C_regex_error;
static int C_regex_error_offset;
<#

(define-foreign-variable C_regex_error c-string)
(define-foreign-variable C_regex_error_offset int)

(define re-error
  (let ([string-append string-append])
    (lambda (loc msg . args)
      (apply ##sys#error loc (string-append msg " - " C_regex_error) args) ) ) )

;;; Compile regular expression:

;FIXME nonnull-unsigned-c-string causes problems - converted string is too long!

(define re-compile
  (foreign-lambda* pcre ((nonnull-c-string patt) (unsigned-integer options) ((const (c-pointer unsigned-char)) tables))
    "return(pcre_compile(patt, options, &C_regex_error, &C_regex_error_offset, tables));") )

(define (re-checked-compile pattern options tables loc)
  (##sys#check-string pattern loc)
  (or (re-compile pattern options #f)
      (re-error loc "cannot compile regular expression" pattern C_regex_error_offset) ) )

;; Compile with subset of options and no tables

(define (regexp pattern . options)
  (let ([options->integer
          (lambda ()
            (if (null? options)
                0
                (+ (if (car options) PCRE_CASELESS 0)
                   (let ((options (cdr options)))
                     (if (null? options)
                         0
                         (+ (if (car options) PCRE_EXTENDED 0)
                            (let ((options (cdr options)))
                              (if (and (pair? options) (car options)) PCRE_UTF8 0 ) ) ) ) ) ) ) )])
    (%make-regexp (re-checked-compile pattern (options->integer) #f 'regexp)) ) )

;; Compile with full options and tables available

(define (regexp* pattern . args)
  (let-optionals args ([options '()] [tables #f])
    (##sys#check-string pattern 'regexp*)
    (##sys#check-list options 'regexp*)
    (when tables (##sys#check-chardef-table tables 'regexp*))
    (%make-regexp (re-checked-compile pattern (pcre-option->number options) tables 'regexp*)) ) )


;;; Optimize compiled regular expression:

;; Invoke optimizer

(define re-study
  (foreign-lambda* pcre_extra (((const nonnull-pcre) code))
    "return(pcre_study(code, 0, &C_regex_error));"))

;; Optimize compiled regular expression
;; Returns whether optimization performed

(define (regexp-optimize rx)
  (##sys#check-structure rx 'regexp 'regexp-optimize)
  (let ([extra (re-study (%regexp-code rx))])
    (cond [C_regex_error
            (re-error 'regexp-optimize "cannot optimize regular expression" rx)]
          [extra
            (%regexp-extra-set! rx extra)
            #t]
          [else
            #f] ) ) )


;;; Captured results vector:

;; Match positions vector (PCRE ovector)

#>
#define OVECTOR_LENGTH_MULTIPLE 3
#define STATIC_OVECTOR_LEN 256
static int C_regex_ovector[OVECTOR_LENGTH_MULTIPLE * STATIC_OVECTOR_LEN];
<#

;;

(define ovector-start-ref
  (foreign-lambda* int ((int i))
    "return(C_regex_ovector[i * 2]);") )

(define ovector-end-ref
  (foreign-lambda* int ((int i))
    "return(C_regex_ovector[(i * 2) + 1]);") )


;;; Gather matched result strings or positions:

(define (gather-result-positions result)
  (let ([mc (car result)]
        [cc (cadr result)])
    (and (fx> mc 0)
         (let loop ([i 0])
           (cond [(fx>= i cc)
                   '()]
                 [(fx>= i mc)
                   (cons #f (loop (fx+ i 1)))]
                 [else
                  (let ([start (ovector-start-ref i)])
                    (cons (and (fx>= start 0)
                               (list start (ovector-end-ref i)))
                          (loop (fx+ i 1)) ) ) ] ) ) ) ) )

(define gather-results
  (let ([substring substring])
    (lambda (str result)
      (let ([ps (gather-result-positions result)])
        (and ps
             (##sys#map (lambda (poss) (and poss (apply substring str poss))) ps) ) ) ) ) )


;;; Common match string with compile regular expression:

(define re-match
  (foreign-lambda* int (((const nonnull-pcre) code) ((const pcre_extra) extra)
                        (nonnull-scheme-pointer str) (int start) (int range)
                        (unsigned-integer options))
    "return(pcre_exec(code, extra, str, start + range, start, options, C_regex_ovector, STATIC_OVECTOR_LEN * OVECTOR_LENGTH_MULTIPLE));") )

(define re-match-capture-count
  (foreign-lambda* int (((const nonnull-pcre) code) ((const pcre_extra) extra))
    "int cc;"
    "pcre_fullinfo(code, extra, PCRE_INFO_CAPTURECOUNT, &cc);"
    "return(cc + 1);") )

(define (perform-match rgxp str si ri loc)
  (let* ([extra #f]
         [options 0]
         [rx
          (cond [(string? rgxp)
                  (re-checked-compile rgxp 0 #f loc)]
                [(%regexp? rgxp)
                  (set! extra (%regexp-extra rgxp))
                  (set! options (%regexp-options rgxp))
                  (%regexp-code rgxp)]
                [else
                  (##sys#signal-hook #:type-error
                                     loc
                                     "bad argument type - not a string or compiled regular expression"
                                     rgxp)] )]
         [cc (re-match-capture-count rx extra)]
         [mc (re-match rx extra str si ri options)])
    (when (string? rgxp) (re-finalizer rx))
    (list mc cc) ) )


;;; Match string with regular expression:

;; Note that start is a BYTE offset

(define string-match)
(define string-match-positions)
(let ()

  (define (prepare-match rgxp str start loc)
    (##sys#check-string str loc)
    (let ([si (if (pair? start) (car start) 0)])
      (##sys#check-exact si loc)
      (perform-match (if (string? rgxp)
                         (make-anchored-pattern rgxp (fx< 0 si))
                         rgxp)
                     str si (fx- (##sys#size str) si)
                     loc) ) )

  (set! string-match
    (lambda (rgxp str . start)
      (gather-results str (prepare-match rgxp str start 'string-match)) ) )

  (set! string-match-positions
    (lambda (rgxp str . start)
      (gather-result-positions (prepare-match rgxp str start 'string-match-positions)) ) ) )


;;; Search string with regular expression:

;; Note that start & range are BYTE offsets


(define string-search)
(define string-search-positions)
(let ()

  (define (prepare-search rgxp str start-and-range loc)
    (##sys#check-string str loc)
    (let* ([range (and (pair? start-and-range) (cdr start-and-range)) ]
           [si (if range (car start-and-range) 0)]
           [ri (if (pair? range) (car range) (fx- (##sys#size str) si))] )
      (##sys#check-exact si loc)
      (##sys#check-exact ri loc)
      (perform-match rgxp str si ri loc) ) )

  (set! string-search
    (lambda (rgxp str . start-and-range)
      (gather-results str (prepare-search rgxp str start-and-range 'string-search)) ) )

  (set! string-search-positions
    (lambda (rgxp str . start-and-range)
      (gather-result-positions (prepare-search rgxp str start-and-range 'string-search-positions)) ) ) )


;;; Split string into fields:

(define string-split-fields
  (let ([reverse reverse]
        [substring substring]
        [string-search-positions string-search-positions] )
    (lambda (rgxp str . mode-and-start)
      (##sys#check-string str 'string-split-fields)
      (let* ([argc (length mode-and-start)]
             [len (##sys#size str)]
             [mode (if (fx> argc 0) (car mode-and-start) #t)]
             [start (if (fx> argc 1) (cadr mode-and-start) 0)]
             [fini (case mode
                     [(#:suffix)
                      (lambda (ms start)
                        (if (fx< start len)
                            (##sys#error 'string-split-fields
                                         "record does not end with suffix" str rgxp)
                            (reverse ms) ) ) ]
                     [(#:infix)
                      (lambda (ms start)
                        (if (fx>= start len)
                            (reverse (cons "" ms))
                            (reverse (cons (substring str start len) ms)) ) ) ]
                     [else (lambda (ms start) (reverse ms)) ] ) ]
             [fetch (case mode
                      [(#:infix #:suffix) (lambda (start from to) (substring str start from))]
                      [else (lambda (start from to) (substring str from to))] ) ] )
        (let loop ([ms '()] [start start])
          (let ([m (string-search-positions rgxp str start)])
            (if m
                (let* ([mp (car m)]
                       [from (car mp)]
                       [to (cadr mp)] )
                  (if (fx= from to)
                      (if (fx= to len)
                          (fini ms start)
                          (loop (cons (fetch start (fx+ from 1) (fx+ to 2)) ms) (fx+ to 1)) )
                      (loop (cons (fetch start from to) ms) to) ) )
                (fini ms start) ) ) ) ) ) ) )


;;; Substitute matching strings:

(define string-substitute
  (let ([substring substring]
        [reverse reverse]
        [make-string make-string]
        [string-search-positions string-search-positions] )
    (lambda (regex subst string . flag)
      (##sys#check-string subst 'string-substitute)
      (let* ([which (if (pair? flag) (car flag) 1)]
             [substlen (##sys#size subst)]
             [substlen-1 (fx- substlen 1)]
             [result '()]
             [total 0] )
        (define (push x)
          (set! result (cons x result))
          (set! total (fx+ total (##sys#size x))) )
        (define (substitute matches)
          (let loop ([start 0] [index 0])
            (if (fx>= index substlen-1)
                (push (if (fx= start 0) subst (substring subst start substlen)))
                (let ([c (##core#inline "C_subchar" subst index)]
                      [index+1 (fx+ index 1)] )
                  (if (char=? c #\\)
                      (let ([c2 (##core#inline "C_subchar" subst index+1)])
                        (if (and (not (char=? #\\ c2)) (char-numeric? c2))
                            (let ([mi (list-ref matches (fx- (char->integer c2) 48))])
                              (push (substring subst start index))
                              (push (substring string (car mi) (cadr mi)))
                              (loop (fx+ index 2) index+1) )
                            (loop start (fx+ index+1 1)) ) )
                      (loop start index+1) ) ) ) ) )
        (let loop ([index 0] [count 1])
          (let ([matches (string-search-positions regex string index)])
            (cond [matches
                   (let* ([range (car matches)]
                          [upto (cadr range)] )
                     (cond ((fx= 0 (fx- (cadr range) (car range)))
                            (##sys#error
                             'string-substitute "empty substitution match"
                             regex) )
                           ((or (not (fixnum? which)) (fx= count which))
                            (push (substring string index (car range)))
                            (substitute matches)
                            (loop upto #f) )
                           (else
                            (push (substring string index upto))
                            (loop upto (fx+ count 1)) ) ) ) ]
                  [else
                   (push (substring string index (##sys#size string)))
                   (##sys#fragments->string total (reverse result)) ] ) ) ) ) ) ) )

(define string-substitute*
  (let ([string-substitute string-substitute])
    (lambda (str smap . mode)
      (##sys#check-string str 'string-substitute*)
      (##sys#check-list smap 'string-substitute*)
      (let ((mode (and (pair? mode) (car mode))))
        (let loop ((str str) (smap smap))
          (if (null? smap)
              str
              (let ((sm (car smap)))
                (loop (string-substitute (car sm) (cdr sm) str mode)
                      (cdr smap) ) ) ) ) ) ) ) )


;;; Glob support:

;FIXME is it worthwhile making this accurate?
(define (glob? str)
  (##sys#check-string str 'glob?)
  (let loop ([idx (fx- (string-length str) 1)])
    (and (fx<= 0 idx)
         (case (string-ref str idx)
           [(#\* #\] #\?)
             (or (fx= 0 idx)
                 (not (char=? #\\ (string-ref str (fx- idx 1))))
                 (loop (fx- idx 2)))]
           [else
             (loop (fx- idx 1))]) ) ) )

(define glob->regexp
  (let ([list->string list->string]
        [string->list string->list] )
    (lambda (s)
      (##sys#check-string s 'glob->regexp)
      (list->string
       (let loop ((cs (string->list s)))
         (if (null? cs)
             '()
             (let ([c (car cs)]
                   [rest (cdr cs)] )
               (cond [(char=? c #\*)  `(#\. #\* ,@(loop rest))]
                     [(char=? c #\?)  (cons '#\. (loop rest))]
                     [(char=? c #\[)
                      (cons
                       #\[
                       (let loop2 ((rest rest))
                         (if (pair? rest)
			     (cond ((char=? #\] (car rest))
				    (cons #\] (loop (cdr rest))))
				   ((and (char=? #\- (car rest)) (pair? (cdr rest)))
				    `(#\- ,(cadr rest) ,@(loop2 (cddr rest))))
				   ((and (pair? (cdr rest)) (pair? (cddr rest))
					 (char=? #\- (cadr rest)) )
				    `(,(car rest) #\- ,(caddr rest)
				      ,@(loop2 (cdddr rest))))
				   ((pair? rest)
				    (cons (car rest) (loop2 (cdr rest))))
				   ((null? rest)
				    (error 'glob->regexp "unexpected end of character class" s))))))]
                     [(or (char-alphabetic? c) (char-numeric? c)) (cons c (loop rest))]
                     [else `(#\\ ,c ,@(loop rest))] ) ) ) ) ) ) ) )


;;; Grep-like function on list:

(define grep
  (let ([string-search string-search])
    (lambda (rgxp lst)
      (##sys#check-list lst 'grep)
      (let loop ([lst lst])
        (if (null? lst)
            '()
            (let ([x (car lst)]
                  [r (cdr lst)] )
              (if (string-search rgxp x)
                  (cons x (loop r))
                  (loop r) ) ) ) ) ) ) )


;;; Escape regular expression (suggested by Peter Bex):

(define regexp-escape
  (let ([open-output-string open-output-string]
        [get-output-string get-output-string] )
    (lambda (str)
      (##sys#check-string str 'regexp-escape)
      (let ([out (open-output-string)]
            [len (##sys#size str)] )
        (let loop ([i 0])
          (cond [(fx>= i len) (get-output-string out)]
                [(memq (##core#inline "C_subchar" str i)
                       '(#\. #\\ #\? #\* #\+ #\^ #\$ #\( #\) #\[ #\] #\| #\{ #\}))
                 (##sys#write-char-0 #\\ out)
                 (##sys#write-char-0 (##core#inline "C_subchar" str i) out)
                 (loop (fx+ i 1)) ]
                [else
                 (##sys#write-char-0 (##core#inline "C_subchar" str i) out)
                 (loop (fx+ i 1)) ] ) ) ) ) ) )


;;; Anchored pattern:

(define make-anchored-pattern
  (let ([string-append string-append])
    (lambda (rgxp . args)
      (let-optionals args ([nos #f] [noe #f])
        (cond [(string? rgxp)
                (string-append (if nos "" "^") rgxp (if noe "" "$"))]
              [else
                (##sys#check-structure rgxp 'regexp 'make-anchored-pattern)
                (when (or nos noe)
                  (warning 'make-anchored-pattern
                           "cannot select partial anchor for compiled regular expression") )
                (%regexp-options-set! rgxp
                                      (bitwise-ior (%regexp-options regexp)
                                                  (pcre-option->number 'anchored)))
                rgxp] ) ) ) ) )
