;;;; regex.scm - Unit for using the POSIX regex package
;
; Copyright (c) 2000-2006, Felix L. Winkelmann
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
;
; Send bugs, suggestions and ideas to: 
;
; felix@call-with-current-continuation.org
;
; Felix L. Winkelmann
; Unter den Gleichen 1
; 37130 Gleichen
; Germany


(cond-expand
 [chicken-compile-shared]
 [else (declare (unit regex))] )

(declare 
  (usual-integrations)
  (fixnum)
  (disable-interrupts)
  (hide ##regexp#re-match ##regexp#buffers ##regexp#gather-results ##regexp#gather-result-positions
	##regexp#buffer-index ##regexp#compile ##regexp#compile-pattern)
  (foreign-declare #<<EOF
#include <regex.h>

#define C_MAXIMAL_NUMBER_OF_SUB_MATCHES 256

regmatch_t C_match_registers[ C_MAXIMAL_NUMBER_OF_SUB_MATCHES + 1 ];

#define C_regexp_alloc_buffer(ptr) (C_set_block_item((ptr), 0, (C_word)calloc(1, sizeof(regex_t))))
#define C_regexp_count_matches(ptr) C_fix(((regex_t *)C_slot(ptr, 0))->re_nsub + 1)
#define C_regexp_register_start(i) C_fix(C_match_registers[ C_unfix(i) ].rm_so)
#define C_regexp_register_end(i) C_fix(C_match_registers[ C_unfix(i) ].rm_eo)
EOF
) )

(cond-expand
 [paranoia]
 [else
  (declare
    (no-bound-checks)
    (no-procedure-checks-for-usual-bindings)
    (bound-to-procedure
     ##sys#check-string ##sys#check-exact ##sys#make-pointer ##sys#cons ##sys#size ##sys#slot
     ##regexp#compile ##regexp#gather-results ##regexp#re-match
     ##regexp#re-compile-pattern) ) ] )

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
  (declare (emit-exports "regexunix.exports"))] )

(register-feature! 'regex)


;;; Create global pattern buffer and initalize:

(define-constant ##regexp#buffer-count 5)

(define ##regexp#buffers
  (list (cons "" (##sys#make-pointer))
	(cons "" (##sys#make-pointer))
	(cons "" (##sys#make-pointer))
	(cons "" (##sys#make-pointer))
	(cons "" (##sys#make-pointer)) ) )

(define ##regexp#buffer-index 0)

(for-each (lambda (b) (##core#inline "C_regexp_alloc_buffer" (cdr b))) ##regexp#buffers)
(set! ##regexp#buffers (list->vector ##regexp#buffers))


;;; Compile regular expression into pattern buffer:

(define ##regexp#re-compile-pattern
  (foreign-lambda* int ((c-string rx) (c-pointer buffer))
    "regfree(buffer);"
    "return(regcomp((regex_t *)buffer, rx, REG_EXTENDED));") )

(define (##regexp#compile regexp loc)
  (##sys#check-string regexp loc)
  (let ([index #f])
    (let loop ([i 0])
      (cond [(fx>= i ##regexp#buffer-count)
	     (set! index ##regexp#buffer-index)
	     (set! ##regexp#buffer-index (fx+ index 1)) 
	     (when (fx>= ##regexp#buffer-index ##regexp#buffer-count)
	       (set! ##regexp#buffer-index 0) ) ]
	    [(string=? regexp (##sys#slot (##sys#slot ##regexp#buffers i) 0))
	     (set! index i) ]
	    [else (loop (fx+ i 1))] ) )
    (let ([b (##sys#slot ##regexp#buffers index)])
      (if (zero? (##regexp#re-compile-pattern regexp (##sys#slot b 1)))
	  (##sys#setslot b 0 regexp) 
	  (##sys#error loc "can not compile regular expression" regexp) )
      (##sys#slot b 1) ) ) )

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
      (string-append "(" pat-ascii "|" pat-utf8-2 "|" pat-utf8-3 ")")))))

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
           (string-append "(" (string-intersperse acc "|") ")"))
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

(define (white-space-ignore-pattern str)
  (let lp ((ls (string->list str)) (res '()))
    (if (null? ls)
      (list->string (reverse res))
      (case (car ls)
        ((#\\)
         (if (pair? (cdr ls))
           (lp (cddr ls) (cons (cadr ls) res))
           (##sys#error 'regexp "trailing unescaped \\" str)))
        ((#\#)
         (let lp2 ((ls (cdr ls)))
           (cond ((null? ls) (lp '() res))
                 ((eqv? (car ls) #\newline) (lp (cdr ls) res))
                 (else (lp2 (cdr ls))))))
        ((#\space #\tab) (lp (cdr ls) res))
        (else (lp (cdr ls) (cons (car ls) res)))))))

(define (re-translate-pattern str ls)
  (if (null? ls)
    str
    (let ([s (if (car ls) (white-space-ignore-pattern str) str)] )
      (if (and (pair? (cdr ls)) (cadr ls))
        (utf8-pattern->byte-pattern s)
        s))))

(define-foreign-variable REG_EXTENDED unsigned-integer)
(define-foreign-variable REG_ICASE unsigned-integer)

(define regexp 
  (let ([alloc
	 (foreign-lambda* c-pointer () "return(calloc(1, sizeof(regex_t)));") ]
	[free (foreign-lambda* void ((c-pointer rx))
		"regfree((regex_t *)rx);"
		"C_free(rx);") ]
	[%comp 
	 (foreign-lambda* int ([c-string rx] [c-pointer ptr] [unsigned-integer flags])
	   "return(regcomp((regex_t *)ptr, rx, flags));") ] )
    (let ([comp
           (lambda (rx rt ls)
             (if (null? ls)
               (%comp rx rt REG_EXTENDED)
               (%comp (re-translate-pattern rx (cdr ls))
                      rt
                      (+ REG_EXTENDED (if (car ls) REG_ICASE 0))))) ] )
      (lambda (rx . o)
        (##sys#check-string rx 'regexp)
        (let ([rt (alloc)])
          (set-finalizer! rt free)
          (if (zero? (comp rx rt o))
	    (##sys#make-structure 'regexp rt)
	    (##sys#error 'regexp "can not compile regular expression" rx) ) ) ) ) ) )

(define (regexp? x)
  (##sys#structure? x 'regexp) )


;;; Gather matched result strings or positions:

(define (##regexp#gather-result-positions result b)
  (and (zero? result)
       (let ([n (##core#inline "C_regexp_count_matches" b)])
	 (let loop ([i 0])
	   (if (fx>= i n)
	       '()
	       (let ([start (##core#inline "C_regexp_register_start" i)])
		 (cons
		  (if (fx>= start 0)
		      (cons start (cons (##core#inline "C_regexp_register_end" i) '()))
		      #f)
		  (loop (fx+ i 1)) ) ) ) ) ) ) )

(define ##regexp#gather-results
  (let ([substring substring])
    (lambda (result str b)
      (let ([ps (##regexp#gather-result-positions result b)])
	(and ps
	     (##sys#map (lambda (poss) (and poss (apply substring str poss)))
		   ps) ) ) ) ) )


;;; Match string with regular expression:

(define ##regexp#re-match
  (foreign-lambda* int ((c-pointer buffer) (c-string str) (int start) (int range))
    "int i, r, n;"
    "regex_t *rx = (regex_t *)buffer;"
    "if(range) str[ range ] = '\0';"
    "n = rx->re_nsub + 1;"
    "r = regexec((regex_t *)buffer, str + start, n, C_match_registers, 0);"
    "if(start != 0) {"
    "  for(i = 0; i < n; ++i) {"
    "    C_match_registers[ i ].rm_so += start;"
    "    C_match_registers[ i ].rm_eo += start;"
    "  }"
    "}"
    "return(r);") )

(let ([b #f]
      [string-append string-append] )

  (define (prepare regexp str start loc)
    (##sys#check-string str loc)
    (let ([si (if (pair? start) (##sys#slot start 0) 0)])
      (##sys#check-exact si loc)
      (set! b 
	(cond [(string? regexp) (##regexp#compile (string-append "^" regexp "$") loc)]
	      [(##sys#structure? regexp 'regexp) (##sys#slot regexp 1)]
	      [else (##sys#signal-hook #:type-error loc "bad argument type - not a string or compiled regexp" regexp)] ) )
      (##regexp#re-match b str si 0) ) )

  (set! string-match
    (lambda (regexp str . start)
      (let ([m (prepare regexp str start 'string-match)])
	(##regexp#gather-results m str b) ) ) )

  (set! string-match-positions
    (lambda (regexp str . start)
      (let ([m (prepare regexp str start 'string-match-positions)])
	(##regexp#gather-result-positions m b) ) ) ) )


;;; Search string with regular expression:

(let ([b #f])

  (define (prepare regexp str start-and-range loc)
    (##sys#check-string str loc)
    (let* ([range (and (##core#inline "C_blockp" start-and-range) 
		       (##sys#slot start-and-range 1) ) ]
	   [si (if range (##sys#slot start-and-range 0) 0)]
	   [ri (if (##core#inline "C_blockp" range) (##sys#slot range 0) 0)] )
      (##sys#check-exact si loc)
      (##sys#check-exact ri loc)
      (set! b
	(cond [(string? regexp) (##regexp#compile regexp loc)]
	      [(##sys#structure? regexp 'regexp) (##sys#slot regexp 1)]
	      [else (##sys#signal-hook #:type-error loc "bad argument type - not a string or compiled regexp" regexp)] ) )
      (##regexp#re-match b str si ri) ) )

  (set! string-search 
    (lambda (regexp str . start-and-range)
      (let ([s (prepare regexp str start-and-range 'string-search)])
	(##regexp#gather-results s str b) ) ) )

  (set! string-search-positions
    (lambda (regexp str . start-and-range)
      (let ([s (prepare regexp str start-and-range 'string-search-positions)])
	(##regexp#gather-result-positions s b) ) ) ) )


(include "regex-common.scm")
