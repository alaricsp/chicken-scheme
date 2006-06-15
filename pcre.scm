;;;; pcre.scm - Unit for using the PCRE regex package
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
  (export regexp string-match string-search string-match-positions string-search-positions regexp-escape
	  string-split-fields string-substitute string-substitute* glob->regexp grep regexp?)
  (foreign-declare #<<EOF
#include <pcre.h>

#define C_REGEX_OVECTOR_SIZE         256

static const char *C_regex_error;
static int C_regex_error_offset;
static int C_regex_ovector[ C_REGEX_OVECTOR_SIZE * 2 ];
EOF
) )

(cond-expand
 [paranoia]
 [else
  (declare
    (no-bound-checks)
    (no-procedure-checks-for-usual-bindings)
    ;(no-procedure-checks)
    ) ] )

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
  (declare (emit-exports "pcre.exports")) ] )

(register-feature! 'regex 'pcre)


;;; Compile regular expression into pattern buffer:

(define-foreign-variable C_regex_error c-string)

(define-foreign-variable PCRE_CASELESS unsigned-integer)
(define-foreign-variable PCRE_EXTENDED unsigned-integer)
(define-foreign-variable PCRE_UTF8 unsigned-integer)

(define re-compile-pattern
  (foreign-lambda* c-pointer ((c-string rx) (unsigned-integer flags))
    "return(pcre_compile(rx, flags, &C_regex_error, &C_regex_error_offset, NULL));") )

(define finalizer
  (foreign-lambda void "pcre_free" c-pointer) )

(define re-compile
  (lambda (regexp loc)
    (##sys#check-string regexp loc)
    (let ([pcre (re-compile-pattern regexp 0)])
      (or pcre
	  (##sys#error loc (##sys#string-append "can not compile regular expression - " C_regex_error) regexp) ) ) ) )

(define (re-compile-options->integer ls)
  (if (null? ls)
    0
    (+ (if (car ls) PCRE_CASELESS 0)
       (let ((ls (cdr ls)))
         (if (null? ls)
           0
           (+ (if (car ls) PCRE_EXTENDED 0)
              (if (and (pair? (cdr ls)) (cadr ls))
                PCRE_UTF8
                0 )))))))

(define (regexp rx . o)
  (##sys#check-string rx 'regexp)
  (let ([rt (re-compile-pattern rx (re-compile-options->integer o))])
    (set-finalizer! rt finalizer)
    (##sys#make-structure 'regexp rt) ) )

(define (regexp? x)
  (##sys#structure? x 'regexp) )


;;; Gather matched result strings or positions:

(define re-register-index
  (foreign-lambda* int ([int i])
    "return(C_regex_ovector[ i ]);") )

(define (gather-result-positions n b c)
  (and (fx> n 0)
       (let loop ([i 0])
	 (cond [(fx>= i c) '()]
	       [(fx>= i n) (cons #f (loop (fx+ i 1)))]
	       [else
		(let ([start (re-register-index (fx* i 2))])
		  (cons
		   (if (fx>= start 0)
		       (cons start (cons (re-register-index (fx+ (fx* i 2) 1)) '()))
			 #f)
		   (loop (fx+ i 1)) ) ) ] ) ) ) )

(define gather-results
  (let ([substring substring])
    (lambda (result str b c)
      (let ([ps (gather-result-positions result b c)])
	(and ps
	     (##sys#map (lambda (poss) (and poss (apply substring str poss)))
			ps) ) ) ) ) )


;;; Match string with regular expression:

(define re-match
  (foreign-lambda* int ((c-pointer buffer) (c-string str) (int start) (int range))
    "return(pcre_exec(buffer, NULL, str, start + range, start, 0, C_regex_ovector, C_REGEX_OVECTOR_SIZE));") )

(define re-capture-count
  (foreign-lambda* int ([c-pointer buffer])
    "int c;"
    "pcre_fullinfo(buffer, NULL, PCRE_INFO_CAPTURECOUNT, &c);"
    "return(c);") )

(let ([b #f]
      [c #f]
      [string-append string-append] )

  (define (prepare regexp str start loc)
    (##sys#check-string str loc)
    (let ([si (if (pair? start) (##sys#slot start 0) 0)])
      (##sys#check-exact si loc)
      (set! b 
	(cond [(string? regexp) (re-compile (string-append (if (fx> si 0) "" "^") regexp "$") loc)]
	      [(##sys#structure? regexp 'regexp) (##sys#slot regexp 1)]
	      [else (##sys#signal-hook #:type-error loc "bad argument type - not a string or compiled regexp" regexp)] ) )
      (set! c (fx+ 1 (re-capture-count b)))
      (let ([r (re-match b str si (fx- (##sys#size str) si))])
	(when (string? regexp) (finalizer b))
	r) ) )

  (set! string-match
    (lambda (regexp str . start)
      (let ([m (prepare regexp str start 'string-match)])
	(gather-results m str b c) ) ) )

  (set! string-match-positions
    (lambda (regexp str . start)
      (let ([m (prepare regexp str start 'string-match-positions)])
	(gather-result-positions m b c) ) ) ) )


;;; Search string with regular expression:

(let ([b #f]
      [c #f] )

  (define (prepare regexp str start-and-range loc)
    (##sys#check-string str loc)
    (let* ([range (and (##core#inline "C_blockp" start-and-range) 
		       (##sys#slot start-and-range 1) ) ]
	   [si (if range (##sys#slot start-and-range 0) 0)]
	   [ri (if (##core#inline "C_blockp" range) (##sys#slot range 0) (fx- (##sys#size str) si))] )
      (##sys#check-exact si loc)
      (##sys#check-exact ri loc)
      (set! b
	(cond [(string? regexp) (re-compile regexp loc)]
	      [(##sys#structure? regexp 'regexp) (##sys#slot regexp 1)]
	      [else (##sys#signal-hook #:type-error loc "bad argument type - not a string or compiled regexp" regexp)] ) )
      (set! c (fx+ 1 (re-capture-count b)))
      (let ([r (re-match b str si ri)])
	(when (string? regexp) (finalizer b))
	r) ) )

  (set! string-search 
    (lambda (regexp str . start-and-range)
      (let ([s (prepare regexp str start-and-range 'string-search)])
	(gather-results s str b c) ) ) )

  (set! string-search-positions
    (lambda (regexp str . start-and-range)
      (let ([s (prepare regexp str start-and-range 'string-search-positions)])
	(gather-result-positions s b c) ) ) ) )


(include "regex-common.scm")
