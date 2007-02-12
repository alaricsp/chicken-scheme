;;;; regex.scm - Unit for using the PCRE regex package
;
; Copyright (c) 2000-2007, Felix L. Winkelmann
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
  (bound-to-procedure
   get-output-string open-output-string ##sys#write-char-0 string-search string->list list->string
   ##sys#substring string-search-positions reverse ##sys#fragments->string substring make-string
   string-substitute
   ##sys#signal-hook string-append ##sys#make-c-string set-finalizer! ##sys#string-append)
  (foreign-declare #<<EOF
#include "pcre/pcre.h"

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
  (declare (emit-exports "regex.exports")) ] )

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


;;; Split string into fields:

(define string-split-fields
  (let ([reverse reverse]
	[substring substring]
	[string-search-positions string-search-positions] )
    (lambda (regexp str . mode-and-start)
      (##sys#check-string str 'string-split-fields)
      (let* ([argc (length mode-and-start)]
	     [len (##sys#size str)]
	     [mode (if (fx> argc 0) (car mode-and-start) #t)]
	     [start (if (fx> argc 1) (cadr mode-and-start) 0)] 
	     [fini (case mode
		     [(#:suffix)
		      (lambda (ms start)
			(if (fx< start len)
			    (##sys#error 'string-split-fields "record does not end with suffix" str regexp)
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
	  (let ([m (string-search-positions regexp str start)])
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
		     (cond [(or (not (fixnum? which)) (fx= count which))
			    (push (substring string index (car range)))
			    (substitute matches)
			    (loop upto #f) ]
			   [else
			    (push (substring string index upto))
			    (loop upto (fx+ count 1)) ] ) ) ]
		  [else
		   (push (substring string index (##sys#size string)))
		   (##sys#fragments->string total (reverse result)) ] ) ) ) ) ) ) )

(define string-substitute*
  (let ((string-substitute string-substitute))
    (lambda (str smap . mode)
      (##sys#check-string str 'string-substitute*)
      (##sys#check-list smap 'string-substitute*)
      (let ((mode (and (pair? mode) (car mode))))
	(let loop ((str str) (smap smap))
	  (if (null? smap)
	      str
	      (let ((sm (##sys#slot smap 0)))
		(loop (string-substitute (car sm) (cdr sm) str mode)
		      (##sys#slot smap 1) ) ) ) ) ) ) ) )


;;; Some useful things:

(define glob->regexp
  (let ((list->string list->string)
	(string->list string->list) )
    (lambda (s)
      (##sys#check-string s 'glob->regexp)
      (list->string
       (let loop ((cs (string->list s)))
	 (if (null? cs)
	     '()
	     (let ((c (car cs))
		   (rest (cdr cs)) )
	       (cond ((char=? c #\*) `(#\. #\* ,@(loop rest)))
		     ((char=? c #\?) (cons '#\. (loop rest)))
		     ((char=? c #\[)
		      (cons
		       #\[
		       (let loop2 ((rest rest))
			 (match rest
			   ((#\] . more)
			    (cons #\] (loop more)) )
			   ((#\- c . more)
			    `(#\- ,c ,@(loop2 more)) )
			   ((c1 #\- c2 . more)
			    `(,c1 #\- ,c2 ,@(loop2 more)) )
			   ((c . more) 
			    (cons c (loop2 more)) )
			   (() 
			    (error 
			     'glob->regexp
			     "unexpected end of character class" 
			     s)) ) ) ) )
		     ((or (char-alphabetic? c) (char-numeric? c)) (cons c (loop rest)))
		     (else `(#\\ ,c ,@(loop rest))) ) ) ) ) ) ) ) )

(define grep
  (let ([string-search string-search])
    (lambda (rx lst)
      (##sys#check-list lst 'grep)
      (let loop ([lst lst])
	(if (null? lst)
	    '()
	    (let ([x (car lst)]
		  [r (cdr lst)] )
	      (if (string-search rx x)
		  (cons x (loop r))
		  (loop r) ) ) ) ) ) ) )


;;;; Escape regular expression (suggested by Peter Bex):

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
