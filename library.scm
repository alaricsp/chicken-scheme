;;;; library.scm - R5RS library for the CHICKEN compiler
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


(declare
  (unit library)
  (disable-interrupts)
  (disable-warning var redef)
  (usual-integrations)
  (hide ##sys#dynamic-unwind ##sys#find-symbol
	##sys#grow-vector ##sys#default-parameter-vector 
	print-length-limit current-print-length setter-tag read-marks
	##sys#fetch-and-check-port-arg ##sys#print-exit)
  (foreign-declare #<<EOF
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <time.h>

#ifdef HAVE_SYSEXITS_H
# include <sysexits.h>
#endif

#if !defined(_MSC_VER) && !defined(__DJGPP__) && !defined(__MWERKS__)
# include <unistd.h>
#endif

#ifndef EX_SOFTWARE
# define EX_SOFTWARE    70
#endif

#define C_close_file(p)       (C_fclose((C_FILEPTR)(C_port_file(p))), C_SCHEME_UNDEFINED)
#define C_f64peek(b, i)       (C_temporary_flonum = ((double *)C_data_pointer(b))[ C_unfix(i) ], C_SCHEME_UNDEFINED)
#define C_fetch_c_strlen(b, i) C_fix(strlen((C_char *)C_block_item(b, C_unfix(i))))
#define C_peek_c_string(b, i, to, len) (C_memcpy(C_data_pointer(to), (C_char *)C_block_item(b, C_unfix(i)), C_unfix(len)), C_SCHEME_UNDEFINED)
#define C_free_mptr(p, i)     (C_free((void *)C_block_item(p, C_unfix(i))), C_SCHEME_UNDEFINED)

#define C_direct_continuation(dummy)  t1

#define C_get_current_seconds(dummy)  (C_temporary_flonum = time(NULL), C_SCHEME_UNDEFINED)
#define C_peek_c_string_at(ptr, i)    ((C_char *)(((C_char **)ptr)[ i ]))

static C_word one_two_three = 123;

static C_word fast_read_line_from_file(C_word str, C_word port, C_word size) {
  int n = C_unfix(size);
  int i;
  int c;
  char *buf = C_c_string(str);
  C_FILEPTR fp = C_port_file(port);

  if ((c = getc(fp)) == EOF)
    return C_SCHEME_END_OF_FILE;

  ungetc(c, fp);

  for (i = 0; i < n; i++) {
    c = getc(fp);
    switch (c) {
    case '\r':
      if ((c = getc(fp)) != '\n') ungetc(c, fp);
    case EOF: clearerr(fp);
    case '\n': return C_fix(i);
    }
    buf[i] = c;
  }
  return C_SCHEME_FALSE;
}
EOF
) )

(cond-expand 
 ((not unsafe)
  (declare (emit-exports "library.exports")))
 (else) )

(cond-expand
 [paranoia]
 [else
  (declare
    (no-bound-checks)
    (no-procedure-checks-for-usual-bindings)
    (bound-to-procedure
     ##sys#check-char ##sys#check-exact ##sys#check-port ##sys#check-string ##sys#substring ##sys#check-port-mode
     ##sys#for-each ##sys#map ##sys#setslot ##sys#allocate-vector ##sys#check-pair 
     ##sys#not-a-proper-list-error ##sys#error ##sys#warn ##sys#signal-hook
     ##sys#check-symbol ##sys#check-vector ##sys#floor ##sys#ceiling ##sys#truncate ##sys#round 
     ##sys#check-number ##sys#cons-flonum ##sys#check-integer ##sys#check-special
     ##sys#flonum-fraction ##sys#make-port ##sys#fetch-and-check-port-arg ##sys#print 
     ##sys#check-structure ##sys#make-structure ##sys#procedure->string
     ##sys#gcd ##sys#lcm ##sys#ensure-heap-reserve ##sys#check-list 
     ##sys#enable-interrupts ##sys#disable-interrupts ##sys#->feature-id
     ##sys#fudge ##sys#user-read-hook ##sys#check-range ##sys#read
     ##sys#string->symbol ##sys#symbol->string ##sys#dynamic-unwind ##sys#pathname-resolution
     ##sys#expand-home-path ##sys#string-append ##sys#symbol->qualified-string
     ##sys#error-handler ##sys#signal ##sys#abort ##sys#port-data
     ##sys#reset-handler ##sys#exit-handler ##sys#dynamic-wind ##sys#port-line
     ##sys#grow-vector ##sys#run-pending-finalizers ##sys#peek-char-0 ##sys#read-char-0
     ##sys#schedule ##sys#make-thread ##sys#print-to-string ##sys#scan-buffer-line
     ##sys#update-thread-state-buffer ##sys#restore-thread-state-buffer ##sys#user-print-hook 
     ##sys#current-exception-handler ##sys#default-exception-handler ##sys#abandon-mutexes ##sys#make-mutex
     ##sys#port-has-file-pointer? ##sys#infix-list-hook char-name ##sys#open-file-port make-parameter
     ##sys#intern-symbol ##sys#make-string ##sys#number? software-type build-platform
     open-output-string get-output-string print-call-chain ##sys#symbol-has-toplevel-binding? repl
     argv condition-property-accessor ##sys#decorate-lambda ##sys#become! ##sys#lambda-decoration
     getter-with-setter ##sys#lambda-info ##sys#lambda-info->string open-input-string ##sys#gc
     ##sys#memory-info ##sys#make-c-string ##sys#find-symbol-table array:make-locative display
     newline string-append ##sys#with-print-length-limit write print vector-fill! ##sys#context-switch
     ##sys#set-finalizer! open-output-string get-output-string read ##sys#make-pointer
     ##sys#pointer->address number->string ##sys#flush-output ##sys#break-entry ##sys#step
     ##sys#apply-values ##sys#signal-hook ##sys#get-call-chain ##sys#really-print-call-chain
     string->keyword keyword? string->keyword getenv ##sys#number->string ##sys#copy-bytes
     call-with-current-continuation ##sys#string->number ##sys#inexact->exact ##sys#exact->inexact
     ##sys#reverse-list->string reverse ##sys#inexact? list? string ##sys#char->utf8-string 
     ##sys#unicode-surrogate? ##sys#surrogates->codepoint
     ##sys#update-errno ##sys#file-info close-output-port close-input-port ##sys#peek-unsigned-integer
     continuation-graft char-downcase string-copy remainder floor ##sys#exact? list->string
     ##sys#append ##sys#list ##sys#cons ##sys#list->vector ##sys#list ##sys#apply ##sys#make-vector
     ##sys#write-char ##sys#force-finalizers ##sys#cleanup-before-exit ##sys#write-char-0
     ##sys#default-read-info-hook ##sys#read-error) ) ] )


(include "build.scm")


(define-constant namespace-size 997)
(define-constant namespace-max-id-len 31)
(define-constant char-name-table-size 37)
(define-constant output-string-initial-size 256)
(define-constant default-parameter-vector-size 16)
(define-constant maximal-string-length #x00ffffff)



;;; System routines:

(define (exit . code) (apply (##sys#exit-handler) code))
(define (reset) ((##sys#reset-handler)))

(define (##sys#error . args)
  (if (pair? args)
      (apply ##sys#signal-hook #:error args)
      (##sys#signal-hook #:error #f)))

(define ##sys#warnings-enabled #t)

(define (##sys#warn msg . args)
  (when ##sys#warnings-enabled
    (apply ##sys#signal-hook #:warning msg args) ) )

(define (enable-warnings . bool)
  (if (pair? bool) 
      (set! ##sys#warnings-enabled (car bool))
      ##sys#warnings-enabled) )

(define error ##sys#error)
(define warning ##sys#warn)

(define-foreign-variable main_argc int "C_main_argc")
(define-foreign-variable main_argv c-pointer "C_main_argv")
(define-foreign-variable strerror c-string "strerror(errno)")

(define (set-gc-report! flag) (##core#inline "C_set_gc_report" flag))
(define ##sys#gc (##core#primitive "C_gc"))
(define (##sys#setslot x i y) (##core#inline "C_i_setslot" x i y))
(define (##sys#setislot x i y) (##core#inline "C_i_set_i_slot" x i y))
(define ##sys#allocate-vector (##core#primitive "C_allocate_vector"))
(define argv (##core#primitive "C_get_argv"))
(define (argc+argv) (##sys#values main_argc main_argv))
(define ##sys#make-structure (##core#primitive "C_make_structure"))
(define ##sys#ensure-heap-reserve (##core#primitive "C_ensure_heap_reserve"))
(define (##sys#fudge fudge-factor) (##core#inline "C_fudge" fudge-factor))
(define ##sys#call-host (##core#primitive "C_return_to_host"))
(define return-to-host ##sys#call-host)
(define ##sys#file-info (##core#primitive "C_file_info"))
(define ##sys#symbol-table-info (##core#primitive "C_get_symbol_table_info"))
(define ##sys#memory-info (##core#primitive "C_get_memory_info"))
(define (current-milliseconds) (##sys#fudge 16))
(define (current-gc-milliseconds) (##sys#fudge 31))
(define cpu-time (##core#primitive "C_cpu_time"))
(define ##sys#decode-seconds (##core#primitive "C_decode_seconds"))
(define getenv (##core#primitive "C_get_environment_variable"))
(define (##sys#start-timer) (##core#inline "C_start_timer"))
(define ##sys#stop-timer (##core#primitive "C_stop_timer"))
(define (##sys#immediate? x) (not (##core#inline "C_blockp" x)))
(define (##sys#message str) (##core#inline "C_message" str))
(define (##sys#byte x i) (##core#inline "C_subbyte" x i))
(define (##sys#setbyte x i n) (##core#inline "C_setbyte" x i n))
(define (##sys#void) (##core#undefined))
(define void ##sys#void)
(define ##sys#undefined-value (##core#undefined))
(define (##sys#halt) (##core#inline "C_halt" #f))
(define ##sys#dload (##core#primitive "C_dload"))
(define ##sys#set-dlopen-flags! (##core#primitive "C_set_dlopen_flags"))
(define (##sys#flo2fix n) (##core#inline "C_quickflonumtruncate" n))
(define ##sys#become! (##core#primitive "C_become"))
(define (##sys#block-ref x i) (##core#inline "C_i_block_ref" x i))
(define ##sys#apply-values (##core#primitive "C_apply_values"))
(define ##sys#copy-closure (##core#primitive "C_copy_closure"))
(define ##sys#apply-argument-limit (##sys#fudge 34))

(define (##sys#block-set! x i y)
  (cond-expand
   [(not unsafe)
    (when (or (not (##core#inline "C_blockp" x)) 
	      (and (##core#inline "C_specialp" x) (fx= i 0))
	      (##core#inline "C_byteblockp" x) ) 
      (##sys#signal-hook '#:type-error '##sys#block-set! "slot not accessible" x) )
    (##sys#check-range i 0 (##sys#size x) '##sys#block-set!) ]
   [else] )
  (##sys#setslot x i y) )

(define (current-seconds) 
  (##core#inline "C_get_current_seconds" #f)
  (##sys#cons-flonum) )

(define (##sys#check-structure x y . z) 
  (if (pair? z)
      (##core#inline "C_i_check_structure_2" x y (car z))
      (##core#inline "C_i_check_structure" x y) ) )

(define (##sys#check-byte-vector x . y) 
  (if (pair? y)
      (##core#inline "C_i_check_bytevector_2" x (car y))
      (##core#inline "C_i_check_bytevector" x) ) )

(define (##sys#check-pair x . y) 
  (if (pair? y)
      (##core#inline "C_i_check_pair_2" x (car y))
      (##core#inline "C_i_check_pair" x) ) )

(define (##sys#check-list x . y) 
  (if (pair? y)
      (##core#inline "C_i_check_list_2" x (car y))
      (##core#inline "C_i_check_list" x) ) )

(define (##sys#check-string x . y) 
  (if (pair? y)
      (##core#inline "C_i_check_string_2" x (car y))
      (##core#inline "C_i_check_string" x) ) )

(define (##sys#check-number x . y) 
  (if (pair? y)
      (##core#inline "C_i_check_number_2" x (car y))
      (##core#inline "C_i_check_number" x) ) )

(define (##sys#check-exact x . y) 
  (if (pair? y)
      (##core#inline "C_i_check_exact_2" x (car y))
      (##core#inline "C_i_check_exact" x) ) )

(define (##sys#check-symbol x . y) 
  (if (pair? y)
      (##core#inline "C_i_check_symbol_2" x (car y))
      (##core#inline "C_i_check_symbol" x) ) )

(define (##sys#check-vector x . y) 
  (if (pair? y)
      (##core#inline "C_i_check_vector_2" x (car y))
      (##core#inline "C_i_check_vector" x) ) )

(define (##sys#check-char x . y) 
  (if (pair? y)
      (##core#inline "C_i_check_char_2" x (car y))
      (##core#inline "C_i_check_char" x) ) )

(define (##sys#check-integer x . y)
  (unless (##core#inline "C_i_integerp" x) 
    (##sys#signal-hook #:type-error (if (pair? y) (car y) #f) "bad argument type - not an integer" x) ) )

(define ##sys#check-range 
  (lambda (i from to loc)
    (##sys#check-exact i loc)
    (if (or (not (fx>= i from))
	    (not (fx< i to)) ) 
	(##sys#signal-hook #:bounds-error loc "out of range" i from to) ) ) )

(define (##sys#check-special ptr loc)
  (unless (and (##core#inline "C_blockp" ptr) (##core#inline "C_specialp" ptr))
    (##sys#signal-hook #:type-error loc "bad argument type - not a pointer-like object" ptr) ) )

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
    (define-macro (##sys#check-port-mode . _) '(##core#undefined))
    (define-macro (##sys#check-number . _) '(##core#undefined))
    (define-macro (##sys#check-special . _) '(##core#undefined))
    (define-macro (##sys#check-byte-vector . _) '(##core#undefined)) ) ]
 [else] )

(define (##sys#force promise)
  (if (##sys#structure? promise 'promise)
      ((##sys#slot promise 1))
      promise) )

(define force ##sys#force)

(define (system cmd)
  (##sys#check-string cmd 'system)
  (##core#inline "C_execute_shell_command" cmd) )


;;; Operations on booleans:

(define (not x) (##core#inline "C_i_not" x))
(define (boolean? x) (##core#inline "C_booleanp" x))


;;; Equivalence predicates:

(define (eq? x y) (##core#inline "C_eqp" x y))
(define (eqv? x y) (##core#inline "C_i_eqvp" x y))
(define (equal? x y) (##core#inline "C_i_equalp" x y))


;;; Pairs and lists:

(define (pair? x) (##core#inline "C_i_pairp" x))
(define (cons x y) (##core#inline_allocate ("C_a_i_cons" 3) x y))
(define (car x) (##core#inline "C_i_car" x))
(define (cdr x) (##core#inline "C_i_cdr" x))

(define (set-car! x y) (##core#inline "C_i_set_car" x y))
(define (set-cdr! x y) (##core#inline "C_i_set_cdr" x y))
(define (cadr x) (##core#inline "C_i_cadr" x))
(define (caddr x) (##core#inline "C_i_caddr" x))
(define (cadddr x) (##core#inline "C_i_cadddr" x))
(define (cddddr x) (##core#inline "C_i_cddddr" x))

(define (caar x) (car (car x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (##core#inline "C_i_cadr" x)))
(define (cadar x) (##core#inline "C_i_cadr" (car x)))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (##core#inline "C_i_cadr" x)))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (##core#inline "C_i_cadr" x))))
(define (caadar x) (car (##core#inline "C_i_cadr" (car x))))
(define (caaddr x) (car (##core#inline "C_i_caddr" x)))
(define (cadaar x) (##core#inline "C_i_cadr" (car (car x))))
(define (cadadr x) (##core#inline "C_i_cadr" (##core#inline "C_i_cadr" x)))
(define (caddar x) (##core#inline "C_i_caddr" (car x)))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (##core#inline "C_i_cadr" x))))
(define (cdadar x) (cdr (##core#inline "C_i_cadr" (car x))))
(define (cdaddr x) (cdr (##core#inline "C_i_caddr" x)))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (##core#inline "C_i_cadr" x))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))

(define (null? x) (eq? x '()))
(define (list . lst) lst)
(define (length lst) (##core#inline "C_i_length" lst))
(define (list-tail lst i) (##core#inline "C_i_list_tail" lst i))
(define (list-ref lst i) (##core#inline "C_i_list_ref" lst i))

(define (##sys#delq x lst)
  (let loop ([lst lst])
    (cond ((null? lst) lst)
	  ((eq? x (##sys#slot lst 0)) (##sys#slot lst 1))
	  (else (cons (##sys#slot lst 0) (loop (##sys#slot lst 1)))) ) ) )

(define ##sys#not-a-proper-list-error
  (lambda (arg . loc)
    (##sys#signal-hook #:type-error (if (pair? loc) (car loc) #f) "argument is not a proper list" arg) ) )

(define append
  (lambda lsts
    (if (eq? lsts '())
	lsts
	(let loop ((lsts lsts))
	  (if (eq? (##sys#slot lsts 1) '())
	      (##sys#slot lsts 0)
	      (let copy ((node (##sys#slot lsts 0)))
		(cond-expand
		 [unsafe
		  (if (eq? node '()) 
		      (loop (##sys#slot lsts 1))
		      (cons (##sys#slot node 0) (copy (##sys#slot node 1))) ) ]
		 [else
		  (cond ((eq? node '()) (loop (##sys#slot lsts 1)))
			((pair? node)
			 (cons (##sys#slot node 0) (copy (##sys#slot node 1))) )
			(else (##sys#not-a-proper-list-error (##sys#slot lsts 0) 'append)) ) ] ) ) ) ) ) ) )

(define reverse 
  (lambda (lst0)
    (let loop ((lst lst0) (rest '()))
      (cond-expand
       [unsafe
	(if (eq? lst '()) 
	    rest
	    (loop (##sys#slot lst 1) (cons (##sys#slot lst 0) rest))  ) ]
       [else
	(cond ((eq? lst '()) rest)
	      ((pair? lst)
	       (loop (##sys#slot lst 1) (cons (##sys#slot lst 0) rest)) )
	      (else (##sys#not-a-proper-list-error lst0 'reverse)) ) ] ) ) ) )

(define (memq x lst) (##core#inline "C_i_memq" x lst))
(define (memv x lst) (##core#inline "C_i_memv" x lst))
(define (member x lst) (##core#inline "C_i_member" x lst))
(define (assq x lst) (##core#inline "C_i_assq" x lst))
(define (assv x lst) (##core#inline "C_i_assv" x lst))
(define (assoc x lst) (##core#inline "C_i_assoc" x lst))

(define (list? x) (##core#inline "C_i_listp" x))


;;; Strings:

(define (string? x) (##core#inline "C_i_stringp" x))
(define (string-length s) (##core#inline "C_i_string_length" s))
(define (string-ref s i) (##core#inline "C_i_string_ref" s i))
(define (string-set! s i c) (##core#inline "C_i_string_set" s i c))

(define (##sys#make-string size . fill)
  (##sys#allocate-vector
   size #t
   (if (null? fill)
       #\space
       (car fill))
   #f) )

(define (make-string size . fill)
  (##sys#check-exact size 'make-string)
  #+(not unsafe)
  (when (fx< size 0)
    (##sys#signal-hook #:bounds-error 'make-string "size is negative" size))
  (##sys#make-string 
   size 
   (if (null? fill)
       #\space
       (let ((c (car fill)))
	 (begin (##sys#check-char c 'make-string) c) ) ) ) )

(define string->list 
  (lambda (s)
    (##sys#check-string s 'string->list)
    (let ((len (##core#inline "C_block_size" s)))
      (let loop ((i 0))
	(if (fx>= i len)
	    '()
	    (cons (##core#inline "C_subchar" s i)
		  (loop (fx+ i 1)) ) ) ) ) ) )

(define (list->string lst0)
  (cond-expand
    [unsafe
    (let* ([len (length lst0)]
           [s (##sys#make-string len)] )
      (do ([i 0 (fx+ i 1)]
           [lst lst0 (##sys#slot lst 1)] )
        ((fx>= i len) s)
        (##core#inline "C_setsubchar" s i (##sys#slot lst 0)) ) )]
    [else
    (if (not (list? lst0))
      (##sys#not-a-proper-list-error lst0 'list->string)
      (let* ([len (length lst0)]
             [s (##sys#make-string len)] )
        (do ([i 0 (fx+ i 1)]
             [lst lst0 (##sys#slot lst 1)] )
          ((fx>= i len) s)
          (let ([c (##sys#slot lst 0)])
            (##sys#check-char c 'list->string)
            (##core#inline "C_setsubchar" s i c) ) ) ) )]
    ))


(define (string-fill! s c)
  (##sys#check-string s 'string-fill!)
  (##sys#check-char c 'string-fill!)
  (##core#inline "C_set_memory" s c (##sys#size s))
  (##core#undefined) )

(define string-copy
  (lambda (s)
    (##sys#check-string s 'string-copy)
    (let* ([len (##sys#size s)]
	   [s2 (##sys#make-string len)] )
      (##core#inline "C_copy_memory" s2 s len)
      s2) ) )

(define substring
  (lambda (s start . end)
    (##sys#check-string s 'substring)
    (##sys#check-exact start 'substring)
    (let ([end (if (pair? end) 
		   (let ([end (car end)])
		     (##sys#check-exact end 'substring)
		     end) 
		   (##sys#size s) ) ] )
      (cond-expand
       [unsafe (##sys#substring s start end)]
       [else
	(let ([len (##sys#size s)])
	  (if (and (fx<= start end)
		   (fx>= start 0)
		   (fx<= end len) )
	      (##sys#substring s start end)
	      (##sys#signal-hook #:bounds-error 'substring "index out of bounds" start end) ) ) ] ) ) ) )

(define ##sys#substring
  (lambda (s start end)
    (let ([s2 (##sys#make-string (fx- end start))])
      (##core#inline "C_substring_copy" s s2 start end 0)
      s2) ) )

(define (string=? x y)
  (cond-expand [unsafe (##core#inline "C_u_i_string_equal_p" x y)]
	       [else (##core#inline "C_i_string_equal_p" x y)] ) )

(define (string-ci=? x y) (##core#inline "C_i_string_ci_equal_p" x y))

(letrec ((compare 
	  (lambda (s1 s2 loc k)
	    (##sys#check-string s1 loc)
	    (##sys#check-string s2 loc)
	    (let ((len1 (##core#inline "C_block_size" s1))
		  (len2 (##core#inline "C_block_size" s2)) )
	      (k len1 len2
		 (##core#inline "C_string_compare"
			    s1
			    s2
			    (if (fx< len1 len2)
				len1
				len2) ) ) ) ) ) )
  (set! string<? (lambda (s1 s2)
		   (compare 
		    s1 s2 'string<?
		    (lambda (len1 len2 cmp)
		      (or (fx< cmp 0)
			  (and (fx< len1 len2)
			       (eq? cmp 0) ) ) ) ) ) )
  (set! string>? (lambda (s1 s2)
		   (compare 
		    s1 s2 'string>?
		    (lambda (len1 len2 cmp)
		      (or (fx> cmp 0)
			  (and (fx< len2 len1)
			       (eq? cmp 0) ) ) ) ) ) )
  (set! string<=? (lambda (s1 s2)
		    (compare 
		     s1 s2 'string<=?
		     (lambda (len1 len2 cmp)
		       (if (eq? cmp 0)
			   (fx<= len1 len2)
			   (fx< cmp 0) ) ) ) ) )
  (set! string>=? (lambda (s1 s2)
		    (compare 
		     s1 s2 'string>=?
		     (lambda (len1 len2 cmp)
		       (if (eq? cmp 0)
			   (fx>= len1 len2)
			   (fx> cmp 0) ) ) ) ) ) )

(letrec ((compare 
	  (lambda (s1 s2 loc k)
	    (##sys#check-string s1 loc)
	    (##sys#check-string s2 loc)
	    (let ((len1 (##core#inline "C_block_size" s1))
		  (len2 (##core#inline "C_block_size" s2)) )
	      (k len1 len2
		 (##core#inline "C_string_compare_case_insensitive"
				s1
				s2
				(if (fx< len1 len2)
				    len1
				    len2) ) ) ) ) ) )
  (set! string-ci<? (lambda (s1 s2)
		      (compare 
		       s1 s2 'string-ci<?
		       (lambda (len1 len2 cmp)
			 (or (fx< cmp 0)
			     (and (fx< len1 len2)
				  (eq? cmp 0) ) ) ) ) ) )
  (set! string-ci>? (lambda (s1 s2)
		      (compare 
		       s1 s2 'string-ci>?
		       (lambda (len1 len2 cmp)
			 (or (fx> cmp 0)
			     (and (fx< len2 len1)
				  (eq? cmp 0) ) ) ) ) ) )
  (set! string-ci<=? (lambda (s1 s2)
		       (compare 
			s1 s2 'string-ci<=?
			(lambda (len1 len2 cmp)
			  (if (eq? cmp 0)
			      (fx>= len1 len2)
			      (fx< cmp 0) ) ) ) ) )
  (set! string-ci>=? (lambda (s1 s2)
		       (compare 
			s1 s2 'string-ci>=?
			(lambda (len1 len2 cmp)
			  (if (eq? cmp 0)
			      (fx<= len1 len2)
			      (fx> cmp 0) ) ) ) ) ) )

(define (##sys#string-append x y)
  (let* ([s1 (##sys#size x)]
	 [s2 (##sys#size y)] 
	 [z (##sys#make-string (fx+ s1 s2))] )
    (##core#inline "C_substring_copy" x z 0 s1 0)
    (##core#inline "C_substring_copy" y z 0 s2 s1)
    z) )

(define string-append
  (lambda all
    (let ([snew #f])
      (let loop ([strs all] [n 0])
	(if (eq? strs '())
	    (set! snew (##sys#make-string n))
	    (let ([s (##sys#slot strs 0)])
	      (##sys#check-string s 'string-append)
	      (let ([len (##sys#size s)])
		(loop (##sys#slot strs 1) (fx+ n len))
		(##core#inline "C_substring_copy" s snew 0 len n) ) ) ) )
      snew) ) )

(define string
  (let ([list->string list->string])
    (lambda chars (list->string chars)) ) )

(define (##sys#fragments->string total fs)
  (let ([dest (##sys#make-string total)])
    (let loop ([fs fs] [pos 0])
      (if (null? fs)
	  dest
	  (let* ([f (##sys#slot fs 0)]
		 [flen (##sys#size f)] )
	    (##core#inline "C_substring_copy" f dest 0 flen pos)
	    (loop (##sys#slot fs 1) (fx+ pos flen)) ) ) ) ) )


;;; By Sven Hartrumpf:

(define (##sys#reverse-list->string l)
  (let* ((n (length l))
         (s (##sys#make-string n)))
    (let iter ((l2 l)
               (n2 (fx- n 1)))
      (cond ((fx>= n2 0)
             (##core#inline "C_setsubchar" s n2 (##sys#slot l2 0))
             (iter (##sys#slot l2 1) (fx- n2 1)))))
    s))

(define reverse-list->string ##sys#reverse-list->string)


;;; Numeric routines:

(define (fixnum? x) (##core#inline "C_fixnump" x))
(define (fx+ x y) (##core#inline "C_fixnum_plus" x y))
(define (fx- x y) (##core#inline "C_fixnum_difference" x y))
(define (fx* x y) (##core#inline "C_fixnum_times" x y))
(define (fx= x y) (eq? x y))
(define (fx> x y) (##core#inline "C_fixnum_greaterp" x y))
(define (fx< x y) (##core#inline "C_fixnum_lessp" x y))
(define (fx>= x y) (##core#inline "C_fixnum_greater_or_equal_p" x y))
(define (fx<= x y) (##core#inline "C_fixnum_less_or_equal_p" x y))
(define (fxmin x y) (##core#inline "C_i_fixnum_min" x y))
(define (fxmax x y) (##core#inline "C_i_fixnum_max" x y))
(define (fxneg x) (##core#inline "C_fixnum_negate" x))
(define (fxand x y) (##core#inline "C_fixnum_and" x y))
(define (fxior x y) (##core#inline "C_fixnum_or" x y))
(define (fxxor x y) (##core#inline "C_fixnum_xor" x y))
(define (fxnot x) (##core#inline "C_fixnum_not" x))
(define (fxshl x y) (##core#inline "C_fixnum_shift_left" x y))
(define (fxshr x y) (##core#inline "C_fixnum_shift_right" x y))

(define fx/
  (lambda (x y)
    (cond-expand
     [unsafe (##core#inline "C_fixnum_divide" x y)]
     [else
      (if (eq? y 0)
	  (##sys#signal-hook #:arithmetic-error 'fx/ "division by zero" x y)
	  (##core#inline "C_fixnum_divide" x y) ) ] ) ) )

(define fxmod
  (lambda (x y)
    (cond-expand
     [unsafe (##core#inline "C_fixnum_modulo" x y)]
     [else
      (if (eq? y 0)
	  (##sys#signal-hook #:arithmetic-error 'fxmod "division by zero" x y)
	  (##core#inline "C_fixnum_modulo" x y) ) ] ) ) )

(define (flonum? x) (##core#inline "C_i_flonump" x))

(define (finite? x) 
  (##sys#check-number x 'finite?)
  (##core#inline "C_i_finitep" x) )

(define (fp+ x y) 
  (cond-expand
   (unsafe (##core#inline_allocate ("C_a_i_flonum_plus" 4) x y))
   (else 
    (if (and (flonum? x)
             (flonum? y))
        (##core#inline_allocate ("C_a_i_flonum_plus" 4) x y)
        (##sys#signal-hook #:type-error 'fp+ "not flonums" x y)))))

(define (fp- x y) 
  (cond-expand
   (unsafe (##core#inline_allocate ("C_a_i_flonum_difference" 4) x y))
   (else 
    (if (and (flonum? x)
             (flonum? y))
        (##core#inline_allocate ("C_a_i_flonum_difference" 4) x y)
        (##sys#signal-hook #:type-error 'fp- "not flonums" x y)))))

(define (fp* x y) 
  (cond-expand
   (unsafe (##core#inline_allocate ("C_a_i_flonum_times" 4) x y))
   (else 
    (if (and (flonum? x)
             (flonum? y))
        (##core#inline_allocate ("C_a_i_flonum_times" 4) x y)
        (##sys#signal-hook #:type-error 'fp* "not flonums" x y)))))

(define (fp= x y) 
  (cond-expand
   (unsafe (##core#inline "C_flonum_equalp" x y))
   (else (if (and (flonum? x)
                  (flonum? y))
             (##core#inline "C_flonum_equalp" x y)
             (##sys#signal-hook #:type-error 'fp= "not flonums" x y)))))

(define (fp> x y) 
  (cond-expand
   (unsafe (##core#inline "C_flonum_greaterp" x y))
   (else (if (and (flonum? x)
                  (flonum? y))
             (##core#inline "C_flonum_greaterp" x y)
             (##sys#signal-hook #:type-error 'fp> "not flonums" x y)))))

(define (fp< x y) 
  (cond-expand 
   (unsafe (##core#inline "C_flonum_lessp" x y))
   (else (if (and (flonum? x)
                  (flonum? y))
             (##core#inline "C_flonum_lessp" x y)
             (##sys#signal-hook #:type-error 'fp< "not flonums" x y)))))

(define (fp>= x y) 
  (cond-expand
   (unsafe (##core#inline "C_flonum_greater_or_equal_p" x y))
   (else (if (and (flonum? x)
                  (flonum? y))
             (##core#inline "C_flonum_greater_or_equal_p" x y)
             (##sys#signal-hook #:type-error 'fp>= "not flonums" x y)))))

(define (fp<= x y) 
  (cond-expand
   (unsafe (##core#inline "C_flonum_less_or_equal_p" x y))
   (else (if (and (flonum? x)
                  (flonum? y))
             (##core#inline "C_flonum_less_or_equal_p" x y)
             (##sys#signal-hook #:type-error 'fp<= "not flonums" x y)))))

(define (fpneg x) 
  (cond-expand
   (unsafe (##core#inline_allocate ("C_a_i_flonum_negate" 4) x))
   (else (if (flonum? x)
             (##core#inline_allocate ("C_a_i_flonum_negate" 4) x)
             (##sys#signal-hook #:type-error 'fpneg "not flonums" x)))))

(define (fpmax x y) 
  (cond-expand
   (unsafe (##core#inline "C_i_flonum_max" x y))
   (else (if (and (flonum? x)
                  (flonum? y))
             (##core#inline "C_i_flonum_max" x y)
             (##sys#signal-hook #:type-error 'fpmax "not flonums" x y)))))

(define (fpmin x y) 
  (cond-expand
   (unsafe (##core#inline "C_i_flonum_min" x y))
   (else (if (and (flonum? x)
                  (flonum? y))
             (##core#inline "C_i_flonum_min" x y)
             (##sys#signal-hook #:type-error 'fpmin "not flonums" x y)))))

(define (fp/ x y)
  (cond-expand
   (unsafe (##core#inline_allocate ("C_a_i_flonum_quotient" 4) x y))
   (else (if (and (flonum? x)
                  (flonum? y))
             (##core#inline_allocate ("C_a_i_flonum_quotient" 4) x y)
             (##sys#signal-hook #:type-error 'fp/ "not flonums" x y)))))

(define * (##core#primitive "C_times"))
(define - (##core#primitive "C_minus"))
(define + (##core#primitive "C_plus"))
(define / (##core#primitive "C_divide"))
(define = (##core#primitive "C_nequalp"))
(define > (##core#primitive "C_greaterp"))
(define < (##core#primitive "C_lessp"))
(define >= (##core#primitive "C_greater_or_equal_p"))
(define <= (##core#primitive "C_less_or_equal_p"))

(define add1 (lambda (n) (+ n 1)))
(define sub1 (lambda (n) (- n 1)))

(define ##sys#floor (##core#primitive "C_flonum_floor"))
(define ##sys#ceiling (##core#primitive "C_flonum_ceiling"))
(define ##sys#truncate (##core#primitive "C_flonum_truncate"))
(define ##sys#round (##core#primitive "C_flonum_round"))
(define quotient (##core#primitive "C_quotient"))
(define ##sys#cons-flonum (##core#primitive "C_cons_flonum"))
(define (##sys#number? x) (##core#inline "C_i_numberp" x))
(define number? ##sys#number?)
(define complex? number?)
(define real? number?)
(define rational? number?)
(define ##sys#flonum-fraction (##core#primitive "C_flonum_fraction"))
(define (##sys#integer? x) (##core#inline "C_i_integerp" x))
(define integer? ##sys#integer?)
(define (##sys#exact? x) (##core#inline "C_i_exactp" x))
(define (##sys#inexact? x) (##core#inline "C_i_inexactp" x))
(define exact? ##sys#exact?)
(define inexact? ##sys#inexact?)
(define expt (##core#primitive "C_expt"))
(define (##sys#fits-in-int? n) (##core#inline "C_fits_in_int_p" n))
(define (##sys#fits-in-unsigned-int? n) (##core#inline "C_fits_in_unsigned_int_p" n))
(define (##sys#flonum-in-fixnum-range? n) (##core#inline "C_flonum_in_fixnum_range_p" n))
(define (##sys#double->number n) (##core#inline "C_double_to_number" n))
(define (zero? n) (##core#inline "C_i_zerop" n))
(define (positive? n) (##core#inline "C_i_positivep" n))
(define (negative? n) (##core#inline "C_i_negativep" n))
(define (abs n) (##core#inline_allocate ("C_a_i_abs" 4) n))	; 4 => words-per-flonum

(define (angle n)
  (##sys#check-number n 'angle)
  0)

(define (real-part n)
  (##sys#check-number n 'real-part)
  n)

(define (imag-part n)
  (##sys#check-number n 'real-part)
  0)

(define magnitude abs)

(define (signum n)
  (cond ((> n 0) (if (##sys#exact? n) 1 1.0))
	((< n 0) (if (##sys#exact? n) -1 -1.0))
	(else (if (##sys#exact? n) 0 0.0) ) ) )

(define ##sys#exact->inexact (##core#primitive "C_exact_to_inexact"))
(define exact->inexact ##sys#exact->inexact)
(define (##sys#inexact->exact n) (##core#inline "C_i_inexact_to_exact" n))
(define inexact->exact ##sys#inexact->exact)

(define (floor x)
  (##sys#check-number x 'floor)
  (if (##core#inline "C_fixnump" x) 
      x
      (##sys#floor x) ) )

(define (ceiling x)
  (##sys#check-number x 'ceiling)
  (if (##core#inline "C_fixnump" x) 
      x
      (##sys#ceiling x) ) )

(define (truncate x)
  (##sys#check-number x 'truncate)
  (if (##core#inline "C_fixnump" x) 
      x
      (##sys#truncate x) ) )

(define (round x)
  (##sys#check-number x 'round)
  (if (##core#inline "C_fixnump" x) 
      x
      (##sys#round x) ) )

(define remainder 
  (lambda (x y) (- x (* (quotient x y) y))) )

(define modulo
  (let ([floor floor])
    (lambda (x y)
      (let ((div (/ x y)))
	(- x (* (if (integer? div)
		    div
		    (let* ([fd (floor div)]
			   [fdx (##core#inline "C_quickflonumtruncate" fd)] )
		      (if (= fd fdx)
			  fdx
			  fd) ) )
		y) ) ) ) ) )

(define (even? n) (##core#inline "C_i_evenp" n))
(define (odd? n) (##core#inline "C_i_oddp" n))

(let ([> >]
      [< <] )
  (letrec ([maxmin
	    (lambda (n1 ns pred)
	      (let loop ((nbest n1) (ns ns))
		(if (eq? ns '())
		    nbest
		    (let ([ni (##sys#slot ns 0)])
		      (loop (if (pred ni nbest)
				(if (and (##core#inline "C_blockp" nbest) 
					 (##core#inline "C_flonump" nbest) 
					 (not (##core#inline "C_blockp" ni)) )
				    (exact->inexact ni)
				    ni)
				nbest)
			    (##sys#slot ns 1) ) ) ) ) ) ] )
    (set! max (lambda (n1 . ns) (maxmin n1 ns >)))
    (set! min (lambda (n1 . ns) (maxmin n1 ns <))) ) )

(define (exp n)
  (##core#inline_allocate ("C_a_i_exp" 4) n) )

(define (log n)
  (##core#inline_allocate ("C_a_i_log" 4) n) )

(define (sin n)
  (##core#inline_allocate ("C_a_i_sin" 4) n) )

(define (cos n)
  (##core#inline_allocate ("C_a_i_cos" 4) n) )

(define (tan n)
  (##core#inline_allocate ("C_a_i_tan" 4) n) )

(define (asin n)
  (##core#inline_allocate ("C_a_i_asin" 4) n) )

(define (acos n)
  (##core#inline_allocate ("C_a_i_acos" 4) n) )

(define (sqrt n)
  (##core#inline_allocate ("C_a_i_sqrt" 4) n) )

(define (atan n1 . n2)
  (if (null? n2) 
      (##core#inline_allocate ("C_a_i_atan" 4) n1)
      (let ([n2 (car n2)])
	(##core#inline_allocate ("C_a_i_atan2" 4) n1 n2) ) ) )

(define ##sys#gcd
  (let ((remainder remainder))
    (lambda (x y)
      (let loop ((x x) (y y))
	(if (zero? y)
	    (abs x)
	    (loop y (remainder x y)) ) ) ) ) )

(define (gcd . ns)
  (if (eq? ns '())
      0
      (let loop ([ns ns] [f #t])
	(let ([head (##sys#slot ns 0)]
	      [next (##sys#slot ns 1)] )
	  (cond-expand [unsafe] [else (when f (##sys#check-integer head 'gcd))])
	  (if (null? next)
	      (abs head)
	      (let ([n2 (##sys#slot next 0)])
		(cond-expand [unsafe] [else (##sys#check-integer n2 'gcd)])
		(loop (cons (##sys#gcd head n2) (##sys#slot next 1)) #f) ) ) ) ) ) )

(define (##sys#lcm x y)
  (quotient (* x y) (##sys#gcd x y)) )

(define (lcm . ns)
  (if (null? ns)
      1
      (let loop ([ns ns] [f #t])
	(let ([head (##sys#slot ns 0)]
	      [next (##sys#slot ns 1)] )
	  (cond-expand [unsafe] [else (when f (##sys#check-integer head 'lcm))])
	  (if (null? next)
	      (abs head)
	      (let ([n2 (##sys#slot next 0)])
		(cond-expand [unsafe] [else (##sys#check-integer n2 'lcm)])
		(loop (cons (##sys#lcm head (##sys#slot next 0)) (##sys#slot next 1)) #f) ) ) ) ) ) )

(define ##sys#string->number (##core#primitive "C_string_to_number"))
(define string->number ##sys#string->number)
(define ##sys#number->string (##core#primitive "C_number_to_string"))
(define number->string ##sys#number->string)


;;; Symbols:

(define ##sys#make-symbol (##core#primitive "C_make_symbol"))
(define (symbol? x) (##core#inline "C_i_symbolp" x))
(define ##sys#snafu '##sys#fnord)
(define ##sys#intern-symbol (##core#primitive "C_string_to_symbol"))
(define (##sys#interned-symbol? x) (##core#inline "C_lookup_symbol" x))

(define (##sys#string->symbol str)
  (##sys#check-string str)
  (##sys#intern-symbol str) )

(let ([string-append string-append]
      [string-copy string-copy] )
  (define (split str len)
    (let ([b0 (##sys#byte str 0)])	; we fetch the byte, wether len is 0 or not
      (if (and (fx> len 0) (fx< b0 len) (fx<= b0 namespace-max-id-len))
	  (fx+ b0 1)
	  #f) ) )
  (set! ##sys#symbol->string
    (lambda (s)
      (let* ([str (##sys#slot s 1)]
	     [len (##sys#size str)]
	     [i (split str len)] )
	(if i (##sys#substring str i len) str) ) ) )
  (set! ##sys#symbol->qualified-string 
    (lambda (s)
      (let* ([str (##sys#slot s 1)]
	     [len (##sys#size str)] 
	     [i (split str len)] )
	(if i
	    (string-append "##" (##sys#substring str 1 i) "#" (##sys#substring str i len))
	    str) ) ) )
  (set! ##sys#qualified-symbol-prefix 
    (lambda (s)
      (let* ([str (##sys#slot s 1)]
	     [len (##sys#size str)]
	     [i (split str len)] )
	(and i (##sys#substring str 0 i)) ) ) ) )

(define ##sys#string->qualified-symbol
  (lambda (prefix str)
    (##sys#string->symbol
     (if prefix
	 (##sys#string-append prefix str)
	 str) ) ) )

(define (symbol->string s)
  (##sys#check-symbol s 'symbol->string)
  (##sys#symbol->string s) )

(define string->symbol
  (let ([string-copy string-copy])
    (lambda (str)
      (##sys#check-string str 'string->symbol)
      (##sys#intern-symbol (string-copy str)) ) ) )

(define string->uninterned-symbol
  (let ([string-copy string-copy])
    (lambda (str)
      (##sys#check-string str 'string->uninterned-symbol)
      (##sys#make-symbol (string-copy str)) ) ) )

(define gensym
  (let ([counter -1])
    (lambda str-or-sym
      (let ([err (lambda (prefix) (##sys#signal-hook #:type-error 'gensym "argument is not a string or symbol" prefix))])
	(set! counter (fx+ counter 1))
	(##sys#make-symbol
	 (##sys#string-append
	  (if (eq? str-or-sym '())
	      "g"
	      (let ([prefix (car str-or-sym)])
		(or (and (##core#inline "C_blockp" prefix)
			 (cond [(##core#inline "C_stringp" prefix) prefix]
			       [(##core#inline "C_symbolp" prefix) (##sys#symbol->string prefix)]
			       [else (err prefix)] ) )
		    (err prefix) ) ) )
	  (##sys#number->string counter) ) ) ) ) ) )


;;; Keywords:

(define (keyword? x)
  (and (symbol? x) (fx= 0 (##sys#byte (##sys#slot x 1) 0))) )

(define string->keyword
  (let ([string string] )
    (lambda (s)
      (##sys#check-string s 'string->keyword)
      (##sys#intern-symbol (##sys#string-append (string (integer->char 0)) s)) ) ) )

(define keyword->string
  (let ([keyword? keyword?])
    (lambda (kw)
      (if (keyword? kw)
	  (##sys#symbol->string kw)
	  (##sys#signal-hook #:type-error 'keyword->string "bad argument type - not a keyword" kw) ) ) ) )

(define ##sys#get-keyword 
  (lambda (key args0 . default)
    (##sys#check-list args0 'get-keyword)
    (let ([a (memq key args0)])
      (if a
	  (let ([r (##sys#slot a 1)])
	    (if (pair? r)
		(##sys#slot r 0)
		(##sys#error 'get-keyword "missing keyword argument" args0 key) ) )
	  (and (pair? default) ((car default))) ) ) ) )

(define get-keyword ##sys#get-keyword)


;;; Vectors:

(define (vector? x) (##core#inline "C_i_vectorp" x))
(define (vector-length v) (##core#inline "C_i_vector_length" v))
(define (vector-ref v i) (##core#inline "C_i_vector_ref" v i))
(define (vector-set! v i x) (##core#inline "C_i_vector_set" v i x))

(define (##sys#make-vector size . fill)
  (##sys#check-exact size 'make-vector)
  (cond-expand [unsafe] [else (when (fx< size 0) (##sys#error 'make-vector "size is negative" size))])
  (##sys#allocate-vector
   size #f
   (if (null? fill)
       (##core#undefined)
       (car fill) )
   #f) )

(define make-vector ##sys#make-vector)

(define (list->vector lst0)
  (cond-expand
    [unsafe
    (let* ([len (length lst0)]
           [v (##sys#make-vector len)] )
      (let loop ([lst lst0]
                 [i 0])
        (if (null? lst)
          v
          (begin
            (##sys#setslot v i (##sys#slot lst 0))
            (loop (##sys#slot lst 1) (fx+ i 1)) ) ) ) )]
    [else
    (if (not (list? lst0))
      (##sys#not-a-proper-list-error lst0 'list->vector)
      (let* ([len (length lst0)]
             [v (##sys#make-vector len)] )
        (let loop ([lst lst0]
                   [i 0])
          (if (null? lst)
            v
            (begin
              (##sys#setslot v i (##sys#slot lst 0))
              (loop (##sys#slot lst 1) (fx+ i 1)) ) ) ) ) )]
    ))


(define vector->list
  (lambda (v)
    (##sys#check-vector v 'vector->list)
    (let ((len (##core#inline "C_block_size" v)))
      (let loop ((i 0))
	(if (fx>= i len)
	    '()
	    (cons (##sys#slot v i)
		  (loop (fx+ i 1)) ) ) ) ) ) )

(define vector
  (lambda xs (##sys#list->vector xs)) )

(define (vector-fill! v x)
  (##sys#check-vector v 'vector-fill!)
  (let ((len (##core#inline "C_block_size" v)))
    (do ((i 0 (fx+ i 1)))
	((fx>= i len))
      (##sys#setslot v i x) ) ) )

(define vector-copy!
  (lambda (from to . n)
    (##sys#check-vector from 'vector-copy!)
    (##sys#check-vector to 'vector-copy!)
    (let* ([len-from (##sys#size from)]
	   [len-to (##sys#size to)] 
	   [n (if (pair? n) (car n) (fxmin len-to len-from))] )
      (##sys#check-exact n 'vector-copy!)
      (cond-expand
       [(not unsafe)
	(when (or (fx> n len-to) (fx> n len-from))
	  (##sys#signal-hook 
	   #:bounds-error 'vector-copy!
	   "can not copy vector - count exceeds length" from to n) ) ]
       [else] )
      (do ([i 0 (fx+ i 1)])
	  ((fx>= i n))
	(##sys#setslot to i (##sys#slot from i)) ) ) ) )

(define (vector-resize v n #!optional init)
  (##sys#check-vector v 'vector-resize)
  (##sys#check-exact n 'vector-resize)
  (##sys#grow-vector v n init) )

(define ##sys#grow-vector 
  (lambda (v n init)
    (let ([v2 (##sys#make-vector n init)]
	  [len (##sys#size v)] )
      (do ([i 0 (fx+ i 1)])
	  ((fx>= i len) v2)
	(##sys#setslot v2 i (##sys#slot v i)) ) ) ) )
	

;;; Characters:

(define (char? x) (##core#inline "C_charp" x))

(define (char->integer c)
  (##sys#check-char c 'char->integer)
  (##core#inline "C_fix" (##core#inline "C_character_code" c)) )

(define (integer->char n)
  (##sys#check-exact n 'integer->char)
  (##core#inline "C_make_character" (##core#inline "C_unfix" n)) )

(define (char=? c1 c2)
  (##sys#check-char c1 'char=?)
  (##sys#check-char c2 'char=?)
  (eq? c1 c2) )

(define (char>? c1 c2)
  (##sys#check-char c1 'char>?)
  (##sys#check-char c2 'char>?)
  (fx> c1 c2) )

(define (char<? c1 c2)
  (##sys#check-char c1 'char<?)
  (##sys#check-char c2 'char<?)
  (fx< c1 c2) )

(define (char>=? c1 c2)
  (##sys#check-char c1 'char>=?)
  (##sys#check-char c2 'char>=?)
  (fx>= c1 c2) )

(define (char<=? c1 c2)
  (##sys#check-char c1 'char<=?)
  (##sys#check-char c2 'char<=?)
  (fx<= c1 c2) )

(define (char-upcase c)
  (##sys#check-char c 'char-upcase)
  (##core#inline "C_make_character"
	     (##core#inline toupper (##core#inline "C_character_code" c)) ) )

(define (char-downcase c)
  (##sys#check-char c 'char-downcase)
  (##core#inline "C_make_character"
	     (##core#inline tolower (##core#inline "C_character_code" c)) ) )

(let ((char-downcase char-downcase))
  (set! char-ci=? (lambda (x y) (eq? (char-downcase x) (char-downcase y))))
  (set! char-ci>? (lambda (x y) (fx> (char-downcase x) (char-downcase y))))
  (set! char-ci<? (lambda (x y) (fx< (char-downcase x) (char-downcase y))))
  (set! char-ci>=? (lambda (x y) (fx>= (char-downcase x) (char-downcase y))))
  (set! char-ci<=? (lambda (x y) (fx<= (char-downcase x) (char-downcase y)))) )

(define (char-upper-case? c)
  (##sys#check-char c 'char-upper-case?)
  (##core#inline "C_u_i_char_upper_casep" c) )

(define (char-lower-case? c)
  (##sys#check-char c 'char-lower-case?)
  (##core#inline "C_u_i_char_lower_casep" c) )

(define (char-numeric? c)
  (##sys#check-char c 'char-numeric?)
  (##core#inline "C_u_i_char_numericp" c) )

(define (char-whitespace? c)
  (##sys#check-char c 'char-whitespace?)
  (##core#inline "C_u_i_char_whitespacep" c) )

(define (char-alphabetic? c)
  (##sys#check-char c 'char-alphabetic?)
  (##core#inline "C_u_i_char_alphabeticp" c) )

(define char-name
  (let ([chars-to-names (make-vector char-name-table-size '())]
	[names-to-chars '()] )
    (define (lookup-char c)
      (let* ([code (char->integer c)]
	     [key (##core#inline "C_fixnum_modulo" code char-name-table-size)] )
	(let loop ([b (##sys#slot chars-to-names key)])
	  (and (pair? b)
	       (let ([a (##sys#slot b 0)])
		 (if (eq? (##sys#slot a 0) c)
		     a
		     (loop (##sys#slot b 1)) ) ) ) ) ) )
    (lambda (x . y)
      (let ([chr (if (pair? y) (car y) #f)])
	(cond [(char? x)
	       (and-let* ([a (lookup-char x)])
		 (##sys#slot a 1) ) ]
	      [chr
	       (##sys#check-symbol x 'char-name)
	       (##sys#check-char chr 'char-name)
	       (let ([a (lookup-char chr)])
		 (if a 
		     (let ([b (assq x names-to-chars)])
		       (##sys#setslot a 1 x)
		       (if b
			   (##sys#setislot b 1 chr)
			   (set! names-to-chars (cons (cons x chr) names-to-chars)) ) )
		     (let ([key (##core#inline "C_fixnum_modulo" (char->integer chr) char-name-table-size)])
		       (set! names-to-chars (cons (cons x chr) names-to-chars))
		       (##sys#setslot 
			chars-to-names key
			(cons (cons chr x) (##sys#slot chars-to-names key))) ) ) ) ]
	      [else
	       (##sys#check-symbol x 'char-name)
	       (and-let* ([a (assq x names-to-chars)])
		 (##sys#slot a 1) ) ] ) ) ) ) )

(char-name 'space #\space)
(char-name 'tab #\tab)
(char-name 'linefeed #\linefeed)
(char-name 'newline #\newline)
(char-name 'vtab (integer->char 11))
(char-name 'delete (integer->char 127))
(char-name 'esc (integer->char 27))
(char-name 'alarm (integer->char 7))
(char-name 'nul (integer->char 0))
(char-name 'return #\return)
(char-name 'page (integer->char 12))
(char-name 'backspace (integer->char 8))


;;; Procedures:

(define (procedure? x) (##core#inline "C_i_closurep" x))
(define apply (##core#primitive "C_apply"))
(define ##sys#call-with-current-continuation (##core#primitive "C_call_cc"))
(define (##sys#call-with-direct-continuation k) (##core#app k (##core#inline "C_direct_continuation" #f)))
(define ##sys#call-with-cthulhu (##core#primitive "C_call_with_cthulhu"))
(define (##sys#direct-return dk x) (##core#inline "C_direct_return" dk x))
(define values (##core#primitive "C_values"))
(define ##sys#call-with-values (##core#primitive "C_call_with_values"))
(define call-with-values ##sys#call-with-values)

(define (##sys#for-each p lst0)
  (let loop ((lst lst0))
    (cond-expand
     [unsafe
      (if (eq? lst '()) 
	  (##core#undefined)
	  (begin
	    (p (##sys#slot lst 0))
	    (loop (##sys#slot lst 1)) ) ) ]
     [else
      (cond ((eq? lst '()) (##core#undefined))
	    ((pair? lst)
	     (p (##sys#slot lst 0))
	     (loop (##sys#slot lst 1)) )
	    (else (##sys#not-a-proper-list-error lst0 'for-each)) ) ] ) ) )

(define (##sys#map p lst0)
  (let loop ((lst lst0))
    (cond-expand
     [unsafe
      (if (eq? lst '()) 
	  lst
	  (cons (p (##sys#slot lst 0)) (loop (##sys#slot lst 1))) ) ]
     [else
      (cond ((eq? lst '()) lst)
	    ((pair? lst)
	     (cons (p (##sys#slot lst 0)) (loop (##sys#slot lst 1))) )
	    (else (##sys#not-a-proper-list-error lst0 'map)) ) ] ) ) )

(let ([car car]
      [cdr cdr] )
  (letrec ((mapsafe
	    (lambda (p lsts start loc)
	      (if (eq? lsts '())
		  lsts
		  (let ((item (##sys#slot lsts 0)))
		    (cond ((eq? item '())
			   (cond-expand [unsafe (##core#undefined)]
					[else (check lsts start loc)] ) )
			  ((pair? item)
			   (cons (p item) (mapsafe p (##sys#slot lsts 1) #f loc)) )
			  (else (##sys#not-a-proper-list-error item loc)) ) ) ) ) )
	   (check 
	    (lambda (lsts start loc)
	      (if (or (not start)
		      (let loop ((lsts lsts))
			(and (not (eq? lsts '()))
			     (not (eq? (##sys#slot lsts 0) '()))
			     (loop (##sys#slot lsts 1)) ) ) )
		  (##sys#error loc "lists are not of same length" lsts) ) ) ) )
    (set! for-each
	  (lambda (fn lst1 . lsts)
	    (if (null? lsts)
		(##sys#for-each fn lst1)
		(let loop ((all (cons lst1 lsts)))
		  (let ((first (##sys#slot all 0)))
		    (cond ((pair? first)
			   (apply fn (mapsafe car all #t 'for-each))
			   (loop (mapsafe cdr all #t 'for-each)) )
			  (else (check all #t 'for-each)) ) ) ) ) ) )
    (set! map
	  (lambda (fn lst1 . lsts)
	    (if (null? lsts)
		(##sys#map fn lst1)
		(let loop ((all (cons lst1 lsts)))
		  (let ((first (##sys#slot all 0)))
		    (cond ((pair? first)
			   (cons (apply fn (mapsafe car all #t 'map))
				 (loop (mapsafe cdr all #t 'map)) ) )
			  (else (check (##core#inline "C_i_cdr" all) #t 'map)
				'() ) ) ) ) ) ) ) ) )


;;; dynamic-wind:
;
; (taken more or less directly from SLIB)
;
; This implementation is relatively costly: we have to shadow call/cc
; with a new version that unwinds suspended thunks, but for this to
; happen the return-values of the escaping procedure have to be saved
; temporarily in a list. Since call/cc is very efficient under this
; implementation, and because allocation of memory that is to be
; garbage soon has also quite low overhead, the performance-penalty
; might be acceptable (ctak needs about 4 times longer).

(define ##sys#dynamic-winds '())

(define dynamic-wind
  (lambda (before thunk after)
    (before)
    (set! ##sys#dynamic-winds (cons (cons before after) ##sys#dynamic-winds))
    (##sys#call-with-values
     thunk
     (lambda results
       (set! ##sys#dynamic-winds (##sys#slot ##sys#dynamic-winds 1))
       (after)
       (apply ##sys#values results) ) ) ) )

(define ##sys#dynamic-wind dynamic-wind)

(define call-with-current-continuation
  (lambda (proc)
    (let ((winds ##sys#dynamic-winds))
      (##sys#call-with-current-continuation
       (lambda (cont)
	 (proc
	  (lambda results
	    (unless (eq? ##sys#dynamic-winds winds)
	      (##sys#dynamic-unwind winds (fx- (length ##sys#dynamic-winds) (length winds))) )
	    (apply cont results) ) ) ) ) ) ) )

(define call/cc call-with-current-continuation)

(define (##sys#dynamic-unwind winds n)
  (cond [(eq? ##sys#dynamic-winds winds)]
	[(fx< n 0)
	 (##sys#dynamic-unwind (##sys#slot winds 1) (fx+ n 1))
	 ((##sys#slot (##sys#slot winds 0) 0))
	 (set! ##sys#dynamic-winds winds) ]
	[else
	 (let ([after (##sys#slot (##sys#slot ##sys#dynamic-winds 0) 1)])
	   (set! ##sys#dynamic-winds (##sys#slot ##sys#dynamic-winds 1))
	   (after)
	   (##sys#dynamic-unwind winds (fx- n 1)) ) ] ) )

(define (continuation-capture proc)
  (let ([winds ##sys#dynamic-winds]
	[k (##core#inline "C_direct_continuation" #f)] )
    (proc (##sys#make-structure 'continuation k winds))) )

(define (continuation? x)
  (##sys#structure? x 'continuation) )

(define ##sys#continuation-graft (##core#primitive "C_continuation_graft"))

(define (continuation-graft k thunk)
  (##sys#check-structure k 'continuation 'continuation-graft)
  (let ([winds (##sys#slot k 2)])
    (unless (eq? ##sys#dynamic-winds winds)
      (##sys#dynamic-unwind winds (fx- (length ##sys#dynamic-winds) (length winds))) )
    (##sys#continuation-graft k thunk) ) )

(define continuation-return
  (let ([continuation-graft continuation-graft])
    (lambda (k . vals)
      (##sys#check-structure k 'continuation 'continuation-return)
      (continuation-graft k (lambda () (apply values vals))) ) ) )


;;; Ports:

(define (port? x) (##core#inline "C_i_portp" x))

(define (input-port? x)
  (and (##core#inline "C_blockp" x)
       (##core#inline "C_portp" x)
       (##sys#slot x 1) ) )

(define (output-port? x)
  (and (##core#inline "C_blockp" x)
       (##core#inline "C_portp" x)
       (not (##sys#slot x 1)) ) )

;;; Port layout:
;
; 0:  FP (special)
; 1:  input/output (bool)
; 2:  class (vector of procedures)
; 3:  name (string)
; 4:  row (fixnum)
; 5:  col (fixnum)
; 6:  EOF (bool)
; 7:  type ('stream | 'custom | 'string | 'socket)
; 8:  closed (bool)
; 9:  data
; 10-15: reserved
;
; Port-class:
;
; 0:  (read-char PORT) -> CHAR | EOF
; 1:  (peek-char PORT) -> CHAR | EOF
; 2:  (write-char PORT CHAR)
; 3:  (write-string PORT STRING)
; 4:  (close PORT)
; 5:  (flush-output PORT)
; 6:  (char-ready? PORT) -> BOOL
; 7:  (read-string! PORT STRING COUNT START) -> COUNT'
; 8:  (read-line PORT LIMIT) -> STRING | EOF

(define (##sys#make-port i/o class name type)
  (let ([port (##core#inline_allocate ("C_a_i_port" 17))])
    (##sys#setislot port 1 i/o)
    (##sys#setslot port 2 class)
    (##sys#setslot port 3 name)
    (##sys#setislot port 4 1)
    (##sys#setislot port 5 0)
    (##sys#setslot port 7 type)
    port) )

(define ##sys#stream-port-class
  (vector (lambda (p)			; read-char
	    (##core#inline "C_read_char" p) )
	  (lambda (p)			; peek-char
	    (##core#inline "C_peek_char" p) )
	  (lambda (p c)			; write-char
	    (##core#inline "C_display_char" p c) )
	  (lambda (p s)			; write-string
	    (##core#inline "C_display_string" p s) )
	  (lambda (p)	    		; close
	    (##core#inline "C_close_file" p)
	    (##sys#update-errno) )
	  (lambda (p)			; flush-output
	    (##core#inline "C_flush_output" p) )
	  (lambda (p)			; char-ready?
	    (##core#inline "C_char_ready_p" p) )
	  #f				; read-string!
	  (lambda (p limit)		; read-line
	    (let ((buffer-len (if limit limit 256))
		  (buffer (make-string buffer-len)))
	      (let loop ([len buffer-len]
			 [buffer buffer]
			 [result ""]
			 [f #f])
		(let ([n (##core#inline "fast_read_line_from_file" buffer p len)])
		  (cond [(eof-object? n) (if f result #!eof)]
			[(and limit (not n))
			 (##sys#string-append result (##sys#substring buffer 0 limit))]
			[(not n)
			 (loop (fx* len 2) (##sys#make-string (fx* len 2))
			       (##sys#string-append 
				result
				(##sys#substring buffer 0 len))
			       #t) ]
			[f (##sys#setislot p 4 (fx+ (##sys#slot p 4) 1))
			   (##sys#string-append result (##sys#substring buffer 0 n))]
			[else
			 (##sys#setislot p 4 (fx+ (##sys#slot p 4) 1))
			 (##sys#substring buffer 0 n)] ) ) ) ) ) ) )

(define ##sys#open-file-port (##core#primitive "C_open_file_port"))

(define ##sys#standard-input (##sys#make-port #t ##sys#stream-port-class "(stdin)" 'stream))
(define ##sys#standard-output (##sys#make-port #f ##sys#stream-port-class "(stdout)" 'stream))
(define ##sys#standard-error (##sys#make-port #f ##sys#stream-port-class "(stderr)" 'stream))

(##sys#open-file-port ##sys#standard-input 0 #f)
(##sys#open-file-port ##sys#standard-output 1 #f)
(##sys#open-file-port ##sys#standard-error 2 #f)

(define ##sys#check-port
  (lambda (x . loc)
    (if (or (not (##core#inline "C_blockp" x))
	    (not (##core#inline "C_portp" x)) )
	(##sys#signal-hook #:type-error (if (pair? loc) (car loc) #f) "argument is not a port" x) ) ) )

(define ##sys#check-port-mode
  (lambda (port mode . loc)
    (unless (eq? mode (##sys#slot port 1))
      (##sys#signal-hook 
       #:type-error (if (pair? loc) (car loc) #f)
       (if mode "port is not an input port" "port is not an output-port") ) ) ) )

(define (##sys#fetch-and-check-port-arg parg default)
  (let ((p (if (eq? parg '())
	       default
	       (##sys#slot parg 0) ) ) )
    (##sys#check-port p)
    (when (##sys#slot p 8)
      (##sys#signal-hook #:file-error "port already closed" p) )
    p) )

(define (current-input-port . arg)
  (if (pair? arg)
      (let ([p (car arg)])
	(##sys#check-port p 'current-input-port)
	(set! ##sys#standard-input p) )
      ##sys#standard-input) )

(define (current-output-port . arg)
  (if (pair? arg)
      (let ([p (car arg)])
	(##sys#check-port p 'current-output-port)
	(set! ##sys#standard-output p) )
      ##sys#standard-output) )

(define (current-error-port . arg)
  (if (pair? arg)
      (let ([p (car arg)])
	(##sys#check-port p 'current-error-port)
	(set! ##sys#standard-error p) )
      ##sys#standard-error) )

(define (##sys#tty-port? port)
  (and (not (zero? (##sys#peek-unsigned-integer port 0)))
       (##core#inline "C_tty_portp" port) ) )

(define (##sys#port-data port) (##sys#slot port 9))

(define (##sys#pathname-resolution name thunk . _)
  (thunk (##sys#expand-home-path name)) )

(define ##sys#expand-home-path
  (let ((getenv getenv))
    (lambda (path)
      (let ((len (##sys#size path)))
	(if (fx> len 0)
	    (case (##core#inline "C_subchar" path 0)
	      ((#\~) 
	       (let ((rest (##sys#substring path 1 len)))
		 (if (and (fx> len 1) (char=? #\/ (##core#inline "C_subchar" path 1)))
		     (##sys#string-append (or (getenv "HOME") "") rest)
		     (##sys#string-append "/home/" rest) ) ) )
	      ((#\$) 
	       (let loop ((i 1))
		 (if (fx>= i len)
		     path
		     (let ((c (##core#inline "C_subchar" path i)))
		       (if (or (eq? c #\/) (eq? c #\\))
			   (##sys#string-append
			    (or (getenv (##sys#substring path 1 i)) "")
			    (##sys#substring path i len))
			   (loop (fx+ i 1)) ) ) ) ) )
	      (else path) )
	    "") ) ) ) )

(let ()
  (define (open name inp modes loc)
    (##sys#check-string name loc)
    (##sys#pathname-resolution
     name
     (lambda (name)
       (let ([fmode (if inp "r" "w")]
	     [bmode ""] )
	 (do ([modes modes (##sys#slot modes 1)])
	     ((null? modes))
	   (let ([o (##sys#slot modes 0)])
	     (case o
	       [(#:binary) (set! bmode "b")]
	       [(#:text) (set! bmode "")]
	       [(#:append) 
		(if inp
		    (##sys#error loc "can not use append mode with input file")
		    (set! fmode "a") ) ]
	       [else (##sys#error loc "invalid file option" o)] ) ) )
	 (let ([port (##sys#make-port inp ##sys#stream-port-class name 'stream)])
	   (unless (##sys#open-file-port port name (##sys#string-append fmode bmode))
	     (##sys#update-errno)
	     (##sys#signal-hook #:file-error loc (##sys#string-append "can not open file - " strerror) name) )
	   port) ) )
     #:open (not inp) modes) )
  (define (close port loc)
    (##sys#check-port port loc)
    (unless (##sys#slot port 8)		; closed?
      ((##sys#slot (##sys#slot port 2) 4) port) ; close
      (##sys#setislot port 8 #t) )
    (##core#undefined) )
  (set! open-input-file (lambda (name . mode) (open name #t mode 'open-input-file)))
  (set! open-output-file (lambda (name . mode) (open name #f mode 'open-output-file)))
  (set! close-input-port (lambda (port) (close port 'close-input-port)))
  (set! close-output-port (lambda (port) (close port 'close-output-port))) )

(define call-with-input-file
  (let ([open-input-file open-input-file]
	[close-input-port close-input-port] )
    (lambda (name p . mode)
      (let ([f (apply open-input-file name mode)])
	(##sys#call-with-values
	 (lambda () (p f))
	 (lambda results
	   (close-input-port f)
	   (apply ##sys#values results) ) ) ) ) ) )

(define call-with-output-file
  (let ([open-output-file open-output-file]
	[close-output-port close-output-port] )
    (lambda (name p . mode)
      (let ([f (apply open-output-file name mode)])
	(##sys#call-with-values
	 (lambda () (p f))
	 (lambda results
	   (close-output-port f)
	   (apply ##sys#values results) ) ) ) ) ) )

(define with-input-from-file 
  (let ((open-input-file open-input-file)
	(close-input-port close-input-port) )
    (lambda (str thunk . mode)
      (let ((old ##sys#standard-input)
	    (file (apply open-input-file str mode)) )
	(set! ##sys#standard-input file)
	(##sys#call-with-values thunk
	  (lambda results
	    (close-input-port file)
	    (set! ##sys#standard-input old)
	    (apply ##sys#values results) ) ) ) ) ) )

(define with-output-to-file 
  (let ((open-output-file open-output-file)
	(close-output-port close-output-port) ) 
    (lambda (str thunk . mode)
      (let ((old ##sys#standard-output)
	    (file (apply open-output-file str mode)) )
	(set! ##sys#standard-output file)
	(##sys#call-with-values thunk
	  (lambda results
	    (close-output-port file)
	    (set! ##sys#standard-output old)
	    (apply ##sys#values results) ) ) ) ) ) )

(define (file-exists? name)
  (##sys#check-string name 'file-exists?)
  (##sys#pathname-resolution
   name
   (lambda (name) (and (##sys#file-info name) name))
   #:exists?) )

(define (##sys#flush-output port)
  ((##sys#slot (##sys#slot port 2) 5) port) ; flush-output
  (##core#undefined) )

(define (flush-output . port)
  (let ([port (##sys#fetch-and-check-port-arg port ##sys#standard-output)])
    (##sys#check-port-mode port #f 'flush-output)
    (##sys#flush-output port) ) )

(define port-name
  (lambda (port)
    (##sys#check-port port 'port-name)
    (##sys#slot port 3) ) )

(define (set-port-name! port name)
  (##sys#check-port port 'set-port-name!)
  (##sys#check-string name 'set-port-name!)
  (##sys#setslot port 3 name) )

(define (##sys#port-line port)
  (and (##sys#slot port 1) 
       (##sys#slot port 4) ) )

(define port-position
  (lambda (#!optional port ##sys#standard-input)
    (##sys#check-port port 'port-position)
    (if (##sys#slot port 1) 
	(##sys#values (##sys#slot port 4) (##sys#slot port 5))
	(##sys#error 'port-position "can not compute position of port" port) ) ) )

(define delete-file
  (lambda (filename)
    (##sys#check-string filename 'delete-file)
    (##sys#pathname-resolution
     filename
     (lambda (filename)
       (unless (eq? 0 (##core#inline "C_delete_file" (##sys#make-c-string filename)))
	 (##sys#update-errno)
	 (##sys#signal-hook #:file-error 'delete-file (##sys#string-append "can not delete file - " strerror) filename) ) )
     #:delete) ) )

(define rename-file
  (lambda (old new)
    (##sys#check-string old 'rename-file)
    (##sys#check-string new 'rename-file)
    (##sys#pathname-resolution
     old
     (lambda (old)
       (##sys#pathname-resolution
	new
	(lambda (new)
	  (unless (eq? 0 (##core#inline "C_rename_file" (##sys#make-c-string old) (##sys#make-c-string new)))
	    (##sys#update-errno)
	    (##sys#signal-hook #:file-error 'rename-file (##sys#string-append "can not rename file - " strerror) old new) ) ) ) )
     #:rename new) ) )


;;; Parameters:

(define ##sys#default-parameter-vector (##sys#make-vector default-parameter-vector-size))
(define ##sys#current-parameter-vector '#())

(define make-parameter
  (let ([count 0])
    (lambda (init . guard)
      (let* ([guard (if (pair? guard) (car guard) (lambda (x) x))]
	     [val (guard init)] 
	     [i count] )
	(set! count (fx+ count 1))
	(when (fx>= i (##sys#size ##sys#default-parameter-vector))
	  (set! ##sys#default-parameter-vector 
	    (##sys#grow-vector ##sys#default-parameter-vector (fx+ i 1) (##core#undefined)) ) )
	(##sys#setslot ##sys#default-parameter-vector i val)
	(lambda arg
	  (let ([n (##sys#size ##sys#current-parameter-vector)])
	    (cond [(pair? arg)
		   (when (fx>= i n)
		     (set! ##sys#current-parameter-vector
		       (##sys#grow-vector ##sys#current-parameter-vector (fx+ i 1) ##sys#snafu) ) )
		   (##sys#setslot ##sys#current-parameter-vector i (guard (##sys#slot arg 0)))
		   (##core#undefined) ]
		  [(fx>= i n)
		   (##sys#slot ##sys#default-parameter-vector i) ]
		  [else
		   (let ([val (##sys#slot ##sys#current-parameter-vector i)])
		     (if (eq? val ##sys#snafu)
			 (##sys#slot ##sys#default-parameter-vector i) 
			 val) ) ] ) ) ) ) ) ) )


;;; Input:

(define (eof-object? x) (##core#inline "C_eofp" x))

(define (char-ready? . port)
  (let ([port (##sys#fetch-and-check-port-arg port ##sys#standard-input)])
    (##sys#check-port-mode port #t 'char-ready?)
    ((##sys#slot (##sys#slot port 2) 6) port) ) ) ; char-ready?

(define (read-char . port)
  (let ([p (##sys#fetch-and-check-port-arg port ##sys#standard-input)])
    (##sys#check-port-mode p #t 'read-char)
    (##sys#read-char-0 p) ) )

(define (##sys#read-char-0 p)
  (let ([c (if (##sys#slot p 6)
	       (begin
		 (##sys#setislot p 6 #f)
		 #!eof)
	       ((##sys#slot (##sys#slot p 2) 0) p) ) ] ) ; read-char
    (cond [(eq? c #\newline)
	   (##sys#setislot p 4 (fx+ (##sys#slot p 4) 1))
	   (##sys#setislot p 5 0) ]
	  [(not (##core#inline "C_eofp" c))
	   (##sys#setislot p 5 (fx+ (##sys#slot p 5) 1)) ] )
    c) )

(define (peek-char . port)
  (let ([p (##sys#fetch-and-check-port-arg port ##sys#standard-input)])
    (##sys#check-port-mode p #t 'peek-char)
    (##sys#peek-char-0 p) ) )

(define (##sys#peek-char-0 p)
  (if (##sys#slot p 6)
      #!eof
      (let ([c ((##sys#slot (##sys#slot p 2) 1) p)]) ; peek-char
	(when (##core#inline "C_eofp" c)
	  (##sys#setislot p 6 #t) )
	c) ) )

(define (read . port)
  (let ([port (##sys#fetch-and-check-port-arg port ##sys#standard-input)])
    (##sys#check-port-mode port #t 'read)
    (##sys#read port ##sys#default-read-info-hook) ) )

(define ##sys#default-read-info-hook #f)
(define ##sys#read-error-with-line-number #f)
(define ##sys#current-namespace #f)
(define ##sys#default-namespace-prefix #f)
(define ##sys#enable-qualifiers #t)
(define (##sys#read-prompt-hook) #f)	; just here so that srfi-18 works without eval
(define (##sys#infix-list-hook lst) lst)

(define (##sys#sharp-number-hook port n)
  (##sys#read-error port "invalid parameterized read syntax" n) )

(define case-sensitive (make-parameter #t))
(define keyword-style (make-parameter #:suffix))
(define current-read-table (make-parameter (##sys#make-structure 'read-table #f #f #f)))

(define ##sys#read-warning
  (let ([string-append string-append])
    (lambda (port msg . args)
      (apply
       ##sys#warn
       (let ((ln (##sys#port-line port)))
	 (if (and ##sys#read-error-with-line-number ln)
	     (string-append msg " in line " (##sys#number->string ln))
	     msg) )
       args) ) ) )

(define ##sys#read-error
  (let ([string-append string-append] )
    (lambda (port msg . args)
      (apply
       ##sys#signal-hook
       #:syntax-error
       (let ((ln (##sys#port-line port)))
	 (if (and ##sys#read-error-with-line-number ln)
	     (string-append msg " in line " (##sys#number->string ln))
	     msg) )
       args) ) ) )

(define ##sys#read
  (let ([reverse reverse]
	[list? list?]
	[string-append string-append]
	[string string]
	[char-name char-name]
	[csp case-sensitive]
	[ksp keyword-style]
	[crt current-read-table]
	[kwprefix (string (integer->char 0))] )
    (lambda (port infohandler)
      (let ([terminating-characters '(#\, #\; #\( #\) #\[ #\] #\{ #\} #\' #\")]
	    [csp (csp)]
	    [ksp (ksp)]
	    [crt (crt)]
	    [rat-flag #f] )

	(define (container c)
	  (##sys#read-error port "unexpected list terminator" c))

	(define (info class data val)
	  (if infohandler
	      (infohandler class data val)
	      data) )

	(define (skip-to-eol)
	  (let skip ((c (##sys#read-char-0 port)))
	    (if (and (not (##core#inline "C_eofp" c)) (not (eq? #\newline c)))
		(skip (##sys#read-char-0 port)) ) ) )

        (define (readrec)

          (define (r-spaces)
            (let loop ([c (##sys#peek-char-0 port)])
	      (cond ((##core#inline "C_eofp" c))
		    ((eq? #\; c) 
		     (skip-to-eol)
		     (loop (##sys#peek-char-0 port)) )
		    ((char-whitespace? c)
		     (##sys#read-char-0 port)
		     (loop (##sys#peek-char-0 port)) ) ) ) )

          (define (r-usequence u n)
	    (let loop ([seq '()] [n n])
	      (if (eq? n 0)
                (let* ([str (##sys#reverse-list->string seq)]
                       [n (string->number str 16)])
                  (or n
                      (##sys#read-error port (string-append "invalid escape-sequence '\\" u str "\'")) ) )
                (let ([x (##sys#read-char-0 port)])
                  (if (or (eof-object? x) (char=? #\" x))
                    (##sys#read-error port "unterminated string constant") 
                    (loop (cons x seq) (fx- n 1)) ) ) ) ) )

          (define (r-cons-codepoint cp lst)
            (let* ((s (##sys#char->utf8-string (integer->char cp)))
                   (len (##sys#size s)))
              (let lp ((i 0) (lst lst))
                (if (fx>= i len)
                  lst
                  (lp (fx+ i 1) (cons (##core#inline "C_subchar" s i) lst))))))

          (define (r-string term)
            (if (eq? (##sys#read-char-0 port) term)
		(let loop ((c (##sys#read-char-0 port)) (lst '()))
		  (cond ((##core#inline "C_eofp" c) 
			 (##sys#read-error port "unterminated string") )
			((eq? #\\ c)
			 (set! c (##sys#read-char-0 port))
			 (case c
			   ((#\t) (loop (##sys#read-char-0 port) (cons #\tab lst)))
			   ((#\r) (loop (##sys#read-char-0 port) (cons #\return lst)))
			   ((#\b) (loop (##sys#read-char-0 port) (cons #\backspace lst)))
			   ((#\n) (loop (##sys#read-char-0 port) (cons #\newline lst)))
			   ((#\a) (loop (##sys#read-char-0 port) (cons (integer->char 7) lst)))
			   ((#\v) (loop (##sys#read-char-0 port) (cons (integer->char 11) lst)))
			   ((#\f) (loop (##sys#read-char-0 port) (cons (integer->char 12) lst)))
			   ((#\x) 
			    (let ([ch (integer->char (r-usequence "x" 2))])
			      (loop (##sys#read-char-0 port) (cons ch lst)) ) )
			   ((#\u)
			    (let ([n (r-usequence "u" 4)])
			      (if (##sys#unicode-surrogate? n)
                                  (if (and (eqv? #\\ (##sys#read-char-0 port))
                                           (eqv? #\u (##sys#read-char-0 port)))
                                      (let* ((m (r-usequence "u" 4))
                                             (cp (##sys#surrogates->codepoint n m)))
                                        (if cp
                                            (loop (##sys#read-char-0 port)
                                                  (r-cons-codepoint cp lst))
                                            (##sys#read-error port "bad surrogate pair" n m)))
                                      (##sys#read-error port "unpaired escaped surrogate" n))
                                  (loop (##sys#read-char-0 port) (r-cons-codepoint n lst)) ) ))
			   ((#\U)
			    (let ([n (r-usequence "U" 8)])
			      (if (##sys#unicode-surrogate? n)
                                  (##sys#read-error port (string-append "invalid escape (surrogate)" n))
                                  (loop (##sys#read-char-0 port) (r-cons-codepoint n lst)) )))
			   ((#\\ #\' #\")
			    (loop (##sys#read-char-0 port) (cons c lst)))
			   (else
			    (##sys#read-warning 
			     port 
			     "undefined escape sequence in string - probably forgot backslash ?"
			     c)
			    (loop (##sys#read-char-0 port) (cons c lst))) ) )
			((eq? term c) (##sys#reverse-list->string lst))
			(else (loop (##sys#read-char-0 port) (cons c lst))) ) )
		(##sys#read-error port (string-append "missing `" (string term) "'")) ) )
                    
	  (define (r-list start end)
	    (if (eq? (##sys#read-char-0 port) start)
		(let ([first #f]
		      [ln0 #f]
		      [outer-container container] )
		  (##sys#call-with-current-continuation
		   (lambda (return)
		     (set! container
		       (lambda (c)
			 (if (eq? c end)
			     (return #f)
			     (##sys#read-error port "list-terminator mismatch" c end) ) ) )
		     (let loop ([last '()])
		       (r-spaces)
		       (unless first (set! ln0 (##sys#port-line port)))
		       (let ([c (##sys#peek-char-0 port)])
			 (cond ((##core#inline "C_eofp" c)
				(##sys#read-error port "unterminated list") )
			       ((eq? c end)
				(##sys#read-char-0 port) )
			       ((eq? c #\.)
				(##sys#read-char-0 port)
				(let ([c2 (##sys#peek-char-0 port)])
				  (cond [(or (char-whitespace? c2)
					     (eq? c2 #\()
					     (eq? c2 #\))
					     (eq? c2 #\")
					     (eq? c2 #\;) )
					 (unless (pair? last)
					   (##sys#read-error port "invalid use of `.'") )
					 (r-spaces)
					 (##sys#setslot last 1 (readrec))
					 (r-spaces)
					 (unless (eq? (##sys#read-char-0 port) end)
					   (##sys#read-error port "missing list terminator" end) ) ]
					[else
					 (let* ((tok (##sys#string-append "." (r-token)))
						(n (and (char-numeric? c2) 
							(##sys#string->number tok)))
						(val (or n (resolve-symbol tok))) 
						(node (cons val '())) )
					   (if first 
					       (##sys#setslot last 1 node)
					       (set! first node) )
					   (loop node) ) ] ) ) )
			       (else
				(let ([node (cons (readrec) '())])
				  (if first
				      (##sys#setslot last 1 node)
				      (set! first node) )
				  (loop node) ) ) ) ) ) ) )
		  (set! container outer-container)
		  (if first
		      (info 'list-info (##sys#infix-list-hook first) ln0)
		      '() ) )
		(##sys#read-error port "missing token" start) ) )
          
	  (define (r-vector)
	    (let ([lst (r-list #\( #\))])
	      (if (list? lst)
		  (##sys#list->vector lst)
		  (##sys#read-error port "invalid vector syntax" lst) ) ) )
          
	  (define (r-number radix)
	    (set! rat-flag #f)
	    (let ([tok (r-token)])
	      (if (string=? tok ".")
		  (##sys#read-error port "invalid use of `.'")
		  (let ([val (##sys#string->number tok (or radix 10))] )
		    (cond [val
			   (when (and (##sys#inexact? val) rat-flag)
			     (##sys#read-warning port "can not represent exact fraction - coerced to flonum" tok) )
			   val]
			  [radix (##sys#read-error port "illegal number syntax" tok)]
			  [else (resolve-symbol tok)] ) ) ) ) )

	  (define (r-number-with-exactness radix)
	    (cond [(char=? #\# (##sys#peek-char-0 port))
		   (##sys#read-char-0 port)
		   (let ([c2 (##sys#read-char-0 port)])
		     (cond [(eof-object? c2) (##sys#read-error port "unexpected end of numeric literal")]
			   [(char=? c2 #\i) (##sys#exact->inexact (r-number radix))]
			   [(char=? c2 #\e) (##sys#inexact->exact (r-number radix))]
			   [else (##sys#read-error port "illegal number syntax - invalid exactness prefix" c2)] ) ) ]
		  [else (r-number radix)] ) )
          
	  (define (r-number-with-radix)
	    (cond [(char=? #\# (##sys#peek-char-0 port))
		   (##sys#read-char-0 port)
		   (let ([c2 (##sys#read-char-0 port)])
		     (cond [(eof-object? c2) (##sys#read-error port "unexpected end of numeric literal")]
			   [(char=? c2 #\x) (r-number 16)]
			   [(char=? c2 #\o) (r-number 8)]
			   [(char=? c2 #\b) (r-number 2)]
			   [else (##sys#read-error port "illegal number syntax - invalid radix" c2)] ) ) ]
		  [else (r-number 10)] ) )
        
	  (define (r-token)
	    (let loop ([c (##sys#peek-char-0 port)] [lst '()])
	      (cond [(or (eof-object? c)
			 (char-whitespace? c)
			 (memq c terminating-characters) )
		     (##sys#reverse-list->string lst) ]
		    [else
		     (when (char=? c #\/) (set! rat-flag #t))
		     (##sys#read-char-0 port)
		     (loop (##sys#peek-char-0 port) 
		       (cons (if csp
				 c
				 (char-downcase c) )
			     lst) ) ] ) ) )

	  (define (r-digits)
	    (let loop ((c (##sys#peek-char-0 port)) (lst '()))
	      (cond ((or (eof-object? c) (not (char-numeric? c)))
		     (##sys#reverse-list->string lst) )
		    (else
		     (##sys#read-char-0 port)
		     (loop (##sys#peek-char-0 port) (cons c lst)) ) ) ) )

	  (define (r-next-token)
	    (r-spaces)
	    (r-token) )
          
	  (define (r-symbol)
	    (let ((s (resolve-symbol
		      (if (char=? (##sys#peek-char-0 port) #\|)
			  (r-xtoken)
			  (r-token) ) ) ) )
	      (info 'symbol-info s (##sys#port-line port)) ) )

	  (define (r-xtoken)
	    (if (char=? #\| (##sys#read-char-0 port))
		(let loop ((c (##sys#read-char-0 port)) (lst '()))
		  (cond ((eof-object? c) (##sys#read-error port "unexpected end of `| ... |' symbol"))
			((char=? c #\\)
			 (let ((c (##sys#read-char-0 port)))
			   (loop (##sys#read-char-0 port) (cons c lst)) ) )
			((char=? c #\|)
			 (##sys#reverse-list->string lst) )
			(else (loop (##sys#read-char-0 port) (cons c lst))) ) )
		(##sys#read-error port "missing \'|\'") ) )
          
	  (define (r-char)
	    (let* ([c (##sys#peek-char-0 port)]
		   [tk (r-token)] )
	      (cond [(char-name (##sys#intern-symbol tk))]
		    [(fx> (string-length tk) 1) 
		     (cond [(and (or (char=? #\x c) (char=? #\u c) (char=? #\U c))
				 (string->number (##sys#substring tk 1 (##sys#size tk)) 16) )
			    => (lambda (n) (integer->char n)) ]
			   [else (##sys#read-error port "unknown named character" tk)] ) ]
		    [(memq c terminating-characters) (##sys#read-char-0 port)]
		    [else c] ) ) )

	  (define (r-comment)
	    (let loop ((i 0))
	      (let ((c (##sys#read-char-0 port)))
		(case c
		  ((#\|) (if (eq? #\# (##sys#read-char-0 port))
			     (if (not (eq? i 0))
				 (loop (fx- i 1)) )
			     (loop i) ) )
		  ((#\#) (loop (if (eq? #\| (##sys#read-char-0 port))
				   (fx+ i 1)
				   i) ) )
		  (else (if (eof-object? c)
			    (##sys#read-error port "unterminated block-comment")
			    (loop i) ) ) ) ) ) )

	  (define (r-namespace)
	    (set! ##sys#current-namespace (##sys#make-vector namespace-size '()))
	    (let* ([ns (r-next-token)]
		   [nslen (##sys#size ns)]
		   [p (##sys#make-string 1)] )
	      (when (fx> nslen namespace-max-id-len)
		(set! ns (##sys#substring ns 0 namespace-max-id-len))
		(set! nslen namespace-max-id-len) )
	      (##sys#setbyte p 0 (##sys#size ns))
	      (let ([prefix (##sys#string-append p ns)])
		(let loop ([toks '()])
		  (r-spaces)
		  (cond [(memq (##sys#peek-char-0 port) '(#\} #\$)) ;*** change this to (char=? #\$ (##sys#peek-char-0 port)) later
			 (##sys#read-char-0 port)
			 (for-each
			  (lambda (tok)
			    (let ([i (##core#inline
				      "C_fixnum_modulo"
				      (##core#inline "C_hash_string" tok) namespace-size)])
			      (##sys#setslot 
			       ##sys#current-namespace i
			       (cons (cons tok (##sys#intern-symbol (##sys#string-append prefix tok)))
				     (##sys#slot ##sys#current-namespace i) ) ) ) )
			  toks) ]
			[else (loop (cons (r-next-token) toks))] ) ) ) ) )

	  (define (r-ext-symbol)
	    (let* ([p (##sys#make-string 1)]
		   [tok (r-token)] 
		   [toklen (##sys#size tok)] )
	      (unless ##sys#enable-qualifiers 
		(##sys#read-error port "qualified symbol syntax is not allowed" tok) )
	      (let loop ([i 0])
		(cond [(fx>= i toklen) (##sys#read-error port "invalid qualified symbol syntax" tok)]
		      [(fx= (##sys#byte tok i) (char->integer #\#))
		       (when (fx> i namespace-max-id-len)
			 (set! tok (##sys#substring tok 0 namespace-max-id-len)) )
		       (##sys#setbyte p 0 i)
		       (##sys#intern-symbol
			(string-append p (##sys#substring tok 0 i) (##sys#substring tok (fx+ i 1) toklen)) ) ]
		      [else (loop (fx+ i 1))] ) ) ) )

	  (define (resolve-symbol tok)
	    (let ([len (##sys#size tok)])
	      (cond [(and (fx> len 1)
			  (or (and (eq? ksp #:prefix)
				   (char=? #\: (##core#inline "C_subchar" tok 0)) 
				   (##sys#substring tok 1 len) )
			      (and (eq? ksp #:suffix) 
				   (char=? #\: (##core#inline "C_subchar" tok (fx- len 1)))
				   (##sys#substring tok 0 (fx- len 1)) ) ) )
		     => build-keyword]	; ugh
		    [(not ##sys#current-namespace) (build-symbol tok)]
		    [else
		     (let ([i (##core#inline "C_fixnum_modulo" (##core#inline "C_hash_string" tok) namespace-size)])
		       (let loop ([bucket (##sys#slot ##sys#current-namespace i)])
			 (if (null? bucket)
			     (build-symbol tok)
			     (let ([e (##sys#slot bucket 0)])
			       (if (string=? tok (##sys#slot e 0))
				   (##sys#slot e 1)
				   (loop (##sys#slot bucket 1)) ) ) ) ) ) ] ) ) )

	  (define (build-symbol tok)
	    (##sys#intern-symbol
	     (if ##sys#default-namespace-prefix
		 (##sys#string-append ##sys#default-namespace-prefix tok)
		 tok) ) )
	  
	  (define (build-keyword tok)
	    (##sys#intern-symbol (##sys#string-append kwprefix tok)) )

	  (r-spaces)
	  (let* ([c (##sys#peek-char-0 port)]
		 [srst (##sys#slot crt 1)]
		 [h (and srst (##sys#slot srst (char->integer c)) ) ] )
	    (if h
		(h c port)
		(case c
		  ((#\')
		   (##sys#read-char-0 port)
		   (list 'quote (readrec)) )
		  ((#\`)
		   (##sys#read-char-0 port)
		   (list 'quasiquote (readrec)) )
		  ((#\,)
		   (##sys#read-char-0 port)
		   (cond ((eq? (##sys#peek-char-0 port) #\@)
			  (##sys#read-char-0 port)
			  (list 'unquote-splicing (readrec)) )
			 (else (list 'unquote (readrec))) ) )
		  ((#\#)
		   (##sys#read-char-0 port)
		   (let ((dchar (##sys#peek-char-0 port)))
		     (if (char-numeric? dchar)
			 (let* ((n (string->number (r-digits)))
				(dchar (##sys#peek-char-0 port))
				(spdrst (##sys#slot crt 3)) 
				(h (and spdrst (##sys#slot spdrst (char->integer dchar)) ) ) )
			   (cond (h (h dchar port n))
				 ((or (eq? dchar #\)) (char-whitespace? dchar)) (##sys#sharp-number-hook port n))
				 (else (##sys#read-error port "invalid parameterized read syntax" dchar n) ) ) )
			 (let* ((sdrst (##sys#slot crt 2))
				(h (and sdrst (##sys#slot sdrst (char->integer dchar)) ) ) )
			   (if h
			       (h dchar port)
			       (case (char-downcase dchar)
				 ((#\x) (##sys#read-char-0 port) (r-number-with-exactness 16))
				 ((#\o) (##sys#read-char-0 port) (r-number-with-exactness 8))
				 ((#\b) (##sys#read-char-0 port) (r-number-with-exactness 2))
				 ((#\i) (##sys#read-char-0 port) (##sys#exact->inexact (r-number-with-radix)))
				 ((#\e) (##sys#read-char-0 port) (##sys#inexact->exact (r-number-with-radix)))
				 ((#\c)
				  (##sys#read-char-0 port)
				  (let ([c (##sys#read-char-0 port)])
				    (fluid-let ([csp 
						 (cond [(eof-object? c)
							(##sys#read-error port "unexpected end of input while reading `#c...' sequence")]
						       [(eq? c #\i) #f]
						       [(eq? c #\s) #t]
						       [else (##sys#read-error port "invalid case specifier in `#c...' sequence" c)] ) ] )
				      (readrec) ) ) )
				 ((#\() (r-vector))
				 ((#\\) (##sys#read-char-0 port) (r-char))
				 ((#\|)
				  (##sys#read-char-0 port)
				  (r-comment) (readrec) )
				 ((#\{)
				  (##sys#read-char-0 port) 
				  (r-namespace) (readrec) )
				 ((#\#) 
				  (##sys#read-char-0 port)
				  (r-ext-symbol) )
				 ((#\;) 
				  (##sys#read-char-0 port)
				  (readrec) (readrec) )
				 ((#\') 
				  (##sys#read-char-0 port)
				  (list 'syntax (readrec)) )
				 ((#\`) 
				  (##sys#read-char-0 port)
				  (list 'quasisyntax (readrec)) )
				 ((#\$)
				  (##sys#read-char-0 port)
				  (list 'location (readrec)) )
				 ((#\:) 
				  (##sys#read-char-0 port)
				  (build-keyword (r-token)) )
				 ((#\%)
				  (build-symbol (##sys#string-append "#" (r-token))) )
				 ((#\+)
				  (##sys#read-char-0 port)
				  (let ((tst (readrec)))
				    (list 'cond-expand (list tst (readrec)) '(else)) ) )
				 ((#\!)
				  (##sys#read-char-0 port)
				  (let ((c (##sys#peek-char-0 port)))
				    (cond ((or (char-whitespace? c) (char=? #\/ c))
					   (skip-to-eol)
					   (readrec) )
					  (else
					   (let ([tok (r-token)])
					     (cond [(string=? "eof" tok) #!eof]
						   [(member tok '("optional" "rest" "key"))
						    (build-symbol (##sys#string-append "#!" tok)) ]
						   [else 
						    (let ((a (assq (string->symbol tok) read-marks)))
						      (if a
							  ((##sys#slot a 1) port)
							  (##sys#read-error port "invalid `#!' token" tok) ) ) ] ) ) ) ) ) )
				 (else (##sys#user-read-hook dchar port)) ) ) ) ) ) )
		  ((#\() (r-list #\( #\)))
		  ((#\[) 
		   (r-list #\[ #\]) )
		  ((#\) #\]) 
		   (##sys#read-char-0 port)
		   (container c) )
		  ((#\{ #\})
		   (##sys#read-char-0 port)
		   (##sys#read-error port "illegal character" c))
		  ((#\") (r-string #\"))
		  ((#\.) (r-number #f))
		  ((#\- #\+) (r-number #f))
		  (else (cond [(eof-object? c) c]
			      [(char-numeric? c) (r-number #f)]
			      [else (r-symbol)] ) ) ) ) ) )

	(readrec) ) ) ) )


;;; This is taken from Alex Shinn's UTF8 egg:

(define (##sys#char->utf8-string c)
  (let ((i (char->integer c)))
    (cond
      ((fx<= i #x7F) (string c))
      ((fx<= i #x7FF)
       (string (integer->char (fxior #b11000000 (fxshr i 6)))
               (integer->char (fxior #b10000000 (fxand i #b111111)))))
      ((fx<= i #xFFFF)
       (string (integer->char (fxior #b11100000 (fxshr i 12)))
               (integer->char (fxior #b10000000 (fxand (fxshr i 6) #b111111)))
               (integer->char (fxior #b10000000 (fxand i #b111111)))))
      ((fx<= i #x1FFFFF)
       (string (integer->char (fxior #b11110000 (fxshr i 18)))
               (integer->char (fxior #b10000000 (fxand (fxshr i 12) #b111111)))
               (integer->char (fxior #b10000000 (fxand (fxshr i 6) #b111111)))
               (integer->char (fxior #b10000000 (fxand i #b111111)))))
      (else (error "unicode codepoint out of range:" i)))))

(define (##sys#unicode-surrogate? n)
  (and (fx<= #xD800 n) (fx<= n #xDFFF)))

;; returns #f if the inputs are not a valid surrogate pair (hi followed by lo)
(define (##sys#surrogates->codepoint hi lo)
  (and (fx<= #xD800 hi) (fx<= hi #xDBFF)
       (fx<= #xDC00 lo) (fx<= lo #xDFFF)
       (fxior (fxshl (fx+ 1 (fxand (fxshr hi 6) #b11111)) 16)
              (fxior (fxshl (fxand hi #b111111) 10)
                     (fxand lo #b1111111111)))))

;;; Hooks for user-defined read-syntax:
;
; - Redefine this to handle new read-syntaxes. If 'char' doesn't match
;   your character then call the previous handler.
; - Don't forget to read 'char', it's only peeked at this point.

(define ##sys#user-read-hook
  (lambda (char port)
    (case char
      ;; I put it here, so the SRFI-4 unit can intercept '#f...'
      ((#\f #\F) (##sys#read-char-0 port) #f)
      ((#\t #\T) (##sys#read-char-0 port) #t)
      (else (##sys#read-error port "invalid sharp-sign read syntax" char) ) ) ) )


;;; Table for specially handled read-syntax:
;
; - should be either #f or a 256-element vector containing procedures
; - the procedure is called with two arguments, a char (peeked) and a port and should return an expression

(define read-marks '())

(define (##sys#set-read-mark! sym proc)
  (let ((a (assq sym read-marks)))
    (if a
	(##sys#setslot a 1 proc)
	(set! read-marks (cons (cons sym proc) read-marks)) ) ) )

(define set-read-syntax!)
(define set-sharp-read-syntax!)
(define set-parameterized-read-syntax!)

(let ((crt current-read-table))
  (define ((syntax-setter loc slot wrap) chr proc)
    (cond ((symbol? chr) (##sys#set-read-mark! chr proc))
	  (else
	   (let ((crt (crt)))
	     (unless (##sys#slot crt slot)
	       (##sys#setslot crt slot (##sys#make-vector 256 #f)) )
	     (##sys#check-char chr loc)
	     (let ([i (char->integer chr)])
	       (##sys#check-range i 0 256 loc)
	       (##sys#setslot (##sys#slot crt slot) i (wrap proc)) ) ) ) ) )
  (set! set-read-syntax!
    (syntax-setter
     'set-read-syntax! 1 
     (lambda (proc)
       (lambda (_ port) 
	 (##sys#read-char-0 port)
	 (proc port) ) ) ) )
  (set! set-sharp-read-syntax!
    (syntax-setter
     'set-sharp-read-syntax! 2
     (lambda (proc)
       (lambda (_ port) 
	 (##sys#read-char-0 port)
	 (proc port) ) ) ) )
  (set! set-parameterized-read-syntax!
    (syntax-setter
     'set-parameterized-read-syntax! 3
     (lambda (proc)
       (lambda (_ port num)
	 (##sys#read-char-0 port)
	 (proc port num) ) ) ) ) )


;;; Read-table operations:

(define (copy-read-table rt)
  (##sys#check-structure rt 'read-table 'copy-read-table)
  (##sys#make-structure 
   'read-table
   (let ((t1 (##sys#slot rt 1)))
     (and t1 (##sys#grow-vector t1 (##sys#size t1) #f) ) )
   (let ((t2 (##sys#slot rt 2)))
     (and t2 (##sys#grow-vector t2 (##sys#size t2) #f) ) ) ))


;;; Output:

(define (##sys#write-char-0 c p)
  ((##sys#slot (##sys#slot p 2) 2) p c) )

(define (##sys#write-char c . port)
  (##sys#check-char c 'write-char)
  (let ([p (##sys#fetch-and-check-port-arg port ##sys#standard-output)])
    (##sys#check-port-mode p #f 'write-char)
    (##sys#write-char-0 c p) ) )

(define write-char ##sys#write-char)
(define (newline . port) (apply ##sys#write-char #\newline port))

(define (write x . port)
  (##sys#print x #t (##sys#fetch-and-check-port-arg port ##sys#standard-output)) )

(define (display x . port)
  (##sys#print x #f (##sys#fetch-and-check-port-arg port ##sys#standard-output)) )

(define print
  (lambda (arg1 . args)
    (for-each (lambda (x) (##sys#print x #f ##sys#standard-output)) (cons arg1 args))
    (##sys#write-char-0 #\newline ##sys#standard-output) 
    arg1) )

(define (print* arg1 . args)
  (for-each (lambda (x) (##sys#print x #f ##sys#standard-output)) (cons arg1 args))
  (##sys#flush-output ##sys#standard-output)
  arg1)

(define current-print-length 0)
(define print-length-limit #f)
(define ##sys#print-exit #f)

(define ##sys#print
  (let ([char-name char-name]
	[csp case-sensitive]
	[ksp keyword-style]
	[string-append string-append] )
    (lambda (x readable port)
      (##sys#check-port-mode port #f)
      (let ([csp (csp)]
	    [ksp (ksp)] )

	(define (outstr port str)
	  (if print-length-limit
	      (let* ((len (##sys#size str))
		     (cpl (fx+ current-print-length len)) )
		(if (fx>= cpl print-length-limit)
		    (cond ((fx> len 3)
			   (let ((n (fx- print-length-limit current-print-length)))
			     (when (fx> n 0) (outstr0 port (##sys#substring str 0 n)))
			     (outstr0 port "...") ) )
			  (else (outstr0 port str)) )
		    (outstr0 port str) )
		(set! current-print-length cpl) )
	      (outstr0 port str) ) )
	       
	(define (outstr0 port str)
	  ((##sys#slot (##sys#slot port 2) 3) port str) )

	(define (outchr port chr)
	  (set! current-print-length (fx+ current-print-length 1))
	  (when (and print-length-limit (fx>= current-print-length print-length-limit))
	    (outstr0 port "...")
	    (##sys#print-exit #t) )
	  ((##sys#slot (##sys#slot port 2) 2) port chr) )

	(define (specialchar? chr)
	  (let ([c (char->integer chr)])
	    (or (fx<= c 32)
		(fx>= c 128)
		(memq chr '(#\( #\) #\| #\, #\[ #\] #\{ #\} #\' #\" #\; #\\)) ) ) )

	(define (outreadablesym port str)
	  (let ([len (##sys#size str)])
	    (outchr port #\|)
	    (let loop ([i 0])
	      (if (fx>= i len)
		  (outchr port #\|)
		  (let ([c (##core#inline "C_subchar" str i)])
		    (when (eq? c #\|) (outchr port #\\))
		    (outchr port c)
		    (loop (fx+ i 1)) ) ) ) ) )

	(define (sym-is-readable? str)
	  (let ([len (##sys#size str)])
	    (and (fx> len 0)
		 (if (eq? len 1)
		     (case (##core#inline "C_subchar" str 0)
		       ((#\. #\#) #f)
		       (else #t) ) )
		 (not (##core#inline "C_substring_compare" "#!" str 0 0 2))
		 (let loop ((i (fx- len 1)))
		   (if (eq? i 0)
		       (let ((c (##core#inline "C_subchar" str 0)))
			 (cond ((or (char-numeric? c)
				    (eq? c #\+)
				    (eq? c #\-)
				    (eq? c #\.) )
				(not (##sys#string->number str)) )
			       ((specialchar? c) #f)
			       (else #t) ) )
		       (let ([c (##core#inline "C_subchar" str i)])
			 (and (or csp (not (char-upper-case? c)))
			      (not (specialchar? c))
			      (loop (fx- i 1)) ) ) ) ) ) ) )

	(let out ([x x])
	  (cond ((eq? x '()) (outstr port "()"))
		((eq? x #t) (outstr port "#t"))
		((eq? x #f) (outstr port "#f"))
		((##core#inline "C_eofp" x) (outstr port "#!eof"))
		((##core#inline "C_undefinedp" x) (outstr port "#<unspecified>"))
		((##core#inline "C_charp" x)
		 (cond [readable
			(outstr port "#\\")
			(let ([code (char->integer x)])
			  (cond [(char-name x) 
				 => (lambda (cn) 
				      (outstr port (##sys#slot cn 1)) ) ]
				[(fx< code 32)
				 (outchr port #\x)
				 (outstr port (##sys#number->string code 16)) ]
				[(fx> code 255)
				 (outchr port (if (fx> code #xffff) #\U #\u))
				 (outstr port (##sys#number->string code 16)) ]
				[else (outchr port x)] ) ) ] 
		       [else (outchr port x)] ) )
		((##core#inline "C_fixnump" x) (outstr port (##sys#number->string x)))
		((eq? x (##sys#slot '##sys#arbitrary-unbound-symbol 0))
		 (outstr port "#<unbound value>") )
		((not (##core#inline "C_blockp" x)) (outstr port "#<unprintable object>"))
		((##core#inline "C_symbolp" x)
		 (cond [(fx= 0 (##sys#byte (##sys#slot x 1) 0))
			(let ([str (##sys#symbol->string x)])
			  (case ksp
			    [(#:prefix) 
			     (outchr port #\:)
			     (outstr port str) ]
			    [(#:suffix) 
			     (outstr port str)
			     (outchr port #\:) ]
			    [else
			     (outstr port "#:")
			     (outstr port str) ] ) ) ]
		       [(memq x '(#!optional #!key #!rest)) (outstr port (##sys#slot x 1))]
		       [else
			(let ([str (##sys#symbol->qualified-string x)])
			  (if (or (not readable) (sym-is-readable? str))
			      (outstr port str)
			      (outreadablesym port str) ) ) ] ) )
		((##sys#number? x) (outstr port (##sys#number->string x)))
		((##core#inline "C_anypointerp" x) (outstr port (##sys#pointer->string x)))
		((##core#inline "C_stringp" x)
		 (cond (readable
			(outchr port #\")
			(do ((i 0 (fx+ i 1))
			     (c (##core#inline "C_block_size" x) (fx- c 1)) )
			    ((eq? c 0)
			     (outchr port #\") )
			  (let ((chr (##core#inline "C_subbyte" x i)))
			    (case chr
			      ((34) (outstr port "\\\""))
			      ((92) (outstr port "\\\\"))
			      (else
			       (cond ((fx< chr 32)
				      (outchr port #\\)
				      (case chr
					((9) (outchr port #\t))
					((10) (outchr port #\n))
					((13) (outchr port #\r))
					((11) (outchr port #\v))
					((12) (outchr port #\f))
					((8) (outchr port #\b))
					(else
					 (outchr port #\x)
					 (when (fx< chr 16) (outchr port #\0))
					 (outstr port (##sys#number->string chr 16)) ) ) )
				     (else (outchr port (##core#inline "C_fix_to_char" chr)) ) ) ) ) ) ) )
		       (else (outstr port x)) ) )
		((##core#inline "C_pairp" x)
		 (outchr port #\()
		 (out (##sys#slot x 0))
		 (do ((x (##sys#slot x 1) (##sys#slot x 1)))
		     ((or (not (##core#inline "C_blockp" x)) (not (##core#inline "C_pairp" x)))
		      (if (not (eq? x '()))
			  (begin
			    (outstr port " . ")
			    (out x) ) )
		      (outchr port #\)) )
		   (outchr port #\space)
		   (out (##sys#slot x 0)) ) )
		((##core#inline "C_bytevectorp" x)
		 (if (##core#inline "C_permanentp" x)
		     (outstr port "#<static byte-vector>")
		     (outstr port "#<byte-vector>") ) )
		((##core#inline "C_structurep" x) (##sys#user-print-hook x readable port))
		((##core#inline "C_closurep" x) (outstr port (##sys#procedure->string x)))
 		((##core#inline "C_locativep" x) (outstr port "#<locative>"))
		((##core#inline "C_lambdainfop" x)
		 (outstr port "#<lambda info ")
		 (outstr port (##sys#lambda-info->string x))
		 (outchr port #\>) )
		((##core#inline "C_portp" x)
		 (if (##sys#slot x 1)
		     (outstr port "#<input port ")
		     (outstr port "#<output port ") )
		 (outstr port (##sys#slot x 3))
		 (outchr port #\>) )
		((##core#inline "C_vectorp" x)
		 (let ((n (##core#inline "C_block_size" x)))
		   (cond ((eq? 0 n)
			  (outstr port "#()") )
			 (else
			  (outstr port "#(")
			  (out (##sys#slot x 0))
			  (do ((i 1 (fx+ i 1))
			       (c (fx- n 1) (fx- c 1)) )
			      ((eq? c 0)
			       (outchr port #\)) )
			    (outchr port #\space)
			    (out (##sys#slot x i)) ) ) ) ) )
		(else (##sys#error "unprintable non-immediate object encountered")) ) ) ) ) ) )

(define ##sys#procedure->string 
  (let ((string-append string-append))
    (lambda (x)
      (let ((info (##sys#lambda-info x)))
	(if info
	    (string-append "#<procedure " (##sys#lambda-info->string info) ">")
	    "#<procedure>") ) ) ) )

(define ##sys#record-printers '())

(define (##sys#register-record-printer type proc)
  (let ([a (assq type ##sys#record-printers)])
    (if a 
	(##sys#setslot a 1 proc)
	(set! ##sys#record-printers (cons (cons type proc) ##sys#record-printers)) )
    (##core#undefined) ) )

(define (##sys#user-print-hook x readable port)
  (let* ([type (##sys#slot x 0)]
	 [a (assq type ##sys#record-printers)] )
    (cond [a ((##sys#slot a 1) x port)]
	  [else
	   (##sys#print "#<" #f port)
	   (##sys#print (##sys#symbol->string type) #f port)
	   (case type
	     [(condition)
	      (##sys#print ": " #f port)
	      (##sys#print (##sys#slot x 1) #f port) ]
	     [(thread)
	      (##sys#print ": " #f port)
	      (##sys#print (##sys#slot x 6) #f port) ] )
	   (##sys#print #\> #f port) ] ) ) )

(define ##sys#with-print-length-limit	; this is not the least bit thread safe
  (let ([call-with-current-continuation call-with-current-continuation])
    (lambda (limit thunk)
      (call-with-current-continuation
       (lambda (return)
	 (fluid-let ((print-length-limit limit)
		     (##sys#print-exit return) 
		     (current-print-length 0) )
	   (thunk) ) ) ) ) ) )


;;; Bitwise fixnum operations:

(define (bitwise-and . xs)
  (let loop ([x -1] [xs xs])
    (if (null? xs)
	x
	(loop (##core#inline_allocate ("C_a_i_bitwise_and" 4) x (##sys#slot xs 0))
	      (##sys#slot xs 1)) ) ) )

(define (bitwise-ior . xs)
  (let loop ([x 0] [xs xs])
    (if (null? xs)
	x
	(loop (##core#inline_allocate ("C_a_i_bitwise_ior" 4) x (##sys#slot xs 0)) 
	      (##sys#slot xs 1)) ) ) )

(define (bitwise-xor . xs)
  (let loop ([x 0] [xs xs])
    (if (null? xs)
	x
	(loop (##core#inline_allocate ("C_a_i_bitwise_xor" 4) x (##sys#slot xs 0))
	      (##sys#slot xs 1)) ) ) )

(define (bitwise-not x)
  (##core#inline_allocate ("C_a_i_bitwise_not" 4) x) )

(define (arithmetic-shift x y)
  (##core#inline_allocate ("C_a_i_arithmetic_shift" 4) x y) )

(define (bit-set? n i)
  (##core#inline "C_i_bit_setp" n i) )


;;; String ports:
;
; - Port-slots:
;
;   Input:
;
;   10: position
;   11: len
;   12: string
;
;   Output:
;
;   10: position
;   11: limit
;   12: output

(define ##sys#string-port-class
  (letrec ([check 
	    (lambda (p n)
	      (let* ([position (##sys#slot p 10)]
		     [limit (##sys#slot p 11)] 
		     [output (##sys#slot p 12)]
		     [limit2 (fx+ position n)] )
		(when (fx>= limit2 limit)
		  (when (fx>= limit2 maximal-string-length)
		    (##sys#error "string buffer full" p) )
		  (let* ([limit3 (fxmin maximal-string-length (fx+ limit limit))]
			 [buf (##sys#make-string limit3)] )
		    (##sys#copy-bytes output buf 0 0 position)
		    (##sys#setslot p 12 buf)
		    (##sys#setislot p 11 limit3)
		    (check p n) ) ) ) ) ] )
    (vector
     (lambda (p)			; read-char
       (let ([position (##sys#slot p 10)]
	     [string (##sys#slot p 12)]
	     [len (##sys#slot p 11)] )
	 (if (>= position len)
	     #!eof
	     (let ((c (##core#inline "C_subchar" string position)))
	       (##sys#setislot p 10 (fx+ position 1))
	       c) ) ) )
     (lambda (p)			; peek-char
       (let ([position (##sys#slot p 10)]
	     [string (##sys#slot p 12)]
	     [len (##sys#slot p 11)] )
	 (if (fx>= position len)
	     #!eof
	     (##core#inline "C_subchar" string position) ) ) )
     (lambda (p c)			; write-char
       (check p 1)	
       (let ([position (##sys#slot p 10)]
	     [output (##sys#slot p 12)] )
	 (##core#inline "C_setsubchar" output position c)
	 (##sys#setislot p 10 (fx+ position 1)) ) )
     (lambda (p str)			; write-string
       (let ([len (##core#inline "C_block_size" str)])
	 (check p len)
	 (let ([position (##sys#slot p 10)]
	       [output (##sys#slot p 12)] )
	   (do ((i 0 (fx+ i 1)))
	       ((fx>= i len) (##sys#setislot p 10 position))
	     (##core#inline "C_setsubchar" output position (##core#inline "C_subchar" str i))
	     (set! position (fx+ position 1)) ) ) ) )
     (lambda (p)	    		; close
       (##sys#setislot p 10 (##sys#slot p 11)) )
     (lambda (p) #f)			; flush-output
     (lambda (p)			; char-ready?
       (fx< (##sys#slot p 10) (##sys#slot p 11)) )
     (lambda (p n dest start)		; read-string!
       (let* ((pos (##sys#slot p 10))
	      (n2 (fx- (##sys#slot p 11) pos) ) )
	 (when (or (not n) (fx> n n2)) (set! n n2))
	 (##core#inline "C_substring_copy" (##sys#slot p 12) dest pos (fx+ pos n) start)
	 (##sys#setislot p 10 (fx+ pos n))
	 n))
     (lambda (p limit)			; read-line
       (let ((pos (##sys#slot p 10))
	     (size (##sys#slot p 11)) 
	     (buf (##sys#slot p 12)) 
	     (end (if limit (fx+ pos limit) size)))
	 (if (fx>= pos size)
	     #!eof
	     (##sys#scan-buffer-line
	      buf 
	      (if (fx> end size) size end)
	      pos 
	      (lambda (pos2 next)
		(when (not (eq? pos2 next))
		  (##sys#setislot p 4 (fx+ (##sys#slot p 4) 1)) )
		(let ((dest (##sys#make-string (fx- pos2 pos))))
		  (##core#inline "C_substring_copy" buf dest pos pos2 0)
		  (##sys#setislot p 10 next)
		  dest) ) ) ) ) ) ) ) )

(define (##sys#scan-buffer-line buf limit pos k)
  (let loop ((pos2 pos))
    (if (fx>= pos2 limit)
	(k pos2 pos2)
	(let ((c (##core#inline "C_subchar" buf pos2)))
	  (cond ((eq? c #\newline) (k pos2 (fx+ pos2 1)))
		((and (eq? c #\return) 
		      (fx> limit (fx+ pos2 1))
		      (eq (##core#inline "C_subchar" buf (fx+ pos2 1)) #\newline) )
		 (k pos2 (fx+ pos2 1)) )
		(else (loop (fx+ pos2 1))) ) ) ) ) )

(define open-input-string 
  (lambda (string)
    (##sys#check-string string 'open-input-string)
    (let ([port (##sys#make-port #t ##sys#string-port-class "(string)" 'string)])
      (##sys#setislot port 11 (##core#inline "C_block_size" string))
      (##sys#setislot port 10 0)
      (##sys#setslot port 12 string)
      port) ) )

(define open-output-string
  (lambda ()
    (let ([port (##sys#make-port #f ##sys#string-port-class "(string)" 'string)])
      (##sys#setislot port 10 0)
      (##sys#setislot port 11 output-string-initial-size)
      (##sys#setslot port 12 (##sys#make-string output-string-initial-size))
      port) ) )

(define get-output-string
  (lambda (port)
    (##sys#check-port port 'get-output-string)
    (##sys#check-port-mode port #f 'get-output-string)
    (if (not (eq? 'string (##sys#slot port 7)))
	(##sys#signal-hook #:type-error 'get-output-string "argument is not a string-output-port" port) 
	(##sys#substring (##sys#slot port 12) 0 (##sys#slot port 10)) ) ) )

(define ##sys#print-to-string
  (let ([get-output-string get-output-string]
	[open-output-string open-output-string] )
    (lambda (xs)
      (let ([out (open-output-string)])
	(for-each (lambda (x) (##sys#print x #f out)) xs)
	(get-output-string out) ) ) ) )

(define ##sys#pointer->string
  (let ((string-append string-append))
    (lambda (x)
      (cond ((##core#inline "C_taggedpointerp" x)
	     (string-append 
	      "#<tagged pointer "
	      (##sys#print-to-string 
	       (let ((tag (##sys#slot x 1)))
		 (list (if (pair? tag) (car tag) tag) ) ) )
	      " "
	      (##sys#number->string (##sys#pointer->address x) 16)
	      ">") )
	    ((##core#inline "C_swigpointerp" x)
	     (string-append "#<SWIG pointer 0x" (##sys#number->string (##sys#pointer->address x) 16) ">") )
	    (else
	     (string-append "#<pointer 0x" (##sys#number->string (##sys#pointer->address x) 16) ">") ) ) ) ) )


;;; Platform configuration inquiry:

(define software-type
  (let ([sym (string->symbol ((##core#primitive "C_software_type")))])
    (lambda () sym) ) )

(define machine-type
  (let ([sym (string->symbol ((##core#primitive "C_machine_type")))])
    (lambda () sym) ) )

(define (machine-byte-order)
  (if (foreign-value "*((C_char *)&one_two_three) != 123" bool)
      'big-endian 'little-endian) )

(define software-version
  (let ([sym (string->symbol ((##core#primitive "C_software_version")))])
    (lambda () sym) ) )

(define build-platform
  (let ([sym (string->symbol ((##core#primitive "C_build_platform")))])
    (lambda () sym) ) )

(define (chicken-version . full)
  (define (get-config)
    (let ([bp (build-platform)]
	  [st (software-type)]
	  [sv (software-version)]
	  [mt (machine-type)] )
      (define (str x)
	(if (eq? x 'unknown)
	    ""
	    (string-append (symbol->string x) "-") ) )
      (string-append (str sv) (str st) (str bp) (symbol->string mt)) ) )
  (if (:optional full #f)
      (let ((spec (string-append
		   (if (##sys#fudge 3) " 64bit" "")
		   (if (##sys#fudge 15) " symbolgc" "")
		   (if (##sys#fudge 22) " libffi" "")
		   (if (##sys#fudge 24) " dload" "") 
		   (if (##sys#fudge 28) " ptables" "")
		   (if (##sys#fudge 32) " gchooks" "") 
		   (if (##sys#fudge 33) " extraslot" "")
		   (if (##sys#fudge 35) " applyhook" "") 
		   (if (##sys#fudge 38) " cmake" "")
		   (if (##sys#fudge 39) " cross" "") ) ) )
	(string-append 
	 "Version " +build-version+
	 " - " (get-config)
	 (if (eq? 0 (##sys#size spec)) "" (string-append " - [" spec " ]") ) ))
      +build-version+) )

(define ##sys#pathname-directory-separator
  (let ([st (software-type)])
    (if (and (eq? 'windows st) (not (eq? (build-platform) 'cygwin)))
	#\\
	#\/) ) )

(define c-runtime
  (let ([sym (string->symbol ((##core#primitive "C_c_runtime")))])
    (lambda () sym) ) )


;;; Feature identifiers:

(define ##sys#->feature-id
  (let ([string->keyword string->keyword]
	[keyword? keyword?] )
    (define (err . args)
      (apply ##sys#signal-hook #:type-error "bad argument type - not a valid feature identifer" args) )
    (define (prefix s)
      (if s 
	  (##sys#string-append s "-")
	  "") )
    (lambda (x)
      (cond [(string? x) (string->keyword x)]
	    [(keyword? x) x]
	    [(symbol? x) (string->keyword (##sys#symbol->string x))]
	    [else (err x)] ) ) ) )

(define ##sys#features '(#:chicken #:srfi-23 #:srfi-30 #:srfi-39 #:srfi-62 #:srfi-17 #:srfi-12))

(let ((check (lambda (f)
	       (unless (eq? 'unknown f)
		 (set! ##sys#features (cons (##sys#->feature-id f) ##sys#features))))))
  (check (software-type))
  (check (software-version))
  (check (machine-type))
  (check (machine-byte-order)) )

(when (##sys#fudge 22) (set! ##sys#features (cons #:libffi ##sys#features)))
(when (##sys#fudge 24) (set! ##sys#features (cons #:dload ##sys#features)))
(when (##sys#fudge 28) (set! ##sys#features (cons #:ptables ##sys#features)))
(when (##sys#fudge 33) (set! ##sys#features (cons #:extraslot ##sys#features)))
(when (##sys#fudge 35) (set! ##sys#features (cons #:applyhook ##sys#features)))

(define (register-feature! . fs)
  (for-each
   (lambda (f)
     (let ([id (##sys#->feature-id f)])
       (unless (memq id ##sys#features) (set! ##sys#features (cons id ##sys#features))) ) )
   fs)
  (##core#undefined) )

(define (unregister-feature! . fs)
  (let ([fs (map ##sys#->feature-id fs)])
    (set! ##sys#features
      (let loop ([ffs ##sys#features])
	(if (null? ffs)
	    '()
	    (let ([f (##sys#slot ffs 0)]
		  [r (##sys#slot ffs 1)] )
	      (if (memq f fs)
		  (loop r)
		  (cons f (loop r)) ) ) ) ) )
    (##core#undefined) ) )

(define (features) ##sys#features)

(define (##sys#feature? . ids)
  (let loop ([ids ids])
    (or (null? ids)
	(and (memq (##sys#->feature-id (##sys#slot ids 0)) ##sys#features)
	     (loop (##sys#slot ids 1)) ) ) ) )

(define feature? ##sys#feature?)
(define test-feature? ##sys#feature?)	; DEPRECATED


;;; Access backtrace:

(define ##sys#get-call-chain
  (let ((extract (foreign-lambda* nonnull-c-string ((scheme-object x)) "return((C_char *)x);")))
    (lambda (#!optional (start 0) (thread ##sys#current-thread))
      (let* ((tbl (foreign-value "C_trace_buffer_size" int))
	     (vec (##sys#make-vector (fx* 4 tbl) #f))
	     (r (##core#inline "C_fetch_trace" start vec)) 
	     (n (if (fixnum? r) r (fx* 4 tbl))) )
	(let loop ((i 0))
	  (if (fx>= i n) 
	      '()
	      (let ((t (##sys#slot vec (fx+ i 3))))
		(if (or (not t) (not thread) (eq? thread t))
		    (cons (vector (extract (##sys#slot vec i))
				  (##sys#slot vec (fx+ i 1))
				  (##sys#slot vec (fx+ i 2)) )
			  (loop (fx+ i 4)) )
		    (loop (fx+ i 4))) ) ) ) ) ) ) )

(define (##sys#really-print-call-chain port chain header)
  (when (pair? chain)
    (##sys#print header #f port)
    (for-each
     (lambda (info) 
       (let ((more1 (##sys#slot info 1))
	     (more2 (##sys#slot info 2)) 
	     (t (##sys#slot info 3)))
	 (##sys#print "\n\t" #f port)
	 (##sys#print (##sys#slot info 0) #f port)
	 (##sys#print "\t\t" #f port)
	 (when more2
	   (##sys#write-char-0 #\[ port)
	   (##sys#print more2 #f port)
	   (##sys#print "] " #f port) )
	 (when more1
	   (##sys#with-print-length-limit
	    100
	    (lambda ()
	      (##sys#print more1 #t port) ) ) ) ) )
     chain)
    (##sys#print "\t<--\n" #f port) ) )

(define print-call-chain
  (lambda (#!optional (port ##sys#standard-output) (start 0) (thread ##sys#current-thread)
		      (header "\n\tCall history:\n") )
    (##sys#really-print-call-chain port (##sys#get-call-chain start thread) header) ) )

(define get-call-chain ##sys#get-call-chain)
(define print-backtrace print-call-chain) ; DEPRECATED


;;; Interrupt handling:

(let ([count 0])			; DEPRECATED
  (set! ##sys#enable-interrupts
    (lambda val
      (set! count (fx+ count (if (pair? val) (car val) 1)))
      (when (eq? count 0) (##core#inline "C_enable_interrupts")) ) )
  (set! ##sys#disable-interrupts
    (lambda ()
      (when (eq? count 0) (##core#inline "C_disable_interrupts"))
      (set! count (fx- count 1)) ) ) )

(define enable-interrupts ##sys#enable-interrupts) ; DEPRECATED
(define disable-interrupts ##sys#disable-interrupts) ; DEPRECATED

(define (##sys#user-interrupt-hook)
  (define (break) (##sys#signal-hook #:user-interrupt #f))
  (if (eq? ##sys#current-thread ##sys#primordial-thread)
      (break)
      (##sys#setslot ##sys#primordial-thread 1 break) ) )


;;; Breakpoints

(define ##sys#last-breakpoint #f)
(define ##sys#break-in-thread #f)

(define (##sys#break-entry name args)
  ;; Does _not_ unwind!
  (##sys#call-with-current-continuation
   (lambda (c)
     (let ((exn (##sys#make-structure
		 'condition
		 '(exn breakpoint)
		 (list '(exn . message) "*** breakpoint ***"
		       '(exn . arguments) (list (cons name args))
		       '(exn . location) name
		       '(exn . continuation) c) ) ) )
       (set! ##sys#last-breakpoint exn)
       (##sys#signal exn) ) ) ) )

(define (##sys#break-resume exn)
  (let ((a (member '(exn . continuation) (##sys#slot exn 2))))
    (if a
	((cadr a) (##core#undefined))
	(##sys#signal-hook #:type-error "condition has no continuation" exn) ) ) )

(define (breakpoint #!optional name)
  (##sys#break-entry (or name 'breakpoint) '()) )


;;; Single stepping

(define ##sys#stepped-thread #f)
(define ##sys#step-ports (cons ##sys#standard-input ##sys#standard-output))

(define (##sys#step thunk)
  (when (eq? ##sys#stepped-thread ##sys#current-thread)
    (##sys#call-with-values
     (lambda () 
       (set! ##sys#apply-hook ##sys#step-hook)
       (##core#app thunk) )
     (lambda vals
       (set! ##sys#apply-hook #f)
       (set! ##sys#stepped-thread #f)
       (##sys#apply-values vals) ) ) ) )

(define (singlestep thunk)
  (unless (##sys#fudge 35)
    (##sys#signal-hook #:runtime-error 'singlestep "apply-hook not available") )
  (unless (procedure? thunk)
    (##sys#signal-hook #:type-error "bad argument type - not a procedure" thunk) )
  (set! ##sys#stepped-thread ##sys#current-thread)
  (##sys#step thunk) )

(define (##sys#step-hook . args)
  (set! ##sys#apply-hook #f)
  (let ((o (##sys#slot ##sys#step-ports 1))
	(i (##sys#slot ##sys#step-ports 0))
	(p ##sys#last-applied-procedure))
    (define (skip-to-nl)
      (let ((c (##sys#read-char-0 i)))
	(unless (or (eof-object? c) (char=? #\newline c))
	  (sip-to-nl) ) ) )
    (define (cont)
      (set! ##sys#stepped-thread #f)
      (##sys#apply p args) )
    (##sys#print "\n " #f o)
    (##sys#with-print-length-limit 
     1024
     (lambda () (##sys#print (cons p args) #t o)) )
    (flush-output o)
    (let loop ()
      (##sys#print "\n        step (RETURN), (s)kip, (c)ontinue or (b)reak ? " #f o)
      (let ((c (##sys#read-char-0 i)))
	(if (eof-object? c)
	    (cont)
	    (case c
	      ((#\newline) 
	       (set! ##sys#apply-hook ##sys#step-hook)
	       (##core#app ##sys#apply p args))
	      ((#\return #\tab #\space) (loop))
	      ((#\c) (skip-to-nl) (cont))
	      ((#\s) 
	       (skip-to-nl)
	       (##sys#call-with-values 
		(lambda () (##core#app ##sys#apply p args))
		(lambda results
		  (set! ##sys#apply-hook ##sys#step-hook)
		  (##core#app ##sys#apply-values results) ) ) )
	      ((#\b) 
	       (skip-to-nl)
	       (set! ##sys#stepped-thread #f)
	       (##sys#break-entry '<step> '())
	       (##sys#apply p args) ) 
	      (else
	       (cond ((eof-object? c) (cont))
		     (else 
		      (skip-to-nl) 
		      (loop))))))))))


;;; Default handlers

(define ##sys#break-on-error (##sys#fudge 25))

(define-foreign-variable _ex_software int "EX_SOFTWARE")

(define ##sys#error-handler
  (make-parameter
   (let ([string-append string-append]
	 [open-output-string open-output-string]
	 [get-output-string get-output-string] 
	 [print-call-chain print-call-chain] )
     (lambda (msg . args)
       (##sys#error-handler (lambda args (##core#inline "C_halt" "error in error")))
       (cond ((##sys#fudge 4)
	      (##core#inline "C_display_string" ##sys#standard-error "Error")
	      (when msg
		(##sys#print ": " #f ##sys#standard-error)
		(##sys#print msg #f ##sys#standard-error) )
	      (cond [(fx= 1 (length args))
		     (##core#inline "C_display_string" ##sys#standard-error ": ")
		     (##sys#print (##sys#slot args 0) #t ##sys#standard-error) ]
		    [else
		     (##sys#for-each
		      (lambda (x)
			(##core#inline "C_display_char" ##sys#standard-error #\newline)
			(##sys#print x #t ##sys#standard-error) )
		      args) ] )
	      (##core#inline "C_display_char" ##sys#standard-error #\newline)
	      (print-call-chain ##sys#standard-error)
	      (when (and ##sys#break-on-error (##sys#symbol-has-toplevel-binding? 'repl))
		(repl) 
		(##core#inline "C_display_char" ##sys#standard-error #\newline)
		(##core#inline "C_exit_runtime" _ex_software) )
	      (##core#inline "C_halt" #f) )
	     (else
	      (let ((out (open-output-string)))
		(when msg (##sys#print msg #f out))
		(##sys#print #\newline #f out)
		(##sys#for-each (lambda (x) (##sys#print x #t out) (##sys#print #\newline #f out)) args)
		(##core#inline "C_halt" (get-output-string out)) ) ) ) ) ) ) )

(define reset-handler 
  (make-parameter 
   (lambda ()
     ((##sys#exit-handler) _ex_software)) ) )

(define exit-handler
  (make-parameter
   (lambda code
     (##sys#cleanup-before-exit)
     (##core#inline
      "C_exit_runtime"
      (if (null? code)
	  0
	  (let ([code (car code)])
	    (##sys#check-exact code)
	    code) ) ) ) ) )

(define implicit-exit-handler
  (make-parameter
   (lambda ()
     (##sys#cleanup-before-exit) ) ) )

(define ##sys#exit-handler exit-handler)
(define ##sys#reset-handler reset-handler)
(define ##sys#implicit-exit-handler implicit-exit-handler)

(define force-finalizers (make-parameter #t))

(define ##sys#cleanup-before-exit
  (let ([ffp force-finalizers])
    (lambda ()
      (when (##sys#fudge 13)
	(##sys#print "[debug] forcing finalizers...\n" #f ##sys#standard-output) )
      (when (ffp) (##sys#force-finalizers)) ) ) )

(define (on-exit thunk)
  (set! ##sys#cleanup-before-exit
    (let ((old ##sys#cleanup-before-exit))
      (lambda () (old) (thunk)) ) ) )


;;; Condition handling:

(define ##sys#signal-hook 
  (lambda (mode msg . args)
    (##core#inline "C_dbg_hook" #f)
    (case mode
      [(#:user-interrupt)
       (##sys#abort
	(##sys#make-structure
	 'condition
	 '(user-interrupt) ) ) ]
      [(#:warning)
       (##sys#print "Warning: " #f ##sys#standard-error)
       (##sys#print msg #f ##sys#standard-error)
       (##sys#write-char-0 #\newline ##sys#standard-error)
       (for-each
	(lambda (x)
	  (##sys#print x #t ##sys#standard-error)
	  (##sys#write-char-0 #\newline ##sys#standard-error) )
	args) 
       (##sys#flush-output ##sys#standard-error) ] 
      [else
       (when (and (symbol? msg) (null? args))
	 (set! msg (##sys#symbol->string msg)) )
       (let* ([hasloc (and (or (not msg) (symbol? msg)) (pair? args))]
	      [loc (and hasloc msg)]
	      [msg (if hasloc (##sys#slot args 0) msg)]
	      [args (if hasloc (##sys#slot args 1) args)] )
	 (##sys#abort
	  (##sys#make-structure
	   'condition 
	   (case mode
	     [(#:type-error) '(exn type)]
	     [(#:syntax-error) '(exn syntax)]
	     [(#:match-error) '(exn match)]
	     [(#:bounds-error) '(exn bounds)]
	     [(#:arithmetic-error) '(exn arithmetic)]
	     [(#:file-error) '(exn i/o file)]
	     [(#:runtime-error) '(exn runtime)]
	     [(#:process-error) '(exn process)]
	     [(#:network-error) '(exn i/o net)]
	     [(#:limit-error) '(exn runtime limit)]
	     [(#:arity-error) '(exn arity)]
	     [(#:access-error) '(exn access)]
	     [else '(exn)] )
	   (list '(exn . message) msg
		 '(exn . arguments) args
		 '(exn . location) loc) ) ) ) ] ) ) )

(define (##sys#abort x)
  (##sys#current-exception-handler x)
  (##sys#abort
   (##sys#make-structure
    'condition
    '(exn) 
    (list '(exn . message) "exception handler returned"
	  '(exn . arguments) '()
	  '(exn . location) #f) ) ) )

(define (##sys#signal x)
  (##sys#current-exception-handler x) )

(define abort ##sys#abort)
(define signal ##sys#signal)

(define ##sys#last-exception #f)

(define ##sys#current-exception-handler
  ;; Exception-handler for the primordial thread:
  (let ([string-append string-append])
    (lambda (c)
      (when (##sys#structure? c 'condition)
	(set! ##sys#last-exception c)
	(let ([kinds (##sys#slot c 1)])
	  (cond [(memq 'exn kinds)
		 (let* ([props (##sys#slot c 2)]
			[msga (member '(exn . message) props)]
			[argsa (member '(exn . arguments) props)]
			[loca (member '(exn . location) props)] )
		   (apply
		    (##sys#error-handler)
		    (if msga
			(let ([msg (cadr msga)]
			      [loc (and loca (cadr loca))] )
			  (if (and loc (symbol? loc))
			      (string-append
			       "(" (##sys#symbol->qualified-string loc) ") " 
			       (cond ((symbol? msg) (##sys#slot msg 1))
				     ((string? msg) msg)
				     (else "") ) ) ; Hm...
			      msg) )
			"<exn: has no `message' property>")
		    (if argsa
			(cadr argsa)
			'() ) )
		   ((##sys#reset-handler)) ) ]
		[(eq? 'user-interrupt (##sys#slot kinds 0))
		 (##sys#print "*** user interrupt ***\n" #f ##sys#standard-error)
		 ((##sys#reset-handler)) ] 
		[(eq? 'uncaught-exception (##sys#slot kinds 0))
		 ((##sys#error-handler)
		  "uncaught exception"
		  (cadr (member '(uncaught-exception . reason) (##sys#slot c 2))) )
		 ((##sys#reset-handler)) ] ) ) )
      (##sys#abort
       (##sys#make-structure
	'condition 
	'(uncaught-exception) 
	(list '(uncaught-exception . reason) c)) ) ) ) )

(define (with-exception-handler handler thunk)
  (let ([oldh ##sys#current-exception-handler])
    (##sys#dynamic-wind 
	(lambda () (set! ##sys#current-exception-handler handler))
	thunk
	(lambda () (set! ##sys#current-exception-handler oldh)) ) ) )

(define (current-exception-handler) ##sys#current-exception-handler)

(define (make-property-condition kind . props)
  (##sys#make-structure
   'condition (list kind)
   (let loop ((props props))
     (if (null? props)
	 '()
	 (cons (cons kind (car props)) (cons (cadr props) (loop (cddr props)))) ) ) ) )

(define make-composite-condition
  (lambda (c1 . conds)
    (let ([conds (cons c1 conds)])
      (for-each (lambda (c) (##sys#check-structure c 'condition 'make-composite-condition)) conds)
      (##sys#make-structure
       'condition
       (apply ##sys#append (map (lambda (c) (##sys#slot c 1)) conds))
       (apply ##sys#append (map (lambda (c) (##sys#slot c 2)) conds)) ) ) ) )

(define (condition? x) (##sys#structure? x 'condition))

(define (condition-predicate kind)
  (lambda (c) 
    (##sys#check-structure c 'condition)
    (if (memv kind (##sys#slot c 1)) #t #f) ) )

(define (condition-property-accessor kind prop . err-def)
  (let ((err? (null? err-def))
	(k+p (cons kind prop)) )
    (lambda (c)
      (##sys#check-structure c 'condition)
      (and (memv kind (##sys#slot c 1))
	   (let ([a (member k+p (##sys#slot c 2))])
	     (cond [a (cadr a)]
		   [err? (##sys#signal-hook 
			  #:type-error 'condition-property-accessor
			  "condition has no such property" prop) ]
		   [else (car err-def)] ) ) ) ) ) )


;;; Error hook (called by runtime-system):

(define ##sys#error-hook
  (let ([string-append string-append])
    (lambda (code loc . args)
      (case code
	((1) (let ([c (car args)]
		   [n (cadr args)] 
		   [fn (caddr args)] )
	       (apply
		##sys#signal-hook 
		#:arity-error loc
		(string-append "bad argument count - received " (##sys#number->string n) " but expected "
			       (##sys#number->string c) )
		(if fn (list fn) '())) ) )
	((2) (let ([c (car args)]
		   [n (cadr args)] 
		   [fn (caddr args)] )
	       (apply
		##sys#signal-hook
		#:arity-error loc
		(string-append "too few arguments - received " (##sys#number->string n) " but expected "
			       (##sys#number->string c) )
		(if fn (list fn) '()))))
	((3) (apply ##sys#signal-hook #:type-error loc "bad argument type" args))
	((4) (apply ##sys#error loc "unbound variable" args))
	((5) (apply ##sys#signal-hook #:limit-error loc "parameter limit exceeded" args))
	((6) (apply ##sys#signal-hook #:limit-error loc "out of memory" args))
	((7) (apply ##sys#signal-hook #:arithmetic-error loc "division by zero" args))
	((8) (apply ##sys#signal-hook #:bounds-error loc "out of range" args))
	((9) (apply ##sys#signal-hook #:type-error loc "call of non-procedure" args))
	((10) (apply ##sys#error loc "continuation can not receive multiple values" args))
	((12) (apply ##sys#signal-hook #:limit-error loc "recursion too deep" args))
	((13) (apply ##sys#signal-hook #:type-error loc "inexact number can not be represented as an exact number" args))
	((14) (apply ##sys#signal-hook #:type-error loc "argument is not a proper list" args))
	((15) (apply ##sys#signal-hook #:type-error loc "bad argument type - not a fixnum" args))
	((16) (apply ##sys#signal-hook #:type-error loc "bad argument type - not a number" args))
	((17) (apply ##sys#signal-hook #:type-error loc "bad argument type - not a string" args))
	((18) (apply ##sys#signal-hook #:type-error loc "bad argument type - not a pair" args))
	((19) (apply ##sys#signal-hook #:type-error loc "bad argument type - not a list" args))
	((20) (apply ##sys#signal-hook #:type-error loc "bad argument type - not a character" args))
	((21) (apply ##sys#signal-hook #:type-error loc "bad argument type - not a vector" args))
	((22) (apply ##sys#signal-hook #:type-error loc "bad argument type - not a symbol" args))
	((23) (apply ##sys#signal-hook #:limit-error loc "stack overflow" args))
	((24) (apply ##sys#signal-hook #:type-error loc "bad argument type - not a structure of the required type" args))
	((25) (apply ##sys#signal-hook #:type-error loc "bad argument type - not a bytevector" args))
	((26) (apply ##sys#signal-hook #:type-error loc "locative refers to reclaimed object" args))
	((27) (apply ##sys#signal-hook #:type-error loc "bad argument type - non-immediate value expected" args))
	((28) (apply ##sys#signal-hook #:type-error loc "bad argument type - not a number-vector or not of the correct type" args))
	((29) (apply ##sys#signal-hook #:type-error loc "bad argument type - not an integer" args))
	((30) (apply ##sys#signal-hook #:type-error loc "bad argument type - not an unsigned integer" args))
	((31) (apply ##sys#signal-hook #:type-error loc "bad argument type - not a pointer" args))
	((32) (apply ##sys#signal-hook #:type-error loc "bad argument type - not a tagged pointer or not of the correct type" args))
	((33) (apply ##sys#signal-hook #:runtime-error loc
		     "code to load dynamically was linked with safe runtime libraries, but executing runtime was not"
		     args) )
	((34) (apply ##sys#signal-hook #:runtime-error loc
		     "code to load dynamically was linked with unsafe runtime libraries, but executing runtime was not"
		     args) )
	((35) (apply ##sys#signal-hook #:runtime-error loc "bad argument type - not a floating-point number" args))
	(else (apply ##sys#signal-hook #:runtime-error loc "unknown internal error" args)) ) ) ) )


;;; Miscellaneous low-level routines:

(define (##sys#structure? x s) (##core#inline "C_i_structurep" x s))
(define (##sys#generic-structure? x) (##core#inline "C_structurep" x))
(define (##sys#slot x i) (##core#inline "C_slot" x i))
(define (##sys#size x) (##core#inline "C_block_size" x))
(define ##sys#make-pointer (##core#primitive "C_make_pointer"))
(define ##sys#make-tagged-pointer (##core#primitive "C_make_tagged_pointer"))
(define (##sys#pointer? x) (##core#inline "C_pointerp" x))
(define (##sys#set-pointer-address! ptr addr) (##core#inline "C_update_pointer" addr ptr))
(define (##sys#bytevector? x) (##core#inline "C_bytevectorp" x))
(define (##sys#string->pbytevector s) (##core#inline "C_string_to_pbytevector" s))
(define (##sys#permanent? x) (##core#inline "C_permanentp" x))
(define (##sys#block-address x) (##core#inline_allocate ("C_block_address" 4) x))
(define (##sys#locative? x) (##core#inline "C_locativep" x))

(define (##sys#null-pointer)
  (let ([ptr (##sys#make-pointer)])
    (##core#inline "C_update_pointer" 0 ptr)
    ptr) )

(define (##sys#null-pointer? x)
  (eq? 0 (##sys#pointer->address x)) )

(define (##sys#address->pointer addr)
  (let ([ptr (##sys#make-pointer)])
    (##core#inline "C_update_pointer" addr ptr)
    ptr) )

(define (##sys#pointer->address ptr)
  ;; *** '4' is platform dependent!
  (##core#inline_allocate ("C_a_int_to_num" 4) (##sys#slot ptr 0)) )

(define ##sys#make-c-string 
  (lambda (str)
    (##sys#string-append str (string (##core#inline "C_make_character" (##core#inline "C_unfix" 0)))) ) )

(define ##sys#peek-signed-integer (##core#primitive "C_peek_signed_integer"))
(define ##sys#peek-unsigned-integer (##core#primitive "C_peek_unsigned_integer"))
(define (##sys#peek-fixnum b i) (##core#inline "C_peek_fixnum" b i))
(define (##sys#peek-byte ptr i) (##core#inline "C_peek_byte" ptr i))

(define (##sys#vector->structure! vec) (##core#inline "C_vector_to_structure" vec))

(define (##sys#peek-double b i)
  (##core#inline "C_f64peek" b i)
  (##sys#cons-flonum) )

(define ##sys#peek-c-string
    (lambda (b i)
      (and (not (##sys#null-pointer? b))
	   (let* ([len (##core#inline "C_fetch_c_strlen" b i)]
		  [str2 (##sys#make-string len)] )
	     (##core#inline "C_peek_c_string" b i str2 len)
	     str2) ) ) )

(define ##sys#peek-nonnull-c-string
    (lambda (b i)
      (let* ([len (##core#inline "C_fetch_c_strlen" b i)]
	     [str2 (##sys#make-string len)] )
	(##core#inline "C_peek_c_string" b i str2 len)
	str2) ) )

(define ##sys#peek-and-free-c-string
    (lambda (b i)
      (and (not (##sys#null-pointer? b))
	   (let* ([len (##core#inline "C_fetch_c_strlen" b i)]
		  [str2 (##sys#make-string len)] )
	     (##core#inline "C_peek_c_string" b i str2 len)
	     (##core#inline "C_free_mptr" b i)
	     str2) ) ) )

(define ##sys#peek-and-free-nonnull-c-string
    (lambda (b i)
      (let* ([len (##core#inline "C_fetch_c_strlen" b i)]
	     [str2 (##sys#make-string len)] )
	(##core#inline "C_peek_c_string" b i str2 len)
        (##core#inline "C_free_mptr" b i)
	str2) ) )

(define (##sys#poke-c-string b i s) 
  (##core#inline "C_poke_c_string" b i (##sys#make-c-string s)) )

(define (##sys#poke-integer b i n) (##core#inline "C_poke_integer" b i n))
(define (##sys#poke-double b i n) (##core#inline "C_poke_double" b i n))

(define ##sys#peek-c-string-list 
  (let ((fetch (foreign-lambda c-string "C_peek_c_string_at" c-pointer int)))
    (lambda (ptr n)
      (let loop ((i 0))
	(if (and n (fx>= i n))
	    '()
	    (let ((s (fetch ptr i)))
	      (if s
		  (cons s (loop (fx+ i 1)))
		  '() ) ) ) ) ) ) )

(define (##sys#vector->closure! vec addr)
  (##core#inline "C_vector_to_closure" vec)
  (##core#inline "C_update_pointer" addr vec) )

(define (##sys#symbol-has-toplevel-binding? s)
  (not (eq? (##sys#slot s 0) (##sys#slot '##sys#arbitrary-unbound-symbol 0))) )

(define (##sys#copy-bytes from to offset1 offset2 bytes)
  (##core#inline 
   "C_substring_copy"
   from to
   offset1 (fx+ offset1 bytes)
   offset2) )

(define (##sys#copy-words from to offset1 offset2 words)
  (##core#inline 
   "C_subvector_copy"
   from to
   offset1 (fx+ offset1 words)
   offset2) )

(define (##sys#compare-bytes from to offset1 offset2 bytes)
  (##core#inline 
   "C_substring_compare"
   from to
   offset1 offset2 bytes) )

(define ##sys#zap-strings (foreign-lambda void "C_zap_strings" scheme-object))

(define (##sys#block-pointer x)
  (let ([ptr (##sys#make-pointer)])
    (##core#inline "C_pointer_to_block" ptr x)
    ptr) )


;;; Support routines for foreign-function calling:

(define (##sys#foreign-char-argument x) (##core#inline "C_i_foreign_char_argumentp" x))
(define (##sys#foreign-fixnum-argument x) (##core#inline "C_i_foreign_fixnum_argumentp" x))
(define (##sys#foreign-flonum-argument x) (##core#inline "C_i_foreign_flonum_argumentp" x))
(define (##sys#foreign-block-argument x) (##core#inline "C_i_foreign_block_argumentp" x))
(define (##sys#foreign-number-vector-argument t x) (##core#inline "C_i_foreign_number_vector_argumentp" t x))
(define (##sys#foreign-string-argument x) (##core#inline "C_i_foreign_string_argumentp" x))
(define (##sys#foreign-symbol-argument x) (##core#inline "C_i_foreign_symbol_argumentp" x))
(define (##sys#foreign-pointer-argument x) (##core#inline "C_i_foreign_pointer_argumentp" x))
(define (##sys#foreign-tagged-pointer-argument x tx) (##core#inline "C_i_foreign_tagged_pointer_argumentp" x tx))
(define (##sys#foreign-integer-argument x) (##core#inline "C_i_foreign_integer_argumentp" x))
(define (##sys#foreign-unsigned-integer-argument x) (##core#inline "C_i_foreign_unsigned_integer_argumentp" x))


;;; Low-level threading interface:

(define ##sys#default-thread-quantum 10000)

(define (##sys#default-exception-handler arg) 
  (##core#inline "C_halt" "internal error: default exception handler shouldn't be called!") )

(define (##sys#make-thread thunk state name q)
  (##sys#make-structure
   'thread
   thunk				; #1 thunk
   #f					; #2 result list
   state				; #3 state
   #f					; #4 block-timeout
   (vector				; #5 state buffer
    ##sys#dynamic-winds
    ##sys#standard-input
    ##sys#standard-output
    ##sys#standard-error
    ##sys#default-exception-handler
    (##sys#grow-vector ##sys#current-parameter-vector (##sys#size ##sys#current-parameter-vector) #f) )
   name					; #6 name
   (##core#undefined)			; #7 end-exception
   '()					; #8 owned mutexes
   q					; #9 quantum
   (##core#undefined)			; #10 specific
   #f					; #11 block-thread (currently unused)
   '() ) )				; #12 recipients (currently unused)

(define ##sys#primordial-thread (##sys#make-thread #f 'running 'primordial ##sys#default-thread-quantum))
(define ##sys#current-thread ##sys#primordial-thread)

(define (##sys#make-mutex id owner)
  (##sys#make-structure
   'mutex
   id					; #1 name
   owner				; #2 thread or #f
   '()					; #3 list of waiting threads
   #f					; #4 abandoned
   #f					; #5 locked
   (##core#undefined) ) )		; #6 specific

(define (##sys#abandon-mutexes thread)
  (let ([ms (##sys#slot thread 8)])
    (unless (null? ms)
      (##sys#for-each
       (lambda (m)
	 (##sys#setislot m 2 #f)
	 (##sys#setislot m 4 #t) 
	 (##sys#setislot m 5 #f)
	 (##sys#setislot m 3 '()) )
       ms) ) ) )

(define (##sys#schedule) ((##sys#slot ##sys#current-thread 1)))

(define (##sys#thread-yield!)
  (##sys#call-with-current-continuation
   (lambda (return)
     (let ((ct ##sys#current-thread))
       (##sys#setslot ct 1 (lambda () (return (##core#undefined))))
       (##sys#schedule) ) ) ) )


;;; Interrupt-handling:

(define ##sys#context-switch (##core#primitive "C_context_switch"))

(define (##sys#interrupt-hook reason state)
  (cond ((fx> (##sys#slot ##sys#pending-finalizers 0) 0)
	 (##sys#run-pending-finalizers state) )
	(else (##sys#context-switch state) ) ) )


;;; Accessing "errno":

(define-foreign-variable ##sys#errno int "errno")

(let ([rn 0])
  (set! ##sys#update-errno (lambda () (set! rn ##sys#errno) rn))
  (set! errno (lambda () rn)) )


;;; Special string quoting syntax:

(set! ##sys#user-read-hook
  (let ([old ##sys#user-read-hook]
	[open-output-string open-output-string]
	[get-output-string get-output-string] 
	[reverse reverse]
	[read read]
	[display display] )
    (define (readln port)
      (let ([ln (open-output-string)])
	(do ([c (##sys#read-char-0 port) (##sys#read-char-0 port)])
	    ((or (eof-object? c) (char=? #\newline c))
	     (cond [(char? c) (get-output-string ln)]
		   [else c] ) )
	  (##sys#write-char-0 c ln) ) ) )
    (define (read-escaped-sexp port skip-brace?)
      (when skip-brace? (##sys#read-char-0 port))
      (let* ((form (read port)))
	(when skip-brace?
	      (let loop ()
		;; Skips all characters until #\}
		(let ([c (##sys#read-char-0 port)])
		  (cond [(eof-object? c)
			 (##sys#read-error port "unexpected end of file - unterminated `#{...}' item in `here' string literal") ]
			[(not (char=? #\} c)) (loop)] ) ) ) )
	form))
    (lambda (char port)
      (cond [(not (char=? #\< char)) (old char port)]
	    [else
	     (read-char port)
	     (case (##sys#peek-char-0 port)
	       [(#\<)
		(##sys#read-char-0 port)
		(let ([str (open-output-string)]
		      [end (readln port)] 
		      [f #f] )
		  (do ([ln (readln port) (readln port)])
		      ((or (eof-object? ln) (string=? end ln)) 
		       (when (eof-object? ln)
			 (##sys#read-warning port "unterminated `here' string literal") )
		       (get-output-string str) )
		    (if f 
			(##sys#write-char-0 #\newline str)
			(set! f #t) )
		    (display ln str) ) ) ]
	       [(#\#)
		(##sys#read-char-0 port)
		(let ([end (readln port)] 
		      [str (open-output-string)] )
		  (define (get/clear-str)
		    (let ((s (get-output-string str)))
		      (set! str (open-output-string))
		      s))
		  (let loop [(lst '())]
		    (let ([c (##sys#read-char-0 port)])
		      (case c
			[(#\newline #!eof)
			 (let ([s (get/clear-str)])
			   (cond [(or (eof-object? c) (string=? end s))
				  (when (eof-object? c)
				    (##sys#read-warning port "unterminated `here' string literal") )
				  `(##sys#print-to-string
				    ;;Can't just use `(list ,@lst) because of 126 argument apply limit
				    ,(let loop2 ((lst (cdr lst)) (next-string '()) (acc ''())) ; drop last newline
				       (cond ((null? lst)
					      `(cons ,(##sys#print-to-string next-string) ,acc))
					     ((or (string? (car lst)) (char? (car lst)))
					      (loop2 (cdr lst) (cons (car lst) next-string) acc))
					     (else
					      (loop2 (cdr lst)
						     '()
						     `(cons ,(car lst)
							    (cons ,(##sys#print-to-string next-string) ,acc))))))) ]
				 [else (loop (cons #\newline (cons s lst)))] ) ) ]
			[(#\#)
			 (let ([c (##sys#peek-char-0 port)])
			   (case c
			     [(#\#)
			      (##sys#write-char-0 (##sys#read-char-0 port) str)
			      (loop lst) ]
			     [(#\{) (loop (cons (read-escaped-sexp port #t)
						(cons (get/clear-str) lst) ) ) ]
			     [else  (loop (cons (read-escaped-sexp port #f)
						(cons (get/clear-str) lst) ) ) ] ) ) ]
			[else
			 (##sys#write-char-0 c str)
			 (loop lst) ] ) ) ) ) ]
	       [else (##sys#read-error port "unreadable object")] ) ] ) ) ) )


;;; Script invocation:

(define command-line-arguments
  (make-parameter
   (let ([args (argv)])
     (if (pair? args)
	 (let loop ([args (##sys#slot args 1)])
	   (if (null? args)
	       '()
	       (let ([arg (##sys#slot args 0)]
		     [r (##sys#slot args 1)] )
		 (if (and (fx>= (##sys#size arg) 3) (string=? "-:" (##sys#substring arg 0 2)))
		     (loop r)
		     (cons arg (loop r)) ) ) ) )
	 args) )
   (lambda (x)
     (##sys#check-list x 'command-line-arguments)
     x) ) )


;;; Finalization:

(define-foreign-variable _max_pending_finalizers int "C_max_pending_finalizers")

(define ##sys#pending-finalizers 
  (##sys#make-vector (fx+ (fx* 2 _max_pending_finalizers) 1) (##core#undefined)) )

(##sys#setislot ##sys#pending-finalizers 0 0)

(define ##sys#set-finalizer! (##core#primitive "C_register_finalizer"))

(define set-finalizer! 
  (let ((print print))
    (lambda (x y)
      (when (fx> (##sys#fudge 26) _max_pending_finalizers)
	(if (##core#inline "C_resize_pending_finalizers" (fx* 2 _max_pending_finalizers))
	    (begin
	      (set! ##sys#pending-finalizers (##sys#grow-vector ##sys#pending-finalizers
								(fx+ (fx* 2 _max_pending_finalizers) 1)
								(##core#undefined)))
	      (when (##sys#fudge 13)
		(print "[debug] too many finalizers (" (##sys#fudge 26)
		       "), resized max finalizers to " _max_pending_finalizers "...") ) )
	    (begin
	      (when (##sys#fudge 13)
		(print "[debug] too many finalizers (" (##sys#fudge 26) "), forcing ...") )
	      (##sys#force-finalizers) ) ) )
      (##sys#set-finalizer! x y) ) ) )

(define ##sys#run-pending-finalizers
  (let ([vector-fill! vector-fill!]
	[print print]
	[working #f] )
    (lambda (state)
      (unless working
	(set! working #t)
	(let* ([n (##sys#size ##sys#pending-finalizers)]
	       [c (##sys#slot ##sys#pending-finalizers 0)] )
	  (when (##sys#fudge 13)
	    (print "[debug] running " c " finalizers (" (##sys#fudge 26) " live, "
		   (##sys#fudge 27) " allocated) ..."))
	  (do ([i 0 (fx+ i 1)])
	      ((fx>= i c))
	    (let ([i2 (fx+ 1 (fx* i 2))])
	      ((##sys#slot ##sys#pending-finalizers (fx+ i2 1))
	       (##sys#slot ##sys#pending-finalizers i2)) ) )
	  (vector-fill! ##sys#pending-finalizers (##core#undefined))
	  (##sys#setislot ##sys#pending-finalizers 0 0) 
	  (set! working #f) ) )
      (when state (##sys#context-switch state) ) ) ) )

(define (##sys#force-finalizers)
  (let loop ()
    (let ([n (##sys#gc)])
      (if (fx> (##sys#slot ##sys#pending-finalizers 0) 0)
	  (begin
	    (##sys#run-pending-finalizers #f)
	    (loop) )
	  n) ) ) )

(define (gc . arg)
  (let ([a (and (pair? arg) (car arg))])
    (if a
	(##sys#force-finalizers)
	(apply ##sys#gc arg) ) ) )


;;; Auxilliary definitions for safe use in quasiquoted forms and evaluated code:

(define ##sys#list->vector list->vector)
(define ##sys#list list)
(define ##sys#cons cons)
(define ##sys#append append)
(define ##sys#vector vector)
(define ##sys#apply apply)
(define ##sys#values values)


;;; Promises:

(define ##sys#make-promise
    (lambda (proc)
      (let ([result-ready #f]
	    [results #f] )
	(##sys#make-structure
	 'promise
	 (lambda ()
	   (if result-ready
	       (apply ##sys#values results)
	       (##sys#call-with-values 
		proc
		(lambda xs
		  (if result-ready
		      (apply ##sys#values results)
		      (begin
			(set! result-ready #t)
			(set! results xs)
			(apply ##sys#values results) ) ) ) ) ) ) ) ) ) )

(define (promise? x)
  (##sys#structure? x 'promise) )


;;; andmap + ormap:

(define andmap
  (lambda (f first . rest)
    (cond ((null? rest)
	   (let mapf ((l first))
	     (or (null? l)
		 (and (f (car l)) (mapf (cdr l))))))
	  ((null? (cdr rest))
	   (let mapf ((l1 first) (l2 (car rest)))
	     (or (null? l1)
		 (and (f (car l1) (car l2)) (mapf (cdr l1) (cdr l2))))))
	  (else
	   (let mapf ((first first) (rest rest))
	     (or (null? first)
		 (and (apply f (car first) (map (lambda (x) (car x)) rest))
		      (mapf (cdr first) (map (lambda (x) (cdr x)) rest)))))))))

(define ormap
  (lambda (f first . rest)
    (if (null? first)
        (or)
	(let ([lists (cons first rest)])
	  (or (apply f (map (lambda (x) (car x)) lists))
	      (apply ormap f (map (lambda (x) (cdr x)) lists)) ) ) ) ) )


;;; Support code for macro libraries (match):

(define ##sys#match-error
  (lambda (val . args)
    (##sys#print "\nFailed match:\n" #f ##sys#standard-error)
    (for-each
     (lambda (x)
       (##sys#print x #t ##sys#standard-error)
       (##sys#write-char-0 #\newline ##sys#standard-error) )
     args)
    (##sys#signal-hook #:match-error "no matching clause for " val)))


;;; Internal string-reader:

(define ##sys#read-from-string 
  (let ([open-input-string open-input-string])
    (lambda (s)
      (let ([i (open-input-string s)])
	(read i) ) ) ) )


;;; Convenient error printing:

(define print-error-message
  (let* ([display display]
	 [newline newline] 
	 [write write]
	 [string-append string-append]
	 [errmsg (condition-property-accessor 'exn 'message #f)]
	 [errloc (condition-property-accessor 'exn 'location #f)]
	 [errargs (condition-property-accessor 'exn 'arguments #f)] 
	 [writeargs
	  (lambda (args port)
	    (##sys#for-each 
	     (lambda (x)
	       (##sys#with-print-length-limit 80 (lambda () (write x port)))
	       (newline port) )
	     args) ) ] )
    (lambda (ex . args)
      (let-optionals args ([port ##sys#standard-output]
			   [header "Error"] )
	(##sys#check-port port 'print-error-message)
	(display header port)
	(cond [(and (not (##sys#immediate? ex)) (eq? 'condition (##sys#slot ex 0)))
	       (cond ((errmsg ex) =>
		      (lambda (msg)
			(display ": " port)
			(let ([loc (errloc ex)])
			  (when (and loc (symbol? loc))
			    (display (string-append "(" (##sys#symbol->qualified-string loc) ") ") port) ) )
			(display msg port) ) )
		     (else 
		      (let ((kinds (##sys#slot ex 1)))
			(if (equal? '(user-interrupt) kinds)
			    (display ": *** user interrupt ***" port)
			    (begin
			      (display ": <condition> " port)
			      (display (##sys#slot ex 1) port) ) ) ) ) )
	       (and-let* ([args (errargs ex)])
		 (if (fx= 1 (length args))
		     (begin
		       (display ": " port)
		       (writeargs args port) )
		     (begin
		       (newline port)
		       (writeargs args port) ) ) ) ]
	      [(string? ex)
	       (display ": " port)
	       (display ex port)
	       (newline port) ]
	      [else
	       (display "uncaught exception: " port)
	       (writeargs (list ex) port) ] ) ) ) ) )


;;; We need this here so `location' works:
 
(define (##sys#make-locative obj index weak? loc)
  (cond [(##sys#immediate? obj)
	 (##sys#signal-hook #:type-error loc "locative can not refer to immediate object" obj) ]
	[(or (vector? obj) (pair? obj))
	 (##sys#check-range index 0 (##sys#size obj) loc)
	 (##core#inline_allocate ("C_a_i_make_locative" 5) 0 obj index weak?) ]
	#;[(symbol? obj)
	 (##sys#check-range index 0 1 loc)
	 (##core#inline_allocate ("C_a_i_make_locative" 5) 0 obj index weak?) ]
	[(and (##core#inline "C_blockp" obj)
	      (##core#inline "C_bytevectorp" obj) )
	 (##sys#check-range index 0 (##sys#size obj) loc)
	 (##core#inline_allocate ("C_a_i_make_locative" 5) 2 obj index weak?) ]
	[(##sys#generic-structure? obj)
	 (case (##sys#slot obj 0)
	   [(array) 
	    (array:make-locative obj index weak?) ]
	   [(u8vector)
	    (let ([v (##sys#slot obj 1)])
	      (##sys#check-range index 0 (##sys#size v) loc)
	      (##core#inline_allocate ("C_a_i_make_locative" 5) 2 v index weak?))  ]
	   [(s8vector)
	    (let ([v (##sys#slot obj 1)])
	      (##sys#check-range index 0 (##sys#size v) loc)
	      (##core#inline_allocate ("C_a_i_make_locative" 5) 3 v index weak?) ) ]
	   [(u16vector)
	    (let ([v (##sys#slot obj 1)])
	      (##sys#check-range index 0 (##sys#size v) loc)
	      (##core#inline_allocate ("C_a_i_make_locative" 5) 4 v index weak?) ) ]
	   [(s16vector)
	    (let ([v (##sys#slot obj 1)])
	      (##sys#check-range index 0 (##sys#size v) loc)
	      (##core#inline_allocate ("C_a_i_make_locative" 5) 5 v index weak?) ) ]
	   [(u32vector)
	    (let ([v (##sys#slot obj 1)])
	      (##sys#check-range index 0 (##sys#size v) loc)
	      (##core#inline_allocate ("C_a_i_make_locative" 5) 6 v index weak?) ) ]
	   [(s32vector)
	    (let ([v (##sys#slot obj 1)])
	      (##sys#check-range index 0 (##sys#size v) loc)
	      (##core#inline_allocate ("C_a_i_make_locative" 5) 7 v index weak?) ) ]
	   [(f32vector)
	    (let ([v (##sys#slot obj 1)])
	      (##sys#check-range index 0 (##sys#size v) loc)
	      (##core#inline_allocate ("C_a_i_make_locative" 5) 8 v index weak?) ) ]
	   [(f64vector)
	    (let ([v (##sys#slot obj 1)])
	      (##sys#check-range index 0 (##sys#size v) loc)
	      (##core#inline_allocate ("C_a_i_make_locative" 5) 9 v index weak?) ) ]
	   [else 
	    (##sys#check-range index 0 (fx- (##sys#size obj) 1) loc)
	    (##core#inline_allocate ("C_a_i_make_locative" 5) 0 obj (fx+ index 1) weak?) ] ) ]
	[(string? obj)
	 (##sys#check-range index 0 (##sys#size obj) loc)
	 (##core#inline_allocate ("C_a_i_make_locative" 5) 1 obj index weak?) ] 
	[else
	 (##sys#signal-hook
	  #:type-error loc
	  "bad argument type - locative can not refer to objects of this type" 
	  obj) ] ) )


;;; Importing from other namespaces:

(define ##sys#find-symbol 
  (foreign-lambda scheme-object "C_find_symbol" scheme-object c-pointer) )

(define ##sys#find-symbol-table
  (foreign-lambda c-pointer "C_find_symbol_table" c-string) )

(define ##sys#import
  (let ([enum-syms! (foreign-lambda scheme-object "C_enumerate_symbols" c-pointer scheme-object)])
    (lambda (ns  . more)
      (let-optionals more ([syms '()] [prefix #f])
	(let ([prefix
	       (and prefix
		    (cond [(symbol? prefix) (##sys#slot prefix 1)]
			  [(string? prefix) prefix]
			  [else (##sys#signal-hook #:type-error "bad argument type - invalid prefix" prefix)] ) ) ] )
	  (let ([nsp (##sys#find-symbol-table (##sys#make-c-string (##sys#slot ns 1)))])
	    (define (copy s str)
	      (let ([s2 (##sys#intern-symbol
			 (if prefix
			     (##sys#string-append prefix str)
			     str) ) ] )
		(##sys#setslot s2 0 (##sys#slot s 0)) ) )
	    (unless nsp (##sys#error "undefined namespace" ns))
	    (if (null? syms)
		(let ([it (cons -1 '())])
		  (let loop ()
		    (let ([s (enum-syms! nsp it)])
		      (when s 
			(copy s (##sys#slot s 1))
			(loop) ) ) ) )
		(for-each
		 (lambda (ss)
		   (let ([old #f]
			 [new #f] )
		     (if (and (pair? ss) (pair? (##sys#slot ss 1)))
			 (begin
			   (set! old (##sys#slot ss 0))
			   (set! new (##sys#slot (##sys#slot ss 1) 0)) )
			 (begin
			   (set! old ss)
			   (set! new ss) ) )
		     (let* ([str (##sys#slot old 1)]
			    [s (##sys#find-symbol str nsp)] )
		       (unless s
			 (##sys#error "symbol not exported from namespace" ss ns) )
		       (copy s (##sys#slot new 1)) ) ) )
		 syms) ) ) ) ) ) ) )

(define (##sys#namespace-ref ns sym . default)
  (let ([s (##sys#find-symbol 
	    (cond [(symbol? sym) (##sys#slot sym 1)]
		  [(string? sym) sym]
		  [else (##sys#signal-hook #:type-error "bad argument type - not a valid import name" sym)] ) 
	    (##sys#find-symbol-table (##sys#make-c-string (##sys#slot ns 1))) ) ] )
    (cond [s (##core#inline "C_retrieve" s)]
	  [(pair? default) (car default)]
	  [else (##sys#error "symbol not exported from namespace" sym ns)] ) ) )

(define (##sys#walk-namespace proc . args)
  (let ([ns (if (pair? args) (car args) ".")])
    (let ([nsp (##sys#find-symbol-table ns)]
          [enum-syms! (foreign-lambda scheme-object "C_enumerate_symbols" c-pointer scheme-object)]
          [pos (cons -1 '())])
      (unless nsp (##sys#error "undefined namespace" ns))
      (let loop ()
        (let ([sym (enum-syms! nsp pos)])
          (when sym
            (proc sym)
            (loop) ) ) ) ) ) )

;;; More memory info

(define (memory-statistics)
  (let* ([free (##sys#gc #t)]
	 [info (##sys#memory-info)] 
	 [hsize (##sys#slot info 0)] )
    (vector hsize (fx- hsize free) (##sys#slot info 1)) ) )


;;; Decorate procedure with arbitrary data

(define (##sys#decorate-lambda proc pred decorator)
  (let ((len (##sys#size proc)))
    (let loop ((i (fx- len 1)))
      (cond ((zero? i)
	     (let ((p2 (make-vector (fx+ len 1))))
	       (do ((i 1 (fx+ i 1)))
		   ((fx>= i len)
		    (##core#inline "C_vector_to_closure" p2)
		    (##core#inline "C_copy_pointer" proc p2)
		    (decorator p2 i) )
		 (##sys#setslot p2 i (##sys#slot proc i)) ) ) )
	    (else
	     (let ((x (##sys#slot proc i)))
	       (if (pred x)
		   (decorator proc i)
		   (loop (fx- i 1)) ) ) ) ) ) ) )

(define (##sys#lambda-decoration proc pred)
  (let loop ((i (fx- (##sys#size proc) 1)))
    (and (fx> i 0)
	 (let ((x (##sys#slot proc i)))
	   (if (pred x)
	       x
	       (loop (fx- i 1)) ) ) ) ) )


;;; Function debug info:

(define (##sys#lambda-info proc)
  (##sys#lambda-decoration 
   proc 
   (lambda (x) (and (not (##sys#immediate? x)) (##core#inline "C_lambdainfop" x))) ) )

(define (##sys#lambda-info->string info)
  (let* ((sz (##sys#size info))
	 (s (##sys#make-string sz)) )
    (##core#inline "C_copy_memory" s info sz)
    s) )

(define procedure-information
  (let ((open-input-string open-input-string))
    (lambda (x)
      (unless (procedure? x)
	(##sys#signal-hook #:type-error 'procedure-information "bad argument type - not a procedure" x) )
      (and-let* ((info (##sys#lambda-info x)))
	(##sys#read (open-input-string (##sys#lambda-info->string info)) #f) ) ) ) )


;;; SRFI-17

(define setter-tag (vector 'setter))

(define-inline (setter? x) 
  (and (pair? x) (eq? setter-tag (##sys#slot x 0))) )

(define ##sys#setter
  (##sys#decorate-lambda 
   (lambda (proc)
     (or (and-let* (((procedure? proc))
		    (d (##sys#lambda-decoration proc setter?)) )
	   (##sys#slot d 1) )
	 (##sys#error 'setter "no setter defined" proc) ) )
   setter?
   (lambda (proc i)
     (##sys#setslot 
      proc i
      (cons 
       setter-tag
       (lambda (get set)
	 (if (procedure? get)
	     (let ((get2 (##sys#decorate-lambda
			  get
			  setter?
			  (lambda (proc i) (##sys#setslot proc i (cons setter-tag set)) proc))))
	       (if (eq? get get2)
		   get
		   (##sys#become! (list (cons get get2))) ) )
	     (error "can't set setter of non-procedure" get) ) ) ) )
     proc) ) )

(define setter ##sys#setter)

(define (getter-with-setter get set)
  (##sys#decorate-lambda
   get
   setter?
   (lambda (proc i)
     (##sys#setslot proc i (cons setter-tag set))
     proc) ) )

(define car (getter-with-setter car set-car!))
(define cdr (getter-with-setter cdr set-cdr!))
(define caar (getter-with-setter caar (lambda (x y) (set-car! (car x) y))))
(define cadr (getter-with-setter cadr (lambda (x y) (set-car! (cdr x) y))))
(define cdar (getter-with-setter cdar (lambda (x y) (set-cdr! (car x) y))))
(define cddr (getter-with-setter cddr (lambda (x y) (set-cdr! (cdr x) y))))
(define caaar (getter-with-setter caaar (lambda (x y) (set-car! (caar x) y))))
(define caadr (getter-with-setter caadr (lambda (x y) (set-car! (cadr x) y))))
(define cadar (getter-with-setter cadar (lambda (x y) (set-car! (cdar x) y))))
(define caddr (getter-with-setter caddr (lambda (x y) (set-car! (cddr x) y))))
(define cdaar (getter-with-setter cdaar (lambda (x y) (set-cdr! (caar x) y))))
(define cdadr (getter-with-setter cdadr (lambda (x y) (set-cdr! (cadr x) y))))
(define cddar (getter-with-setter cddar (lambda (x y) (set-cdr! (cdar x) y))))
(define cdddr (getter-with-setter cdddr (lambda (x y) (set-cdr! (cddr x) y))))
(define string-ref (getter-with-setter string-ref string-set!))
(define vector-ref (getter-with-setter vector-ref vector-set!))

(define (##sys#dunload name)		; not available on all platforms and to be used with caution...
  (and-let* ((r (##core#inline "C_dunload" (##sys#make-c-string name))))
    (##sys#gc #t) 
    #t) )
