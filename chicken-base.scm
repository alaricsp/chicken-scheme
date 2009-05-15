;;;; chicken-base.scm - convenience wrapper module for various often used modules
;
; Copyright (c) 2009, The Chicken Team
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


(require-library
 extras data-structures ports files utils)


(module chicken-base 

    ;; scheme
    (not boolean? eq? eqv? equal? pair?
	 cons car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr
	 cddar cdddr caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar
	 cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr set-car! set-cdr!
	 null? list? list length list-tail list-ref append reverse memq memv
	 member assq assv assoc symbol? symbol->string string->symbol number?
	 integer? exact? real? complex? inexact? rational? zero? odd? even?
	 positive? negative?  max min + - * / = > < >= <= quotient remainder
	 modulo gcd lcm abs floor ceiling truncate round exact->inexact
	 inexact->exact exp log expt sqrt sin cos tan asin acos atan
	 number->string string->number char? char=? char>? char<? char>=?
	 char<=? char-ci=? char-ci<? char-ci>?  char-ci>=? char-ci<=?
	 char-alphabetic? char-whitespace? char-numeric? char-upper-case?
	 char-lower-case? char-upcase char-downcase char->integer integer->char
	 string? string=?  string>? string<? string>=? string<=? string-ci=?
	 string-ci<? string-ci>? string-ci>=? string-ci<=?  make-string
	 string-length string-ref string-set! string-append string-copy
	 string->list list->string substring string-fill! vector? make-vector
	 vector-ref vector-set! string vector vector-length vector->list
	 list->vector vector-fill! procedure? map for-each apply force
	 call-with-current-continuation input-port? output-port?
	 current-input-port current-output-port call-with-input-file
	 call-with-output-file open-input-file open-output-file
	 close-input-port close-output-port load read eof-object? read-char
	 peek-char write display write-char newline with-input-from-file
	 with-output-to-file dynamic-wind values call-with-values eval
	 char-ready? imag-part real-part magnitude numerator denominator
	 scheme-report-environment null-environment interaction-environment
	 er-macro-transformer

	 ;; chicken     
	 abort
	 add1
	 argc+argv
	 argv
	 arithmetic-shift
	 bit-set?
	 bitwise-and
	 bitwise-ior
	 bitwise-not
	 bitwise-xor
	 blob->string
	 blob-size
	 blob?
	 blob=?
	 breakpoint
	 build-platform
	 c-runtime
	 call/cc
	 case-sensitive
	 char-name
	 chicken-home
	 chicken-version
	 command-line-arguments
	 condition-predicate
	 condition-property-accessor
	 condition?
	 continuation-capture
	 continuation-graft
	 continuation-return
	 continuation?
	 copy-read-table
	 cpu-time
	 current-error-port
	 current-exception-handler
	 current-gc-milliseconds
	 current-milliseconds
	 current-read-table
	 current-seconds
	 define-reader-ctor
	 delete-file
	 enable-warnings
	 errno
	 error
	 exit
	 exit-handler
	 expand
	 extension-information
	 feature?
	 features
	 file-exists?
	 fixnum-bits
	 fixnum-precision
	 fixnum?
	 flonum-decimal-precision
	 flonum-epsilon
	 flonum-maximum-decimal-exponent
	 flonum-maximum-exponent
	 flonum-minimum-decimal-exponent
	 flonum-minimum-exponent
	 flonum-precision
	 flonum-print-precision
	 flonum-radix
	 flonum?
	 flush-output
	 force-finalizers
	 fp-
	 fp*
	 fp/
	 fp+
	 fp<
	 fp<=
	 fp=
	 fp>
	 fp>=
	 fpmax
	 fpmin
	 fpneg
	 fx-
	 fx*
	 fx/
	 fx+
	 fx<
	 fx<=
	 fx=
	 fx>
	 fx>=
	 fxand
	 fxior
	 fxmax
	 fxmin
	 fxmod
	 fxneg
	 fxnot
	 fxshl
	 fxshr
	 fxxor
	 gc
	 gensym
	 get
	 get-call-chain
	 get-environment-variable
	 get-keyword
	 get-output-string
	 get-properties
	 getenv
	 getter-with-setter
	 implicit-exit-handler
	 keyword->string
	 keyword-style
	 keyword?
	 load-library
	 load-relative
	 load-verbose
	 machine-byte-order
	 machine-type
	 make-blob
	 make-composite-condition
	 make-parameter
	 make-property-condition
	 maximum-flonum
	 memory-statistics
	 minimum-flonum
	 most-negative-fixnum
	 most-positive-fixnum
	 on-exit
	 open-input-string
	 open-output-string
	 parentheses-synonyms
	 port-name
	 port-position
	 port?
	 print
	 print-call-chain
	 print-error-message
	 print*
	 procedure-information
	 program-name
	 promise?
	 put!
	 register-feature!
	 remprop!
	 rename-file
	 repl
	 repl-prompt
	 repository-path
	 require
	 reset
	 reset-handler
	 return-to-host
	 reverse-list->string
	 set-finalizer!
	 set-gc-report!
	 set-parameterized-read-syntax!
	 set-port-name!
	 set-read-syntax!
	 set-sharp-read-syntax!
	 setter
	 signal
	 signum
	 singlestep
	 software-type
	 software-version
	 string->blob
	 string->keyword
	 string->uninterned-symbol
	 strip-syntax
	 sub1
	 symbol-escape
	 symbol-plist
	 syntax-error
	 system
	 unregister-feature!
	 vector-resize
	 void
	 warning
	 eval-handler
	 dynamic-load-libraries
	 with-exception-handler

	 ;; extras
	 format
	 fprintf
	 pp
	 pretty-print
	 pretty-print-width
	 printf
	 random
	 random-seed
	 randomize
	 read-byte
	 read-file
	 read-line
	 read-lines
	 read-string
	 read-string!
	 read-token
	 sprintf
	 write-byte
	 write-line
	 write-string

	 ;; files
	 delete-file*
	 file-copy
	 file-move
	 make-pathname
	 directory-null?
	 make-absolute-pathname
	 create-temporary-file
	 decompose-pathname
	 absolute-pathname?
	 pathname-directory
	 pathname-extension
	 pathname-file
	 pathname-replace-directory
	 pathname-replace-extension
	 pathname-replace-file
	 pathname-strip-directory
	 pathname-strip-extension
	 normalize-pathname

	 ;; ports
	 call-with-input-string
	 call-with-output-string
	 make-input-port
	 make-output-port
	 port-for-each
	 port-map
	 port-fold
	 make-broadcast-port
	 make-concatenated-port
	 with-error-output-to-port
	 with-input-from-port
	 with-input-from-string
	 with-output-to-port
	 with-output-to-string
	 with-error-output-to-port

	 ;;data-structures
	 ->string
	 alist-ref
	 alist-update!
	 always?
	 any?
	 atom?
	 binary-search
	 butlast
	 chop
	 complement
	 compose
	 compress
	 conc
	 conjoin
	 constantly
	 disjoin
	 each
	 flatten
	 flip
	 identity
	 intersperse
	 join
	 left-section
	 list->queue
	 list-of?
	 make-queue
	 merge
	 merge!
	 never?
	 none?
	 noop
	 o
	 project
	 queue->list
	 queue-add!
	 queue-empty?
	 queue-first
	 queue-last
	 queue-push-back!
	 queue-push-back-list!
	 queue-remove!
	 queue?
	 rassoc
	 right-section
	 shuffle
	 sort
	 sort!
	 sorted?
	 string-chomp
	 string-chop
	 string-compare3
	 string-compare3-ci
	 string-intersperse
	 string-split
	 string-translate
	 string-translate*
	 substring-ci=?
	 substring-index
	 substring-index-ci
	 substring=?
	 tail?

	 ;; utils
	 for-each-argv-line
	 for-each-line
	 read-all
	 system*
	 qs)

  (import scheme chicken data-structures utils ports files extras)

  )
