;;;; scheme.import.scm - import library for "scheme" module
;
; Copyright (c) 2008-2009, The Chicken Team
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


(##sys#register-primitive-module
 'scheme
 '(not boolean? eq? eqv? equal? pair?
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
       else)
 ##sys#default-macro-environment)
