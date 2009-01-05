;;;; srfi-14.import.scm - import library for "srfi-14" module
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
 'srfi-14
 '(->char-set
   char-set
   char-set->list
   char-set->string
   char-set-adjoin
   char-set-adjoin!
   char-set-any
   char-set-complement
   char-set-complement!
   char-set-contains?
   char-set-copy
   char-set-count
   char-set-cursor
   char-set-cursor-next
   char-set-delete
   char-set-delete!
   char-set-diff+intersection
   char-set-diff+intersection!
   char-set-difference
   char-set-difference!
   char-set-every
   char-set-filter
   char-set-filter!
   char-set-fold
   char-set-for-each
   char-set-hash
   char-set-intersection
   char-set-intersection!
   char-set-map
   char-set-ref
   char-set-size
   char-set-unfold
   char-set-unfold!
   char-set-union
   char-set-union!
   char-set-xor
   char-set-xor!
   char-set:ascii
   char-set:blank
   char-set:digit
   char-set:empty
   char-set:full
   char-set:graphic
   char-set:hex-digit
   char-set:iso-control
   char-set:letter
   char-set:letter+digit
   char-set:lower-case
   char-set:printing
   char-set:punctuation
   char-set:s
   char-set:symbol
   char-set:title-case
   char-set:upper-case
   char-set:whitespace
   char-set<=
   char-set=
   char-set?
   end-of-char-set?
   list->char-set
   list->char-set!
   make-char-set
   string->char-set
   string->char-set!
   ucs-range->char-set
   ucs-range->char-set!))
