;;;; unsafe-declarations.scm - various settings for libraries compiled in unsafe mode
;
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
 (unsafe
  (define-syntax ##sys#check-closure
    (syntax-rules ()
      ((_ . _) (##core#undefined))))
  (define-syntax ##sys#check-inexact
    (syntax-rules ()
      ((_ . _) (##core#undefined))))
  (define-syntax ##sys#check-range
    (syntax-rules ()
      ((_ . _) (##core#undefined))))
  (define-syntax ##sys#check-pair
    (syntax-rules ()
      ((_ . _) (##core#undefined))))
  (define-syntax ##sys#check-blob
    (syntax-rules ()
      ((_ . _) (##core#undefined))))
  (define-syntax ##sys#check-list
    (syntax-rules ()
      ((_ . _) (##core#undefined))))
  (define-syntax ##sys#check-symbol
    (syntax-rules ()
      ((_ . _) (##core#undefined))))
  (define-syntax ##sys#check-string
    (syntax-rules ()
      ((_ . _) (##core#undefined))))
  (define-syntax ##sys#check-char
    (syntax-rules ()
      ((_ . _) (##core#undefined))))
  (define-syntax ##sys#check-exact
    (syntax-rules ()
      ((_ . _) (##core#undefined))))
  (define-syntax ##sys#check-port
    (syntax-rules ()
      ((_ . _) (##core#undefined))))
  (define-syntax ##sys#check-port-mode
    (syntax-rules ()
      ((_ . _) (##core#undefined))))
  (define-syntax ##sys#check-port*
    (syntax-rules ()
      ((_ . _) (##core#undefined))))
  (define-syntax ##sys#check-number
    (syntax-rules ()
      ((_ . _) (##core#undefined))))
  (define-syntax ##sys#check-special
    (syntax-rules ()
      ((_ . _) (##core#undefined))))
  (define-syntax ##sys#check-byte-vector
    (syntax-rules ()
      ((_ . _) '(##core#undefined)) ) ))
 (else))
