;;;; srfi-13.import.scm - import library for "srfi-13" module
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
 'srfi-13
 '(check-substring-spec
   kmp-step
   make-kmp-restart-vector
   string->list
   string-any
   string-append/shared
   string-ci<
   string-ci<=
   string-ci<>
   string-ci=
   string-ci>
   string-ci>=
   string-compare
   string-compare-ci
   string-concatenate
   string-concatenate-reverse
   string-concatenate-reverse/shared
   string-concatenate/shared
   string-contains
   string-contains-ci
   string-copy
   string-copy!
   string-count
   string-delete
   string-downcase
   string-downcase!
   string-drop
   string-drop-right
   string-every
   string-fill!
   string-filter
   string-fold
   string-fold-right
   string-for-each
   string-for-each-index
   string-index
   string-index-right
   string-join
   string-kmp-partial-search
   string-map
   string-map!
   string-null?
   string-pad
   string-pad-right
   string-parse-final-start+end
   string-parse-start+end
   string-prefix-ci?
   string-prefix-length
   string-prefix-length-ci
   string-prefix?
   string-replace
   string-reverse
   string-reverse!
   string-skip
   string-skip-right
   string-suffix-ci?
   string-suffix-length
   string-suffix-length-ci
   string-suffix?
   string-tabulate
   string-take
   string-take-right
   string-titlecase
   string-titlecase!
   string-tokenize
   string-trim
   string-trim-both
   string-trim-right
   string-unfold
   string-unfold-right
   string-upcase
   string-upcase!
   string-xcopy!
   string<
   string<=
   string<>
   string=
   string>
   string>=
   substring-spec-ok?
   substring/shared
   xsubstring)
 `((let-string-start+end 
    ()
    ,(##sys#er-transformer
      (lambda (form r c)
	(##sys#check-syntax 'let-string-start+end form '(_ _ _ _ _ . _))
	(let ((s-e-r (cadr form))
	      (proc (caddr form))
	      (s-exp (cadddr form))
	      (args-exp (car (cddddr form)))
	      (body (cdr (cddddr form)))
	      (%receive (r 'receive))
	      (%string-parse-start+end (r 'string-parse-start+end))
	      (%string-parse-final-start+end (r 'string-parse-final-start+end)))
	  (if (pair? (cddr s-e-r))
	      `(,%receive (,(caddr s-e-r) ,(car s-e-r) ,(cadr s-e-r))
			  (,%string-parse-start+end ,proc ,s-exp ,args-exp)
			  ,@body)
	      `(,%receive ,s-e-r
			  (,%string-parse-final-start+end ,proc ,s-exp ,args-exp)
			  ,@body) ) ))))))
