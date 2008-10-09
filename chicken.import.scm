;;;; chicken.import.scm - import library for "chicken" module
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


(##sys#register-primitive-module
 'chicken
 '(abort add1 argc+argv argv 
	 bit-set?
	 arithmetic-shift
	 bitwise-and
	 bitwise-ior
	 bitwise-not
	 bitwise-xor
	 blob->string
	 blob-size
	 blob=?
	 blob?
	 breakpoint
	 build-platform
	 c-runtime
	 call/cc
	 case-sensitive
	 char-name
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
	 delete-file
	 enable-warnings
	 errno
	 error
	 exit
	 exit-handler
	 feature?
	 features
	 file-exists?
	 fixnum?
	 flonum-print-precision
	 flonum?
	 flush-output
	 force-finalizers
	 fp*
	 fp+
	 fp-
	 fp/
	 fp<
	 fp<=
	 fp=
	 fp>
	 fp>=
	 fpmax
	 fpmin
	 fpneg
	 fx*
	 fx+
	 fx-
	 fx/
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
	 get-keyword
	 get-output-string
	 get-properties
	 getenv			       
	 get-environment-variable
	 getter-with-setter
	 implicit-exit-handler
	 keyword->string
	 keyword-style
	 keyword?
	 load-relative
	 load-library
	 load-verbose
	 machine-byte-order
	 machine-type
	 make-blob
	 make-composite-condition
	 make-parameter
	 make-property-condition
	 memory-statistics
	 on-exit
	 open-input-string
	 open-output-string
	 port-name
	 port-position
	 port?
	 print
	 print*
	 print-call-chain
	 print-error-message
	 procedure-information
	 program-name
	 promise?
	 put!
	 register-feature!
	 remprop!
	 require
	 rename-file
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
	 syntax-error
	 setter
	 signal
	 signum
	 singlestep
	 software-type
	 software-version
	 string->blob
	 string->keyword
	 string->uninterned-symbol
	 sub1
	 symbol-plist
	 system
	 unregister-feature!
	 vector-resize
	 void
	 warning
	 expand
	 extension-information
	 repository-path
	 with-exception-handler
	 strip-syntax)
 ##sys#chicken-macro-environment)	;*** incorrect - won't work in compiled executable
