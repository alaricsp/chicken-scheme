;;;; parameters.scm - Miscellaneous system parameters
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


(define-constant copyright "(c)2000-2006 Felix L. Winkelmann")

(define-constant namespace-size 997)
(define-constant namespace-max-id-len 31)

(define-constant char-name-table-size 37)
(define-constant macro-table-size 301)

(define-constant output-string-initial-size 256)

(declare
  (foreign-declare "#define C_METHOD_CACHE_SIZE 8") )

(eval-when (load)
  (define-foreign-variable method-cache-size int "C_METHOD_CACHE_SIZE") )

(define-constant basic-class-instance-size 11)

(define-constant default-parameter-vector-size 16)

(define-constant debug-output-print-limit 80)
(define-constant debug-repl-result-print-limit 512)
(define-constant debug-variable-info-size 3)
(define-constant debug-lambda-info-size 7)
(define-constant anonymous-object-identifier '-anonymous-)

(define-constant maximal-string-length #x00ffffff)

(define-constant profile-info-entry-size 5)

(define-constant foreign-type-table-size 301)
(define-constant analysis-database-size 3001)
(define-constant default-line-number-database-size 997)
(define-constant inline-table-size 301)
(define-constant constant-table-size 301)
(define-constant real-name-table-size 997)
(define-constant import-table-size 997)

(define-constant default-literal-compression-threshold 50)

(define-constant maximal-number-of-free-variables-for-liftable 16)

(define-constant unix-default-dynamic-load-libraries '("libchicken"))
(define-constant cygwin-default-dynamic-load-libraries '("cygchicken-0"))
(define-constant mingw-default-dynamic-load-libraries '("libchicken-0"))
(define-constant msvc-default-dynamic-load-libraries '("libchicken"))

(define-constant macosx-load-library-extension ".dylib")
(define-constant windows-load-library-extension ".dll")
(define-constant hppa-load-library-extension ".sl")
(define-constant default-load-library-extension ".so")

(define-constant environment-table-size 301)

(define-constant source-file-extension ".scm")
(define-constant setup-file-extension "setup-info")
(define-constant alternative-setup-file-extension "setup")

(define-constant repository-environment-variable "CHICKEN_REPOSITORY")
(define-constant home-environment-variable "CHICKEN_HOME")
(define-constant prefix-environment-variable "CHICKEN_PREFIX")

(define-constant default-profile-name "PROFILE")

(define-constant remote-repository-name "repository")

(define-constant core-library-modules
  '(extras lolevel tinyclos utils tcp regex posix match srfi-1 srfi-4 srfi-14 srfi-18 srfi-13))

(define-constant special-syntax-files
  '(chicken-ffi-macros chicken-more-macros) )

(define-constant builtin-features
  '(chicken srfi-23 srfi-30 srfi-39 srfi-6 srfi-10 srfi-2 srfi-31
	    srfi-69 srfi-28) )		; these are actually in extras, but that is used by default

(define-constant builtin-features/compiled
  '(easyffi srfi-11 srfi-8 srfi-6 srfi-16 srfi-15 srfi-26 srfi-55 srfi-9 srfi-17) )

(define-constant installed-executables
  '("chicken" "csc" "csi" "chicken-setup" "chicken-profile") )

(define-constant default-inline-max-size 10)
