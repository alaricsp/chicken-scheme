;;;; chicken.scm - The CHICKEN Scheme compiler (loader/main-module)
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
  (uses extras srfi-1 match srfi-4 utils support compiler optimizer driver platform backend)
  (run-time-macros) )


#{compiler
  compiler-arguments
  default-standard-bindings default-extended-bindings side-effecting-standard-bindings
  non-foldable-standard-bindings foldable-standard-bindings non-foldable-extended-bindings foldable-extended-bindings
  standard-bindings-that-never-return-false side-effect-free-standard-bindings-that-never-return-false
  installation-home optimization-iterations process-command-line
  file-io-only nonwinding-call/cc debugging
  unit-name insert-timer-checks used-units zap-strings-flag
  foreign-declarations debugging-executable block-compilation line-number-database-size
  target-heap-size target-stack-size 
  default-default-target-heap-size default-default-target-stack-size verbose-mode original-program-size
  current-program-size line-number-database-2 foreign-lambda-stubs immutable-constants foreign-variables
  rest-parameters-promoted-to-vector inline-table inline-table-used constant-table constants-used mutable-constants
  broken-constant-nodes inline-substitutions-enabled compiler-warning
  direct-call-ids foreign-type-table first-analysis
  initialize-compiler canonicalize-expression expand-foreign-lambda update-line-number-database scan-toplevel-assignments
  perform-cps-conversion analyze-expression simplifications perform-high-level-optimizations perform-pre-optimization!
  reorganize-recursive-bindings substitution-table simplify-named-call
  perform-closure-conversion prepare-for-code-generation compiler-source-file create-foreign-stub expand-foreign-lambda*
  transform-direct-lambdas!
  debugging-chicken bomb check-signature posq stringify symbolify build-lambda-list
  string->c-identifier c-ify-string words check-and-open-input-file close-checked-input-file fold-inner constant?
  collapsable-literal? immediate? canonicalize-begin-body extract-mutable-constants string->expr get get-all
  put! collect! count! get-line get-line-2 find-lambda-container display-analysis-database varnode qnode 
  build-node-graph build-expression-tree fold-boolean inline-lambda-bindings match-node expression-has-side-effects?
  simple-lambda-node? compute-database-statistics print-program-statistics output gen gen-list 
  pprint-expressions-to-file foreign-type-check estimate-foreign-result-size scan-used-variables scan-free-variables
  topological-sort print-version print-usage initialize-analysis-database
  default-declarations units-used-by-default words-per-flonum
  foreign-string-result-reserve parameter-limit default-output-filename eq-inline-operator optimizable-rest-argument-operators
  membership-test-operators membership-unfold-limit valid-compiler-options valid-compiler-options-with-argument
  generate-code make-variable-list make-argument-list generate-foreign-stubs foreign-type-declaration
  foreign-argument-conversion foreign-result-conversion}


(include "tweaks")

(eval-when (load) 
  (include "chicken-more-macros")
  (include "chicken-ffi-macros") )

;;(##sys#provide 'extras 'srfi-1 'srfi-4)


;;; Prefix argument list with default options:

(define compiler-arguments
  (append
   (cdr (argv))
   (remove (lambda (x) (string=? x "")) (string-split (or (getenv "CHICKEN_OPTIONS") ""))) ) )


;;; Process command-line options:
; 
; - remove runtime-options ("-:...")
; - filter out source-filename
; - convert options into symbols (without the initial hyphens)

(define (process-command-line args)
  (let loop ([args args] [options '()] [filename #f])
    (if (null? args)
	(values filename (reverse options))
	(let* ([arg (car args)]
	       [len (string-length arg)]
	       [char0 (string-ref arg 0)] )
	  (if (and (char=? #\- char0) (> len 1))
	      (if (and (> len 1) (char=? #\: (string-ref arg 1)))
		  (loop (cdr args) options filename)
		  (loop (cdr args) (cons (string->symbol (substring arg 1 len)) options) filename) )
	      (if filename
		  (loop (cdr args) (cons arg options) filename)
		  (loop (cdr args) options arg) ) ) ) ) ) )


;;; Run compiler with command-line options:

(receive (filename options) ((or (user-options-pass) process-command-line) compiler-arguments)
  (let loop ([os options])
    (unless (null? os)
      (let ([o (car os)]
	    [rest (cdr os)] )
	(cond [(eq? 'optimize-level o)
	       (let ([level (string->number (car rest))])
		 (case level
		   [(0) #f]
		   [(1)
		    (set! options (cons* 'optimize-leaf-routines options)) ]
		   [(2)
		    (set! options
		      (cons 'optimize-leaf-routines options) ) ] 
		   [(3)
		    (set! options
		      (cons* 'optimize-leaf-routines 'unsafe options) ) ]
		   [else (compiler-warning 'usage "invalid optimization level ~S - ignored" (car rest))] )
		 (loop (cdr rest)) ) ]
	      [(eq? 'debug-level o)
	       (let ([level (string->number (car rest))])
		 (case level
		   [(0) (set! options (cons* 'no-lambda-info 'no-trace options))]
		   [(1) (set! options (cons 'no-trace options))]
		   [(2) #f]
		   [else (compiler-warning 'usage "invalid debug level ~S - ignored" (car rest))] )
		 (loop (cdr rest)) ) ]
	      [(eq? 'benchmark-mode o)
	       (set! options 
		 (cons* 'fixnum-arithmetic 'disable-interrupts 'no-trace 'unsafe
			'optimize-leaf-routines 'block 'lambda-lift 'no-lambda-info
			options) )
	       (loop rest) ]
	      [(memq o valid-compiler-options) (loop rest)]
	      [(memq o valid-compiler-options-with-argument)
	       (if (pair? rest)
		   (loop (cdr rest))
		   (quit "missing argument to `-~s' option" o) ) ]
	      [else
	       (compiler-warning 
		'usage "invalid compiler option `~a' - ignored" 
		(if (string? o) o (conc "-" o)) )
	       (loop rest) ] ) ) ) )
  (apply compile-source-file filename options)
  (exit) )
