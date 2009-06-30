;;;; csc.scm - Driver program for the CHICKEN compiler - felix -*- Scheme -*-
;
; Copyright (c) 2000-2007, Felix L. Winkelmann
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


(declare
  (block)
  (uses data-structures ports srfi-1 srfi-13 utils files extras))

(define-foreign-variable INSTALL_BIN_HOME c-string "C_INSTALL_BIN_HOME")
(define-foreign-variable INSTALL_CC c-string "C_INSTALL_CC")
(define-foreign-variable INSTALL_CXX c-string "C_INSTALL_CXX")
(define-foreign-variable TARGET_CC c-string "C_TARGET_CC")
(define-foreign-variable TARGET_CXX c-string "C_TARGET_CXX")
(define-foreign-variable TARGET_CFLAGS c-string "C_TARGET_CFLAGS")
(define-foreign-variable INSTALL_CFLAGS c-string "C_INSTALL_CFLAGS")
(define-foreign-variable TARGET_LDFLAGS c-string "C_TARGET_LDFLAGS")
(define-foreign-variable INSTALL_LDFLAGS c-string "C_INSTALL_LDFLAGS")
(define-foreign-variable INSTALL_MORE_LIBS c-string "C_INSTALL_MORE_LIBS")
(define-foreign-variable INSTALL_MORE_STATIC_LIBS c-string "C_INSTALL_MORE_STATIC_LIBS")
(define-foreign-variable INSTALL_SHARE_HOME c-string "C_INSTALL_SHARE_HOME")
(define-foreign-variable INSTALL_LIB_HOME c-string "C_INSTALL_LIB_HOME")
(define-foreign-variable INSTALL_INCLUDE_HOME c-string "C_INSTALL_INCLUDE_HOME")
(define-foreign-variable INSTALL_STATIC_LIB_HOME c-string "C_INSTALL_STATIC_LIB_HOME")
(define-foreign-variable TARGET_MORE_LIBS c-string "C_TARGET_MORE_LIBS")
(define-foreign-variable TARGET_MORE_STATIC_LIBS c-string "C_TARGET_MORE_STATIC_LIBS")
(define-foreign-variable TARGET_BIN_HOME c-string "C_TARGET_BIN_HOME")
(define-foreign-variable TARGET_SHARE_HOME c-string "C_TARGET_SHARE_HOME")
(define-foreign-variable TARGET_LIB_HOME c-string "C_TARGET_LIB_HOME")
(define-foreign-variable TARGET_INCLUDE_HOME c-string "C_TARGET_INCLUDE_HOME")
(define-foreign-variable TARGET_STATIC_LIB_HOME c-string "C_TARGET_STATIC_LIB_HOME")
(define-foreign-variable TARGET_RUN_LIB_HOME c-string "C_TARGET_RUN_LIB_HOME")
(define-foreign-variable CHICKEN_PROGRAM c-string "C_CHICKEN_PROGRAM")
(define-foreign-variable WINDOWS_SHELL bool "C_WINDOWS_SHELL")


;;; Parameters:

(define mingw (eq? (build-platform) 'mingw32))
(define msvc (eq? (build-platform) 'msvc))
(define osx (eq? (software-version) 'macosx))
(define hpux-hppa (and (eq? (software-version) 'hpux)
                       (eq? (machine-type) 'hppa)))

(define (quit msg . args)
  (fprintf (current-error-port) "csc: ~?~%" msg args)
  (exit 64) )

(define chicken-prefix (get-environment-variable "CHICKEN_PREFIX"))
(define arguments (command-line-arguments))
(define host-mode (member "-host" arguments))
(define cross-chicken (##sys#fudge 39))

(define (prefix str dir default)
  (if chicken-prefix
      (make-pathname (list chicken-prefix dir) str)
      default) )

(define (quotewrap str)
  (qs (normalize-pathname str)))

(define home
  (quotewrap 
   (prefix "" "share" (if host-mode INSTALL_SHARE_HOME TARGET_SHARE_HOME))))

(define translator
  (quotewrap 
   (prefix "chicken" "bin"
	   (make-pathname
	    (if host-mode INSTALL_BIN_HOME TARGET_BIN_HOME)
	    CHICKEN_PROGRAM))))

(define compiler (quotewrap (if host-mode INSTALL_CC TARGET_CC)))
(define c++-compiler (quotewrap (if host-mode INSTALL_CXX TARGET_CXX)))
(define linker (quotewrap (if msvc "link" (if host-mode INSTALL_CC TARGET_CC))))
(define c++-linker (quotewrap (if msvc "link" (if host-mode INSTALL_CXX TARGET_CXX))))
(define object-extension (if msvc "obj" "o"))
(define library-extension (if msvc "lib" "a"))
(define link-output-flag (if msvc "-out:" "-o "))
(define executable-extension (if msvc "exe" ""))
(define compile-output-flag (if msvc "-Fo" "-o "))
(define nonstatic-compilation-options '())
(define shared-library-extension ##sys#load-dynamic-extension)
(define default-translation-optimization-options '())
(define pic-options (if (or mingw msvc) '("-DPIC") '("-fPIC" "-DPIC")))
(define windows-shell WINDOWS_SHELL)

(define default-library (string-append
                         (if msvc "libchicken-static." "libchicken.")
                         library-extension))
(define default-unsafe-library (string-append
                                (if msvc "libuchicken-static." "libuchicken.")
                                library-extension))

(define cleanup-filename quotewrap)

(define default-compilation-optimization-options (string-split (if host-mode INSTALL_CFLAGS TARGET_CFLAGS)))
(define best-compilation-optimization-options default-compilation-optimization-options)
(define default-linking-optimization-options (string-split (if host-mode INSTALL_LDFLAGS TARGET_LDFLAGS)))
(define best-linking-optimization-options default-linking-optimization-options)

(define-constant simple-options
  '(-explicit-use -no-trace -no-warnings -no-usual-integrations -optimize-leaf-routines -unsafe
    -block -disable-interrupts -fixnum-arithmetic -to-stdout -profile -raw -accumulate-profile
    -check-syntax -case-insensitive -benchmark-mode -shared -compile-syntax -no-lambda-info
    -lambda-lift -dynamic -disable-stack-overflow-checks -local
    -emit-external-prototypes-first -inline -release -scrutinize
    -analyze-only -keep-shadowed-macros -inline-global -ignore-repository
    -no-symbol-escape -no-parentheses-synonyms -r5rs-syntax
    -no-argc-checks -no-bound-checks -no-procedure-checks -no-compiler-syntax
    -no-procedure-checks-for-usual-bindings))

(define-constant complex-options
  '(-debug -output-file -heap-size -nursery -stack-size -compiler -unit -uses -keyword-style
    -optimize-level -include-path -database-size -extend -prelude -postlude -prologue -epilogue 
    -inline-limit -profile-name -disable-warning -emit-inline-file -types
    -feature -debug-level -heap-growth -heap-shrinkage -heap-initial-size -consult-inline-file
    -emit-import-library -static-extension))

(define-constant shortcuts
  '((-h "-help")
    (-s "-shared")
    (-S "-scrutinize")
    (|-P| "-check-syntax")
    (|-V| "-version")
    (|-Ob| "-benchmark-mode")
    (-f "-fixnum-arithmetic")
    (|-D| "-feature")
    (-i "-case-insensitive")
    (|-K| "-keyword-style")
    (|-X| "-extend")
    (|-N| "-no-usual-integrations")
    (-x "-explicit-use")
    (-u "-unsafe")
    (-j "-emit-import-library")
    (-n "-emit-inline-file")
    (-b "-block") ) )

(define short-options
  (string->list "PHhsfiENxubvwAOeWkctgS") )


;;; Variables:

(define scheme-files '())
(define generated-scheme-files '())
(define c-files '())
(define generated-c-files '())
(define object-files '())
(define generated-object-files '())
(define cpp-mode #f)
(define objc-mode #f)
(define embedded #f)
(define inquiry-only #f)
(define show-cflags #f)
(define show-ldflags #f)
(define show-libs #f)
(define dry-run #f)

(define extra-libraries
  (if host-mode
      INSTALL_MORE_STATIC_LIBS
      TARGET_MORE_STATIC_LIBS))
(define extra-shared-libraries 
  (if host-mode 
      INSTALL_MORE_LIBS
      TARGET_MORE_LIBS))
(define default-library-files 
  (list
   (quotewrap
    (prefix default-library "lib"
	    (string-append
	     (if host-mode INSTALL_LIB_HOME TARGET_LIB_HOME)
	     (string-append "/" default-library)))) ))
(define default-shared-library-files (if msvc
                                         (list (string-append "libchicken." library-extension))
                                         '("-lchicken")))
(define unsafe-library-files
  (list
   (quotewrap 
    (prefix default-unsafe-library "lib"
	    (string-append 
	     (if host-mode INSTALL_LIB_HOME TARGET_LIB_HOME)
	     (string-append "/" default-unsafe-library)))) ))
(define unsafe-shared-library-files (if msvc
                                        (list (string-append "libuchicken." library-extension))
                                        '("-luchicken")))
(define gui-library-files default-library-files)
(define gui-shared-library-files default-shared-library-files)
(define library-files default-library-files)
(define shared-library-files default-shared-library-files)

(define translate-options '())

(define include-dir
  (let ((id (prefix "" "include" 
		    (if host-mode INSTALL_INCLUDE_HOME TARGET_INCLUDE_HOME))))
    (and (not (member id '("/usr/include" "")))
	 id) ) )

(define compile-options '())
(define builtin-compile-options
  (if include-dir (list (conc "-I" (quotewrap include-dir))) '()))

(define compile-only-flag "-c")
(define translation-optimization-options default-translation-optimization-options)
(define compilation-optimization-options default-compilation-optimization-options)
(define linking-optimization-options default-linking-optimization-options)

(define library-dir
  (prefix "" "lib"
         (if host-mode
             INSTALL_LIB_HOME
             TARGET_LIB_HOME)) )

(define link-options '())
(define builtin-link-options
  (cond ((or osx hpux-hppa mingw)
	 (list (conc "-L" (quotewrap library-dir))))
        (msvc
         (list (conc "-LIBPATH:" (quotewrap library-dir))))
	(else 
	 (list
	  (conc "-L" (quotewrap library-dir))
	  (conc " -Wl,-R" (quotewrap (prefix "" "lib"
					     (if host-mode
						 INSTALL_LIB_HOME
						 TARGET_RUN_LIB_HOME)))) ) ) ) )

(define target-filename #f)
(define verbose #f)
(define keep-files #f)
(define translate-only #f)
(define compile-only #f)
(define to-stdout #f)
(define shared #f)
(define static #f)
(define static-libs #f)
(define static-extensions '())
(define required-extensions '())
(define gui #f)


;;; Display usage information:

(define (usage)
  (display #<<EOF
Usage: csc FILENAME | OPTION ...

  `csc' is a driver program for the CHICKEN compiler. Files given on the
  command line are translated, compiled or linked as needed.

  FILENAME is a Scheme source file name with optional extension or a
  C/C++/Objective-C source, object or library file name with extension. OPTION
  may be one of the following:

  General options:

    -h  -help                      display this text and exit
    -v                             show intermediate compilation stages
    -vv  -verbose                  display information about translation
                                    progress
    -vvv                           display information about all compilation
                                    stages
    -V  -version                   display Scheme compiler version and exit
    -release                       display release number and exit

  File and pathname options:

    -o -output-file FILENAME       specifies target executable name
    -I -include-path PATHNAME      specifies alternative path for included
                                    files
    -to-stdout                     write compiler to stdout (implies -t)
    -s -shared -dynamic            generate dynamically loadable shared object
                                    file

  Language options:

    -D  -DSYMBOL  -feature SYMBOL  register feature identifier
    -c++                           compile via a C++ source file (.cpp) 
    -objc                          compile via Objective-C source file (.m)

  Syntax related options:

    -i -case-insensitive           don't preserve case of read symbols    
    -k  -keyword-style STYLE       enable alternative keyword-syntax
                                    (prefix, suffix or none)
        -no-parentheses-synonyms   disables list delimiter synonyms
        -no-symbol-escape          disables support for escaped symbols
        -r5rs-syntax               disables the Chicken extensions to
                                    R5RS syntax
    -compile-syntax                macros are made available at run-time
    -j -emit-import-library MODULE write compile-time module information into
                                    separate file
    -no-compiler-syntax            disable expansion of compiler-macros

  Translation options:

    -x  -explicit-use              do not use units `library' and `eval' by
                                    default
    -P  -check-syntax              stop compilation after macro-expansion
    -A  -analyze-only              stop compilation after first analysis pass

  Debugging options:

    -w  -no-warnings               disable warnings
    -disable-warning CLASS         disable specific class of warnings
    -d0 -d1 -d2 -debug-level NUMBER
                                   set level of available debugging information
    -no-trace                      disable rudimentary debugging information
    -profile                       executable emits profiling information 
    -accumulate-profile            executable emits profiling information in
                                    append mode
    -profile-name FILENAME         name of the generated profile information
                                    file
    -S  -scrutinize                perform local flow analysis
    -types FILENAME                load additional type database

  Optimization options:

    -O -O1 -O2 -O3 -O4 -optimize-level NUMBER
                                   enable certain sets of optimization options
    -optimize-leaf-routines        enable leaf routine optimization
    -N  -no-usual-integrations     standard procedures may be redefined
    -u  -unsafe                    disable safety checks
    -local                         assume globals are only modified in current
                                    file
    -b  -block                     enable block-compilation
    -disable-interrupts            disable interrupts in compiled code
    -f  -fixnum-arithmetic         assume all numbers are fixnums
    -Ob  -benchmark-mode           equivalent to '-block -optimize-level 4
                                    -debug-level 0 -fixnum-arithmetic
                                    -lambda-lift -inline -disable-interrupts'
    -lambda-lift                   perform lambda-lifting
    -unsafe-libraries              link with unsafe runtime system
    -disable-stack-overflow-checks disables detection of stack-overflows
    -inline                        enable inlining
    -inline-limit                  set inlining threshold
    -inline-global                 enable cross-module inlining
    -n -emit-inline-file FILENAME  generate file with globally inlinable
                                    procedures (implies -inline -local)
    -consult-inline-file FILENAME  explicitly load inline file
    -no-argc-checks                disable argument count checks
    -no-bound-checks               disable bound variable checks
    -no-procedure-checks           disable procedure call checks
    -no-procedure-checks-for-usual-bindings
                                   disable procedure call checks only for usual
                                    bindings

  Configuration options:

    -unit NAME                     compile file as a library unit
    -uses NAME                     declare library unit as used.
    -heap-size NUMBER              specifies heap-size of compiled executable
    -heap-initial-size NUMBER      specifies heap-size at startup time
    -heap-growth PERCENTAGE        specifies growth-rate of expanding heap
    -heap-shrinkage PERCENTAGE     specifies shrink-rate of contracting heap
    -nursery NUMBER  -stack-size NUMBER
                                   specifies nursery size of compiled
                                   executable
    -X -extend FILENAME            load file before compilation commences
    -prelude EXPRESSION            add expression to beginning of source file
    -postlude EXPRESSION           add expression to end of source file
    -prologue FILENAME             include file before main source file
    -epilogue FILENAME             include file after main source file

    -e  -embedded                  compile as embedded
                                    (don't generate `main()')
    -W  -windows                   compile as Windows GUI application
                                    (MSVC only)
    -R  -require-extension NAME    require extension and import in compiled
                                    code
    -dll -library                  compile multiple units into a dynamic
                                    library

  Options to other passes:

    -C OPTION                      pass option to C compiler
    -L OPTION                      pass option to linker
    -I<DIR>                        pass \"-I<DIR>\" to C compiler
                                    (add include path)
    -L<DIR>                        pass \"-L<DIR>\" to linker
                                    (add library path)
    -k                             keep intermediate files
    -c                             stop after compilation to object files
    -t                             stop after translation to C
    -cc COMPILER                   select other C compiler than the default
    -cxx COMPILER                  select other C++ compiler than the default
    -ld COMPILER                   select other linker than the default 
    -lLIBNAME                      link with given library
                                    (`libLIBNAME' on UNIX,
                                     `LIBNAME.lib' on Windows)
    -static-libs                   link with static CHICKEN libraries
    -static                        generate completely statically linked
                                    executable
    -static-extension NAME         link extension NAME statically
                                    (if available)
    -F<DIR>                        pass \"-F<DIR>\" to C compiler
                                    (add framework header path on Mac OS X)
    -framework NAME                passed to linker on Mac OS X
    -rpath PATHNAME                add directory to runtime library search path
    -Wl,...                        pass linker options
    -strip                         strip resulting binary

  Inquiry options:

    -home                          show home-directory (where support files go)
    -cflags                        show required C-compiler flags and exit
    -ldflags                       show required linker flags and exit
    -libs                          show required libraries and exit
    -cc-name                       show name of default C compiler used
    -cxx-name                      show name of default C++ compiler used
    -ld-name                       show name of default linker used
    -dry-run                       just show commands executed, don't run them
                                    (implies `-v')

  Obscure options:

    -debug MODES                   display debugging output for the given modes
    -compiler PATHNAME             use other compiler than default `chicken'
    -disable-c-syntax-checks       disable syntax checks of C code fragments
    -raw                           do not generate implicit init- and exit code
    -emit-external-prototypes-first
                                   emit prototypes for callbacks before foreign
                                    declarations
    -ignore-repository             do not refer to repository for extensions
    -keep-shadowed-macros          do not remove shadowed macro
    -host                          compile for host when configured for
                                    cross-compiling

  Options can be collapsed if unambiguous, so

    -vkfO

  is the same as

    -v -k -fixnum-arithmetic -optimize

  The contents of the environment variable CSC_OPTIONS are implicitly passed to
  every invocation of `csc'.

EOF
) )


;;; Parse arguments:

(define (run args)

  (define (t-options . os)
    (set! translate-options (append translate-options os)) )

  (define (check o r . n)
    (unless (>= (length r) (optional n 1))
      (quit "not enough arguments to option `~A'" o) ) )

  (define (shared-build lib)
    (set! translate-options (cons* "-feature" "chicken-compile-shared" translate-options))
    (set! compile-options (append pic-options '("-DC_SHARED") compile-options))
    (set! link-options
      (cons (cond
             (osx (if lib "-dynamiclib" "-bundle"))
             (msvc "-dll")
             (else "-shared")) link-options))
    (set! shared #t) )

  (let loop ([args args])
    (cond [(null? args)
           ;Builtin search directory options do not override explict options
           (set! compile-options (append compile-options builtin-compile-options))
           (set! link-options (append link-options builtin-link-options))
           ;
	   (when inquiry-only
	     (when show-cflags (print* (compiler-options) #\space))
	     (when show-ldflags (print* (linker-options) #\space))
	     (when show-libs (print* (linker-libraries #t) #\space))
	     (newline)
	     (exit) )
	   #; ;UNUSED
	   (when (null? scheme-files)
	     (set! scheme-files c-files)
	     (set! c-files '()) )
	   (cond [(null? scheme-files)
		  (when (and (null? c-files) (null? object-files))
		    (quit "no source files specified") )
		  (let ((f0 (last (if (null? c-files) object-files c-files))))
		    (unless target-filename
		      (set! target-filename 
			(if shared
			    (pathname-replace-extension f0 shared-library-extension)
			    (pathname-replace-extension f0 executable-extension) ) ) ) ) ]
		 [else
		  (when (and shared (not embedded))
		    (set! translate-options (cons "-dynamic" translate-options)) )
		  (unless target-filename
		    (set! target-filename
		      (if shared
			  (pathname-replace-extension (first scheme-files) shared-library-extension)
			  (pathname-replace-extension (first scheme-files) executable-extension) ) ) )
		  (run-translation) ] )
	   (unless translate-only 
	     (run-compilation)
	     (unless compile-only
	       (when (member target-filename scheme-files)
		 (printf "Warning: output file will overwrite source file `~A' - renaming source to `~A.old'~%"
			 target-filename target-filename)
		 (unless (zero? ($system (sprintf "~A ~A ~A" 
						  (if *windows-shell* "move" "mv")
						  (quotewrap target-filename)
						  (quotewrap (string-append target-filename ".old")))))
		   (exit last-exit-code) ) )
	       (run-linking)) ) ]
	  [else
	   (let* ([arg (car args)]
		  [rest (cdr args)]
		  [s (string->symbol arg)] )
	     (case s
	       [(-help --help)
		(usage)
		(exit) ]
	       [(-release)
		(print (chicken-version))
		(exit) ]
	       [(-version)
		(system (sprintf translator " -version"))
		(exit) ]
	       [(-c++) 
		(set! cpp-mode #t)
		(when osx (set! compile-options (cons "-no-cpp-precomp" compile-options))) ]
	       [(-objc) 
		(set! objc-mode #t) ]
	       [(-static) 
		(set! translate-options (cons* "-feature" "chicken-compile-static" translate-options))
		(set! static #t) ]
	       [(-static-libs) 
		(set! translate-options (cons* "-feature" "chicken-compile-static" translate-options))
		(set! static-libs #t) ]
	       [(-cflags)
		(set! inquiry-only #t) 
		(set! show-cflags #t) ]
	       [(-ldflags)
		(set! inquiry-only #t)
		(set! show-ldflags #t) ]
	       [(-cc-name) (print compiler) (exit 0)]
	       [(-cxx-name) (print c++-compiler) (exit 0)]
	       [(-ld-name) (print linker) (exit 0)]
	       [(-home) (print home) (exit 0)]
	       [(-libs)
		(set! inquiry-only #t)
		(set! show-libs #t) ]
	       [(-v)
		(when (and (number? verbose) (not msvc))
		  (set! compile-options (cons* "-v" "-Q" compile-options))
		  (set! link-options (cons (if msvc "-VERBOSE" "-v") link-options)) )
		(cond (verbose
		       (t-options "-verbose") 
		       (set! verbose 2)) 
		      (else (set! verbose #t))) ]
	       [(-v2 -verbose)		; DEPRECATED
		(set! verbose #t)
		(t-options "-verbose") ]
	       [(-v3)			; DEPRECATED
		(set! verbose #t)
		(t-options "-verbose")
                (if (not msvc)
                    (set! compile-options (cons* "-v" "-Q" compile-options)))
		(set! link-options (cons (if msvc "-VERBOSE" "-v") link-options)) ]
	       [(-w -no-warnings)
		(set! compile-options (cons "-w" compile-options))
		(t-options "-no-warnings") ]
	       [(|-A| -analyze-only)
		(set! translate-only #t)
		(t-options "-analyze-only") ]
	       [(|-P| -check-syntax)
		(set! translate-only #t)
		(t-options "-check-syntax") ]
	       [(-k) (set! keep-files #t)]
	       [(-c) (set! compile-only #t)]
	       [(-t) (set! translate-only #t)]
	       [(-e -embedded)
		(set! embedded #t)
		(set! compile-options (cons "-DC_EMBEDDED" compile-options)) ]
	       [(-require-extension -R)
		(check s rest)
		(set! required-extensions (append required-extensions (list (car rest))))
		(t-options "-require-extension" (car rest))
		(set! rest (cdr rest)) ]
	       [(-static-extension)
		(check s rest)
		(set! static-extensions (append static-extensions (list (car rest))))
		(t-options "-static-extension" (car rest))
		(set! rest (cdr rest)) ]
	       [(-windows |-W|)
		(set! gui #t)
		(cond
                 (mingw
		  (set! link-options
		    (cons* "-lkernel32" "-luser32" "-lgdi32" "-mwindows"
			   link-options))
		  (set! compile-options (cons "-DC_WINDOWS_GUI" compile-options)))
                 (msvc
                  (set! link-options
                    (cons* "kernel32.lib" "user32.lib" "gdi32.lib" link-options))
		  (set! compile-options (cons "-DC_WINDOWS_GUI" compile-options)))) ]
	       [(-framework)
		(check s rest)
		(when osx 
		  (set! link-options (cons* "-framework" (car rest) link-options)) )
		(set! rest (cdr rest)) ]
	       [(-o)
		(check s rest)
		(let ([fn (car rest)])
		  (set! rest (cdr rest))
		  (set! target-filename fn) ) ]
	       [(|-O| |-O1|) (set! rest (cons* "-optimize-level" "1" rest))]
	       [(|-O2|) (set! rest (cons* "-optimize-level" "2" rest))]
	       [(|-O3|) (set! rest (cons* "-optimize-level" "3" rest))]
	       [(|-O4|) (set! rest (cons* "-optimize-level" "4" rest))]
	       [(-d0) (set! rest (cons* "-debug-level" "0" rest))]
	       [(-d1) (set! rest (cons* "-debug-level" "1" rest))]
	       [(-d2) (set! rest (cons* "-debug-level" "2" rest))]
	       [(-dry-run) 
		(set! verbose #t)
		(set! dry-run #t)]
	       [(-s -shared -dynamic)
		(shared-build #f) ]
	       [(-dll -library)
		(shared-build #t) ]
	       [(-compiler)
		(check s rest)
		(set! translator (car rest))
		(set! rest (cdr rest)) ]
	       [(-cc)
		(check s rest)
		(set! compiler (car rest))
		(set! rest (cdr rest)) ]
	       [(-cxx)
		(check s rest)
		(set! c++-compiler (car rest))
		(set! rest (cdr rest)) ]
	       [(-ld)
		(check s rest)
		(set! linker (car rest))
		(set! rest (cdr rest)) ]
	       [(|-I|)
		(check s rest)
		(set! rest (cons* "-include-path" (car rest) (cdr rest))) ]
	       [(|-C|)
		(check s rest)
		(set! compile-options (append compile-options (string-split (car rest))))
		(set! rest (cdr rest)) ]
	       [(-strip)
		(set! link-options (append link-options (list "-s")))]
	       [(|-L|)
		(check s rest)
		(set! link-options (append link-options (string-split (car rest))))
		(set! rest (cdr rest)) ]
	       [(-unsafe-libraries)
		(t-options arg)
		(set! library-files unsafe-library-files)
		(set! shared-library-files unsafe-shared-library-files) ]
	       [(-rpath)
		(check s rest)
		(when (eq? 'gnu (build-platform))
		  (set! link-options (append link-options (list (string-append "-Wl,-R" (car rest)))))
		  (set! rest (cdr rest)) ) ]
	       [(-host) #f]
	       [(-) 
		(set! target-filename (make-pathname #f "a" executable-extension))
		(set! scheme-files (append scheme-files '("-")))]
	       [else
		(when (memq s '(-unsafe -benchmark-mode))
		  (when (eq? s '-benchmark-mode)
		    (set! library-files unsafe-library-files)
		    (set! shared-library-files unsafe-shared-library-files) ) )
		(when (eq? s '-to-stdout) 
		  (set! to-stdout #t)
		  (set! translate-only #t) )
		(when (memq s '(-optimize-level -benchmark-mode))
		  (set! compilation-optimization-options best-compilation-optimization-options)
		  (set! linking-optimization-options best-linking-optimization-options) )
		(cond [(assq s shortcuts) => (lambda (a) (set! rest (cons (cadr a) rest)))]
		      [(memq s simple-options) (t-options arg)]
		      [(memq s complex-options) 
		       (check s rest)
		       (let* ([n (car rest)]
			      [ns (string->number n)] )
			 (t-options arg n)
			 (set! rest (cdr rest)) ) ]
		      [(and (> (string-length arg) 2) (string=? "-:" (substring arg 0 2)))
		       (t-options arg) ]
		      [(and (> (string-length arg) 1)
			    (char=? #\- (string-ref arg 0)) )
		       (cond [(char=? #\l (string-ref arg 1))
			      (set! link-options (append link-options (list arg))) ]
 			     [(char=? #\L (string-ref arg 1))
 			      (set! link-options (append link-options (list arg))) ]
 			     [(char=? #\I (string-ref arg 1))
 			      (set! compile-options (append compile-options (list arg))) ]
			     [(char=? #\D (string-ref arg 1))
			      (t-options "-feature" (substring arg 2)) ]
			     [(char=? #\F (string-ref arg 1))
			      (when osx
				(set! compile-options (append compile-options (list arg))) ) ]
			     [(and (> (string-length arg) 3) (string=? "-Wl," (substring arg 0 4)))
			      (set! link-options (append link-options (list arg))) ]
			     [(> (string-length arg) 2)
 			      (let ([opts (cdr (string->list arg))])
 				(if (null? (lset-difference char=? opts short-options))
 				    (set! rest
 				      (append (map (lambda (o) (string-append "-" (string o))) opts) rest) )
 				    (quit "invalid option `~A'" arg) ) ) ]
			     [else (quit "invalid option `~A'" s)] ) ]
		      [(file-exists? arg)
		       (let-values ([(dirs name ext) (decompose-pathname arg)])
			 (cond [(not ext) (set! scheme-files (append scheme-files (list arg)))]
			       [(member ext '("h" "c"))
				(set! c-files (append c-files (list arg))) ]
			       [(member ext '("cpp" "C" "cc" "cxx" "hpp"))
				(when osx (set! compile-options (cons "-no-cpp-precomp" compile-options)))
				(set! cpp-mode #t)
				(set! c-files (append c-files (list arg))) ]
			       [(member ext '("m" "M" "mm"))
				(set! objc-mode #t)
				(set! c-files (append c-files (list arg))) ]
			       [(or (string=? ext object-extension)
				    (string=? ext library-extension) )
				(set! object-files (append object-files (list arg))) ]
			       [else (set! scheme-files (append scheme-files (list arg)))] ) ) ]
		      [else
		       (let ([f2 (string-append arg ".scm")])
			 (if (file-exists? f2)
			     (set! rest (cons f2 rest))
			     (quit "file `~A' does not exist" arg) ) ) ] ) ] )
	     (loop rest) ) ] ) ) )


;;; Translate all Scheme files:

(define (run-translation)
  (for-each
   (lambda (f)
     (let ([fc (pathname-replace-extension
		(if (= 1 (length scheme-files))
		    target-filename
		    f)
		(cond (cpp-mode "cpp")
		      (objc-mode "m")
		      (else "c") ) ) ] )
       (unless (zero?
		($system 
		 (string-intersperse 
		  (cons* translator (cleanup-filename f) 
			 (append 
			  (if to-stdout 
			      '("-to-stdout")
			      `("-output-file" ,(cleanup-filename fc)) )
			  (map quote-option (append translate-options translation-optimization-options)) ) )
		  " ") ) )
	 (exit last-exit-code) )
       (set! c-files (append (list fc) c-files))
       (set! generated-c-files (append (list fc) generated-c-files))))
   scheme-files)
  (unless keep-files (for-each $delete-file generated-scheme-files)) )


;;; Compile all C files:

(define (run-compilation)
  (let ((ofiles '()))
    (for-each
     (lambda (f)
       (let ([fo (pathname-replace-extension f object-extension)])
	 (unless (zero?
		  ($system
		   (string-intersperse
		    (list (cond (cpp-mode c++-compiler)
				(else compiler) )
			  (cleanup-filename f)
			  (string-append compile-output-flag (cleanup-filename fo)) 
			  compile-only-flag
			  (compiler-options) ) ) ) )
	   (exit last-exit-code) )
	 (set! generated-object-files (cons fo generated-object-files))
	 (set! ofiles (cons fo ofiles))))
     c-files)
    (set! object-files (append (reverse ofiles) object-files)) ; put generated object files first
    (unless keep-files (for-each $delete-file generated-c-files)) ) )

(define (compiler-options)
  (string-intersperse
   (map quote-option
	(append
	 (if (or static static-libs) '() nonstatic-compilation-options)
	 compilation-optimization-options
	 compile-options) ) ) )


;;; Link object files and libraries:

(define (run-linking)
  (let ((files (map cleanup-filename
		    (append object-files
			    (nth-value 0 (static-extension-info)) ) ) )
	(target (cleanup-filename target-filename)))
    (unless (zero?
	     ($system
	      (string-intersperse 
	       (cons* (cond (cpp-mode c++-linker)
			    (else linker) )
		      (append
		       files
		       (list (string-append link-output-flag target)
			     (linker-options)
			     (linker-libraries #f) ) ) ) ) ) )
      (exit last-exit-code) )
    (when (and osx (or (not cross-chicken) host-mode))
      (unless (zero? ($system 
		      (string-append
		       "install_name_tool -change libchicken.dylib "
		       (quotewrap 
			(make-pathname
			 (prefix "" "lib"
				 (if host-mode
				     INSTALL_LIB_HOME
				     TARGET_RUN_LIB_HOME))
			 "libchicken.dylib") )
		       " " 
		       target) ) )
	(exit last-exit-code) ) )
    (unless keep-files (for-each $delete-file generated-object-files)) ) )

(define (static-extension-info)
  (let ((rpath (repository-path)))
    (if (and rpath (pair? static-extensions))
	(let loop ((exts static-extensions) (libs '()) (opts '()))
	  (if (null? exts)
	      (values (reverse libs) (reverse opts))
	      (let ((info (extension-information (car exts))))
		(if info
		    (let ((a (assq 'static info)) 
			  (o (assq 'static-options info)) )
		      (loop (cdr exts) 
			(if a (cons (make-pathname rpath (cadr a)) libs) libs)
			(if o (cons (cadr o) opts) opts) ) ) 
		    (loop (cdr exts) libs opts)) ) ) )
	(values '() '()) ) ) )

(define (linker-options)
  (string-append
   (string-intersperse
    (append linking-optimization-options link-options
	    (nth-value 1 (static-extension-info)) ) )
   (if (and static (not mingw) (not msvc) (not osx)) " -static" "") ) )

(define (linker-libraries #!optional staticexts)
  (string-intersperse
   (append
    (if staticexts (nth-value 0 (static-extension-info)) '())
    (if (or static static-libs)
        (if gui gui-library-files library-files)
        (if gui gui-shared-library-files shared-library-files))
    (if (or static static-libs)
        (list extra-libraries)
        (list extra-shared-libraries)))))


;;; Helper procedures:

(define-constant +hairy-chars+ '(#\\ #\#))

(define (cleanup s)
  (let* ((q #f)
	 (s (list->string
	     (let fold ([s (string->list s)])
	       (if (null? s) 
		   '()
		   (let ([c (car s)])
		     (cond ((memq c +hairy-chars+) (cons* #\\ c (fold (cdr s))))
			   (else
			    (when (char-whitespace? c) (set! q #t))
			    (cons c (fold (cdr s))) ) ) ) ) ) ) ) )
    (if q 
	(string-append "\"" (string-translate* s '(("\"" . "\\\""))) "\"")
	s) ) )

(define (quote-option x)
  (if (string-any (lambda (c)
		    (or (char-whitespace? c) (memq c +hairy-chars+)) )
		  x)
      (cleanup x)
      x) )

(define last-exit-code #f)

(define ($system str)
  (when verbose (print str))
  (let ((str (if windows-shell
		 (string-append "\"" str "\"")
		 str)))
    (set! last-exit-code
      (if dry-run 
	  0
	  (system str)))
    (unless (zero? last-exit-code)
      (printf "\nError: shell command terminated with non-zero exit status ~S: ~A~%" last-exit-code str) )
    last-exit-code))

(define ($delete-file str)
  (when verbose 
    (print "rm " str) )
  (unless dry-run (delete-file str) ))


;;; Run it:

(run (append (string-split (or (get-environment-variable "CSC_OPTIONS") "")) arguments))
