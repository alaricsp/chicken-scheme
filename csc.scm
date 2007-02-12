;;;; csc.scm - Driver program for the CHICKEN compiler - felix -*- Hen -*-
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
  (block)
  (uses extras srfi-1 srfi-13 regex utils))

#>
#include "chicken-defaults.h"

#ifndef C_TARGET_CC
# define C_TARGET_CC  C_INSTALL_CC
#endif

#ifndef C_TARGET_CXX
# define C_TARGET_CXX  C_INSTALL_CXX
#endif

#ifndef C_TARGET_CFLAGS
# define C_TARGET_CFLAGS  C_INSTALL_CFLAGS
#endif

#ifndef C_TARGET_BIN_HOME
# define C_TARGET_BIN_HOME  C_INSTALL_BIN_HOME
#endif

#ifndef C_TARGET_LIB_HOME
# define C_TARGET_LIB_HOME  C_INSTALL_LIB_HOME
#endif

#ifndef C_TARGET_STATIC_LIB_HOME
# define C_TARGET_STATIC_LIB_HOME  C_INSTALL_STATIC_LIB_HOME
#endif

#ifndef C_TARGET_INCLUDE_HOME
# define C_TARGET_INCLUDE_HOME  C_INSTALL_INCLUDE_HOME
#endif

#ifndef C_TARGET_SHARE_HOME
# define C_TARGET_SHARE_HOME  C_INSTALL_SHARE_HOME
#endif
<#

(define-foreign-variable INSTALL_BIN_HOME c-string "C_INSTALL_BIN_HOME")
(define-foreign-variable INSTALL_CC c-string "C_INSTALL_CC")
(define-foreign-variable INSTALL_CXX c-string "C_INSTALL_CXX")
(define-foreign-variable TARGET_CC c-string "C_TARGET_CC")
(define-foreign-variable TARGET_CXX c-string "C_TARGET_CXX")
(define-foreign-variable TARGET_CFLAGS c-string "C_TARGET_CFLAGS")
(define-foreign-variable INSTALL_CFLAGS c-string "C_INSTALL_CFLAGS")
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


;;; Parameters:

(define win (eq? (build-platform) 'msvc))
(define cygwin (eq? (build-platform) 'cygwin))
(define mingw (eq? (build-platform) 'mingw32))
(define osx (eq? (software-version) 'macosx))
(define hpux-hppa (and (eq? (software-version) 'hpux)
                       (eq? (machine-type) 'hppa)))

(define (quit msg . args)
  (fprintf (current-error-port) "csc: ~?~%" msg args)
  (exit 64) )

(define chicken-prefix (getenv "CHICKEN_PREFIX"))
(define cmake-build (##sys#fudge 38))
(define arguments (command-line-arguments))
(define host-mode (member "-host" arguments))

(define (prefix str dir default)
  (if chicken-prefix
      (make-pathname (list chicken-prefix dir) str)
      default) )

(define home
  (or (getenv "CHICKEN_HOME") 
      (if (and win (not cmake-build))
	  (quit "`CHICKEN_HOME' environment variable not set - please set it to the directory where CHICKEN is installed")
	  (quotewrap (prefix "" "share" (if host-mode INSTALL_SHARE_HOME TARGET_SHARE_HOME)))
      )
  )
)

(define (homize str) (make-pathname home str))

(define (quotewrap str)
  (if (string-any char-whitespace? str)
      (string-append "\"" str "\"") 
      str) )

(define translator
  (if (and win (not cmake-build))
      (quotewrap (homize "chicken"))
      (quotewrap 
       (prefix "chicken" "bin"
	       (make-pathname
		(if host-mode INSTALL_BIN_HOME TARGET_BIN_HOME)
		"chicken")))))

(if win
    (begin
      (define compiler "cl")
      (define c++-compiler "cl")
      (define linker "link")
      (define c++-linker "link")
      (define object-extension "obj")
      (define library-extension "lib")
      (define link-output-flag "/out:")
      (define compile-output-flag "/Fo")
      (define executable-extension "exe")
      (define shared-library-extension "dll")
      (define nonstatic-compilation-options '("/DPIC")) )
    (begin
      (define compiler (quotewrap (if host-mode INSTALL_CC TARGET_CC)))
      (define c++-compiler (quotewrap (if host-mode INSTALL_CXX TARGET_CXX)))
      (define linker (quotewrap (if host-mode INSTALL_CC TARGET_CC)))
      (define c++-linker (quotewrap (if host-mode INSTALL_CXX TARGET_CXX)))
      (define object-extension "o")
      (define library-extension "a")
      (define link-output-flag "-o ")
      (define executable-extension "")
      (define compile-output-flag "-o ")
      (define nonstatic-compilation-options '())
      (define shared-library-extension (cond ((or cygwin mingw) "dll")
                                             ;(hpux-hppa "sl")
                                             (else "so")))))

(define default-translation-optimization-options '())

(if win
    (begin
      (define (cleanup-filename s) (string-translate s "/" "\\")) ; we need this to please the MSVC tools
      (define default-compilation-optimization-options '("/nologo"))
      (define default-linking-optimization-options '("/nologo"))
      (define best-linking-optimization-options '("/nologo"))
      (define best-compilation-optimization-options '("/O2" "/nologo")) )
    (begin
      (define (cleanup-filename s) s)
      (define default-compilation-optimization-options (string-split (if host-mode INSTALL_CFLAGS TARGET_CFLAGS)))
      (define best-compilation-optimization-options default-compilation-optimization-options)
      (define default-linking-optimization-options '())
      (define best-linking-optimization-options '()) ) )

(define-constant simple-options
  '(-explicit-use -no-trace -no-warnings -no-usual-integrations -optimize-leaf-routines -unsafe
    -block -disable-interrupts -fixnum-arithmetic -to-stdout -profile -raw -accumulate-profile
    -check-syntax -case-insensitive -benchmark-mode -shared -run-time-macros -no-lambda-info
    -lambda-lift -dynamic -disable-stack-overflow-checks -emit-debug-info -check-imports
    -emit-external-prototypes-first -inline -extension -release -static-extensions
    -analyze-only -keep-shadowed-macros) )

(define-constant complex-options
  '(-debug -output-file -heap-size -nursery -stack-size -compiler -unit -uses -keyword-style
    -optimize-level -include-path -database-size -extend -prelude -postlude -prologue -epilogue 
    -inline-limit -profile-name -disable-warning -import -require-static-extension
    -feature -debug-level -heap-growth -heap-shrinkage -heap-initial-size -emit-exports
    -compress-literals) )

(define-constant shortcuts
  '((-h "-help")
    (-s "-shared")
    (|-E| "-extension")
    (|-P| "-check-syntax")
    (|-V| "-version")
    (|-Ob| "-benchmark-mode")
    (-f "-fixnum-arithmetic")
    (|-D| "-feature")
    (-i "-case-insensitive")
    (|-K| "-keyword-style")
    (|-X| "-extend")
    (|-N| "-no-usual-integrations")
    (|-G| "-check-imports")
    (-x "-explicit-use")
    (-u "-unsafe")
    (-b "-block") ) )

(define short-options
  (string->list "PHhsfiENxubvwAOeWkctgG") )


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

(if win
    (begin
      (define extra-libraries "")
      (define extra-shared-libraries "")
      (if (not cmake-build)
        (begin				; flat destination directory
          (define default-library-files
	    (map homize '("libchicken-static.lib")))
          (define default-shared-library-files
	    (map homize '("libchicken.lib")))
          (define unsafe-library-files
	    (map homize '("libuchicken-static.lib")))
          (define unsafe-shared-library-files
	    (map homize '("libuchicken.lib")))
          (define gui-library-files
	    (map homize '("libchickengui-static.lib")))
          (define gui-shared-library-files
	    (map homize '("libchickengui.lib")))
        )
        (begin
          (define default-library-files 
	    (list
	     (quotewrap 
	      (prefix "libchicken-s.lib" "lib"
		      (string-append
		       (if host-mode INSTALL_LIB_HOME TARGET_LIB_HOME)
		       "\\libchicken-s.lib"))))
	    )
	  (define default-shared-library-files (list
            (quotewrap 
	     (prefix "libchicken.lib" "lib"
		     (string-append
		      (if host-mode INSTALL_LIB_HOME TARGET_LIB_HOME)
		      "\\libchicken.lib")))
          ))
          (define unsafe-library-files (list
            (quotewrap
	     (prefix "libuchicken-s.lib" "lib"
		     (string-append
		      (if host-mode INSTALL_LIB_HOME TARGET_LIB_HOME)
		      "\\libuchicken-s.lib")))
          ))
          (define unsafe-shared-library-files (list
            (quotewrap
	     (prefix "libuchicken.lib" "lib"
		     (string-append
		      (if host-mode INSTALL_LIB_HOME TARGET_LIB_HOME)
		      "\\libuchicken.lib")))
          ))
          (define gui-library-files (list
            (quotewrap 
	     (prefix "libchickengui-s.lib" "lib"
		     (string-append
		      (if host-mode INSTALL_LIB_HOME TARGET_LIB_HOME)
		      "\\libchickengui-s.lib")))
          ))
          (define gui-shared-library-files (list
            (quotewrap
	     (prefix "libchickengui.lib" "lib"
		     (string-append
		      (if host-mode INSTALL_LIB_HOME TARGET_LIB_HOME)
		      "\\libchickengui.lib")))
          ))
        )
      )
    )
    (begin
      ; will more libs ever need quotewrapping?
      (define extra-libraries
	(if host-mode
	    INSTALL_MORE_STATIC_LIBS
	    TARGET_MORE_STATIC_LIBS))
      (define extra-shared-libraries 
	(if host-mode 
	    INSTALL_MORE_LIBS
	    TARGET_MORE_LIBS))
      (define default-library-files (list
        (quotewrap
	 (prefix "libchicken.a" "lib"
		 (string-append
		  (if host-mode INSTALL_LIB_HOME TARGET_LIB_HOME)
		  "/libchicken.a")))
      ))
      (define default-shared-library-files '("-lchicken"))
      (define unsafe-library-files (list
        (quotewrap 
	 (prefix "libuchicken.a" "lib"
		 (string-append 
		  (if host-mode INSTALL_LIB_HOME TARGET_LIB_HOME)
		  "/libuchicken.a")))
      ))
      (define unsafe-shared-library-files '("-luchicken"))
      (define gui-library-files default-library-files)
      (define gui-shared-library-files default-shared-library-files)
    )
)

(define library-files default-library-files)
(define shared-library-files default-shared-library-files)

(define translate-options '("-quiet"))

(define include-dir
  (let ((id (prefix "" "include" 
		    (if host-mode INSTALL_INCLUDE_HOME TARGET_INCLUDE_HOME))))
    (and (not (member id '("/usr/include" "")))
	 id) ) )

(define compile-options
  (if win
      (if (not cmake-build)
	  (cons* "/I%CHICKEN_HOME%" "/DC_NO_PIC_NO_DLL" (if (eq? (c-runtime) 'dynamic) '("/MD") '()))
	  (cons* (string-append 
		  "/I" 
		  (if host-mode INSTALL_INCLUDE_HOME TARGET_INCLUDE_HOME)
		 "/DC_NO_PIC_NO_DLL" (if (eq? (c-runtime) 'dynamic) '("/MD") '())) ) )
      (if include-dir (list "-I" include-dir) '())) )

(define compile-only-flag
  (if win "/c" "-c") )

(define translation-optimization-options default-translation-optimization-options)
(define compilation-optimization-options default-compilation-optimization-options)
(define linking-optimization-options default-linking-optimization-options)

(define link-options
  (cond [win '()]
	[(or osx hpux-hppa) 
	 (list (conc "-L" (quotewrap
			   (prefix "" "lib"
				   (if host-mode 
				       INSTALL_LIB_HOME
				       TARGET_LIB_HOME)) )))]
	[else 
	 (let ((p (quotewrap (prefix "" "lib"
				     (if host-mode
					 INSTALL_LIB_HOME
					 TARGET_LIB_HOME)))))
	   (list (conc "-L" p " -Wl,-R" p) ) ) ] ) )

(define target-filename #f)
(define verbose #f)
(define keep-files #f)
(define translate-only #f)
(define compile-only #f)
(define to-stdout #f)
(define shared #f)
(define static #f)
(define static-libs #f)
(define static-extensions #f)
(define required-extensions '())
(define gui #f)


;;; Display usage information:

(define (usage)
  (display
"Usage: csc FILENAME | OPTION ...

  `csc' is a driver program for the CHICKEN compiler. Any Scheme, C or object
  files and all libraries given on the command line are translated, compiled or
  linked as needed.

  General options:

    -h  -help                   display this text and exit
    -v                          show intermediate compilation stages
    -v2  -verbose               display information about translation progress
    -v3                         display information about all compilation stages
    -V  -version                display Scheme compiler version and exit
    -release                    display release number and exit

  File and pathname options:

    -o -output-file FILENAME    specifies target executable name
    -I -include-path PATHNAME   specifies alternative path for included files
    -to-stdout                  write compiler to stdout (implies -t)
    -s -shared -dynamic         generate dynamically loadable shared object file

  Language options:

    -D  -DSYMBOL  -feature SYMBOL 
                                register feature identifier
    -c++                        Compile via a C++ source file (.cpp) 
    -objc                       Compile via Objective-C source file (.m)

  Syntax related options:

    -i -case-insensitive        don't preserve case of read symbols    
    -K -keyword-style STYLE     allow alternative keyword syntax (prefix or suffix)
    -run-time-macros            macros are made available at run-time

  Translation options:

    -x  -explicit-use           do not use units `library' and `eval' by default
    -P  -check-syntax           stop compilation after macro-expansion
    -A  -analyze-only           stop compilation after first analysis pass

  Debugging options:

    -w  -no-warnings            disable warnings
    -disable-warning CLASS      disable specific class of warnings
    -d0 -d1 -d2 -debug-level NUMBER
                                set level of available debugging information
    -no-trace                   disable rudimentary debugging information
    -profile                    executable emits profiling information 
    -accumulate-profile         executable emits profiling information in append mode
    -profile-name FILENAME      name of the generated profile information file
    -emit-debug-info            emit additional debug-information
    -emit-exports FILENAME      write exported toplevel variables to FILENAME
    -G  -check-imports          look for undefined toplevel variables
    -import FILENAME            read externally exported symbols from FILENAME

  Optimization options:

    -O -O1 -O2 -O3 -optimize-level NUMBER
			        enable certain sets of optimization options
    -optimize-leaf-routines     enable leaf routine optimization
    -N  -no-usual-integrations  standard procedures may be redefined
    -u  -unsafe                 disable safety checks
    -b  -block                  enable block-compilation
    -disable-interrupts         disable interrupts in compiled code
    -f  -fixnum-arithmetic      assume all numbers are fixnums
    -Ob  -benchmark-mode        equivalent to '-block -optimize-level 3 
                                 -debug-level 0 -fixnum-arithmetic -lambda-lift 
                                 -disable-interrupts'
    -lambda-lift                perform lambda-lifting
    -unsafe-libraries           link with unsafe runtime system
    -disable-stack-overflow-checks  disables detection of stack-overflows
    -inline                     enable inlining
    -inline-limit               set inlining threshold

  Configuration options:

    -unit NAME                  compile file as a library unit
    -uses NAME                  declare library unit as used.
    -heap-size NUMBER           specifies heap-size of compiled executable
    -heap-initial-size NUMBER   specifies heap-size at startup time
    -heap-growth PERCENTAGE     specifies growth-rate of expanding heap
    -heap-shrinkage PERCENTAGE  specifies shrink-rate of contracting heap
    -nursery NUMBER  -stack-size NUMBER
		                specifies nursery size of compiled executable
    -X -extend FILENAME         load file before compilation commences
    -prelude EXPRESSION         add expression to beginning of source file
    -postlude EXPRESSION        add expression to end of source file
    -prologue FILENAME          include file before main source file
    -epilogue FILENAME          include file after main source file

    -e  -embedded               compile as embedded (don't generate `main()')
    -W  -windows                compile as Windows GUI application (MSVC only)
    -R  -require-extension NAME require extension in compiled code
    -E  -extension              compile as extension (dynamic or static)
    -dll -library               compile multiple units into a dynamic library

  Options to other passes:

    -C OPTION                   pass option to C compiler
    -L OPTION                   pass option to linker
    -I<DIR>                     pass \"-I<DIR>\" to C compiler (add include path)
    -L<DIR>                     pass \"-L<DIR>\" to linker (add library path)
    -k                          keep intermediate files
    -c                          stop after compilation to object files
    -t                          stop after translation to C
    -cc COMPILER                select other C compiler than the default one
    -cxx COMPILER               select other C++ compiler than the default one
    -ld COMPILER                select other linker than the default one
    -lLIBNAME                   link with given library (`libLIBNAME' on UNIX,
                                 `LIBNAME.lib' on Windows)                                
    -static-libs                link with static CHICKEN libraries
    -static                     generate completely statically linked executable
    -static-extensions          link with static extensions (if available)
    -F<DIR>                     pass \"-F<DIR>\" to C compiler (add framework 
                                 header path on Mac OS X)
    -framework NAME             passed to linker on Mac OS X
    -rpath PATHNAME             add directory to runtime library search path
    -Wl,...                     pass linker options
    -strip                      strip resulting binary

  Inquiry options:

    -home                       show home-directory (where support files go)
    -cflags                     show required C-compiler flags and exit
    -ldflags                    show required linker flags and exit
    -libs                       show required libraries and exit
    -cc-name                    show name of default C compiler used
    -ld-name                    show name of default linker used
    -dry-run                    just show commands executed, don't run them 
                                 (implies `-v')

  Obscure options:

    -debug MODES                display debugging output for the given modes
    -compiler PATHNAME          use other compiler than default `chicken'
    -compress-literals NUMBER   compile literals above threshold as strings
    -disable-c-syntax-checks    disable syntax checks of C code fragments
    -raw                        do not generate implicit init- and exit code			       
    -emit-external-prototypes-first  emit protoypes for callbacks before foreign
                                 declarations
    -keep-shadowed-macros       do not remove shadowed macro
    -host                       compile for host when configured for cross-compiling

  Options can be collapsed if unambiguous, so

    -vkfO

  is the same as

    -v -k -fixnum-arithmetic -optimize

  The contents of the environment variable CSC_OPTIONS are implicitly
  passed to every invocation of `csc'.
"
) )


;;; Parse arguments:

(define (run args)

  (define (t-options . os)
    (set! translate-options (append translate-options os)) )

  (define (check o r . n)
    (unless (>= (length r) (:optional n 1))
      (quit "not enough arguments to option `~A'" o) ) )

  (define (shared-build lib)
    (set! translate-options (cons* "-feature" "chicken-compile-shared" translate-options))
    (if win
	(begin
	  (set! compile-options (cons* "/DPIC" "/DC_SHARED" compile-options)) 
	  (set! link-options (cons* "/dll" link-options)))
	(begin
	  (set! compile-options (cons* "-fPIC" "-DPIC" "-DC_SHARED" compile-options)) 
	  (set! link-options
	    (cons* "-fPIC" (if osx (if lib "-dynamiclib" "-bundle") "-shared") link-options))))
    (set! shared #t) )

  (let loop ([args args])
    (cond [(null? args)
	   (when inquiry-only
	     (when show-cflags (print* (compiler-options) #\space))
	     (when show-ldflags (print* (linker-options) #\space))
	     (when show-libs (print* (linker-libraries #t) #\space))
	     (newline)
	     (exit) )
	   #;(when (null? scheme-files)
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
			  (pathname-replace-extension (last scheme-files) shared-library-extension)
			  (pathname-replace-extension (last scheme-files) executable-extension) ) ) )
		  (run-translation) ] )
	   (unless translate-only 
	     (run-compilation)
	     (unless compile-only
	       (when (member target-filename scheme-files)
		 (printf "Warning: output file will overwrite source file `~A' - renaming source to `~A.old'~%"
			 target-filename target-filename)
		 (unless (zero? (system* (sprintf "mv ~A ~A.old" target-filename target-filename)))
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
	       [(-static-extensions)
		(set! static-extensions #t) ]
	       [(-cflags)
		(set! inquiry-only #t) 
		(set! show-cflags #t) ]
	       [(-ldflags)
		(set! inquiry-only #t)
		(set! show-ldflags #t) ]
	       [(-cc-name) (print compiler) (exit 0)]
	       [(-ld-name) (print linker) (exit 0)]
	       [(-home) (print home) (exit 0)]
	       [(-libs)
		(set! inquiry-only #t)
		(set! show-libs #t) ]
	       [(-v)
		(set! verbose #t) ]
	       [(-v2 -verbose)
		(set! verbose #t)
		(t-options "-verbose") ]
	       [(-w -no-warnings)
		(set! compile-options (cons "-w" compile-options))
		(t-options "-no-warnings") ]
	       [(-v3)
		(set! verbose #t)
		(t-options "-verbose")
		(set! compile-options (cons "-v" compile-options))
		(set! link-options (cons "-v" link-options)) ]
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
	       [(-windows |-W|)
		(set! gui #t)
		(when win
		  (set! link-options (cons* "kernel32.lib" "user32.lib" "gdi32.lib" link-options))
		  (set! compile-options (cons "-DC_WINDOWS_GUI" compile-options))) ]
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
		(unless win (set! link-options (append link-options "-s")))]
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
			      (if win
				  (set! object-files
				    (append object-files (list (string-append (substring arg 2) ".lib"))) )
				  (set! link-options (append link-options (list arg))) ) ]
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
			 (cond [(not ext) (set! scheme-files (cons arg scheme-files))]
			       [(member ext '("h" "c"))
				(set! c-files (cons arg c-files)) ]
			       [(member ext '("cpp" "C" "cc" "cxx" "hpp"))
				(when osx (set! compile-options (cons "-no-cpp-precomp" compile-options)))
				(set! cpp-mode #t)
				(set! c-files (cons arg c-files)) ]
			       [(member ext '("m" "M" "mm"))
				(set! objc-mode #t)
				(set! c-files (cons arg c-files)) ]
			       [(or (string=? ext object-extension)
				    (string=? ext library-extension) )
				(set! object-files (cons arg object-files)) ]
			       [else (set! scheme-files (cons arg scheme-files))] ) ) ]
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
     (let ([cscf (pathname-replace-extension f "csc")])
       (when (and (file-exists? cscf)
		  (let ([x (with-input-from-file cscf read-line)])
		    (or (eof-object? x) (string=? "#%eof" x)) ) )
	 (delete-file* cscf) )
       (let ([fc (pathname-replace-extension
		  (if (= 1 (length scheme-files))
		      target-filename
		      f)
		  (cond (cpp-mode "cpp")
			(objc-mode "m")
			(else "c") ) ) ] )
	 (unless (zero?
		  (system* 
		   (string-intersperse 
		    (cons* translator f 
			   (append 
			    (if to-stdout 
				'("-to-stdout")
				`("-output-file" ,fc) )
			    (if (or static static-libs static-extensions)
				(map (lambda (e) (conc "-uses " e)) required-extensions)
				'() )
			    (map quote-option (append translate-options translation-optimization-options)) ) )
		    " ") ) )
	   (exit last-exit-code) )
	 (set! c-files (append (list fc) c-files))
	 (set! generated-c-files (append (list fc) generated-c-files))
	 (when (file-exists? cscf)
	   (with-input-from-file cscf
	     (lambda ()
	       (read-line)
	       (for-each
		(match-lambda
 		  [('post-process commands ...)
 		   (for-each system* commands) ]
 		  [('c-options opts ...)
 		   (set! compile-options (append compile-options opts)) ]
 		  [('link-options opts ...)
 		   (set! link-options (append link-options opts)) ]
		  [x (error "invalid entry in csc control file" x)] )
		(read-file) ) ) )
	   (delete-file* cscf) ) ) ) )
   (reverse scheme-files) )
  (unless keep-files (for-each delete-file* generated-scheme-files)) )


;;; Compile all C files:

(define (run-compilation)
  (for-each
   (lambda (f)
     (let ([fo (pathname-replace-extension f object-extension)])
       (unless (zero?
		(system*
		 (string-intersperse
		  (list (cond (cpp-mode c++-compiler)
			      (else compiler) )
			(cleanup-filename f)
			(string-append compile-output-flag (cleanup-filename fo)) 
			compile-only-flag
			(compiler-options) ) ) ) )
	 (exit last-exit-code) )
       (set! generated-object-files (cons fo generated-object-files))
       (set! object-files (cons fo object-files)) ) )
   (reverse c-files) )
  (unless keep-files (for-each delete-file* generated-c-files)) )

(define (compiler-options)
  (string-intersperse
   (map quote-option
	(append
	 (if (or static static-libs) '() nonstatic-compilation-options)
	 compilation-optimization-options
	 compile-options) ) ) )


;;; Link object files and libraries:

(define (run-linking)
  (let ([files (map cleanup-filename
		    (append (reverse object-files)
			    (nth-value 0 (static-extension-info))
			    (if (or static static-libs)
				(if gui gui-library-files library-files)
				(if gui gui-shared-library-files shared-library-files) ) ) ) ] )
    (unless (zero?
	     (system*
	      (string-intersperse 
	       (cons* (cond (cpp-mode c++-linker)
			    (else linker) )
		      (append 
		       files
		       (list (string-append link-output-flag (cleanup-filename target-filename)) 
			     (linker-options)
			     (linker-libraries #f) ) ) ) ) ) )
      (exit last-exit-code) )
    (when (and win (not static) (not static-libs) (not shared))
      (delete-file* (pathname-replace-extension target-filename "exp"))
      (delete-file* (pathname-replace-extension target-filename "lib")) )
    (unless keep-files (for-each delete-file* generated-object-files)) ) )

(define (static-extension-info)
  (let ((rpath (repository-path)))
    (if (or static static-libs static-extensions)
	(let loop ((exts required-extensions) (libs '()) (opts '()))
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
   (if (and static (not win) (not osx)) " -static" "") ) )

(define (linker-libraries #!optional staticexts)
  (string-intersperse
   (append
    (if staticexts (nth-value 0 (static-extension-info)) '())
    (cons
     (if (or static static-libs) extra-libraries extra-shared-libraries)
     (if (or static static-libs) 
	 (if gui gui-library-files library-files)
	 (if gui gui-shared-library-files shared-library-files) ) ) ) ) )


;;; Helper procedures:

(define-constant +hairy-chars+ '(#\\ #\#))

(define (quote-option x)
  (if (any (lambda (c)
	     (or (char-whitespace? c) (memq c +hairy-chars+)) )
	   (string->list x) )
      (cleanup x)
      x) )

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

(define last-exit-code #f)

(define (system* str)
  (when verbose (print str))
  (set! last-exit-code
    (if dry-run 
	0
	(system str)) )
  (unless (zero? last-exit-code)
    (printf "*** Shell command terminated with exit status ~S: ~A~%" last-exit-code str) )
  last-exit-code)

(define (delete-file* str)
  (when verbose 
    (if win
	(print "del " str) 
	(print "rm " str) ) )
  (unless dry-run (delete-file str) ))


;;; Run it:

(run (append (string-split (or (getenv "CSC_OPTIONS") "")) arguments))
