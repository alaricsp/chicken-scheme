;;;; chicken-build.scm

;;; We use this for driving the build process

(include "build.scm")


;;; Build variables (these are intended to be overridden from the command line)

(define BUILDVERSION (with-input-from-file "buildversion" read))
(define HOST_ARCH (machine-type))
(define HOST_OS (software-version))
(define CYGWIN (feature? 'cygwin))
(define LINUX (feature? 'linux))
(define UNIX (feature? 'unix))
(define OSX (feature? 'macosx))
(define PREFIX "/usr/local")
(define DESTDIR #f)
(define BINDIR #f)
(define LIBDIR #f)
(define EGGDIR #f)
(define INCDIR #f)
(define SHAREDIR #f)
(define DOCDIR #f)
(define MANDIR #f)
(define CC 
  (case (build-platform)
    ((gnu) "gcc")
    ((msvc) "cl") 
    (else "cc") ) )
(define CXX
  (case (build-platform)
    ((gnu) "g++")
    ((msvc) "cl") 
    (else "c++") ) )
(define LD
  (case (build-platform)
    ((gnu) "gcc")
    ((msvc) "link") 
    (else "cc") ) )
(define AR #f)
(define CC_OPT_FLAGS #f)
(define CFLAGS_SCHEME #f)
(define CFLAGS #f)
(define CFLAGS_SHARED #f)
(define LDFLAGS #f)
(define LDFLAGS_SHARED #f)
(define LDFLAGS_STATIC #f)
(define LDFLAGS_EXECUTABLE_SHARED #f)
(define LDFLAGS_EXECUTABLE_STATIC #f)
(define LDFLAGS_LIBRARY_SHARED #f)
(define SFLAGS "-quiet -no-trace -optimize-level 2 -include-path .")
(define SFLAGS_PROGRAM "-no-lambda-info")
(define SFLAGS_LIBRARY "-explicit-use")
(define SFLAGS_UNSAFE "-unsafe -no-lambda-info")
(define CHICKEN "chicken")
(define CSI "csi")
(define BINARYVERSION "1")
(define PTABLES #t)
(define LIBS #f)
(define STATICLIBS #f)
(define SHARED #t)
(define NURSERY "(128*1024)")
(define STACKDIRECTION 1)
(define TARGET_CFLAGS #f)
(define TARGET_CC_OPT_FLAGS #f)
(define TARGET_PREFIX #f)
(define TARGET_CC #f)
(define TARGET_CXX #f)
(define CROSS_CHICKEN #f)
;(define DEBUG #f)
(define DEBUG #t)			;* testing
(define SHARED_LIB_EXTENSION #f)
(define MANUALDIR (path (current-directory) "../wiki"))
(define WIKIEXTENSIONS (path (current-directory) "../stream-wiki/tags/1.9/extensions"))
(define INCLUDES "-I.")
(define INSTALL_PROGRAM #f)
(define INSTALL_EXE_FLAGS #f)
(define INSTALL_FILE_FLAGS #f)
(define PROGRAM_PREFIX #f)
(define PROGRAM_SUFFIX #f)
(define PROFILE_CHICKEN #f)
(define GCHOOKS #f)
(define EXTRASLOT #f)
(define SYMBOLGC #f)
(define NOAPPLYHOOK #f)
(define HACKEDAPPLY #f)


;;; Helper procedures

(define canon (o string->symbol ->string))


;;; Source files

(define *libchicken-scheme-sources*
  '("eval"
    "extras"
    "library"
    "lolevel"
    "utils"
    "tcp"
    "srfi-1"
    "srfi-4"
    "srfi-13"
    "srfi-14"
    "srfi-18"
    "posixunix" 
    "regex"))

(define *libchicken-safe-scheme-sources*
  '("scheduler"
    "profiler"
    "stub"
    "match") )

(define *pcre-sources*
  (prefix
   "pcre"
   '("pcre_compile"
     "pcre_get"
     "pcre_printint" 
     "pcre_ucp_findchar"
     "pcre_config"
     "pcre_globals"
     "pcre_refcount" 
     "pcre_valid_utf8"
     "pcre_dfa_exec"
     "pcre_info"
     "pcre_study"
     "pcre_version"
     "pcre_exec" 
     "pcre_maketables"
     "pcre_tables"
     "pcre_xclass"
     "pcre_fullinfo"
     "pcre_ord2utf8"
     "pcre_try_flipped"
     "chartables") ) )

(define *apply-hack* #f)

(define *pcre-c-sources* (suffix "c" *pcre-sources*))
(define *pcre-objects* (suffix "o" *pcre-sources*))

(define *pcre-static-objects* 
  (suffix "o" (map (cut conc <> "-static") *pcre-sources*)) )

(define *libchicken-c-sources*
  (append 
   '("runtime.c")
   (suffix "c" *libchicken-scheme-sources*)
   (suffix "c" *libchicken-safe-scheme-sources*) ) )

(define *libchicken-objects*
  (suffix "o" *libchicken-c-sources*) )

(define *libchicken-scheme-objects*
  (suffix "o" *libchicken-c-sources*) )

(define *libchicken-static-objects*
  (map (lambda (f) (suffix "o" (conc (pathname-file f) "-static.c")))
       *libchicken-c-sources*) )

(define *libuchicken-c-sources*
  (append 
   (suffix "c" (map (cut conc "u" <>) *libchicken-scheme-sources*))
   (suffix "c" *libchicken-safe-scheme-sources*) ) )

(define *libuchicken-objects*
  (cons "uruntime.o" (suffix "o" *libuchicken-c-sources*) ) )

(define *libuchicken-static-objects*
  (cons 
   "uruntime-static.o"
   (map (lambda (f) (suffix "o" (conc (pathname-file f) "-static.c")))
	*libuchicken-c-sources*) ) )

(define *chicken-scheme-sources*
  '("chicken"
    "support"
    "compiler"
    "optimizer"
    "c-platform"
    "c-backend"
    "batch-driver") )

(define *chicken-c-sources*
  (suffix "c" *chicken-scheme-sources*) )

(define *chicken-objects*
  (suffix "o" *chicken-c-sources*) )

(define *chicken-static-objects*
  (map (lambda (f) (suffix "o" (conc (pathname-file f) "-static.c")))
       *chicken-c-sources*) )

;;; These are set by fixup-variables

(define *libchicken-shared*)
(define *libchicken-static*)
(define *libuchicken-shared*)
(define *libuchicken-static*)


;;; Fixup variables (may be changed by command line)

(define (fixup-variables)
  (set! HOST_ARCH (canon HOST_ARCH))
  (set! HOST_OS (canon HOST_OS))
  (when (-e (conc "apply-hack." HOST_ARCH ".s"))
    (message "apply hack is available for this architecture (~a)" HOST_ARCH)
    (set! HACKEDAPPLY #t) )
  (set! CC_OPT_FLAGS
    (or CC_OPT_FLAGS 
	(case (canon CC)
	  ((gcc)
	   (if (true? DEBUG)
	       "-g -Wall -Wno-unused"
	       "-Os -fomit-frame-pointer") )
	  ((cl) "/O2")
	  (else "") ) ) )
  (set! CFLAGS (conc (or CFLAGS "") " -DHAVE_CHICKEN_CONFIG_H"))
  (case (canon CC)
    ((gcc) (set! CFLAGS (conc "-fno-strict-aliasing " (or CFLAGS "")))) )
  (case (canon LD)
    ((gcc ld) 
     (when LINUX
       (set! LDFLAGS_EXECUTABLE_SHARED
	 (conc (or LDFLAGS_EXECUTABLE_SHARED "")
	       " -Wl,-R" (path PREFIX "lib") ) ) )
     (when (true? OSX)
       (set! CFLAGS (conc (or CFLAGS "") " -fno-common -no-cpp-precomp")) )
     (set! LDFLAGS (conc (or LDFLAGS "") "-L."))
     (set! CFLAGS_SHARED (conc (or CFLAGS_SHARED "") " -fPIC -DPIC"))
     (unless OSX
       (set! LDFLAGS_EXECUTABLE_STATIC (conc (or LDFLAGS_EXECUTABLE_STATIC "") " -static")) )
     (set! LDFLAGS_LIBRARY_SHARED 
       (conc (or LDFLAGS_LIBRARY_SHARED "")
	     (if (true? OSX) " -dynamiclib" " -shared")))))
  (when (true? PTABLES) 
    (set! CFLAGS_SCHEME (conc (or CFLAGS_SCHEME "") " -DC_ENABLE_PTABLES")) )
  (when (and (true? SHARED) (or (true? CYGWIN) (true? UNIX)))
    (set! LIBS (conc (or LIBS "") " -ldl")) )
  (when (or (true? OSX) (true? CYGWIN) (true? UNIX))
    (set! STATICLIBS (conc (or STATICLIBS "") " -lm"))
    (set! AR (or AR "ar cru"))
    (set! LIBS (conc LIBS " -lm")) 
    (set! INSTALL_PROGRAM "install")
    (set! INSTALL_EXE_FLAGS "-m755")
    (set! INSTALL_FILE_FLAGS "-m644") )
  (set! SHARED_LIB_EXTENSION
    (or SHARED_LIB_EXTENSION 
	(cond ((true? OSX) "dylib")
	      (else "so") ) ) )
  (set! CROSS_CHICKEN (if (or TARGET_CC TARGET_PREFIX) 1 0))
  (set! LDFLAGS (or LDFLAGS ""))
  (set! LDFLAGS_EXECUTABLE_SHARED (or LDFLAGS_EXECUTABLE_SHARED ""))
  (set! LDFLAGS_LIBRARY_SHARED (or LDFLAGS_LIBRARY_SHARED ""))
  (set! LDFLAGS_EXECUTABLE_STATIC (or LDFLAGS_EXECUTABLE_STATIC ""))
  (set! TARGET_CC (or TARGET_CC CC))
  (set! TARGET_CXX (or TARGET_CXX CXX))
  (set! TARGET_CC_OPT_FLAGS (or TARGET_CC_OPT_FLAGS CC_OPT_FLAGS))
  (set! TARGET_PREFIX (or TARGET_PREFIX PREFIX))
  (set! TARGET_CFLAGS (or TARGET_CFLAGS CFLAGS))
  (set! DESTDIR (or DESTDIR PREFIX))
  (set! BINDIR (or BINDIR (path DESTDIR "bin")))
  (set! LIBDIR (or LIBDIR (path DESTDIR "lib")))
  (set! EGGDIR (or EGGDIR (path DESTDIR (conc "lib/chicken/" BINARYVERSION))))
  (set! INCDIR (or INCDIR (path DESTDIR "include")))
  (set! SHAREDIR (or SHAREDIR (path DESTDIR "share/chicken")))
  (set! DOCDIR (or DOCDIR (path DESTDIR "share/chicken/doc")))
  (set! MANDIR (or MANDIR (path DESTDIR "man/man1")))
  (set! *libchicken-shared* (path #f "libchicken" SHARED_LIB_EXTENSION))
  (set! *libchicken-static* "libchicken.a")
  (set! *libuchicken-shared* (path #f "libuchicken" SHARED_LIB_EXTENSION))
  (set! *libuchicken-static* "libuchicken.a"))


;;; Generate configuration headers

(define (configure)
  (actions
   "configure"
   "chicken-config.h"
   configure-chicken-config.h)
  (actions 
   "configure"
   "chicken-defaults.h"
   configure-chicken-defaults.h) 
  (notfile "confclean")
  (actions 
   "do" "confclean"
   (lambda ()
     (run (rm -f Makefile chicken-config.h chicken-defaults.h)) ) ) )

(define (configure-chicken-config.h)
  (with-output-to-file "chicken-config.h"
    (lambda ()
      (print #<#EOF
##define C_STACK_GROWS_DOWNWARD 1
##define C_USE_C_DEFAULTS 1
##define HAVE_ALLOCA 1
##define HAVE_DIRENT_H 1
##define HAVE_DLFCN_H 1
##define HAVE_GRP_H 1
##define HAVE_ALLOCA_H 1
##define HAVE_STRERROR 1
##ifdef __APPLE__
##define HAVE_CRT_EXTERNS_H 1
##endif
##define HAVE_ERRNO_H 1
#{(if (true? GCHOOKS) "#define C_GC_HOOKS"  "")}
#{(if (true? EXTRASLOT) "#define C_EXTRA_SYMBOL_SLOT"  "")}
#{(if (true? SYMBOLGC) "#define C_COLLECT_ALL_SYMBOLS"  "")}
#{(if (true? NOAPPLYHOOK) "#define C_NO_APPLY_HOOK"  "")}
#{(if (true? HACKEDAPPLY) "#define C_HACKED_APPLY"  "")}
##if !defined(__APPLE__) && !defined(C_XXXBSD)
##define HAVE_GCVT 1
##endif
##define HAVE_INTTYPES_H 1
##define HAVE_LIMITS_H 1
##define HAVE_SYSEXITS_H 1
##define HAVE_MEMMOVE 1
EOF
) ) ) )

(define (configure-chicken-defaults.h)
  (with-output-to-file "chicken-defaults.h"
    (lambda ()
      (print #<#EOF
/*** Program generated file: any edits may be lost ***/
##ifndef C_INSTALL_CC
## define C_INSTALL_CC "#CC"
##endif
##ifndef C_INSTALL_CXX
## define C_INSTALL_CXX "#CXX"
##endif
##ifndef C_INSTALL_CFLAGS
## define C_INSTALL_CFLAGS "#CFLAGS #CFLAGS_SCHEME #CC_OPT_FLAGS"
##endif
##ifndef C_INSTALL_SHARE_HOME
## define C_INSTALL_SHARE_HOME "#{PREFIX}/share/chicken"
##endif
##ifndef C_INSTALL_BIN_HOME
## define C_INSTALL_BIN_HOME "#{PREFIX}/bin"
##endif
##ifndef C_INSTALL_EGG_HOME
## define C_INSTALL_EGG_HOME "#{PREFIX}/lib/chicken/#BINARYVERSION"
##endif
##ifndef C_INSTALL_LIB_HOME
## define C_INSTALL_LIB_HOME "#{PREFIX}/lib"
##endif
##ifndef C_INSTALL_STATIC_LIB_HOME
## define C_INSTALL_STATIC_LIB_HOME "#{PREFIX}/lib"
##endif
##ifndef C_INSTALL_INCLUDE_HOME
## define C_INSTALL_INCLUDE_HOME "#{PREFIX}/include"
##endif
##ifndef C_INSTALL_MORE_LIBS
## define C_INSTALL_MORE_LIBS "#LIBS"
##endif
##ifndef C_INSTALL_MORE_STATIC_LIBS
## define C_INSTALL_MORE_STATIC_LIBS "#STATICLIBS"
##endif
##ifndef C_DEFAULT_TARGET_STACK_SIZE
## define C_DEFAULT_TARGET_STACK_SIZE #NURSERY
##endif
##ifndef C_STACK_GROWS_DOWNWARD
## define C_STACK_GROWS_DOWNWARD "#STACKDIRECTION"
##endif
##ifndef C_TARGET_MORE_LIBS
## define C_TARGET_MORE_LIBS "#LIBS"
##endif
##ifndef C_TARGET_MORE_STATIC_LIBS
## define C_TARGET_MORE_STATIC_LIBS "#STATICLIBS"
##endif
##ifndef C_TARGET_CC
## define C_TARGET_CC "#TARGET_CC"
##endif
##ifndef C_TARGET_CXX
## define C_TARGET_CXX "#TARGET_CXX"
##endif
##ifndef C_TARGET_CFLAGS
## define C_TARGET_CFLAGS "#TARGET_CFLAGS #CFLAGS_SCHEME #TARGET_CC_OPT_FLAGS"
##endif
##ifndef C_CROSS_CHICKEN
## define C_CROSS_CHICKEN #CROSS_CHICKEN
##endif
##ifndef C_TARGET_LIB_HOME
## define C_TARGET_LIB_HOME "#{TARGET_PREFIX}/lib"
##endif
##ifndef C_TARGET_RUN_LIB_HOME
## define C_TARGET_RUN_LIB_HOME "#{TARGET_PREFIX}/lib"
##endif
##ifndef C_TARGET_SHARE_HOME
## define C_TARGET_SHARE_HOME "#{TARGET_PREFIX}/share"
##endif
##ifndef C_TARGET_INCLUDE_HOME
## define C_TARGET_INCLUDE_HOME "#{TARGET_PREFIX}/include"
##endif
##ifndef C_TARGET_STATIC_LIB_HOME
## define C_TARGET_STATIC_LIB_HOME "#{TARGET_PREFIX}/lib"
##endif
EOF
) ) ) )


;;; Cleaning up

(notfile "clean")

(actions 
 "do" "clean"
 (lambda ()
   (run (rm -f chicken-defaults.h chicken-config.h
	    chicken csc csi chicken-static csi-static
	    chicken-profile chicken-setup
	    *.o *.exports
	    ,@*pcre-objects*
	    ,@*pcre-static-objects*
	    ,@*libchicken-objects*
	    ,@*libuchicken-objects*
	    ,@*chicken-objects*
	    ,@*libchicken-static-objects*
	    ,@*libuchicken-static-objects*
	    ,@*chicken-static-objects*) ) ) )


;;; Clean, including generated C files

(notfile "spotless")
(depends "spotless" "clean")

(actions 
 "clean" "spotless"
 (lambda ()
   (run (rm -f ,@(suffix "c" *libchicken-scheme-sources*)
	    ,@(suffix "c" *libchicken-safe-scheme-sources*)
	    ,@(suffix "c" *chicken-c-sources*)
	    chicken-profile.c csi.c csc.c chicken-setup.c
	    html/*) ) ) )


;;; General build procedures

(define (cc in out flags)
  (actions "cc" out (lambda () (run (,CC -c ,flags ,in -o ,out)))) )

(define (cscm in out flags)
  (actions "chicken" out (lambda () (run (,CHICKEN ,in -output-file ,out ,flags)))))

(define (simple-program name)
  (let ((o (suffix "o" name))
	(c (suffix "c" name))
	(s (suffix "scm" name)))
    (depends "all" name)
    (depends name o *libchicken-shared*)
    (depends o c "chicken-config.h" "chicken-defaults.h" "chicken.h")
    (cc c o (conc CFLAGS " " CC_OPT_FLAGS " " CFLAGS_SCHEME
		  " " INCLUDES " "  CFLAGS_SHARED) )
    (actions 
     "link" name
     (lambda ()
       (run (,LD ,LDFLAGS ,LDFLAGS_EXECUTABLE_SHARED -o ,name ,o
		 -lchicken ,LIBS))))
    (depends c s)
    (cscm s c (conc SFLAGS " " SFLAGS_PROGRAM)) ) )


;;; Check for apply hack availability

(define (apply-hack)
  (when (true? HACKEDAPPLY)
    (let ((ahs (conc "apply-hack." HOST_ARCH ".s")))
      (set! *apply-hack* #t)
      (depends "apply-hack.o" ahs)
      (cc ahs "apply-hack.o" (conc CFLAGS " " CC_OPT_FLAGS))
      (depends *libchicken-shared* "apply-hack.o")
      (depends *libuchicken-shared* "apply-hack.o")
      (depends *libchicken-shared* "apply-hack.o")
      (depends *libchicken-static* "apply-hack.o")
      (depends *libuchicken-static* "apply-hack.o") ) ) )


;;; Chicken runtime library

(define (libchicken)
  (let ((so *libchicken-shared*))
    (depends "all" so)
    (depends so *libchicken-objects* *pcre-objects*)
    (for-each
     (lambda (c o)
       (depends o "pcre/pcre_internal.h" "pcre/pcre.h" "pcre/config.h" "pcre/ucp.h" "pcre/ucp_findchar.c")
       (cc c o (conc CFLAGS " " CC_OPT_FLAGS " " CFLAGS_SHARED 
		     " " INCLUDES " -Ipcre -DPCRE_STATIC")) )
     *pcre-c-sources* *pcre-objects*)
    (for-each 
     (lambda (o)
       (depends o (suffix "c" o) "chicken-config.h" "chicken-defaults.h" "chicken.h") 
       (cc (suffix "c" o) o 
	   (conc CFLAGS " -DC_BUILDING_LIBCHICKEN -DC_DIY_BUILD " INCLUDES " -Ipcre "
		 CFLAGS_SCHEME " " CFLAGS_SHARED " " CC_OPT_FLAGS " -DPCRE_STATIC")) )
     *libchicken-objects*) 
    (for-each
     (lambda (f)
       (depends (suffix "c" f) (suffix "scm" f)) 
       (cscm (suffix "scm" f) (suffix "c" f) (conc SFLAGS " " SFLAGS_LIBRARY)))
     (append '("posixwin") *libchicken-scheme-sources* *libchicken-safe-scheme-sources*) )
    (depends "library.c" "version.scm")
    (depends "ulibrary.c" "version.scm")
    (actions 
     "link" so
     (lambda ()
       (run (,LD ,LDFLAGS ,LDFLAGS_LIBRARY_SHARED -o ,so
		 ,@*libchicken-objects* 
		 ,(if *apply-hack* "apply-hack.o" "")
		 ,@*pcre-objects* 
		 ,LIBS) ) ) ) ) )


;;; Runtime library, unsafe version

(define (libuchicken)
  (let ((so *libuchicken-shared*))
    (depends "all" so)
    (depends so *libuchicken-objects* *pcre-objects*)
    (for-each 
     (lambda (f0)
       (let ((f (conc "u" f0)))
	 (depends (suffix "o" f) (suffix "c" f) "chicken-config.h" "chicken-defaults.h" "chicken.h") 
	 (cc (suffix "c" f) (suffix "o" f) 
	     (conc CFLAGS " -DC_BUILDING_LIBCHICKEN -DC_DIY_BUILD -DNDEBUG -DC_UNSAFE_RUNTIME "
		   " -DPCRE_STATIC "
		   CFLAGS_SCHEME " " INCLUDES " -Ipcre " CFLAGS_SHARED " " CC_OPT_FLAGS)) 
	 (depends (suffix "c" f) (suffix "scm" f0))
	 (cscm (suffix "scm" f0) (suffix "c" f)
	       (conc SFLAGS " " SFLAGS_LIBRARY " " SFLAGS_UNSAFE) ) ) )
     (cons "posixwin" *libchicken-scheme-sources*) )
    (depends "uruntime.o" "runtime.c" "chicken-config.h" "chicken-defaults.h" "chicken.h")
    (cc "runtime.c" "uruntime.o"
	(conc CFLAGS " -DC_BUILDING_LIBCHICKEN -DC_DIY_BUILD -DNDEBUG -DC_UNSAFE_RUNTIME "
	      CFLAGS_SCHEME " -DPCRE_STATIC " 
	      INCLUDES " -Ipcre " CFLAGS_SHARED " " CC_OPT_FLAGS))
    (actions 
     "link" so
     (lambda ()
       (run (,LD ,LDFLAGS ,LDFLAGS_LIBRARY_SHARED -o ,so
		 ,@*libuchicken-objects* 
		 ,(if *apply-hack* "apply-hack.o" "")
		 ,@*pcre-objects*
		 ,LIBS) ) ) ) ) )


;;; Runtime library, static

(define (libchicken-static)
  (let ((a "libchicken.a"))
    (depends "all" a)
    (depends a *libchicken-static-objects* *pcre-static-objects*)
    (for-each
     (lambda (c o)
       (depends o "pcre/pcre_internal.h" "pcre/pcre.h" "pcre/config.h" "pcre/ucp.h" "pcre/ucp_findchar.c") 
       (cc c o (conc CFLAGS " " CC_OPT_FLAGS " " CFLAGS_SHARED 
		     " " INCLUDES " -Ipcre -DPCRE_STATIC") ) )
     *pcre-c-sources* *pcre-static-objects*)
    (for-each 
     (lambda (c)
       (let ((o (suffix "o" (conc (pathname-file c) "-static"))))
	 (depends o c "chicken-config.h" "chicken-defaults.h" "chicken.h") 
	 (cc c o (conc CFLAGS " -DC_BUILDING_LIBCHICKEN -DC_DIY_BUILD "
		       CFLAGS_SCHEME " " INCLUDES " -Ipcre -DPCRE_STATIC "
		       CC_OPT_FLAGS)) ) )
     *libchicken-c-sources*) 
    (cc "runtime.c" "runtime-static.o"
	(conc CFLAGS " -DC_BUILDING_LIBCHICKEN -DC_DIY_BUILD "
	      CFLAGS_SCHEME " " INCLUDES " -Ipcre " CC_OPT_FLAGS))
    (actions 
     "link" a
     (lambda ()
       (run (,AR ,a ,@*libchicken-static-objects*
		 ,(if *apply-hack* "apply-hack.o" "")
		 ,@*pcre-static-objects*) ) ) ) ) )


;;; Runtime library, unsafe and static

(define (libuchicken-static)
  (let ((a "libuchicken.a"))
    (depends "all" a)
    (depends a *libuchicken-static-objects* *pcre-static-objects*)
    (for-each 
     (lambda (f)
       (let ((o (suffix "o" (conc "u" (pathname-file f) "-static")))
	     (c (suffix "c" (conc "u" (pathname-file f)))) )
	 (depends o c "chicken-config.h" "chicken-defaults.h" "chicken.h") 
	 (cc c o (conc CFLAGS " -DC_BUILDING_LIBCHICKEN -DC_DIY_BUILD "
		       INCLUDES " -Ipcre -DNDEBUG -DC_UNSAFE_RUNTIME "
		       "-DPCRE_STATIC " CFLAGS_SCHEME " " CC_OPT_FLAGS)) ) )
     *libchicken-scheme-sources*) 
    (cc "runtime.c" "uruntime-static.o"
	(conc CFLAGS " -DC_BUILDING_LIBCHICKEN -DC_DIY_BUILD "
	      INCLUDES " -Ipcre -DNDEBUG -DC_UNSAFE_RUNTIME "
	      CFLAGS_SCHEME " " CC_OPT_FLAGS))
    (actions 
     "link" a
     (lambda ()
       (run (,AR ,a ,@*libuchicken-static-objects*
		 ,(if *apply-hack* "apply-hack.o" "")
		 ,@*pcre-static-objects*) ) ) ) ) )


;;; The compiler

(define (chicken)
  (depends "all" "chicken")
  (depends "chicken" *libchicken-shared* *chicken-objects*)
  (for-each
   (lambda (c)
     (let ((o (suffix "o" c)))
       (depends o c "chicken-config.h" "chicken-defaults.h" "chicken.h")
       (cc c o (conc CFLAGS " " CFLAGS_SHARED " " CC_OPT_FLAGS " "
		     CFLAGS_SCHEME " " INCLUDES)) ) )
   *chicken-c-sources*)
  (for-each
   (lambda (f)
     (let ((c (suffix "c" f))
	   (s (suffix "scm" f)))
       (depends c s "tweaks.scm")
       (cscm s c (conc SFLAGS " " SFLAGS_PROGRAM (if (true? PROFILE_CHICKEN) " -profile" "")))))
   *chicken-scheme-sources*)
  (depends "chicken.c" "chicken-more-macros.scm" "chicken-ffi-macros.scm")
  (depends "support.c" "banner.scm")
  (actions 
   "link" "chicken"
   (lambda ()
     (run (,LD ,LDFLAGS ,LDFLAGS_EXECUTABLE_SHARED -o chicken 
	       ,@*chicken-objects* -lchicken ,LIBS)))))


;;; The static compiler

(define (chicken-static)
  (depends "all" "chicken-static")
  (depends "chicken-static" 
	   "libchicken.a"
	   *chicken-static-objects*)
  (for-each
   (lambda (c)
     (let ((o (suffix "o" (conc (pathname-file c) "-static"))))
       (depends o c "chicken-config.h" "chicken-defaults.h" "chicken.h")
       (cc c o (conc CFLAGS " " CC_OPT_FLAGS " "
		     CFLAGS_SCHEME " " INCLUDES)) ) )
   *chicken-c-sources*)
  (actions 
   "link" "chicken-static"
   (lambda ()
     (run (,LD ,LDFLAGS ,LDFLAGS_EXECUTABLE_STATIC -o chicken-static 
	       ,@*chicken-static-objects* 
	       "libchicken.a"
	       ,STATICLIBS)))))


;;; The compiler driver "csc"

(define (csc)
  (simple-program "csc") )


;;; The interpreter

(define (csi)
  (simple-program "csi")
  (depends "csi.c" "banner.scm" "chicken-more-macros.scm"))


;;; The static interpreter

(define (csi-static)
  (depends "all" "csi-static")
  (depends "csi-static" "csi-static.o" 
	   "libchicken.a")
  (depends "csi-static.o" "csi.c" "chicken-config.h" "chicken-defaults.h" "chicken.h")
  (cc "csi.c" "csi-static.o"
      (conc CFLAGS " " CC_OPT_FLAGS " " CFLAGS_SCHEME " " INCLUDES))
  (actions 
   "link" "csi-static"
   (lambda ()
     (run (,LD ,LDFLAGS ,LDFLAGS_EXECUTABLE_STATIC -o csi-static
	       csi-static.o
	       "libchicken.a"
	       ,STATICLIBS)))))


;;; The setup program

(define (chicken-setup)
  (simple-program "chicken-setup")
  (depends "chicken-setup.c" "chicken-more-macros.scm"))


;;; The profile formatter

(define (chicken-profile)
  (simple-program "chicken-profile") )


;;; chicken-build.c itself

(define (chicken-build)
  (depends "chicken-build.c" "chicken-build.scm")
  (cscm "chicken-build.scm" "chicken-build.c" (conc SFLAGS " " SFLAGS_PROGRAM)))


;;; Documentation

(define (documentation)
  (actions "ChangeLog" (lambda () (run (hg log >ChangeLog)))) 
  (notfile "manualsync")
  (actions 
   "do" "manualsync" 
   (lambda () (run (rsync -av --existing ,(conc MANUALDIR "/") manual/))))
  (notfile "doc")
  (actions 
   "doc"
   (lambda ()
     (run (,CSI -s misc/makedoc --pdf --extension-path= ,WIKIEXTENSIONS))
     (run (cp chicken.pdf site)) ) ) )


;;; Installation

(define (installation)
  (notfile "install")
  (depends 
   "install" 
   "ChangeLog" "chicken" "chicken-static" "csi" "csi-static" "chicken-profile"
   "chicken-setup" 
   *libchicken-shared* *libchicken-static* *libuchicken-shared* *libuchicken-static*
   "chicken-config.h" "chicken-defaults.h")
  (actions
   "do" "install"
   (lambda ()
     (let ((exes '(chicken chicken-static csi csi-static csc chicken-profile chicken-setup))
	   (pref (if (true? PROGRAM_PREFIX) PROGRAM_PREFIX ""))
	   (suf (if (true? PROGRAM_SUFFIX) PROGRAM_SUFFIX "")) )
       (run (mkdir -p ,BINDIR ,EGGDIR ,INCDIR ,DOCDIR ,MANDIR))
       (for-each
	(lambda (exe)
	  (run (,INSTALL_PROGRAM 
		,INSTALL_EXE_FLAGS ,exe
		,(prefix BINDIR (conc pref exe suf)))))
	exes)
       (when (true? OSX)
	 (for-each 
	  (lambda (e) 
	    (run (install_name_tool -change libchicken.dylib ,(prefix LIBDIR "libchicken.dylib")
				    ,(prefix BINDIR e))))
	  exes) ) 
       (run (,INSTALL_PROGRAM ,INSTALL_EXE_FLAGS ,*libchicken-shared* ,*libuchicken-shared* ,LIBDIR))
       (run (,INSTALL_PROGRAM ,INSTALL_FILE_FLAGS ,*libchicken-static* ,*libuchicken-static* ,LIBDIR))
       (run (,INSTALL_PROGRAM ,INSTALL_FILE_FLAGS chicken.1 csi.1 csc.1 chicken-profile.1 chicken-setup.1 ,MANDIR))
       (run (,INSTALL_PROGRAM ,INSTALL_FILE_FLAGS chicken.h chicken-defaults.h chicken-config.h ,INCDIR))
       (run (,INSTALL_PROGRAM ,INSTALL_FILE_FLAGS ChangeLog README LICENSE ,DOCDIR))
       (run (,INSTALL_PROGRAM ,INSTALL_FILE_FLAGS chicken-more-macros.scm chicken-ffi-macros.scm 
		     ,@(map (cut make-pathname #f <> "exports") 
			    '("library" "eval" "srfi-1" "srfi-4" "srfi-13" "srfi-14" "srfi-18" "utils" "extras"
			      "tcp" "regex" "posix" "lolevel" "scheduler") )
		     ,SHAREDIR)))) ) )


;;; Uninstallation

(define (uninstallation)
  (notfile "uninstall")
  (actions
   "do" "uninstall"
   (lambda ()
     (let ((pref (if (true? PROGRAM_PREFIX) PROGRAM_PREFIX ""))
	   (suf (if (true? PROGRAM_SUFFIX) PROGRAM_SUFFIX "")) )
       (run (rm -f ,@(map (lambda (exe)
			    (path BINDIR (conc pref exe suf)) )
			  '("chicken" "chicken-static" "csi" "csi-static" "csc" "chicken-profile" "chicken-setup") )
		,@(map (cut path LIBDIR <>)
		       '("libchicken.*" "libuchicken.*") )
		,@(map (cut path MANDIR <> "1")
		       '("chicken" "csi" "csc" "chicken-profile" "chicken-setup") )
		,@(map (cut path INCDIR <> "h")
		       '("chicken" "chicken-defaults" "chicken-config")) ) )
       (run (rm -fr ,SHAREDIR)))) ) )


;;; Targets for creating a distribution (undocumented)

(define (distribution)
  (define ((release full?))
    (let* ((files (read-lines "distribution/manifest"))
	   (distname (conc "chicken-" BUILDVERSION)) 
	   (distfiles (map (cut prefix distname <>) files)) 
	   (tgz (conc distname ".tar.gz"))
	   (zip (conc distname ".zip")) )
      (run (rm -fr ,distname ,tgz ,zip))
      (run (mkdir -p ,distname
		  ,@(map (cut path distname <>) 
			 (delete-duplicates (filter-map prefix files) string=?))))
      (let ((missing '()))
	(for-each
	 (lambda (f)
	   (if (equal? "boot/cfiles" (prefix f))
	       (run (cp -p ,(pathname-strip-directory f) ,(path distname f)))
	       (begin
		 (if (-e f)
		     (run (cp -p ,f ,(path distname f))) 
		     (set! f (cons f missing))))))
	 files)
	(unless (null? missing)
	  (warning "files missing" missing) ) )
      (run (cd ,distname ";" sh autogen.sh))
      (run (tar cfz ,(conc distname ".tar.gz") ,distname))
      (when full?
	(run (zip -qr ,(conc distname ".zip") ,distname)) 
	(run (cp ,tgz ,zip site)) )
      (run (rm -fr ,distname)) ) )
  (depends "doc" "site/ChangeLog.txt")
  (depends "site/ChangeLog.txt" "ChangeLog")
  (actions "site/ChangeLog.txt" (lambda () (run (cp ChangeLog site/ChangeLog.txt))))
  (notfile "dist")
  (actions "dist" (release #f))
  (notfile "release")
  (depends "release" "dist")
  (actions "do" "release" (release #t))
  (depends 
   "dist" 
   (append
    *libchicken-c-sources*
    *libuchicken-c-sources*
    *chicken-c-sources*
    '("posixwin.c" "uposixwin.c" "chicken-build.c" "site/ChangeLog.txt"
      "chicken-profile.c" "csi.c" "chicken-setup.c" "csc.c") ) )
  (notfile "testdist")
  (actions 
   "do" "testdist"
   (lambda ()
     (let* ((bdir "/tmp/test-dist-build")
	    (sdir "/tmp/test-dist-build/chicken-*")
	    (bbdir "/tmp/test-dist-build/build")
	    (idir "/tmp/test-dist-build/inst")
	    (tgz (conc "chicken-" BUILDVERSION ".tar.gz")) )
       (run (,CSI -s chicken-build.scm dist ,(if *verbose* "-v" "")))
       (run (mkdir -p ,bdir))
       (run (tar xfz ,(conc "site/" tgz) -C ,bdir))
       (run (cd ,sdir ";" ./configure ,(conc "--prefix=" bdir "/inst")))
       (run (cd ,sdir ";" make))
       (run (cd ,sdir ";" make install))
       (run (cd ,idir ";" bin/chicken-setup -dv bloom-filter))
       (run (cd ,idir ";" "CSI_OPTIONS= echo ,r |" bin/csi -n -R bloom-filter))
       (run (rm -fr ,sdir ,idir))
       (run (tar xfz ,(conc "site/" tgz) -C ,bdir))
       (run (mkdir -p ,bbdir))
       (run (cd ,bbdir ";" cmake ,(conc "-DCMAKE_INSTALL_PREFIX=" idir) ../chicken-*))
       (run (cd ,bbdir ";" make VERBOSE=1))
       (run (cd ,bbdir ";" make VERBOSE=1 install))
       (run (cd ,idir ";" bin/chicken-setup -dv bloom-filter))
       (run (cd ,idir ";" "CSI_OPTIONS= echo ,r |" bin/csi -n -R bloom-filter))
       (run (rm -fr ,bdir)) ) ) )
  (notfile "release")
  (depends "release" "dist" "doc") )


;;; Miscellaneous targets

(define (miscellaneous)
  (notfile "tags")
  (actions "tags" (lambda () (run (etags *.scm runtime.c chicken.h)))) 
  (notfile "help")
  (actions 
   "show"
   "help"
   (lambda ()
     (print #<<EOF
available build targets:

  help                        show this message
  all                         build all (default)
  clean                       remove all build targets
  spotless                    remove all generated sources
  confclean                   remove configured headers
  install                     install into system
  uninstall                   remove from system

EOF
) ) ) )


;;; Start building

(build*
 fixup-variables
 configure
 libchicken
 libuchicken
 libchicken-static
 libuchicken-static
 apply-hack
 chicken
 chicken-static
 csc
 csi
 csi-static
 chicken-setup
 chicken-profile
 chicken-build
 documentation
 installation
 uninstallation
 distribution
 miscellaneous)


;;; TODO
;
; - test for stack-direction
