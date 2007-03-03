;;;; Buildfile for CHICKEN -*- Hen -*-

(set!? PREFIX "/usr/local")

(case (software-version)
  ((macosx)
   (set!? WIKIDIR "~/chicken-eggs/wiki")
   (set! PREFIX "/usr")
   (set! CCFLAGS "-fno-common -no-cpp-precomp") )
  ((linux)
   (set!? WIKIDIR "~/stuff/chicken-eggs/wiki")
   (set! STATICLINKFLAGS "-static")
   (set! SHAREDLINKFLAGS (conc "-Wl,-R" ($ PREFIX) "/lib")) )
  (else (quit "argh!")) )

(set!? DESTDIR PREFIX)
(set! SCANHEADERS #f)
(set!? NURSERY "(128*1024)")
(set!? STACKDIRECTION "1")
(set!? BOOTSTRAP_PATH (path PREFIX "bin"))
(set!? CHICKEN (path BOOTSTRAP_PATH "chicken"))
(set!? CSI (path BOOTSTRAP_PATH "csi"))
(set! BINARYVERSION "1")
(set! BINDIR (path DESTDIR "bin"))
(set! SHAREDIR (path DESTDIR "share/chicken"))
(set! DOCDIR (path DESTDIR "share/chicken/doc"))
(set! INCDIR (path DESTDIR "include"))
(set! LIBDIR (path DESTDIR "lib"))
(set! EGGDIR (path (list LIBDIR "chicken") BINARYVERSION))
(set! MANDIR (path DESTDIR "man"))
(set! OPTIM "-g")
(set!+ CCFLAGS " -DHAVE_CHICKEN_CONFIG_H -DC_ENABLE_PTABLES -DC_NO_PIC_NO_DLL -fno-strict-aliasing")
(set!+ LINKLIBS "-lffi -ldl -lm")
(set!+ STATICLINKLIBS "-lffi -lm")
(set! LIBSOURCES0 `(eval extras library lolevel utils tcp srfi-1 srfi-4 srfi-13 srfi-14 srfi-18 posixunix regex))
(set! ULIBSOURCES0 (map (cut conc "u" <>) LIBSOURCES0))
(set! LIBSOURCES1 '(profiler scheduler stub match))
(set! LIBSOURCES (append LIBSOURCES0 LIBSOURCES1))
(set! ULIBSOURCES (append ULIBSOURCES0 LIBSOURCES1))
(set! CHICKENSOURCES '("support" "compiler" "optimizer" "c-platform" "c-backend" "batch-driver"))
(set! CHICKENFLAGS "-quiet -no-trace -optimize-level 2 -include-path .")
(set! BUILDVERSION (with-input-from-file "buildversion" read))
(set! XLIBSOURCES '(posixwin))
(set! UXLIBSOURCES (map (cut conc "u" <>) XLIBSOURCES))
(set!? TARGET_PREFIX PREFIX)
(set! TARGET_LIB_HOME (path TARGET_PREFIX "lib"))
(set! TARGET_INCLUDE_HOME (path TARGET_PREFIX "include"))
(set! TARGET_STATIC_LIB_HOME (path TARGET_PREFIX "lib"))
(set! TARGET_SHARE_HOME (path TARGET_PREFIX "share"))
(set!? TARGET_CFLAGS (conc CCFLAGS " " OPTIM))

(set! PCRESOURCES
  '(pcre_compile
    pcre_get
    pcre_printint 
    pcre_ucp_findchar
    pcre_config
    pcre_globals
    pcre_refcount 
    pcre_valid_utf8
    pcre_dfa_exec
    pcre_info
    pcre_study
    pcre_version
    pcre_exec 
    pcre_maketables
    pcre_tables
    pcre_xclass
    pcre_fullinfo
    pcre_ord2utf8
    pcre_try_flipped
    chartables) )

(set! STATICPCRESOURCES (map (lambda (f) (conc "-static" f)) PCRESOURCES))

(if (not (file-exists? "chicken-config.h"))
    (begin
      (print "generating chicken-config.h ...")
      (with-output-to-file "chicken-config.h"
	^(print #<<EOF
#define C_STACK_GROWS_DOWNWARD 1
#define C_USE_C_DEFAULTS 1
#define HAVE_ALLOCA 1
#define HAVE_ALLOCA_H 1
#ifdef __APPLE__
#define HAVE_CRT_EXTERNS_H 1
#endif
#define HAVE_DIRENT_H 1
#define HAVE_DLFCN_H 1
#define HAVE_ERRNO_H 1
#define HAVE_FFI_H 1
#ifndef __APPLE__
#define HAVE_GCVT
#endif
#define HAVE_GRP_H 1
#define HAVE_INTTYPES_H 1
#define HAVE_LIMITS_H 1
#define HAVE_SYSEXITS_H 1
#define HAVE_MEMMOVE 1
#define HAVE_STRERROR 1
EOF
) ) ) )

(@ (set! (on (conc x ".c") 'CHICKENFLAGS) (conc CHICKENFLAGS " -explicit-use")) LIBSOURCES CHICKENSOURCES XLIBSOURCES)
(@ (set! (on (conc x ".c") 'CHICKENFLAGS) (conc CHICKENFLAGS " -no-lambda-info -unsafe -feature unsafe -explicit-use"))
   ULIBSOURCES0 UXLIBSOURCES)

(define (scheme t s)
  (depends (suffix "c" t) (suffix "scm" s))
  (translate (suffix "c" t) (suffix "scm" s)) )

(define (program p . s)
  (main p (suffix "c" p))
  (scheme p p)
  (depends p (cons (conc "libchicken" SUFSHR) s))
  (set! (on p 'LINKLIBS) (conc "-L. -lchicken " ($ LINKLIBS)))
  (set! (on p 'LINKFLAGS) ($ SHAREDLINKFLAGS)))

(define (static-program p . s)
  (let ((s (normalize s)))
    (main-from-objects p (map (lambda (s) (conc s "-static.o")) s))
    (depends p "libchicken.a")
    (set! (on p 'LINKLIBS) (conc "libchicken.a " ($ STATICLINKLIBS)))
    (set! (on p 'LINKFLAGS) ($ STATICLINKFLAGS))
    (for-each (cut static-object <>) s) ) )

(define (translate t s)
  (actions "chicken" t ^{,CHICKEN ,s -output-file ,t ,($ CHICKENFLAGS)}) )

(define (gendefaults f)
  (depends (suffix "h" f) (suffix "h.in" f))
  (actions
   (suffix "h" f)
   ^{,#<#EOF
sed -e "s,@C_INSTALL_CC[@],\"#{CC}\"," \
      -e "s,@C_INSTALL_CXX[@],\"#{C++}\"," \
      -e "s%@C_INSTALL_CFLAGS[@]%\"#{CCFLAGS} #{OPTIM}\"%" \
      -e "s,@C_INSTALL_SHARE_HOME[@],\"#{SHAREDIR}\"," \
      -e "s,@C_INSTALL_BIN_HOME[@],\"#{BINDIR}\"," \
      -e "s,@C_INSTALL_EGG_HOME[@],\"#{EGGDIR}\"," \
      -e "s,@C_INSTALL_LIB_HOME[@],\"#{LIBDIR}\"," \
      -e "s,@C_INSTALL_STATIC_LIB_HOME[@],\"#{LIBDIR}\"," \
      -e "s,@C_INSTALL_INCLUDE_HOME[@],\"#{INCDIR}\"," \
      -e "s%@C_INSTALL_MORE_LIBS[@]%\"#{LINKLIBS}\"%" \
      -e "s%@C_INSTALL_MORE_STATIC_LIBS[@]%\"#{STATICLINKLIBS}\"%" \
      -e "s%@C_DEFAULT_TARGET_STACK_SIZE[@]%#{NURSERY}%" \
      -e "s,@C_TARGET_CC[@],\"#{CC}\"," \
      -e "s,@C_TARGET_CXX[@],\"#{C++}\"," \
      -e "s,@C_TARGET_CFLAGS[@],\"#{TARGET_CFLAGS}\"," \
      -e "s,@C_TARGET_LIB_HOME[@],\"#{TARGET_LIB_HOME}\"," \
      -e "s,@C_TARGET_STATIC_LIB_HOME[@],\"#{TARGET_STATIC_LIB_HOME}\"," \
      -e "s,@C_TARGET_INCLUDE_HOME[@],\"#{TARGET_INCLUDE_HOME}\"," \
      -e "s,@C_TARGET_SHARE_HOME[@],\"#{TARGET_SHARE_HOME}\"," \
      -e "s%@C_TARGET_MORE_LIBS[@]%\"#{LINKLIBS}\"%" \
      -e "s%@C_TARGET_MORE_STATIC_LIBS[@]%\"#{STATICLINKLIBS}\"%" \
      -e "s,@C_STACK_GROWS_DOWNWARD[@],#{STACKDIRECTION},g" \
      -e "s,@C_CROSS_CHICKEN[@],0,g" \
    <#{(suffix "h.in" f)} >#{(suffix "h" f)}
EOF
} ) )

(define (static-object s)
  (cc (conc s "-static.o") (conc s ".c")) )

(gendefaults "chicken-defaults")

(for-each
 (cut depends <> "chicken.h" "chicken-config.h" "chicken-defaults.h")
 (normalize
  `(runtime.c ,@(suffix "c" LIBSOURCES ULIBSOURCES XLIBSOURCES UXLIBSOURCES)
	      chicken.c ,@(suffix "c" CHICKENSOURCES) csc.c csi.c chicken-profile.c chicken-setup.c)) )

(set! PCREOBJECTS (map (lambda (f) (conc "pcre/" f ".o")) PCRESOURCES))

(@ (begin
     (set! (on x 'CCFLAGS) (conc CCFLAGS " -I. -Ipcre -DSUPPORT_UTF8 -DSUPPORT_UCP -fPIC -DPIC"))
     (depends x "pcre/pcre_internal.h" "pcre/pcre.h" "pcre/config.h" "pcre/ucp.h" "pcre/ucp_findchar.c") )
   PCREOBJECTS)

(@ (object x (suffix "c" x)) PCREOBJECTS)

(link-shared-library "libchicken" "runtime.o" (suffix "o" LIBSOURCES) PCREOBJECTS)
(link-shared-library "libuchicken" "uruntime.o" (suffix "o" ULIBSOURCES) PCREOBJECTS)
(object "uruntime.o" "runtime.c")
(object "runtime.o" "runtime.c")

(program "csi" "banner.scm")
(program "chicken-profile")
(program "chicken-setup")
(program "csc")

(set! (on (cons "runtime.o" (suffix "o" LIBSOURCES)) 'CCFLAGS) 
  (conc CCFLAGS " -fPIC -DPIC -DC_BUILDING_LIBCHICKEN "))
(set! (on (cons "runtime-static.o" (map (cut conc <> "-static.o") LIBSOURCES)) 'CCFLAGS) 
  (conc CCFLAGS " -DC_BUILDING_LIBCHICKEN "))
(set! (on (cons "uruntime.o" (suffix "o" ULIBSOURCES0)) 'CCFLAGS)
  (conc CCFLAGS " -fPIC -DPIC -DC_BUILDING_LIBCHICKEN -DC_UNSAFE_RUNTIME -DNDEBUG "))
(set! (on (cons "uruntime-static.o" (map (cut conc <> "-static.o") ULIBSOURCES0)) 'CCFLAGS)
  (conc CCFLAGS " -DC_BUILDING_LIBCHICKEN -DC_UNSAFE_RUNTIME -DNDEBUG "))
(set! (on '(csi.o chicken.o chicken-profile.o chicken-setup.o csc.o) 'CCFLAGS) (conc CCFLAGS " "))

(main "chicken" "chicken.c" (suffix "c" CHICKENSOURCES))
(depends "chicken.c" "chicken-more-macros.scm" "chicken-ffi-macros.scm" "tweaks.scm" (suffix SUFSHR "libchicken"))
(set! (on "chicken" 'LINKLIBS) (conc LINKLIBS " -L. -lchicken"))
(set! (on "chicken" 'LINKFLAGS) ($ SHAREDLINKFLAGS))

(static-program "chicken-static" "chicken" CHICKENSOURCES)
(static-program "csi-static" "csi")

(depends "support.c" "banner.scm")
(depends "csi.c" "banner.scm" "chicken-more-macros.scm")
(depends "csc.c" "chicken-more-macros.scm")
(depends "chicken-profile.c" "chicken-more-macros.scm")
(depends "chicken-setup.c" "chicken-more-macros.scm")

(set! PCRESTATICOBJECTS (map (lambda (f) (conc "pcre/" f "-static.o")) PCRESOURCES))
(@ (cc (conc "pcre/" x "-static.o") (conc "pcre/" x ".c")) PCRESOURCES)

(@ (begin
     (set! (on x 'CCFLAGS) (conc CCFLAGS " -DSUPPORT_UTF8 -DSUPPORT_UCP"))
     (depends x "pcre/pcre_internal.h" "pcre/pcre.h" "pcre/config.h" "pcre/ucp.h" "pcre/ucp_findchar.c") )
   PCRESTATICOBJECTS)

(link-library "libchicken" "runtime-static.o" (map (cut conc <> "-static.o") LIBSOURCES) PCRESTATICOBJECTS)
(link-library "libuchicken" "uruntime-static.o" (map (cut conc <> "-static.o") ULIBSOURCES) PCRESTATICOBJECTS)
(depends "library.c" "build.scm")
(depends "ulibrary.c" "build.scm")

(clean "chicken-defaults.h" (suffix "o" ULIBSOURCES))

(@ (scheme x x) (append '("chicken") LIBSOURCES CHICKENSOURCES XLIBSOURCES))
(@ (scheme (conc "u" x) x) LIBSOURCES0 XLIBSOURCES)
(for-each static-object (append LIBSOURCES ULIBSOURCES0))
(@ (cc (suffix "o" x) (suffix ".c" x)) LIBSOURCES ULIBSOURCES0)
(cc "uruntime-static.o" "runtime.c")
(cc "runtime-static.o" "runtime.c")

(define (dest d)
  (if (->boolean ($ DESTDIR))
      (path DESTDIR (pathname-file d))
      d) )

(install-bin (dest BINDIR) "chicken" "chicken-static" "csi" "csi-static" "csc" "chicken-profile" "chicken-setup")
(install-bin (dest LIBDIR) (suffix SUFSHR "libchicken" "libuchicken"))
(install-lib (dest LIBDIR) "libchicken.a" "libuchicken.a")
(install-man (dest MANDIR) "chicken.1" "csi.1" "csc.1" "chicken-profile.1" "chicken-setup.1")
(install-file (dest INCDIR) "chicken.h" "chicken-defaults.h" "chicken-config.h")
(install-file (dest DOCDIR) "ChangeLog" "README" "LICENSE")
(install-file (path (dest DOCDIR) "html") (glob "html/*") )

(notfile "spotless")
(depends "spotless" "clean")
(actions
 "spotless"
 ^{rm -f ,(suffix "c" LIBSOURCES) 
      ,(suffix "c" CHICKENSOURCES) 
      ,(suffix "c" ULIBSOURCES0) 
      html/*})

(notfile "wikisync")
(actions "wikisync" ^{rsync -av --existing ,(conc WIKIDIR "/") wiki/})

(notfile "doc")
(depends "doc" "site/ChangeLog.txt")
(actions 
 "doc"
 ^(begin
    {,CSI -s misc/makedoc -pdf}
    {cp chicken.pdf site}))
(depends "site/ChangeLog.txt" "ChangeLog")
(actions "ChangeLog" ^{darcs changes >ChangeLog})
(actions "site/ChangeLog.txt" ^{cp ChangeLog site/ChangeLog.txt})

(notfile "dist")
(actions 
 "dist"
 ^(let* ((files (read-lines "distribution/manifest"))
	 (distname (conc "chicken-" BUILDVERSION)) 
	 (distfiles (map (cut prefix distname <>) files)) 
	 (tgz (conc distname ".tar.gz"))
	 (zip (conc distname ".zip")) )
    {rm -fr ,distname ,tgz ,zip}
    {mkdir -p ,distname
	   ,@(map (cut path distname <>) 
		  (delete-duplicates (filter-map prefix files) string=?))}
    (for-each
     (lambda (f)
       (if (equal? "boot/cfiles" (prefix f))
	   {cp -p ,(pathname-strip-directory f) ,(path distname f)}
	   (begin
	     (if (file-exists? f)
		 {cp -p ,f ,(path distname f)} 
		 (warning "file does not exist" f) ) ) ) )
     files)
    {cd ,distname ";" sh autogen.sh}
    {tar cfz ,(conc distname ".tar.gz") ,distname}
    {zip -qr ,(conc distname ".zip") ,distname}
    {cp ,tgz ,zip site}
    {rm -fr ,distname} ) )

(depends "dist" (suffix "c" XLIBSOURCES UXLIBSOURCES) "site/ChangeLog.txt")

(notfile "tags")
(actions "tags" ^{etags *.scm runtime.c chicken.h})

(notfile "testdist")
(actions 
 "testdist"
 ^(let* ((bdir "/tmp/test-dist-build")
	 (sdir "/tmp/test-dist-build/chicken-*")
	 (bbdir "/tmp/test-dist-build/build")
	 (idir "/tmp/test-dist-build/inst")
	 (tgz (conc "chicken-" BUILDVERSION ".tar.gz")) )
    {build dist}
    {mkdir -p ,bdir}
    {tar xfz ,(conc "site/" tgz) -C ,bdir}
    {cd ,sdir ";" ./configure ,(conc "--prefix=" bdir "/inst")}
    {cd ,sdir ";" make}
    {cd ,sdir ";" make install}
    {cd ,idir ";" "CSI_OPTIONS= echo ,r |" bin/csi -n}
    {cd ,idir ";" bin/chicken-setup -dv bloom-filter}
    {rm -fr ,sdir ,idir}
    {tar xfz *.tar.gz -C ,bdir}
    {mkdir -p ,bbdir}
    {cd ,bbdir ";" cmake ,(conc "-DCMAKE_INSTALL_PREFIX=" idir) ../chicken-*}
    {cd ,bbdir ";" make VERBOSE=1}
    {cd ,bbdir ";" make VERBOSE=1 install}
    {cd ,idir ";" "CSI_OPTIONS= echo ,r |" bin/csi -n}
    {cd ,idir ";" bin/chicken-setup -dv bloom-filter}
    {rm -fr ,bdir} ) )

(notfile "release")
(depends "release" "dist" "doc")
