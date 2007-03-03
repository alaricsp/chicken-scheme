### Makefile.am - by Doug Quale, modified by felix and improved by Tony Garnock-Jones, and Panagiotis Vossos.
###               Unification with CMake build implemented by Brandon Van Every.

AUTOMAKE_OPTIONS = foreign no-dependencies

LIBTOOL_DEPS = @LIBTOOL_DEPS@
libtool: $(LIBTOOL_DEPS)
	$(SHELL) ./config.status --recheck

# -version-info for libtool to use in setting the SONAME of the shared libs.
lt_version_info = 0:0:0

HAVE_ALLOCA_H=@HAVE_ALLOCA_H@
STACK_GROWS_DOWNWARD=@STACK_GROWS_DOWNWARD@
NO_STATIC_EXECUTABLES=@NO_STATIC_EXECUTABLES@
BINARY_VERSION=@BINARY_VERSION@

if IS_MINGW
MORE_LIBS = @LIBS@ @SHLIBS@ @WINSOCKLIB@
MORE_STATIC_LIBS = @LIBS@ @WINSOCKLIB@
else
MORE_LIBS = @LIBS@ @SHLIBS@ 
MORE_STATIC_LIBS = @LIBS@ 
endif

# These C_INSTALL_* variables are necessary so that
# both ./configure and CMake do the same variable substitutions.

C_INSTALL_CC=\"$(CC)\"
C_INSTALL_CXX=\"$(CXX)\"
C_INSTALL_CFLAGS=\"-DHAVE_CHICKEN_CONFIG_H $(CFLAGS)\"
C_INSTALL_SHARE_HOME=\"$(pkgdatadir)\"
C_INSTALL_BIN_HOME=\"$(bindir)\"
C_INSTALL_EGG_HOME=\"$(pkglibdir)/$(BINARY_VERSION)\"
C_INSTALL_LIB_HOME=\"$(libdir)\"
C_INSTALL_STATIC_LIB_HOME=\"$(libdir)\"
C_INSTALL_INCLUDE_HOME=\"$(includedir)\"
C_INSTALL_MORE_LIBS=\"$(MORE_LIBS)\"
C_INSTALL_MORE_STATIC_LIBS=\"$(MORE_STATIC_LIBS)\"
C_DEFAULT_TARGET_STACK_SIZE=(128*1024)
C_STACK_GROWS_DOWNWARD=($(STACK_GROWS_DOWNWARD))
C_TARGET_CC=\"@TARGET_CC@\"
C_TARGET_CXX=\"@TARGET_CXX@\"
C_TARGET_CFLAGS=\"@TARGET_CFLAGS@\"
C_CROSS_CHICKEN=@CROSS_CHICKEN@

if USE_TARGET_MORE_LIBS
 C_TARGET_MORE_LIBS=\"@TARGET_MORE_LIBS@\"
else
 C_TARGET_MORE_LIBS=\"$(MORE_LIBS)\"
endif

if USE_TARGET_MORE_LIBS
 C_TARGET_MORE_STATIC_LIBS=\"@TARGET_MORE_STATIC_LIBS@\"
else
 C_TARGET_MORE_STATIC_LIBS=\"$(MORE_STATIC_LIBS)\"
endif

if USE_TARGET_PREFIX
 C_TARGET_LIB_HOME=\"@TARGET_PREFIX@/lib\"
 C_TARGET_SHARE_HOME=\"@TARGET_PREFIX@/share\"
 C_TARGET_INCLUDE_HOME=\"@TARGET_PREFIX@/include\"
 C_TARGET_STATIC_LIB_HOME=\"@TARGET_PREFIX@/lib\"
else
 C_TARGET_LIB_HOME=\"$(libdir)\"
 C_TARGET_SHARE_HOME=\"$(pkgdatadir)\"
 C_TARGET_INCLUDE_HOME=\"$(includedir)\"
 C_TARGET_STATIC_LIB_HOME=\"$(libdir)/lib\"
endif

BOOTSTRAP_PATH = .
CHICKEN = $(BOOTSTRAP_PATH)/chicken
CSI = $(BOOTSTRAP_PATH)/csi
CHICKEN_FLAGS = -quiet -no-trace -optimize-level 2 -include-path $(srcdir) $(CHICKEN_EXTRA_FLAGS)
CHICKEN_UFLAGS = -quiet -no-trace -no-lambda-info -optimize-level 2 -unsafe -feature unsafe -include-path $(srcdir)
DOCFILES = ChangeLog README LICENSE

# The following seems to be needed to make the `install-strip' target work with scripts, on some UNIXen
# (suggested by Sven Hartrumpf)
INSTALL_SCRIPT = @INSTALL@

bin_PROGRAMS = chicken csi csc chicken-profile chicken-setup
if ! NO_STATIC_EXECUTABLES
  bin_PROGRAMS += chicken-static csi-static
endif

man_MANS = chicken.1 csi.1 csc.1 chicken-profile.1 chicken-setup.1

# Everything relies on chicken.h, and it includes chicken-defaults.h.
# chicken-defaults.h does not exist at configuration time because
# Autoconf enforces the GNU Coding Standards.  Installation pathnames
# cannot be expanded in configure.in, they must be expanded in Makefile.am.
# So...
#
# chicken-defaults.h can be built with "make all" or "make chicken-defaults.h".
# Unfortunately, it cannot be built by a "make foo" that needs it, as
# there is no dependency information.  This is a limitation of the
# Autotools dependency generation.  See the "Built Sources" section of
# its documentation for examples of this general problem.  Explicit
# build rules are possible, but they interfere with automatically
# generated build rules, and thus are a bad idea.

BUILT_SOURCES = chicken-defaults.h

# We make chicken-defaults.h depend on the Makefile in case anyone
# changes these installation variables manually.  Unfortunately,
# people could make Makefile changes that have nothing to do with
# these variables, but nevertheless trigger lotsa rebuilding.
# Remains to be seen whether it's a problem in practice.

chicken-defaults.h: chicken-defaults.h.in Makefile
	echo creating $@; \
	sed -e "s,@C_INSTALL_CC[@],$(C_INSTALL_CC)," \
	    -e "s,@C_INSTALL_CXX[@],$(C_INSTALL_CXX)," \
	    -e "s%@C_INSTALL_CFLAGS[@]%$(C_INSTALL_CFLAGS)%" \
	    -e "s,@C_INSTALL_SHARE_HOME[@],$(C_INSTALL_SHARE_HOME)," \
	    -e "s,@C_INSTALL_BIN_HOME[@],$(C_INSTALL_BIN_HOME)," \
	    -e "s,@C_INSTALL_EGG_HOME[@],$(C_INSTALL_EGG_HOME)," \
	    -e "s,@C_INSTALL_LIB_HOME[@],$(C_INSTALL_LIB_HOME)," \
	    -e "s,@C_INSTALL_STATIC_LIB_HOME[@],$(C_INSTALL_STATIC_LIB_HOME)," \
	    -e "s,@C_INSTALL_INCLUDE_HOME[@],$(C_INSTALL_INCLUDE_HOME)," \
	    -e "s%@C_INSTALL_MORE_LIBS[@]%$(C_INSTALL_MORE_LIBS)%" \
	    -e "s%@C_INSTALL_MORE_STATIC_LIBS[@]%$(C_INSTALL_MORE_STATIC_LIBS)%" \
	    -e "s%@C_DEFAULT_TARGET_STACK_SIZE[@]%$(C_DEFAULT_TARGET_STACK_SIZE)%" \
	    -e "s%@C_TARGET_MORE_LIBS[@]%$(C_TARGET_MORE_LIBS)%" \
	    -e "s%@C_TARGET_MORE_STATIC_LIBS[@]%$(C_TARGET_MORE_STATIC_LIBS)%" \
	    -e "s,@C_TARGET_CC[@],$(C_TARGET_CC)," \
	    -e "s,@C_TARGET_CXX[@],$(C_TARGET_CXX)," \
	    -e "s,@C_TARGET_CFLAGS[@],$(C_TARGET_CFLAGS)," \
	    -e "s,@C_TARGET_LIB_HOME[@],$(C_TARGET_LIB_HOME)," \
	    -e "s,@C_TARGET_STATIC_LIB_HOME[@],$(C_TARGET_STATIC_LIB_HOME)," \
	    -e "s,@C_TARGET_INCLUDE_HOME[@],$(C_TARGET_INCLUDE_HOME)," \
	    -e "s,@C_TARGET_SHARE_HOME[@],$(C_TARGET_SHARE_HOME)," \
	    -e "s,@C_STACK_GROWS_DOWNWARD[@],$(C_STACK_GROWS_DOWNWARD),g" \
	    -e "s,@C_CROSS_CHICKEN[@],$(C_CROSS_CHICKEN),g" \
	  <$(srcdir)/chicken-defaults.h.in >chicken-defaults.h;

# If we're a tarball and planning a chickenless boot, we need to copy
# the BOOT_CFILES rather than build them from .scm sources.  Because
# these are BUILT_SOURCES, a "make all" is required when doing a
# chickenless boot.

HAVE_ALL_BOOT_CFILES = @HAVE_ALL_BOOT_CFILES@
BOOT_CFILES = @BOOT_CFILES@

if USE_BOOT_CFILES
############################################################################
# We don't need any .scm dependencies because we're just copying .c files. #
############################################################################

BUILT_SOURCES += $(BOOT_CFILES)
$(BOOT_CFILES) :
	cp $(srcdir)/boot/cfiles/$@ $(srcdir)


else !USE_BOOT_CFILES
###########################################################
# We need .scm dependencies as we will generate .c files. #
###########################################################

compiler.c : tweaks.scm
optimizer.c : tweaks.scm
c-backend.c : tweaks.scm
support.c : tweaks.scm banner.scm
c-platform.c : tweaks.scm
library.c: build.scm

# Rules to build C files from Scheme sources.  These are not used in a
# normal build but are required for stage2 and stage3 builds.  They
# won't work for a complete bootstrap since they require chicken.
# That's OK since we distribute all the chicken compiler-generated C
# files.  These rules should depend on chicken but that
# would introduce a circular dependency.  (Even if it wasn't a
# circular dependency a change to a single Scheme source file would
# force make to recompile all of them.)

chicken.c: chicken.scm chicken-more-macros.scm build.scm chicken-ffi-macros.scm tweaks.scm
	$(CHICKEN) chicken.scm $(CHICKEN_FLAGS) -output-file chicken.c

csi.c: csi.scm banner.scm chicken-more-macros.scm
	$(CHICKEN) $< $(CHICKEN_FLAGS) -output-file $@

csc.c: csc.scm
	$(CHICKEN) $< $(CHICKEN_FLAGS) -output-file $@

chicken-profile.c: chicken-profile.scm
	$(CHICKEN) $< $(CHICKEN_FLAGS) -output-file $@

chicken-setup.c: chicken-setup.scm
	$(CHICKEN) $< $(CHICKEN_FLAGS) -output-file $@

# Building C files from Scheme sources.

SUFFIXES = .scm

# Scheme library units must be compiled with -explicit-use.
%.c: %.scm
	$(CHICKEN) $< $(CHICKEN_FLAGS) -output-file $@ -explicit-use

# Specific rules for unsafe library units:
ueval.c: eval.scm
	$(CHICKEN) eval.scm $(CHICKEN_UFLAGS) -output-file ueval.c -explicit-use
ulibrary.c: library.scm $(srcdir)/build.scm
	$(CHICKEN) library.scm $(CHICKEN_UFLAGS) -output-file ulibrary.c -explicit-use
uextras.c: extras.scm
	$(CHICKEN) extras.scm $(CHICKEN_UFLAGS) -output-file uextras.c -explicit-use
usrfi-4.c: srfi-4.scm
	$(CHICKEN) srfi-4.scm $(CHICKEN_UFLAGS) -output-file usrfi-4.c -explicit-use
usrfi-1.c: srfi-1.scm
	$(CHICKEN) srfi-1.scm $(CHICKEN_UFLAGS) -output-file usrfi-1.c -explicit-use
usrfi-13.c: srfi-13.scm
	$(CHICKEN) srfi-13.scm $(CHICKEN_UFLAGS) -output-file usrfi-13.c -explicit-use
usrfi-14.c: srfi-14.scm
	$(CHICKEN) srfi-14.scm $(CHICKEN_UFLAGS) -output-file usrfi-14.c -explicit-use
uposixwin.c: posixwin.scm
	$(CHICKEN) posixwin.scm $(CHICKEN_UFLAGS) -output-file uposixwin.c -explicit-use
uposixunix.c: posixunix.scm
	$(CHICKEN) posixunix.scm $(CHICKEN_UFLAGS) -output-file uposixunix.c -explicit-use
uutils.c: utils.scm
	$(CHICKEN) utils.scm $(CHICKEN_UFLAGS) -output-file uutils.c -explicit-use
utcp.c: tcp.scm
	$(CHICKEN) tcp.scm $(CHICKEN_UFLAGS) -output-file utcp.c -explicit-use
ulolevel.c: lolevel.scm
	$(CHICKEN) lolevel.scm $(CHICKEN_UFLAGS) -output-file ulolevel.c -explicit-use
uregex.c: regex.scm
	$(CHICKEN) regex.scm $(CHICKEN_UFLAGS) -output-file uregex.c -explicit-use
usrfi-18.c: srfi-18.scm
	$(CHICKEN) srfi-18.scm $(CHICKEN_UFLAGS) -output-file usrfi-18.c -explicit-use

endif !USE_BOOT_CFILES
########################################################
# All .c files have been generated one way or another. #
########################################################


chicken_SOURCES = chicken.c support.c compiler.c optimizer.c c-platform.c c-backend.c batch-driver.c
chicken_LDADD = libchicken.la $(MORE_LIBS)

chicken_static_SOURCES = chicken.c support.c compiler.c optimizer.c c-platform.c c-backend.c batch-driver.c
chicken_static_LDADD = libchicken.la $(MORE_STATIC_LIBS)
chicken_static_LDFLAGS = -static

# csi.c is a built source compiled by chicken from csi.scm
csi_SOURCES = csi.c
csi_LDADD = libchicken.la $(MORE_LIBS)

csi_static_SOURCES = csi.c
csi_static_LDADD = libchicken.la $(MORE_STATIC_LIBS)
csi_static_LDFLAGS = -static

csc_SOURCES = csc.c
csc_LDADD = libchicken.la $(MORE_LIBS)

chicken_profile_SOURCES = chicken-profile.c
chicken_profile_LDADD = libchicken.la $(MORE_LIBS)

chicken_setup_SOURCES = chicken-setup.c
chicken_setup_LDADD = libchicken.la $(MORE_LIBS)

include_HEADERS = chicken.h chicken-config.h chicken-defaults.h
lib_LTLIBRARIES = libchicken.la libuchicken.la

commonlibsources = match.c profiler.c runtime.c scheduler.c stub.c
chickenlibsources = eval.c extras.c library.c lolevel.c utils.c tcp.c \
  srfi-1.c srfi-4.c srfi-13.c srfi-14.c srfi-18.c regex.c
chickenulibsources = ueval.c uextras.c ulibrary.c ulolevel.c uutils.c utcp.c \
  usrfi-1.c usrfi-4.c usrfi-13.c usrfi-14.c usrfi-18.c uregex.c

if IS_MINGW
chickenlibsources += posixwin.c
chickenulibsources += uposixwin.c
else
chickenlibsources += posixunix.c
chickenulibsources += uposixunix.c
endif

pcresources = pcre/pcre_compile.c pcre/pcre_get.c pcre/pcre_printint.c pcre/pcre_ucp_findchar.c \
	pcre/pcre_config.c pcre/pcre_globals.c pcre/pcre_refcount.c pcre/pcre_valid_utf8.c \
	pcre/pcre_dfa_exec.c pcre/pcre_info.c pcre/pcre_study.c pcre/pcre_version.c \
	pcre/pcre_exec.c pcre/pcre_maketables.c pcre/pcre_tables.c pcre/pcre_xclass.c \
	pcre/pcre_fullinfo.c pcre/pcre_ord2utf8.c pcre/pcre_try_flipped.c pcre/chartables.c
pcreoptions = -Ipcre -DSUPPORT_UTF8 -DSUPPORT_UCP -DPCRE_STATIC

libchicken_la_SOURCES = $(commonlibsources) $(chickenlibsources) $(pcresources)
libchicken_la_CFLAGS  = -DC_BUILDING_LIBCHICKEN $(pcreoptions)
libchicken_la_LDFLAGS = -version-info=$(lt_version_info) -no-undefined $(MORE_STATIC_LIBS)

libuchicken_la_SOURCES =$(commonlibsources) $(chickenulibsources) $(pcresources)
libuchicken_la_CFLAGS  = -DC_BUILDING_LIBCHICKEN -DC_UNSAFE_RUNTIME -DNDEBUG $(pcreoptions)
libuchicken_la_LDFLAGS = -version-info=$(lt_version_info) -no-undefined $(MORE_STATIC_LIBS)


# Scheme library files to install.
pkgdata_DATA = chicken-more-macros.scm \
  chicken-ffi-macros.scm library.exports eval.exports srfi-1.exports srfi-4.exports \
  srfi-13.exports srfi-14.exports srfi-18.exports utils.exports extras.exports \
  eval.exports tcp.exports regex.exports posix.exports lolevel.exports scheduler.exports

# Install docs and sources.
install-data-local:
	$(mkinstalldirs) $(DESTDIR)$(pkgdatadir)/doc
	$(mkinstalldirs) $(DESTDIR)$(pkgdatadir)/doc/html
	-for file in $(DOCFILES); do cp $(srcdir)/$$file $(DESTDIR)$(pkgdatadir)/doc; done
	-cp $(srcdir)/html/* $(DESTDIR)$(pkgdatadir)/doc/html
	$(mkinstalldirs) $(DESTDIR)$(pkglibdir)/$(BINARY_VERSION)

# Some generated files we need to clean up.
# Note that a tarball distribution can clobber its *.c boot files
# and have no way of getting them back from the Makefile, but the
# user can always just unpack the tarball again.
CLEANFILES = c-backend.c c-platform.c chicken-defaults.h chicken-profile.c chicken-setup.c chicken.c compiler.c csc.c csc csi.c eval.c extras.c library.c lolevel.c match.c optimizer.c posixunix.c profiler.c scheduler.c srfi-1.c srfi-4.c srfi-13.c srfi-14.c srfi-18.c stub.c support.c tcp.c ueval.c uextras.c ulibrary.c ulolevel.c uposixunix.c usrfi-1.c usrfi-4.c usrfi-13.c usrfi-14.c usrfi-18.c utcp.c utils.c uutils.c regex.c uregex.c
DISTCLEANFILES = $(PACKAGE)-$(VERSION).*
clean-local:
	-rm -rf stage2 stage3 stage2-stamp stage3-stamp

# All of the files like manual.tex should be
# installed by a binary distribution.  Unfortunately there is no
# standard place to install these sorts of files so we must leave that
# to the package builder.
EXTRA_DIST = makefile.vc INSTALL win-install.bat INSTALL-CMake.txt chicken.spec.in *.1


## Rules for the chicken maintainer.

# Staged builds.

# Native C files (not compiled from Scheme source).  We also include
# config.status so that we recreate the same configuration we used
# originally and the libtool script so that we don't need to rebuild it.
STAGEFILES = runtime.c config.status libtool

stage2: stagedir = stage2
stage2: stagefromdir = $(srcdir)
stage3: stagedir = stage3
stage3: stagefromdir = stage2

stage2: $(CHICKEN) $(DISTFILES) stage2-stamp
	cd stage2 && $(SHELL) ./config.status
	$(MAKE) -C stage2 CHICKEN=../chicken
	banner="Comparing C files in ./ and stage2/" \
	dashes=`echo "$$banner" | sed s/./=/g`; \
	echo "$$dashes"; \
	echo "$$banner"; \
	echo "$$dashes"; \
	for file in *.c; do \
	   diff -I "^   200.-..-.. ..:.." -q ./$$file stage2/$$file; \
	done; \
	echo "$$dashes"

stage3: stage2 stage3-stamp
	cd stage3 && $(SHELL) ./config.status	
	$(MAKE) -C stage3 CHICKEN=../stage2/chicken
	banner="Comparing C files in stage2/ and stage3/" \
	dashes=`echo "$$banner" | sed s/./=/g`; \
	echo "$$dashes"; \
	echo "$$banner"; \
	echo "$$dashes"; \
	for file in *.c; do \
           diff -I "^   200.-..-.. ..:.." -q stage2/$$file stage3/$$file; \
	done; \
	echo "$$dashes"

stage2-stamp:
	test -d $(stagedir) || mkdir $(stagedir)
	-chmod 777 $(stagedir)
	$(mkinstalldirs) $(stagedir)/$(srcdir)
	@for file in $(DISTFILES); do \
	  d=$(stagefromdir); \
	  if test -d $$d/$$file; then \
	    cp -pr $$d/$$file $(stagedir)/$$file; \
	  else \
	    test -f $(stagedir)/$$file \
	    || ln $$d/$$file $(stagedir)/$$file 2> /dev/null \
	    || cp -p $$d/$$file $(stagedir)/$$file || :; \
	  fi; \
	done
	-rm -f $(stagedir)/*.c
	@for file in $(STAGEFILES); do \
	  d=$(srcdir); \
	  if test -d $$d/$$file; then \
	    cp -pr $$d/$$file $(stagedir)/$$file; \
	  else \
	    test -f $(stagedir)/$$file \
	    || ln $$d/$$file $(stagedir)/$$file 2> /dev/null \
	    || cp -p $$d/$$file $(stagedir)/$$file || :; \
	  fi; \
	done
	touch stage2-stamp

stage3-stamp:
	test -d $(stagedir) || mkdir $(stagedir)
	-chmod 777 $(stagedir)
	$(mkinstalldirs) $(stagedir)/$(srcdir)
	@for file in $(DISTFILES); do \
	  d=$(stagefromdir); \
	  if test -d $$d/$$file; then \
	    cp -pr $$d/$$file $(stagedir)/$$file; \
	  else \
	    test -f $(stagedir)/$$file \
	    || ln $$d/$$file $(stagedir)/$$file 2> /dev/null \
	    || cp -p $$d/$$file $(stagedir)/$$file || :; \
	  fi; \
	done
	-rm -f $(stagedir)/*.c
	@for file in $(STAGEFILES); do \
	  d=$(srcdir); \
	  if test -d $$d/$$file; then \
	    cp -pr $$d/$$file $(stagedir)/$$file; \
	  else \
	    test -f $(stagedir)/$$file \
	    || ln $$d/$$file $(stagedir)/$$file 2> /dev/null \
	    || cp -p $$d/$$file $(stagedir)/$$file || :; \
	  fi; \
	done
	touch stage3-stamp

.PHONY: stage2 stage3 stage2build stage3build

# Rebuild with all checks enabled:

.PHONY : paranoid

paranoid :
	touch *.scm
	$(MAKE) CFLAGS="-g -DPARANOIA" CHICKEN_EXTRA_FLAGS="-feature paranoid"


# Benchmarks:

.PHONY : bench

bench : cscbench
	cd benchmarks; ../cscbench

cscbench: cscbench.scm
	./csc cscbench.scm -o cscbench -compiler $(CHICKEN)


# If nothing helps...

.PHONY: uninstall

uninstall:
	rm -f $(bindir)/chicken
	rm -f $(bindir)/chicken.exe
	rm -f $(bindir)/chicken-static
	rm -f $(bindir)/chicken-static.exe
	rm -f $(bindir)/chicken-profile
	rm -f $(bindir)/chicken-profile.exe
	rm -f $(bindir)/chicken-setup
	rm -f $(bindir)/chicken-setup.exe
	rm -f $(bindir)/csc
	rm -f $(bindir)/csc.exe
	rm -f $(bindir)/csi
	rm -f $(bindir)/csi.exe
	rm -f $(bindir)/csi-static
	rm -f $(bindir)/csi-static.exe
	rm -f $(bindir)/csibatch.bat
	rm -f $(bindir)/cygchicken-0.dll
	rm -f $(bindir)/cyguchicken-0.dll
	rm -f $(mandir)/man1/chicken.1
	rm -f $(mandir)/man1/chicken-profile.1
	rm -f $(mandir)/man1/chicken-setup.1
	rm -f $(mandir)/man1/csc.1
	rm -f $(mandir)/man1/csi.1
	rm -f $(libdir)/libchicken.*
	rm -f $(libdir)/libuchicken.*
	rm -fr $(pkglibdir)
	rm -fr $(pkgdatadir)
	rm -f $(includedir)/chicken.h
	rm -f $(includedir)/chicken-config.h
	rm -f $(includedir)/chicken-defaults.h
