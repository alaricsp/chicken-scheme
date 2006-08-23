### Makefile.am - by Doug Quale, modified by felix and improved by Tony Garnock-Jones, Panagiotis Vossos, and Brandon Van Every

AUTOMAKE_OPTIONS = foreign no-dependencies dist-zip

LIBTOOL_DEPS = @LIBTOOL_DEPS@
libtool: $(LIBTOOL_DEPS)
	$(SHELL) ./config.status --recheck

# -version-info for libtool to use in setting the SONAME of the shared libs.
lt_version_info = 0:0:0

HAVE_ALLOCA_H=@HAVE_ALLOCA_H@
STACK_GROWS_DOWNWARD=@STACK_GROWS_DOWNWARD@
NO_STATIC_EXECUTABLES=@NO_STATIC_EXECUTABLES@
LINKFLAGS=@LINKFLAGS@
EXTRA_LINK_FLAGS=@EXTRA_LINK_FLAGS@
BINARY_VERSION=@BINARY_VERSION@
if IS_MINGW
MORE_LIBS = @LIBS@ @SHLIBS@ @WINSOCKLIB@
MORE_STATIC_LIBS = @LIBS@ @WINSOCKLIB@
else
MORE_LIBS = @LIBS@ @SHLIBS@ 
MORE_STATIC_LIBS = @LIBS@ 
endif

BOOTSTRAP_PATH = .
CHICKEN = $(BOOTSTRAP_PATH)/chicken
CSI = $(BOOTSTRAP_PATH)/csi
CHICKEN_FLAGS = -quiet -no-trace -optimize-level 2 -include-path $(srcdir) $(CHICKEN_EXTRA_FLAGS)
CHICKEN_UFLAGS = -quiet -no-trace -no-lambda-info -optimize-level 2 -unsafe -feature unsafe -include-path $(srcdir)
DOCFILES = ChangeLog README LICENSE
AM_MAKEINFOFLAGS=--no-split

# The following seems to be needed to make the `install-strip' target work with scripts, on some UNIXen
# (suggested by Sven Hartrumpf)
INSTALL_SCRIPT = @INSTALL@

if NO_STATIC_EXECUTABLES
bin_PROGRAMS = chicken csi csc chicken-profile chicken-setup
else
bin_PROGRAMS = chicken chicken-static csi csi-static csc chicken-profile chicken-setup
endif

man_MANS = chicken.1 csi.1 csc.1 chicken-profile.1 chicken-setup.1

chicken_SOURCES = 
chicken_LDADD = chicken.lo support.lo compiler.lo optimizer.lo c-platform.lo c-backend.lo batch-driver.lo libchicken.la $(MORE_LIBS)
chicken_LDFLAGS = $(LINKFLAGS)

chicken_static_SOURCES = 
chicken_static_LDADD = chicken.o support.o compiler.o optimizer.o c-platform.o c-backend.o batch-driver.o libchicken.la $(MORE_STATIC_LIBS)
chicken_static_LDFLAGS = -static $(LINKFLAGS)

INCLUDES= -DC_NO_PIC_NO_DLL

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

compiler.c : tweaks.scm
optimizer.c : tweaks.scm
c-backend.c : tweaks.scm
support.c : tweaks.scm banner.scm
c-platform.c : tweaks.scm

chicken-defaults.h:
	@echo creating $@; \
	echo the CHICKEN_HOME install directory is $(pkgdatadir); \
	echo "/*** Program generated file: any edits may be lost ***/" >chicken-defaults.h; \
	echo "#ifndef C_INSTALL_CC" >> chicken-defaults.h; \
	echo "# define C_INSTALL_CC \"$(CC)\"" >>chicken-defaults.h; \
	echo "#endif" >> chicken-defaults.h; \
	echo "#ifndef C_INSTALL_CXX" >> chicken-defaults.h; \
	echo "# define C_INSTALL_CXX \"$(CXX)\"" >>chicken-defaults.h; \
	echo "#endif" >> chicken-defaults.h; \
	echo "#ifndef C_INSTALL_CFLAGS" >> chicken-defaults.h; \
	echo "# define C_INSTALL_CFLAGS \"-DHAVE_CHICKEN_CONFIG_H $(CFLAGS)\"" >>chicken-defaults.h; \
	echo "#endif" >> chicken-defaults.h; \
	echo "#ifndef C_INSTALL_SHARE_HOME" >> chicken-defaults.h; \
	echo "# define C_INSTALL_SHARE_HOME \"$(pkgdatadir)\"" >>chicken-defaults.h; \
	echo "#endif" >> chicken-defaults.h; \
	echo "#ifndef C_INSTALL_BIN_HOME" >> chicken-defaults.h; \
	echo "# define C_INSTALL_BIN_HOME \"$(bindir)\"" >>chicken-defaults.h; \
	echo "#endif" >> chicken-defaults.h; \
	echo "#ifndef C_INSTALL_EGG_HOME" >> chicken-defaults.h; \
	echo "# define C_INSTALL_EGG_HOME \"$(pkglibdir)/$(BINARY_VERSION)\"" >>chicken-defaults.h; \
	echo "#endif" >> chicken-defaults.h; \
	echo "#ifndef C_INSTALL_LIB_HOME" >> chicken-defaults.h; \
	echo "# define C_INSTALL_LIB_HOME \"$(libdir)\"" >>chicken-defaults.h; \
	echo "#endif" >> chicken-defaults.h; \
	echo "#ifndef C_INSTALL_STATIC_LIB_HOME" >> chicken-defaults.h; \
	echo "# define C_INSTALL_STATIC_LIB_HOME \"$(libdir)\"" >>chicken-defaults.h; \
	echo "#endif" >> chicken-defaults.h; \
	echo "#ifndef C_INSTALL_INCLUDE_HOME" >> chicken-defaults.h; \
	echo "# define C_INSTALL_INCLUDE_HOME \"$(includedir)\"" >>chicken-defaults.h; \
	echo "#endif" >> chicken-defaults.h; \
	echo "#ifndef C_INSTALL_MORE_LIBS" >> chicken-defaults.h; \
	echo "# define C_INSTALL_MORE_LIBS \"$(MORE_LIBS)\"" >>chicken-defaults.h; \
	echo "#endif" >> chicken-defaults.h; \
	echo "#ifndef C_INSTALL_MORE_STATIC_LIBS" >> chicken-defaults.h; \
	echo "# define C_INSTALL_MORE_STATIC_LIBS \"$(MORE_STATIC_LIBS)\"" >>chicken-defaults.h; \
	echo "#endif" >> chicken-defaults.h; \
	echo "#ifndef C_DEFAULT_TARGET_STACK_SIZE" >> chicken-defaults.h; \
	echo "# define C_DEFAULT_TARGET_STACK_SIZE (128*1024)" >>chicken-defaults.h; \
	echo "#endif" >> chicken-defaults.h; \
	echo "#ifndef C_STACK_GROWS_DOWNWARD" >> chicken-defaults.h; \
	echo "# define C_STACK_GROWS_DOWNWARD ($(STACK_GROWS_DOWNWARD))" >> chicken-defaults.h; \
	echo "#endif" >> chicken-defaults.h

# csi.c is a built source compiled by chicken from csi.scm
csi_SOURCES =
csi_LDADD = csi.lo libchicken.la $(MORE_LIBS)
csi_LDFLAGS = $(LINKFLAGS)

csi_static_SOURCES =
csi_static_LDADD = csi.o libchicken.la $(MORE_STATIC_LIBS)
csi_static_LDFLAGS = -static $(LINKFLAGS)

csi.c: csi.scm banner.scm $(CHICKEN)
	$(CHICKEN) $< $(CHICKEN_FLAGS) -output-file $@

csc_SOURCES =
csc_LDADD = csc.lo libchicken.la $(MORE_LIBS)
csc_LDFLAGS = $(LINKFLAGS)

csc.c: csc.scm $(CHICKEN)
	$(CHICKEN) $< $(CHICKEN_FLAGS) -output-file $@

chicken_profile_SOURCES =
chicken_profile_LDADD = chicken-profile.lo libchicken.la $(MORE_LIBS)
chicken_profile_LDFLAGS = $(LINKFLAGS)

chicken-profile.c: chicken-profile.scm $(srcdir)/build.scm $(CHICKEN)
	$(CHICKEN) $< $(CHICKEN_FLAGS) -output-file $@

chicken_setup_SOURCES =
chicken_setup_LDADD = chicken-setup.lo libchicken.la $(MORE_LIBS)
chicken_setup_LDFLAGS = $(LINKFLAGS)

chicken-setup.c: chicken-setup.scm $(srcdir)/build.scm $(CHICKEN)
	$(CHICKEN) $< $(CHICKEN_FLAGS) -output-file $@

include_HEADERS = chicken.h chicken-config.h chicken-defaults.h
lib_LTLIBRARIES = libchicken.la libuchicken.la

if IS_MINGW
posixexports = posixwin.exports
if USE_PCRE
regexexports = pcre.exports
chickenlibsources = extras.c match.c lolevel.c utils.c tcp.c pcre.c posixwin.c stub.c
chickenulibsources = uextras.c match.c ulolevel.c uutils.c utcp.c upcre.c uposixwin.c stub.c
else
if USE_PREGEX
regexexports = pregexp.exports
chickenlibsources = extras.c match.c lolevel.c utils.c tcp.c pregexp.c posixwin.c stub.c
chickenulibsources = uextras.c match.c ulolevel.c uutils.c utcp.c upregexp.c uposixwin.c stub.c
else
regexexports = regexunix.exports
chickenlibsources = extras.c match.c lolevel.c utils.c tcp.c regexunix.c posixwin.c stub.c
chickenulibsources = uextras.c match.c ulolevel.c uutils.c utcp.c uregexunix.c uposixwin.c stub.c
endif
endif
else
posixexports = posixunix.exports
if USE_PCRE
regexexports = pcre.exports
chickenlibsources = extras.c match.c lolevel.c utils.c tcp.c pcre.c posixunix.c stub.c
chickenulibsources = uextras.c match.c ulolevel.c uutils.c utcp.c upcre.c uposixunix.c stub.c
else
if USE_PREGEX
regexexports = pregexp.exports
chickenlibsources = extras.c match.c lolevel.c utils.c tcp.c pregexp.c posixunix.c stub.c
chickenulibsources = uextras.c match.c ulolevel.c uutils.c utcp.c upregexp.c uposixunix.c stub.c
else
regexexports = regexunix.exports
chickenlibsources = extras.c match.c lolevel.c utils.c tcp.c regexunix.c posixunix.c stub.c
chickenulibsources = uextras.c match.c ulolevel.c uutils.c utcp.c uregexunix.c uposixunix.c stub.c
endif
endif
endif

libchicken_la_SOURCES = runtime.c library.c eval.c profiler.c scheduler.c $(chickenlibsources) \
  srfi-1.c srfi-4.c srfi-13.c srfi-14.c srfi-18.c
libchicken_la_CFLAGS  = -DC_BUILDING_LIBCHICKEN
libchicken_la_LDFLAGS = -version-info=$(lt_version_info) -no-undefined $(MORE_STATIC_LIBS)

libuchicken_la_SOURCES = runtime.c ulibrary.c ueval.c profiler.c scheduler.c $(chickenulibsources) \
  usrfi-1.c usrfi-4.c usrfi-13.c usrfi-14.c usrfi-18.c
libuchicken_la_CFLAGS  = -DC_BUILDING_LIBCHICKEN -DC_UNSAFE_RUNTIME -DNDEBUG
libuchicken_la_LDFLAGS = -version-info=$(lt_version_info) -no-undefined $(MORE_STATIC_LIBS)


# Scheme library files to install.
pkgdata_DATA = chicken-more-macros.scm \
  banner.scm chicken-ffi-macros.scm library.exports eval.exports srfi-1.exports srfi-4.exports \
  srfi-13.exports srfi-14.exports srfi-18.exports utils.exports extras.exports \
  eval.exports tcp.exports regex.exports posix.exports lolevel.exports scheduler.exports

posix.exports: $(posixexports)
	cp $(posixexports) posix.exports

regex.exports: $(regexexports)
	cp $(regexexports) regex.exports

# Install docs and sources.
install-data-local:
	$(mkinstalldirs) $(DESTDIR)$(pkgdatadir)/doc
	-for file in $(DOCFILES); do cp $(srcdir)/$$file $(DESTDIR)$(pkgdatadir)/doc; done
	$(mkinstalldirs) $(DESTDIR)$(pkglibdir)/$(BINARY_VERSION)

# Some generated files we need to clean up.
# Note that a tarball distribution can clobber its *.c boot files
# and have no way of getting them back from the Makefile, but the
# user can always just unpack the tarball again.
CLEANFILES = c-backend.c c-platform.c chicken-defaults.h chicken-profile.c chicken-setup.c chicken.c compiler.c csc.c csc csi.c eval.c extras.c library.c lolevel.c match.c optimizer.c posixunix.c profiler.c regexunix.c scheduler.c srfi-1.c srfi-4.c srfi-13.c srfi-14.c srfi-18.c stub.c support.c tcp.c ueval.c uextras.c ulibrary.c ulolevel.c uposixunix.c uregexunix.c usrfi-1.c usrfi-4.c usrfi-13.c usrfi-14.c usrfi-18.c utcp.c utils.c uutils.c
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

# Building C files from Scheme sources.

SUFFIXES = .scm

# Rules to build C files from Scheme sources.  These are not used in a
# normal build but are required for stage2 and stage3 builds.  They
# won't work for a complete bootstrap since they require chicken.
# That's OK since we distribute all the chicken compiler-generated C
# files except csi.c.  These rules should depend on chicken but that
# would introduce a circular dependency.  (Even if it wasn't a
# circular dependency a change to a single Scheme source file would
# force make to recompile all of them.)

chicken.c: chicken.scm tweaks.scm
	$(CHICKEN) chicken.scm $(CHICKEN_FLAGS) -output-file chicken.c

# Scheme library units must be compiled with -explicit-use.
%.c: %.scm
	$(CHICKEN) $< $(CHICKEN_FLAGS) -output-file $@ -explicit-use

library.c: $(srcdir)/build.scm
pcre.c pregexp.c regexunix.c: regex-common.scm

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
uregexunix.c: regexunix.scm regex-common.scm
	$(CHICKEN) regexunix.scm $(CHICKEN_UFLAGS) -output-file uregexunix.c -explicit-use
upregexp.c: pregexp.scm regex-common.scm
	$(CHICKEN) pregexp.scm $(CHICKEN_UFLAGS) -output-file upregexp.c -explicit-use
upcre.c: pcre.scm regex-common.scm
	$(CHICKEN) pcre.scm $(CHICKEN_UFLAGS) -output-file upcre.c -explicit-use
usrfi-18.c: srfi-18.scm
	$(CHICKEN) srfi-18.scm $(CHICKEN_UFLAGS) -output-file usrfi-18.c -explicit-use

# Extra dependencies in the Scheme source from Scheme includes.
chicken.c: chicken.scm chicken-more-macros.scm $(srcdir)/build.scm chicken-ffi-macros.scm
csi.c: csi.scm chicken-more-macros.scm $(srcdir)/build.scm


# Rebuild with all checks enabled:

.PHONY : paranoid

paranoid :
	touch *.scm
	$(MAKE) CFLAGS="-g -DPARANOIA" CHICKEN_EXTRA_FLAGS="-feature paranoid"


# Benchmarks:

.PHONY : bench

bench : benchmarks/cscbench
	cd benchmarks; ./cscbench

benchmarks/cscbench: benchmarks/cscbench.scm
	./csc benchmarks/cscbench.scm -o benchmarks/cscbench -compiler $(CHICKEN)


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
