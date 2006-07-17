# Copyright (c) 2006 by Brandon J. Van Every under MIT-style license.
# See LICENSE section at end of file for full license text.

CMAKE_MINIMUM_REQUIRED(VERSION 2.4)
INCLUDE(CheckIncludeFile)
INCLUDE(CheckSymbolExists)

PROJECT(Chicken)
INCLUDE_DIRECTORIES(${Chicken_SOURCE_DIR})


####################################################################
#  TEXT UTILITIES                                                  #
####################################################################

# General style is to list the rootnames of modules, and tack on
# .scm or .c suffixes as appropriate in context.  Similarly, units
# in unsafe libraries often need 'u' prefixes.  These text
# utilities are more readable than writing the same clunky string
# processing code all the time.

MACRO(ADD_SUFFIX rootlist suffix)
  SET(outlist )
  FOREACH(root ${${rootlist}})
    LIST(APPEND outlist ${root}${suffix})
  ENDFOREACH(root)
  SET(${rootlist} ${outlist})
ENDMACRO(ADD_SUFFIX)

MACRO(ADD_PREFIX prefix rootlist)
  SET(outlist )
  FOREACH(root ${${rootlist}})
    LIST(APPEND outlist ${prefix}${root})
  ENDFOREACH(root)
  SET(${rootlist} ${outlist})
ENDMACRO(ADD_PREFIX)

# In CMake 2.4.2 under the MSYS generator, FILE(TO_NATIVE_PATH ...)
# generates Unix style pathnames.  Although this may be correct behavior
# under the MSYS shell itself, Chicken probably won't be used under the
# MSYS shell.  Generally MSYS is just there for build assistance.
# So, manually convert to Windows native pathnames.

# Quoting policy: if Chicken can't handle a native pathname specified in a
# straightforward manner, then the Chicken sources are considered broken.
# Should not have to do any double secret quoting escape rituals.

MACRO(MAKE_WINDOWS_PATH pathname)
  # An extra \\ escape is necessary to get a \ through CMake's processing.
  # Remove all escapes from whitespace.
  STRING(REPLACE "\\ " " " ${pathname} "${${pathname}}")
  # Use backslashes for directory separation
  STRING(REPLACE "/" "\\" ${pathname} "${${pathname}}")
  # Enclose with UNESCAPED quotes.  This means we need to escape our
  # quotes once here, i.e. with \"
  SET(${pathname} \"${${pathname}}\")
ENDMACRO(MAKE_WINDOWS_PATH)


####################################################################
#  .SCM TO .C BUILD RULES                                          #
####################################################################


# The generated .c files are valid on all platforms, not just the
# ones they are built on.  In particular, this means the POSIX
# units (posixunix.c posixwin.c) and the REGEX units
# (pcre.c pregexp.c regexunix.c) will all be built.  By building all
# .c files, new platforms can be targeted.

# .o files are of
# course platform dependent.  If there are no matching .h files
# or link libraries available on a given platform, then the .o
# files won't be built.  That's the platform dependent part of
# a Chicken build.

# SAFE and UNSAFE builds need different flags.

SET(CHICKEN_FLAGS -quiet -no-trace -optimize-level 2 -include-path ${Chicken_SOURCE_DIR})
SET(CHICKEN_UFLAGS ${CHICKEN_FLAGS} -no-lambda-info -unsafe -feature unsafe)

# We're stuck with using DEPENDS to
# specify our dependencies, there's no other way.  
# In CMake 2.4.2, ADD_FILE_DEPENDENCIES will *not* work in
# conjunction with ADD_CUSTOM_COMMAND.  It is meant to be
# used for .o file dependencies on .c files, not for
# source-file-to-source-file dependencies.  

MACRO(SIMPLE_SCM_TO_C root)
  SET(deproots ${ARGN})
  ADD_PREFIX(${Chicken_SOURCE_DIR}/ deproots)
  ADD_SUFFIX(deproots .scm)
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${root}.c
    MAIN_DEPENDENCY ${Chicken_SOURCE_DIR}/${root}.scm
    DEPENDS ${deproots}
    COMMAND ${CURRENT_CHICKEN} ${Chicken_SOURCE_DIR}/${root}.scm -output-file ${CMAKE_CURRENT_BINARY_DIR}/${root}.c ${CHICKEN_FLAGS}
    COMMENT "Compiling ${Chicken_SOURCE_DIR}/${root}.scm to ${CMAKE_CURRENT_BINARY_DIR}/${root}.c"
  )
ENDMACRO(SIMPLE_SCM_TO_C)

# Library units must be compiled with -explicit-use

MACRO(LIBRARY_SCM_TO_C root)
  SET(deproots ${ARGN})
  ADD_PREFIX(${Chicken_SOURCE_DIR}/ deproots)
  ADD_SUFFIX(deproots .scm)
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${root}.c
    MAIN_DEPENDENCY ${Chicken_SOURCE_DIR}/${root}.scm
    DEPENDS ${deproots}
    COMMAND ${CURRENT_CHICKEN} ${Chicken_SOURCE_DIR}/${root}.scm -output-file ${CMAKE_CURRENT_BINARY_DIR}/${root}.c -explicit-use ${CHICKEN_FLAGS}
    COMMENT "Compiling ${Chicken_SOURCE_DIR}/${root}.scm to ${CMAKE_CURRENT_BINARY_DIR}/${root}.c"
  )
ENDMACRO(LIBRARY_SCM_TO_C)

# Some library units have .exports, others don't.

MACRO(LIBRARY_SCM_TO_C_EXPORTS root)
  SET(deproots ${ARGN})
  ADD_PREFIX(${Chicken_SOURCE_DIR}/ deproots)
  ADD_SUFFIX(deproots .scm)
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${root}.c ${CMAKE_CURRENT_BINARY_DIR}/${root}.exports
    MAIN_DEPENDENCY ${Chicken_SOURCE_DIR}/${root}.scm
    DEPENDS ${deproots}
    COMMAND ${CURRENT_CHICKEN} ${Chicken_SOURCE_DIR}/${root}.scm -output-file ${CMAKE_CURRENT_BINARY_DIR}/${root}.c -explicit-use ${CHICKEN_FLAGS}
    COMMENT "Compiling ${Chicken_SOURCE_DIR}/${root}.scm to ${CMAKE_CURRENT_BINARY_DIR}/${root}.c Exporting ${CMAKE_CURRENT_BINARY_DIR}/${root}.exports"
  )
ENDMACRO(LIBRARY_SCM_TO_C_EXPORTS)

# Unsafe library units get a 'u' prefix and must be compiled with -explicit-use
# NOTE: UNSAFE libraries do not emit .exports as they don't need them.

MACRO(UNSAFE_LIBRARY_SCM_TO_C saferoot)
  SET(deproots ${ARGN})
  ADD_PREFIX(${Chicken_SOURCE_DIR}/ deproots)
  ADD_SUFFIX(deproots .scm)
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/u${saferoot}.c
    MAIN_DEPENDENCY ${Chicken_SOURCE_DIR}/${saferoot}.scm
    DEPENDS ${deproots}
    COMMAND ${CURRENT_CHICKEN} ${Chicken_SOURCE_DIR}/${saferoot}.scm -output-file ${CMAKE_CURRENT_BINARY_DIR}/u${saferoot}.c -explicit-use ${CHICKEN_UFLAGS}
    COMMENT "Compiling ${Chicken_SOURCE_DIR}/${saferoot}.scm to ${CMAKE_CURRENT_BINARY_DIR}/u${saferoot}.c"
  )
ENDMACRO(UNSAFE_LIBRARY_SCM_TO_C)

# Need to generate easyffi.l.silex first.
# This is a key 1st step in the bootstrap.

MACRO(GEN_EASYFFI_C)
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/easyffi.c
    MAIN_DEPENDENCY ${Chicken_SOURCE_DIR}/easyffi.scm
    DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/easyffi.l.silex
    COMMAND ${CURRENT_CHICKEN} ${Chicken_SOURCE_DIR}/easyffi.scm -output-file ${CMAKE_CURRENT_BINARY_DIR}/easyffi.c ${CHICKEN_FLAGS}
    COMMENT "Compiling ${Chicken_SOURCE_DIR}/easyffi.scm to ${CMAKE_CURRENT_BINARY_DIR}/easyffi.c"
  )
ENDMACRO(GEN_EASYFFI_C)

MACRO(GEN_EASYFFI_L_SILEX)
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/easyffi.l.silex
    MAIN_DEPENDENCY ${Chicken_SOURCE_DIR}/easyffi.l
    COMMAND ${CURRENT_SILEX} ${Chicken_SOURCE_DIR}/easyffi.l ${CMAKE_CURRENT_BINARY_DIR}/easyffi.l.silex counters none
    COMMENT "Lexing ${Chicken_SOURCE_DIR}/easyffi.l to ${CMAKE_CURRENT_BINARY_DIR}/easyffi.l.silex"
  )
ENDMACRO(GEN_EASYFFI_L_SILEX)


####################################################################
#  DEDUCE OS SPECIFIC BUILD OPTIONS                                #
####################################################################


# Be careful about whether NOT CYGWIN or NOT CMAKE_COMPILER_IS_GNUCC
# is really intended.  GCC has many similarities of operation across
# Linux, Cygwin, and MinGW, but there are also differences.


#All our various optional libraries go in here.
SET(EXTRA_LIBS )

# Distinguish our .h file logic from other build systems.
ADD_DEFINITIONS(-DCMAKE_BUILD)

# vcbuild.bat installs everything in one flat directory, but that's not
# how we install our build.  Chicken has the flat directory behavior
# hardwired for msvc by default.  Use this to work around it.

ADD_DEFINITIONS(-DHIERARCHICAL_INSTALL)

# Chicken uses the INSTALL_* paths in various tools to generate shell
# commands; for instance, under a Windows command prompt.  So they need
# to be native paths.

# First specify the paths relative to each other in CMake style.
SET(INSTALL_HOME ${CMAKE_INSTALL_PREFIX})
SET(INSTALL_BIN_HOME ${INSTALL_HOME}/bin)
SET(INSTALL_INCLUDE_HOME ${INSTALL_HOME}/include)
SET(INSTALL_LIB_HOME ${INSTALL_HOME}/lib)
SET(CC_PATH ${CMAKE_C_COMPILER})
SET(CXX_PATH ${CMAKE_CXX_COMPILER})

# Then make them native after the path logic is done.
IF(WIN32 AND NOT CYGWIN)
  MAKE_WINDOWS_PATH(INSTALL_HOME)
  MAKE_WINDOWS_PATH(INSTALL_BIN_HOME)
  MAKE_WINDOWS_PATH(INSTALL_INCLUDE_HOME)
  MAKE_WINDOWS_PATH(INSTALL_LIB_HOME)
  MAKE_WINDOWS_PATH(CC_PATH)
  MAKE_WINDOWS_PATH(CXX_PATH)
ELSE(WIN32 AND NOT CYGWIN)
  # Assume that TO_NATIVE_PATH does the right thing.
  # This is a dicey assumption.  Need to test it on other OSes.
  FILE(TO_NATIVE_PATH "${INSTALL_HOME}" INSTALL_HOME)
  FILE(TO_NATIVE_PATH "${INSTALL_BIN_HOME}" INSTALL_BIN_HOME)
  FILE(TO_NATIVE_PATH "${INSTALL_INCLUDE_HOME}" INSTALL_INCLUDE_HOME)
  FILE(TO_NATIVE_PATH "${INSTALL_LIB_HOME}" INSTALL_LIB_HOME)
  FILE(TO_NATIVE_PATH "${CC_PATH}" CC_PATH)
  FILE(TO_NATIVE_PATH "${CXX_PATH}" CXX_PATH)
ENDIF(WIN32 AND NOT CYGWIN)

CONFIGURE_FILE(${Chicken_SOURCE_DIR}/chicken-paths.h.in
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-paths.h)

# Does MSVC really require special treatment?
# Do any other compilers?
# What if the nursery determines a smaller stack size is better?
IF(MSVC)
  SET(DEFAULT_TARGET_STACK_SIZE 300000)
ELSE(MSVC)
  SET(DEFAULT_TARGET_STACK_SIZE 131072)
ENDIF(MSVC)

CHECK_INCLUDE_FILE(alloca.h HAVE_ALLOCA_H)
IF(HAVE_ALLOCA_H)
  ADD_DEFINITIONS(-DHAVE_ALLOCA_H)
ENDIF(HAVE_ALLOCA_H)

CHECK_INCLUDE_FILE(crt_externs.h HAVE_CRT_EXTERNS_H)
IF(HAVE_CRT_EXTERNS_H)
  ADD_DEFINITIONS(-DHAVE_CRT_EXTERNS_H)
ENDIF(HAVE_CRT_EXTERNS_H)

CHECK_INCLUDE_FILE(direct.h HAVE_DIRECT_H)
IF(HAVE_DIRECT_H)
  ADD_DEFINITIONS(-DHAVE_DIRECT_H)
ENDIF(HAVE_DIRECT_H)

CHECK_SYMBOL_EXISTS(gcvt stdlib.h HAVE_GCVT)
IF(HAVE_GCVT)
  ADD_DEFINITIONS(-DHAVE_GCVT)
ENDIF(HAVE_GCVT)

CHECK_INCLUDE_FILE(grp.h HAVE_GRP_H)
IF(HAVE_GRP_H)
  ADD_DEFINITIONS(-DHAVE_GRP_H)
ENDIF(HAVE_GRP_H)

CHECK_INCLUDE_FILE(stdint.h HAVE_STDINT_H)
IF(HAVE_STDINT_H)
  ADD_DEFINITIONS(-DHAVE_STDINT_H)
ENDIF(HAVE_STDINT_H)

CHECK_INCLUDE_FILE(sysexits.h HAVE_SYSEXITS_H)
IF(HAVE_SYSEXITS_H)
  ADD_DEFINITIONS(-DHAVE_SYSEXITS_H)
ENDIF(HAVE_SYSEXITS_H)


# We can create REGEX .c files for all platforms when
# generating .scm --> c. code.  However, we can only
# generate the .o file appropriate to this platform.
# If no REGEX is available, don't generate any REGEX_UNIT
# at all.

# Note that in practice, right now we're not generating
# all the REGEX files we could.  Will need to address
# this when preparing a .c snapshot for other platforms.

SET(REGEX_UNIT )
CHECK_INCLUDE_FILE(pcre.h HAVE_PCRE_H)
IF(HAVE_PCRE_H)
  ADD_DEFINITIONS(-DHAVE_PCRE_H)
  SET(REGEX_UNIT pcre)
  SET(EXTRA_LIBS ${EXTRA_LIBS} pcre)
ELSE(HAVE_PCRE_H)
  IF(WIN32 AND NOT CYGWIN)
    SET(REGEX_UNIT pregexp)
  ELSE(WIN32 AND NOT CYGWIN)
    SET(REGEX_UNIT regexunix)
  ENDIF(WIN32 AND NOT CYGWIN)
ENDIF(HAVE_PCRE_H)

# check for dlfcn.h or dl.h
CHECK_INCLUDE_FILE(dlfcn.h HAVE_DLFCN_H)
IF(HAVE_DLFCN_H)
  ADD_DEFINITIONS(-DHAVE_DLFCN_H)
  # The presence of dlfcn.h does NOT mean that -ldl is used.
  # For instance, Cygwin has dlfcn.h and does not use -ldl.
  FIND_LIBRARY(DL_LIBRARY dl)
  IF(DL_LIBRARY)
    SET(EXTRA_LIBS ${EXTRA_LIBS} dl)
  ENDIF(DL_LIBRARY)
ENDIF(HAVE_DLFCN_H)
CHECK_INCLUDE_FILE(dl.h HAVE_DL_H)
IF(HAVE_DL_H)
  ADD_DEFINITIONS(-DHAVE_DL_H)
  SET(EXTRA_LIBS ${EXTRA_LIBS} ldl)
ENDIF(HAVE_DL_H)

# check for ffi.h
CHECK_INCLUDE_FILE(ffi.h HAVE_FFI_H)
IF(HAVE_FFI_H)
  ADD_DEFINITIONS(-DHAVE_FFI_H)
  SET(EXTRA_LIBS ${EXTRA_LIBS} ffi)
ENDIF(HAVE_FFI_H)

# check for windows.h
CHECK_INCLUDE_FILE(windows.h HAVE_WINDOWS_H)
IF(HAVE_WINDOWS_H)
  ADD_DEFINITIONS(-DHAVE_WINDOWS_H)

  # NOTE: Don't assume we're on Windows.  We could be cross-compiling.

  CHECK_SYMBOL_EXISTS(LoadLibrary windows.h HAVE_LOADLIBRARY)
  IF(HAVE_LOADLIBRARY)
    ADD_DEFINITIONS(-DHAVE_LOADLIBRARY)
  ENDIF(HAVE_LOADLIBRARY)

  CHECK_SYMBOL_EXISTS(GetProcAddress windows.h HAVE_GETPROCADDRESS)
  IF(HAVE_GETPROCADDRESS)
    ADD_DEFINITIONS(-DHAVE_GETPROCADDRESS)
  ENDIF(HAVE_GETPROCADDRESS)

  CHECK_INCLUDE_FILE(winsock2.h HAVE_WINSOCK2_H)
  IF(HAVE_WINSOCK2_H)
    ADD_DEFINITIONS(-DHAVE_WINSOCK2_H)
  ENDIF(HAVE_WINSOCK2_H)

  CHECK_INCLUDE_FILE(ws2tcpip.h HAVE_WS2TCPIP_H)
  IF(HAVE_WS2TCPIP_H)
    ADD_DEFINITIONS(-DHAVE_WS2TCPIP_H)
  ENDIF(HAVE_WS2TCPIP_H)

ENDIF(HAVE_WINDOWS_H)

# We can create POSIX .c files for all platforms when
# generating .scm --> c. code.  However, we can only
# generate the .o file appropriate to this platform.
# If no POSIX is available, don't generate any POSIX_UNIT
# at all.

# Note that in practice, right now we're not generating
# all the POSIX files we could.  Will need to address
# this when preparing a .c snapshot for other platforms.

SET(POSIX_UNIT )
IF(WIN32 AND NOT CYGWIN)
  SET(POSIX_UNIT posixwin)
  SET(EXTRA_LIBS ${EXTRA_LIBS} ws2_32)
ENDIF(WIN32 AND NOT CYGWIN)
IF(UNIX)
  SET(POSIX_UNIT posixunix)
ENDIF(UNIX)

# Shared and static libraries are built with different flags.
# In CMake, SET(x a b c) produces x="a;b;c"
# SET(x "a b c") produces x="a b c", which is what we want.

SET(SHARED_FLAGS "-DPIC -DC_NO_PIC_NO_DLL")
  IF(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
    SET(SHARED_FLAGS "${SHARED_FLAGS} -fno-common -no-cpp-precomp")
  ENDIF(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
SET(STATIC_FLAGS -DC_NO_PIC_NO_DLL)

# Unix platforms can get into trouble if they don't have -lm.
# Windows platforms generally don't need it.
# Uncertain if this is strictly necessary, but we're paranoid.
IF(UNIX)
  SET(EXTRA_LIBS ${EXTRA_LIBS} m)
ENDIF(UNIX)


####################################################################
#  DEFINE STACK_GROWS_DOWNWARD                                     #
####################################################################

TRY_RUN(STACK_GROWS_DOWNWARD
  STACK_TEST_COMPILED
  ${CMAKE_CURRENT_BINARY_DIR}
  ${Chicken_SOURCE_DIR}/StackGrowsDownward.c
)
IF(STACK_TEST_COMPILED)
  IF(STACK_GROWS_DOWNWARD STREQUAL "FAILED_TO_RUN")
    MESSAGE(SEND_ERROR "Compiled stack growth test, but unable to run it.  Using default.")
    SET(STACK_GROWS_DOWNWARD -1)
  ENDIF(STACK_GROWS_DOWNWARD STREQUAL "FAILED_TO_RUN")
ELSE(STACK_TEST_COMPILED)
  MESSAGE(SEND_ERROR "Unable to compile stack growth test.  Using default.")
  SET(STACK_GROWS_DOWNWARD -1)
ENDIF(STACK_TEST_COMPILED)
ADD_DEFINITIONS(-DC_STACK_GROWS_DOWNWARD="${STACK_GROWS_DOWNWARD}")
MESSAGE(STATUS "C_STACK_GROWS_DOWNWARD=${STACK_GROWS_DOWNWARD}")


####################################################################
#  USER OPTIONS                                                    #
####################################################################

# These appear as checkbox items in the CMakeSetup and CCMake GUIs.

OPTION(ENABLE_PROCEDURE_TABLES "enable support for serialization of procedures" TRUE)
IF(ENABLE_PROCEDURE_TABLES)
  ADD_DEFINITIONS(-DC_ENABLE_PTABLES)
ENDIF(ENABLE_PROCEDURE_TABLES)

OPTION(ENABLE_SYMBOL_GC "enable support for GC of unused symbols" FALSE)
IF(ENABLE_SYMBOL_GC)
  ADD_DEFINITIONS(-DC_COLLECT_ALL_SYMBOLS)
ENDIF(ENABLE_SYMBOL_GC)

OPTION(ENABLE_EXTRA_SYMBOL_SLOT "enable extra data slot for symbols" FALSE)
IF(ENABLE_EXTRA_SYMBOL_SLOT)
  ADD_DEFINITIONS(-DC_EXTRA_SYMBOL_SLOT)
ENDIF(ENABLE_EXTRA_SYMBOL_SLOT)

OPTION(DISABLE_APPLY_HOOK "disable apply hook" FALSE)
IF(DISABLE_APPLY_HOOK)
  ADD_DEFINITIONS(-DC_NO_APPLY_HOOK)
ENDIF(DISABLE_APPLY_HOOK)

OPTION(DETERMINE_BEST_STACK_SIZE "run nsample to find best stack size" TRUE)


####################################################################
#  LISTS OF .C SOURCES                                             #
####################################################################


# Library files that are not generated.

SET(LIB_CSOURCE
  chicken.h
  runtime.c
)
MACRO(GET_LIB_CSOURCE out)
  SET(${out} ${LIB_CSOURCE})
  ADD_PREFIX(${Chicken_SOURCE_DIR}/ ${out})
ENDMACRO(GET_LIB_CSOURCE)

# Library files that are always generated the same way and named the
# same thing regardless of whether the library is SAFE or UNSAFE.

SET(LIB_COMMON_CGEN
  match
  profiler
  scheduler
  stub
) 
MACRO(GET_LIB_COMMON_CGEN out)
  SET(${out} ${LIB_COMMON_CGEN})
  ADD_SUFFIX(${out} .c)
  ADD_PREFIX(${CMAKE_CURRENT_BINARY_DIR}/ ${out})
ENDMACRO(GET_LIB_COMMON_CGEN)

# Library files that are generated and named differently depending on
# whether the library is SAFE or UNSAFE.  These are the SAFE
# names.

SET(LIB_SAFE_CGEN
  eval
  extras
  library
  lolevel
  ${POSIX_UNIT}
  ${REGEX_UNIT}
  srfi-1
  srfi-4
  srfi-13
  srfi-14
  srfi-18
  tcp
  tinyclos
  utils
)
MACRO(GET_LIB_SAFE_CGEN out)
  SET(${out} ${LIB_SAFE_CGEN})
  ADD_SUFFIX(${out} .c)
  ADD_PREFIX(${CMAKE_CURRENT_BINARY_DIR}/ ${out})
ENDMACRO(GET_LIB_SAFE_CGEN)

# The UNSAFE library names have a 'u' prefix.

SET(LIB_UNSAFE_CGEN
  ${LIB_SAFE_CGEN}
)
ADD_PREFIX(u LIB_UNSAFE_CGEN)

MACRO(GET_LIB_UNSAFE_CGEN out)
  SET(${out} ${LIB_UNSAFE_CGEN})
  ADD_SUFFIX(${out} .c)
  ADD_PREFIX(${CMAKE_CURRENT_BINARY_DIR}/ ${out})
ENDMACRO(GET_LIB_UNSAFE_CGEN)

# libchicken files that emit .exports
# Wouldn't it be clever to automagically deduce these?
# Note: it's the SAFE list + 'scheduler'.

SET(LIB_EXPORTS
  eval
  extras
  library
  lolevel
  ${POSIX_UNIT}
  ${REGEX_UNIT}
  scheduler
  srfi-1
  srfi-4
  srfi-13
  srfi-14
  srfi-18
  tcp
  tinyclos
  utils
)
MACRO(GET_LIB_EXPORTS out)
  SET(${out} ${LIB_EXPORTS})
  ADD_SUFFIX(${out} .exports)
  ADD_PREFIX(${CMAKE_CURRENT_BINARY_DIR}/ ${out})
ENDMACRO(GET_LIB_EXPORTS)

# Files generated for Chicken.  No SAFE or UNSAFE distinctions.

SET(CHICKEN_CGEN
  batch-driver
  c-backend
  c-platform
  chicken
  compiler
  easyffi
  optimizer
  support
)
MACRO(GET_CHICKEN_CGEN out)
  SET(${out} ${CHICKEN_CGEN})
  ADD_SUFFIX(${out} .c)
  ADD_PREFIX(${CMAKE_CURRENT_BINARY_DIR}/ ${out})
ENDMACRO(GET_CHICKEN_CGEN)

# Localize the .c sources to a particular build subdirectory.
# This is needed because we're doing a multiple stage bootstrap.
# The 1st crop of generated .c files is not going to be the
# same as the 2nd crop.

MACRO(GET_LIBCHICKEN_C_SOURCES out)
  GET_LIB_CSOURCE(csource)
  GET_LIB_COMMON_CGEN(common)
  GET_LIB_SAFE_CGEN(safe)
  SET(${out} ${csource} ${common} ${safe}
    ${CMAKE_CURRENT_BINARY_DIR}/chicken-paths.h)
ENDMACRO(GET_LIBCHICKEN_C_SOURCES)

MACRO(GET_LIBUCHICKEN_C_SOURCES out)
  GET_LIB_CSOURCE(csource)
  GET_LIB_COMMON_CGEN(common)
  GET_LIB_UNSAFE_CGEN(unsafe)
  SET(${out} ${csource} ${common} ${unsafe}
    ${CMAKE_CURRENT_BINARY_DIR}/chicken-paths.h)
ENDMACRO(GET_LIBUCHICKEN_C_SOURCES)

# libchicken-gui just compiles libchicken with -DC_WINDOWS_GUI.
# There are no .c files specific to it.

MACRO(GET_CHICKEN_C_SOURCES out)
  GET_CHICKEN_CGEN(cgen)
  SET(${out}
    ${Chicken_SOURCE_DIR}/chicken.h
    ${CMAKE_CURRENT_BINARY_DIR}/chicken-paths.h
    ${CMAKE_CURRENT_BINARY_DIR}/stack-size.h
    ${Chicken_SOURCE_DIR}/chicken.rc
    ${cgen}
  )
ENDMACRO(GET_CHICKEN_C_SOURCES)


####################################################################
#  BOOTSTRAP                                                       #
####################################################################

# BOOT_CFILES is a list of all .c files necessary to build a bootstrap
# Chicken compiler, without a valid Chicken compiler being available.
# A Chicken build tree can come either with or without these files.
# Typically, when downloaded as a tarball distribution, these files
# will be present.  But when pulled straight from Darcs, these files
# will be absent.  If they are present, then we don't really need a
# Chicken compiler to do the build, although we may choose to use one
# anyways.

SET(BOOT_CFILES
  batch-driver.c
  c-backend.c
  c-platform.c
  chicken.c
  compiler.c
  easyffi.c
  eval.c
  extras.c
  library.c
  lolevel.c
  match.c
  optimizer.c
  pcre.c
  posixunix.c
  posixwin.c
  pregexp.c
  profiler.c
  regexunix.c
  scheduler.c
  silex.c
  srfi-1.c
  srfi-4.c
  srfi-13.c
  srfi-14.c
  srfi-18.c
  stub.c
  support.c
  tcp.c
  tinyclos.c
  utils.c
)

# Now that we've determined everything we need to know to build,
# we can transfer control to a subdirectory and create a 1st
# stage boot compiler.  /boot worries about whether any previous
# Chicken is installed, and whether a previous Chicken is even
# needed.  Let /boot handle any fatal errors.

ADD_SUBDIRECTORY(boot)

# If we come back, then we know we can build chicken-boot.
GET_TARGET_PROPERTY(CHICKEN_BOOT_EXE chicken-boot LOCATION)
SET(CURRENT_CHICKEN ${CHICKEN_BOOT_EXE})

# Now we're compiling with the up-to-date version of Chicken. Yippie!


####################################################################
#  CREATE LIBCHICKEN                                               #
####################################################################

LIBRARY_SCM_TO_C_EXPORTS(eval parameters)
UNSAFE_LIBRARY_SCM_TO_C(eval parameters)
LIBRARY_SCM_TO_C_EXPORTS(extras)
UNSAFE_LIBRARY_SCM_TO_C(extras)
LIBRARY_SCM_TO_C_EXPORTS(library build parameters)
UNSAFE_LIBRARY_SCM_TO_C(library build parameters)
LIBRARY_SCM_TO_C_EXPORTS(lolevel)
UNSAFE_LIBRARY_SCM_TO_C(lolevel)
LIBRARY_SCM_TO_C(match)
SIMPLE_SCM_TO_C(optimizer parameters tweaks)
LIBRARY_SCM_TO_C_EXPORTS(pcre regex-common)
UNSAFE_LIBRARY_SCM_TO_C(pcre regex-common)
LIBRARY_SCM_TO_C_EXPORTS(posixunix)
UNSAFE_LIBRARY_SCM_TO_C(posixunix)
LIBRARY_SCM_TO_C_EXPORTS(posixwin)
UNSAFE_LIBRARY_SCM_TO_C(posixwin)
LIBRARY_SCM_TO_C_EXPORTS(pregexp regex-common)
UNSAFE_LIBRARY_SCM_TO_C(pregexp regex-common)
LIBRARY_SCM_TO_C(profiler)
LIBRARY_SCM_TO_C_EXPORTS(regexunix regex-common)
UNSAFE_LIBRARY_SCM_TO_C(regexunix regex-common)
LIBRARY_SCM_TO_C_EXPORTS(scheduler)
SIMPLE_SCM_TO_C(silex)
LIBRARY_SCM_TO_C_EXPORTS(srfi-1)
UNSAFE_LIBRARY_SCM_TO_C(srfi-1)
LIBRARY_SCM_TO_C_EXPORTS(srfi-4)
UNSAFE_LIBRARY_SCM_TO_C(srfi-4)
LIBRARY_SCM_TO_C_EXPORTS(srfi-13)
UNSAFE_LIBRARY_SCM_TO_C(srfi-13)
LIBRARY_SCM_TO_C_EXPORTS(srfi-14)
UNSAFE_LIBRARY_SCM_TO_C(srfi-14)
LIBRARY_SCM_TO_C_EXPORTS(srfi-18)
UNSAFE_LIBRARY_SCM_TO_C(srfi-18)
LIBRARY_SCM_TO_C(stub)
SIMPLE_SCM_TO_C(support banner parameters tweaks)
LIBRARY_SCM_TO_C_EXPORTS(tcp)
UNSAFE_LIBRARY_SCM_TO_C(tcp)
LIBRARY_SCM_TO_C_EXPORTS(tinyclos)
UNSAFE_LIBRARY_SCM_TO_C(tinyclos)
LIBRARY_SCM_TO_C_EXPORTS(utils)
UNSAFE_LIBRARY_SCM_TO_C(utils)


GET_LIBCHICKEN_C_SOURCES(CHICKEN_LIB_SOURCES)
GET_LIBUCHICKEN_C_SOURCES(CHICKEN_UNSAFE_LIB_SOURCES)

# We want the library to be named 'libchicken' on all platfomrs.
# Default Unix, Cygwin, and MinGW behavior is to add 'lib' as a 
# prefix onto every library.  For cross-platform stuff,
# this often results in the 'liblibfoo" problem.  We don't want
# to write a bunch of special case code.  So, we take
# explicit control of the library prefix and suppress it.
# This works on all platforms.

ADD_LIBRARY(libchicken SHARED ${CHICKEN_LIB_SOURCES})
SET_TARGET_PROPERTIES(libchicken PROPERTIES
  PREFIX "" IMPORT_PREFIX ""
  COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN ${SHARED_FLAGS}"
)
TARGET_LINK_LIBRARIES(libchicken ${EXTRA_LIBS})
ADD_DEPENDENCIES(libchicken chicken-boot)

ADD_LIBRARY(libchicken-static STATIC ${CHICKEN_LIB_SOURCES})
SET_TARGET_PROPERTIES(libchicken-static PROPERTIES
  PREFIX "" IMPORT_PREFIX ""
  COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN ${STATIC_FLAGS}"
)
TARGET_LINK_LIBRARIES(libchicken-static ${EXTRA_LIBS})
ADD_DEPENDENCIES(libchicken-static chicken-boot)

ADD_LIBRARY(libuchicken SHARED ${CHICKEN_UNSAFE_LIB_SOURCES})
SET_TARGET_PROPERTIES(libuchicken PROPERTIES
  PREFIX "" IMPORT_PREFIX ""
  COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN -DC_UNSAFE_RUNTIME ${SHARED_FLAGS}"
)
TARGET_LINK_LIBRARIES(libuchicken ${EXTRA_LIBS})
ADD_DEPENDENCIES(libuchicken chicken-boot)

ADD_LIBRARY(libuchicken-static STATIC ${CHICKEN_UNSAFE_LIB_SOURCES})
SET_TARGET_PROPERTIES(libuchicken-static PROPERTIES
  PREFIX "" IMPORT_PREFIX ""
  COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN -DC_UNSAFE_RUNTIME ${STATIC_FLAGS}"
)
TARGET_LINK_LIBRARIES(libuchicken-static ${EXTRA_LIBS})
ADD_DEPENDENCIES(libuchicken-static chicken-boot)

IF(WIN32 AND NOT CYGWIN)

  SET(GUI_LIBS kernel32 user32 gdi32)

  ADD_LIBRARY(libchickengui SHARED ${CHICKEN_LIB_SOURCES})
  SET_TARGET_PROPERTIES(libchickengui PROPERTIES
    PREFIX "" IMPORT_PREFIX ""
    COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN -DC_WINDOWS_GUI ${SHARED_FLAGS}"
  )
  TARGET_LINK_LIBRARIES(libchickengui ${GUI_LIBS} ${EXTRA_LIBS})
  ADD_DEPENDENCIES(libchickengui chicken-boot)

  ADD_LIBRARY(libchickengui-static STATIC ${CHICKEN_LIB_SOURCES})
  SET_TARGET_PROPERTIES(libchickengui-static PROPERTIES
    PREFIX "" IMPORT_PREFIX ""
    COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN -DC_WINDOWS_GUI ${STATIC_FLAGS}"
  )
  TARGET_LINK_LIBRARIES(libchickengui-static ${GUI_LIBS} ${EXTRA_LIBS})
  ADD_DEPENDENCIES(libchickengui-static chicken-boot)

ENDIF(WIN32 AND NOT CYGWIN)


####################################################################
#  STACK SIZE                                                      #
####################################################################

ADD_CUSTOM_COMMAND(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/nsample.c
  MAIN_DEPENDENCY nsample.scm
  COMMAND ${CURRENT_CHICKEN} ${Chicken_SOURCE_DIR}/nsample.scm -output-file ${CMAKE_CURRENT_BINARY_DIR}/nsample.c -explicit-use -uses library -uses eval ${CHICKEN_FLAGS}
  COMMENT "Compiling ${Chicken_SOURCE_DIR}/nsample.scm to ${CMAKE_CURRENT_BINARY_DIR}/nsample.c"
)
ADD_EXECUTABLE(nsample ${CMAKE_CURRENT_BINARY_DIR}/nsample.c)
SET_TARGET_PROPERTIES(nsample PROPERTIES COMPILE_FLAGS "${STATIC_FLAGS}")
TARGET_LINK_LIBRARIES(nsample libchicken-static)
ADD_DEPENDENCIES(nsample chicken-boot)

GET_TARGET_PROPERTY(NSAMPLE_EXE nsample LOCATION)
ADD_CUSTOM_COMMAND(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/stack-size.h
  MAIN_DEPENDENCY ${Chicken_SOURCE_DIR}/stack-size.h.in
  DEPENDS ${Chicken_SOURCE_DIR}/stack-size.cmake
  COMMAND ${CMAKE_COMMAND}
    -DChicken_SOURCE_DIR=${Chicken_SOURCE_DIR}
    -DNSAMPLE_EXE=${NSAMPLE_EXE}
    -DDEFAULT_TARGET_STACK_SIZE=${DEFAULT_TARGET_STACK_SIZE}
    -DDETERMINE_BEST_STACK_SIZE=${DETERMINE_BEST_STACK_SIZE}
    -P ${Chicken_SOURCE_DIR}/stack-size.cmake
  COMMENT "Configuring ${Chicken_SOURCE_DIR}/stack-size.h.in to ${CMAKE_CURRENT_BINARY_DIR}/stack-size.h"
)
ADD_CUSTOM_TARGET(stack-size
  DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/stack-size.h)
ADD_DEPENDENCIES(stack-size nsample)


####################################################################
#  CREATE CSC                                                      #
####################################################################

# These are flags that CSC wants to hear about when it does its thing.
# These will be substituted into csc.scm using csc.replace.cmake.

GET_DIRECTORY_PROPERTY(CSCFLAGS
  DIRECTORY ${Chicken_SOURCE_DIR}
  DEFINITIONS
)

# CPPFLAGS_STRING can contain quoted pathnames with spaces in them.
# This makes passing it to a CMake script rather tricky.
# Rather than try to mess with -D arguments and quoting escapes,
# we write exactly what we want to a file, then read it when
# performing substitutions on csc.scm.in

CONFIGURE_FILE(${Chicken_SOURCE_DIR}/cscflags.in
  ${CMAKE_CURRENT_BINARY_DIR}/cscflags)

# csc.scm.in needs regex replacement.  CONFIGURE_FILE doesn't have a
# regex capability, but we can get around this using a CMake script.

ADD_CUSTOM_COMMAND(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/csc.scm
  MAIN_DEPENDENCY ${Chicken_SOURCE_DIR}/csc.scm.in
  DEPENDS ${Chicken_SOURCE_DIR}/csc.replace.cmake
  COMMAND ${CMAKE_COMMAND}
    -DChicken_SOURCE_DIR=${Chicken_SOURCE_DIR}
    -DINSTALL_HOME=${INSTALL_HOME}
    -DINSTALL_BIN_HOME=${INSTALL_BIN_HOME}
    -DINSTALL_INCLUDE_HOME=${INSTALL_INCLUDE_HOME}
    -DINSTALL_LIB_HOME=${INSTALL_LIB_HOME}
    -DCC_PATH=${CC_PATH}
    -DCXX_PATH=${CXX_PATH}
    -DMORE_LIBS="${MORE_LIBS}"
    -DMORE_STATIC_LIBS="${MORE_STATIC_LIBS}"
    -P ${Chicken_SOURCE_DIR}/csc.replace.cmake
  COMMENT "Replacing ${Chicken_SOURCE_DIR}/csc.scm.in to ${CMAKE_CURRENT_BINARY_DIR}/csc.scm"
)

# csc.c needs its own custom rule because csc.scm is generated.

ADD_CUSTOM_COMMAND(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/csc.c
  MAIN_DEPENDENCY ${CMAKE_CURRENT_BINARY_DIR}/csc.scm
  DEPENDS ${Chicken_SOURCE_DIR}/banner.scm
  COMMAND ${CURRENT_CHICKEN} ${CMAKE_CURRENT_BINARY_DIR}/csc.scm -output-file ${CMAKE_CURRENT_BINARY_DIR}/csc.c ${CHICKEN_FLAGS}
  COMMENT "Compiling ${CMAKE_CURRENT_BINARY_DIR}/csc.scm to ${CMAKE_CURRENT_BINARY_DIR}/csc.c"
)

ADD_EXECUTABLE(csc ${CMAKE_CURRENT_BINARY_DIR}/csc.c)
SET_TARGET_PROPERTIES(csc PROPERTIES COMPILE_FLAGS "${SHARED_FLAGS}")
TARGET_LINK_LIBRARIES(csc libchicken)
ADD_DEPENDENCIES(csc chicken-boot)


####################################################################
#  CREATE CSI                                                      #
####################################################################

# csi gets special treatment with a prologue.

ADD_CUSTOM_COMMAND(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/csi.c
  MAIN_DEPENDENCY ${Chicken_SOURCE_DIR}/csi.scm
  DEPENDS
    ${Chicken_SOURCE_DIR}/banner.scm
    ${Chicken_SOURCE_DIR}/build.scm
    ${Chicken_SOURCE_DIR}/chicken-more-macros.scm
    ${Chicken_SOURCE_DIR}/parameters.scm
  COMMAND ${CURRENT_CHICKEN} ${Chicken_SOURCE_DIR}/csi.scm -output-file ${CMAKE_CURRENT_BINARY_DIR}/csi.c -prologue ${Chicken_SOURCE_DIR}/build.scm ${CHICKEN_FLAGS}
  COMMENT "Compiling ${Chicken_SOURCE_DIR}/csi.scm to ${CMAKE_CURRENT_BINARY_DIR}/csi.c"
)

ADD_EXECUTABLE(csi ${CMAKE_CURRENT_BINARY_DIR}/csi.c)
SET_TARGET_PROPERTIES(csi PROPERTIES COMPILE_FLAGS "${SHARED_FLAGS}")
TARGET_LINK_LIBRARIES(csi libchicken)
ADD_DEPENDENCIES(csi chicken-boot)

ADD_EXECUTABLE(csi-static ${CMAKE_CURRENT_BINARY_DIR}/csi.c)
SET_TARGET_PROPERTIES(csi-static PROPERTIES COMPILE_FLAGS "${STATIC_FLAGS}")
TARGET_LINK_LIBRARIES(csi-static libchicken-static)
ADD_DEPENDENCIES(csi-static chicken-boot)


####################################################################
#  CREATE CHICKEN-PROFILE                                          #
####################################################################

SIMPLE_SCM_TO_C(chicken-profile)
ADD_EXECUTABLE(chicken-profile ${CMAKE_CURRENT_BINARY_DIR}/chicken-profile.c)
SET_TARGET_PROPERTIES(chicken-profile PROPERTIES COMPILE_FLAGS "${SHARED_FLAGS}")
TARGET_LINK_LIBRARIES(chicken-profile libchicken)
ADD_DEPENDENCIES(chicken-profile chicken-boot)


####################################################################
#  CREATE CHICKEN-SETUP                                            #
####################################################################

SIMPLE_SCM_TO_C(chicken-setup build parameters)
ADD_EXECUTABLE(chicken-setup ${CMAKE_CURRENT_BINARY_DIR}/chicken-setup.c)
SET_TARGET_PROPERTIES(chicken-setup PROPERTIES COMPILE_FLAGS "${SHARED_FLAGS}")
TARGET_LINK_LIBRARIES(chicken-setup libchicken)
ADD_DEPENDENCIES(chicken-setup chicken-boot stack-size)


####################################################################
#  CREATE silex, easyffi.l.silex, and easyffi.c                    #
####################################################################

ADD_EXECUTABLE(silex
  ${Chicken_SOURCE_DIR}/chicken.h
  ${CMAKE_CURRENT_BINARY_DIR}/silex.c
)
SET_TARGET_PROPERTIES(silex PROPERTIES COMPILE_FLAGS "${STATIC_FLAGS}")
TARGET_LINK_LIBRARIES(silex libchicken-static)
ADD_DEPENDENCIES(silex chicken-boot)
GET_TARGET_PROPERTY(SILEX_EXE silex LOCATION)
SET(CURRENT_SILEX ${SILEX_EXE})

GEN_EASYFFI_L_SILEX()
ADD_CUSTOM_TARGET(easyffi-l-silex
  DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/easyffi.l.silex)
ADD_DEPENDENCIES(easyffi-l-silex silex)

GEN_EASYFFI_C()
ADD_CUSTOM_TARGET(easyffi-c
  DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/easyffi.c
)
ADD_DEPENDENCIES(easyffi-c easyffi-l-silex)


####################################################################
#  CREATE CHICKEN.EXE                                              #
####################################################################

SIMPLE_SCM_TO_C(batch-driver parameters)
SIMPLE_SCM_TO_C(c-backend tweaks)
SIMPLE_SCM_TO_C(c-platform)
SIMPLE_SCM_TO_C(chicken build chicken-ffi-macros chicken-more-macros tweaks)
SIMPLE_SCM_TO_C(compiler easyffi parameters tweaks)
# easyffi.c is declared earlier

GET_CHICKEN_C_SOURCES(CHICKEN_EXE_SOURCES)

ADD_EXECUTABLE(chicken ${CHICKEN_EXE_SOURCES})
SET_TARGET_PROPERTIES(chicken PROPERTIES COMPILE_FLAGS "${SHARED_FLAGS}")
TARGET_LINK_LIBRARIES(chicken libchicken)
ADD_DEPENDENCIES(chicken chicken-boot easyffi-c stack-size)

ADD_EXECUTABLE(chicken-static ${CHICKEN_EXE_SOURCES})
SET_TARGET_PROPERTIES(chicken-static PROPERTIES COMPILE_FLAGS "${STATIC_FLAGS}")
TARGET_LINK_LIBRARIES(chicken-static libchicken-static)
ADD_DEPENDENCIES(chicken-static chicken-boot easyffi-c stack-size)


####################################################################
#  DOCUMENTATION                                                   #
####################################################################

# build the html documentation if makeinfo is available
FIND_PROGRAM(MAKEINFO_EXE NAMES makeinfo)
ADD_CUSTOM_COMMAND(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/chicken.html
  MAIN_DEPENDENCY ${Chicken_SOURCE_DIR}/chicken.texi
  COMMAND ${MAKEINFO_EXE} --no-split --html --output=${CMAKE_CURRENT_BINARY_DIR}/chicken.html ${Chicken_SOURCE_DIR}/chicken.texi
  COMMENT "Compiling ${Chicken_SOURCE_DIR}/chicken.texi to ${CMAKE_CURRENT_BINARY_DIR}/chicken.html"
)
ADD_CUSTOM_COMMAND(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/chicken.info
  MAIN_DEPENDENCY ${Chicken_SOURCE_DIR}/chicken.texi
  COMMAND ${MAKEINFO_EXE} --no-split --output=${CMAKE_CURRENT_BINARY_DIR}/chicken.info ${Chicken_SOURCE_DIR}/chicken.texi
  COMMENT "Compiling ${Chicken_SOURCE_DIR}/chicken.texi to ${CMAKE_CURRENT_BINARY_DIR}/chicken.info"
)

IF(MAKEINFO_EXE)
  ADD_CUSTOM_TARGET(html
    ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/chicken.html
  )
  ADD_CUSTOM_TARGET(info
    ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/chicken.info
  )
ELSE(MAKEINFO_EXE)
  ADD_CUSTOM_TARGET(html
    COMMAND ${CMAKE_COMMAND} -E echo makeinfo not available, cannot create chicken.html
  )
  ADD_CUSTOM_TARGET(info
    COMMAND ${CMAKE_COMMAND} -E echo makeinfo not available, cannot create chicken.info
  )
ENDIF(MAKEINFO_EXE)

FIND_PROGRAM(DARCS_EXE darcs)
ADD_CUSTOM_COMMAND(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/ChangeLog
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMAND ${DARCS_EXE} changes --repodir=${Chicken_SOURCE_DIR} > ChangeLog
  COMMENT Generating ${CMAKE_CURRENT_BINARY_DIR}/ChangeLog from Darcs repository.
)

SET(HAVE_CHANGELOG FALSE)
IF(DARCS_EXE)
  IF(EXISTS ${Chicken_SOURCE_DIR}/_darcs)
    SET(HAVE_CHANGELOG TRUE)
    ADD_CUSTOM_TARGET(darcs_changelog
      ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/ChangeLog
    )
  ELSE(EXISTS ${Chicken_SOURCE_DIR}/_darcs)
    ADD_CUSTOM_TARGET(darcs_changelog
      COMMAND ${CMAKE_COMMAND} -E echo ${Chicken_SOURCE_DIR}/_darcs doesn't exist, cannot create ChangeLog.
    )
  ENDIF(EXISTS ${Chicken_SOURCE_DIR}/_darcs)
ELSE(DARCS_EXE)
  ADD_CUSTOM_TARGET(darcs_changelog
    COMMAND ${CMAKE_COMMAND} -E echo Can't find darcs, cannot create ChangeLog.
  )
ENDIF(DARCS_EXE)


####################################################################
#  INSTALLATION                                                    #
####################################################################


INSTALL(TARGETS
  chicken
  chicken-setup
  chicken-static
  csc
  csi
  libchicken
  libchicken-static
  libuchicken
  libuchicken-static
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib
)

GET_LIB_EXPORTS(exports)
INSTALL(FILES ${exports} DESTINATION share/chicken)

IF(WIN32)
  INSTALL(FILES csibatch.bat DESTINATION bin)
  IF(NOT CYGWIN)
    INSTALL(TARGETS
      libchickengui
      libchickengui-static
      RUNTIME DESTINATION bin
      LIBRARY DESTINATION lib
      ARCHIVE DESTINATION lib
    )
  ENDIF(NOT CYGWIN)
ENDIF(WIN32)

# Where to put the eggs directory?

INSTALL(FILES chicken-ffi-macros.scm chicken-more-macros.scm
  DESTINATION share/chicken)

INSTALL(FILES chicken.h DESTINATION include)
INSTALL(FILES chicken.1 csi.1 csc.1 chicken-setup.1 chicken-profile.1 
  DESTINATION man/man1)
INSTALL(FILES README LICENSE INSTALL-CMake.txt DESTINATION share/chicken/doc)
IF(MAKEINFO_EXE)
  INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/chicken.html DESTINATION share/chicken/doc)
  INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/chicken.info DESTINATION info)
ENDIF(MAKEINFO_EXE)
# It's confusing to install ChangeLog.0-20040412 without installing
# ChangeLog.  It makes Chicken looks like it hasn't been updated in a long time.
# So, either install both, or neither.
IF(HAVE_CHANGELOG)
  INSTALL(FILES
    ${CMAKE_CURRENT_BINARY_DIR}/ChangeLog
    ${Chicken_SOURCE_DIR}/ChangeLog.0-20040412
    DESTINATION share/chicken/doc
  )
ENDIF(HAVE_CHANGELOG)


####################################################################
#   UNINSTALLATION                                                 #
####################################################################

# Well, we don't have one yet.  CMake won't do it automagically.

ADD_CUSTOM_TARGET(uninstall
  COMMAND ${CMAKE_COMMAND} -E echo Don't you wish!)


####################################################################
#   DISTRIBUTION                                                   #
####################################################################

SET(BOOT_CFILES_DEPS ${BOOT_CFILES})
ADD_PREFIX(${CMAKE_CURRENT_BINARY_DIR}/ BOOT_CFILES_DEPS)
ADD_CUSTOM_TARGET(boot_cfiles DEPENDS ${BOOT_CFILES_DEPS})
ADD_DEPENDENCIES(boot_cfiles easyffi-c)

# tarball.cmake contains the list of all files that should be present
# in the distribution.  It also contains a script for creating a
# distribution tree and verifying its contents.

FILE(READ ${Chicken_SOURCE_DIR}/buildversion BUILDVERSION)
FILE(READ ${Chicken_SOURCE_DIR}/buildnumber BUILDNUMBER)
SET(DIST_DIR chicken-${BUILDVERSION}.${BUILDNUMBER})

ADD_CUSTOM_TARGET(dist
  COMMAND ${CMAKE_COMMAND} -DChicken_SOURCE_DIR=${Chicken_SOURCE_DIR} -DDIST_DIR=${DIST_DIR} -DBOOT_CFILES="${BOOT_CFILES}" -P ${Chicken_SOURCE_DIR}/dist.cmake
)
ADD_DEPENDENCIES(dist boot_cfiles darcs_changelog html info)


####################################################################
#   LICENSE                                                        #
####################################################################

# Copyright (c) 2006 by Brandon J. Van Every
# 
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
# IN THE SOFTWARE.
