# Copyright (c) 2006 by Brandon J. Van Every under MIT-style license.
# See LICENSE section at end of file for full license text.

CMAKE_MINIMUM_REQUIRED(VERSION 2.4.3)
INCLUDE(CheckIncludeFile)
INCLUDE(CheckIncludeFiles)
INCLUDE(CheckSymbolExists)

PROJECT(Chicken)
INCLUDE_DIRECTORIES(${Chicken_SOURCE_DIR})


####################################################################
#  VERSION                                                         #
####################################################################

# A basic handy thing to know

FILE(READ ${Chicken_SOURCE_DIR}/buildversion BUILDVERSION)
FILE(READ ${Chicken_SOURCE_DIR}/buildnumber BUILDNUMBER)
SET(VERSION ${BUILDVERSION}.${BUILDNUMBER})

CONFIGURE_FILE(${Chicken_SOURCE_DIR}/chicken.spec.in
  ${CMAKE_CURRENT_BINARY_DIR}/chicken.spec)


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

# Returns an unquoted string.  Note that CMake will readily
# turn such strings back into lists, so be careful how you use it.
MACRO(LIST2STRING alist astring)
  FOREACH(elem ${${alist}})
   SET(${astring} "${${astring}} ${elem}")
  ENDFOREACH(elem)
ENDMACRO(LIST2STRING)


# In CMake 2.4.2 under the MSYS generator, FILE(TO_NATIVE_PATH ...)
# generates Unix style pathnames.  Although this may be correct behavior
# under the MSYS shell itself, Chicken probably won't be used under the
# MSYS shell.  Generally MSYS is just there for build assistance.
# So, manually convert to Windows native pathnames.

# Quoting policy:
# - quote the path.  Output is expected to be a C string.
# - backslashes in Windows pathnames need to be escaped, i.e. \\
# - whitespace is not escaped.
#
# Be very careful because we're using unprotected whitespace!

MACRO(NATIVE_C_PATH cmake_path_in native_path_out)
  SET(${native_path_out} ${${cmake_path_in}})
  # An extra \\ escape is necessary to get a \ through CMake's processing.
  # Check that pathname has no escapes in it already.
  STRING(REGEX MATCH "\\\\" ALREADY_ESCAPED ${native_path_out} "${${native_path_out}}")
  IF(ALREADY_ESCAPED)
    MESSAGE(FATAL_ERROR "Pathname already contains escapes or backslashes.
      Not allowed, terminating build.")
  ENDIF(ALREADY_ESCAPED)

  IF(WIN32 AND NOT CYGWIN)
    # As proven above, we have no escaped whitespace.
    # Use backslashes for directory separation.
    STRING(REPLACE "/" "\\\\" ${native_path_out} "${${native_path_out}}")
  ELSE(WIN32 AND NOT CYGWIN)
    # Assume that TO_NATIVE_PATH does the right thing.
    # This is a dicey assumption.  Need to test it on other OSes.
    FILE(TO_NATIVE_PATH "${${native_path_out}}" ${native_path_out})
    # TO_NATIVE_PATH generates escaped whitespace.  Remove the escapes.
    STRING(REPLACE "\\ " " " ${native_path_out} "${${native_path_out}}")
  ENDIF(WIN32 AND NOT CYGWIN)

  # Quote the results
  SET(${native_path_out} \"${${native_path_out}}\")

ENDMACRO(NATIVE_C_PATH)

# It is common to look for tools at configuration time, and if they are
# not found, to provide make targets that give error messages.  We also
# typically want to know at configuration time that such a target has
# been created.  This keeps us from having to type the same error
# strings twice, and from making mistakes about STATUS vs. dialog boxes.

MACRO(ECHO_TARGET target quoted_message)
  # Sanity checking.  Unfortunately, it doesn't have the desired effect.
  # It won't catch code that isn't being exercised by the current
  # build settings.  So, a hapless user will receive any fatal errors,
  # not the developer, whose system is probably configured with all
  # needed tools and not calling ECHO_TARGET at all.

  # CMake 2.4.2 can't handle apostrophes in -E echo, even when
  # surrounded by double quotes.
  STRING(REGEX MATCH "'" CONTAINS_APOSTROPHE ${quoted_message})
  IF(CONTAINS_APOSTROPHE)
    MESSAGE(STATUS "CMake 2.4.2 cannot handle ' apostrophes in -E echo.")
  ENDIF(CONTAINS_APOSTROPHE)

  # Make sure the message is quoted.
  # Apostrophe errors also show up as unquoted strings, so do this 2nd.
  STRING(REGEX MATCH "\"" QUOTED ${quoted_message})
  IF(NOT QUOTED)
    MESSAGE(STATUS "Unquoted ECHO_TARGET string.")
  ENDIF(NOT QUOTED)
  
  # The actual functionality
  MESSAGE(STATUS ${quoted_message})
  ADD_CUSTOM_TARGET(${target}
    COMMAND ${CMAKE_COMMAND} -E echo ${quoted_message})
ENDMACRO(ECHO_TARGET)


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

####################################################################
#  DEDUCE OS SPECIFIC BUILD OPTIONS                                #
####################################################################


# Be careful about whether NOT CYGWIN or NOT CMAKE_COMPILER_IS_GNUCC
# is really intended.  GCC has many similarities of operation across
# Linux, Cygwin, and MinGW, but there are also differences.

# ****** TODO ******
# What about OS, compiler, and CPU specific optimizations?
# Make VC++ have Release or MinSizeRel as default build?
# I want to see benchmarks integrated as a test suite before bothering.
# Need proof regarding what's worth it, and also proof that the
# benchmarks measure something useful.

#All our various optional libraries go in here.
SET(MORE_LIBS )
SET(MORE_STATIC_LIBS )

# Any CMake-specific defines are in chicken-config.h
ADD_DEFINITIONS(-DHAVE_CHICKEN_CONFIG_H)

# Chicken uses the INSTALL_* paths in various tools to generate shell
# commands; for instance, under a Windows command prompt.  So they need
# to be native paths.

# First specify the paths relative to each other in CMake style.
SET(SHARE_HOME   ${CMAKE_INSTALL_PREFIX}/share/chicken)
SET(BIN_HOME     ${CMAKE_INSTALL_PREFIX}/bin)
SET(INCLUDE_HOME ${CMAKE_INSTALL_PREFIX}/include)
SET(LIB_HOME     ${CMAKE_INSTALL_PREFIX}/lib)
SET(STATIC_LIB_HOME     ${CMAKE_INSTALL_PREFIX}/lib)
SET(INFO_HOME    ${CMAKE_INSTALL_PREFIX}/info)
SET(MAN_HOME     ${CMAKE_INSTALL_PREFIX}/man/man1)
SET(EGG_HOME     ${LIB_HOME}/chicken/1)
SET(DOC_HOME     ${SHARE_HOME}/doc)

# Then make them native after the path logic is done.
NATIVE_C_PATH(SHARE_HOME         C_INSTALL_SHARE_HOME)
NATIVE_C_PATH(BIN_HOME           C_INSTALL_BIN_HOME)
NATIVE_C_PATH(INCLUDE_HOME       C_INSTALL_INCLUDE_HOME)
NATIVE_C_PATH(LIB_HOME           C_INSTALL_LIB_HOME)
NATIVE_C_PATH(STATIC_LIB_HOME    C_INSTALL_STATIC_LIB_HOME)
NATIVE_C_PATH(EGG_HOME           C_INSTALL_EGG_HOME)

# Do we actually want absolute pathnames for the compilers,
# or just simple definitions like "gcc" or "cl" ?
GET_FILENAME_COMPONENT(THE_CC ${CMAKE_C_COMPILER} NAME_WE)
GET_FILENAME_COMPONENT(THE_CXX ${CMAKE_CXX_COMPILER} NAME_WE)
SET(C_INSTALL_CC \"${THE_CC}\")
SET(C_INSTALL_CXX \"${THE_CXX}\")

# Historically, MSVC was given a stack size of 300000.
# Not convinced this is necessary, but noted in case of problems.
SET(C_DEFAULT_TARGET_STACK_SIZE \(128*1024\))

CHECK_INCLUDE_FILE(alloca.h HAVE_ALLOCA_H)
CHECK_INCLUDE_FILE(crt_externs.h HAVE_CRT_EXTERNS_H)
CHECK_INCLUDE_FILE(direct.h HAVE_DIRECT_H)
CHECK_SYMBOL_EXISTS(gcvt stdlib.h HAVE_GCVT)
CHECK_INCLUDE_FILE(grp.h HAVE_GRP_H)
CHECK_INCLUDE_FILE(stdint.h HAVE_STDINT_H)
CHECK_INCLUDE_FILE(sysexits.h HAVE_SYSEXITS_H)

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
  SET(REGEX_UNIT pcre)
  SET(MORE_LIBS ${MORE_LIBS} pcre)
  SET(MORE_STATIC_LIBS ${MORE_STATIC_LIBS} pcre)
ELSE(HAVE_PCRE_H)
  IF(WIN32 AND NOT CYGWIN)
    SET(REGEX_UNIT pregexp)
  ELSE(WIN32 AND NOT CYGWIN)
    SET(REGEX_UNIT regexunix)
  ENDIF(WIN32 AND NOT CYGWIN)
ENDIF(HAVE_PCRE_H)

CHECK_INCLUDE_FILE(dlfcn.h HAVE_DLFCN_H)
IF(HAVE_DLFCN_H)
  # The presence of dlfcn.h does NOT mean that -ldl is used.
  # For instance, Cygwin has dlfcn.h and does not use -ldl.
  FIND_LIBRARY(DL_LIBRARY dl)
  IF(DL_LIBRARY)
    SET(MORE_LIBS ${MORE_LIBS} dl)
    SET(MORE_STATIC_LIBS ${MORE_STATIC_LIBS} dl)
  ENDIF(DL_LIBRARY)
ENDIF(HAVE_DLFCN_H)

CHECK_INCLUDE_FILE(dl.h HAVE_DL_H)
IF(HAVE_DL_H)
  SET(MORE_LIBS ${MORE_LIBS} ldl)
  SET(MORE_STATIC_LIBS ${MORE_STATIC_LIBS} ldl)
ENDIF(HAVE_DL_H)

CHECK_INCLUDE_FILE(ffi.h HAVE_FFI_H)
IF(HAVE_FFI_H)
  SET(MORE_LIBS ${MORE_LIBS} ffi)
  SET(MORE_STATIC_LIBS ${MORE_STATIC_LIBS} ffi)
ENDIF(HAVE_FFI_H)

# check for windows.h
CHECK_INCLUDE_FILE(windows.h HAVE_WINDOWS_H)
IF(HAVE_WINDOWS_H)

  # NOTE: Don't assume we're on Windows.  We could be cross-compiling.
  CHECK_SYMBOL_EXISTS(LoadLibrary windows.h HAVE_LOADLIBRARY)
  CHECK_SYMBOL_EXISTS(GetProcAddress windows.h HAVE_GETPROCADDRESS)

  CHECK_INCLUDE_FILE(winsock2.h HAVE_WINSOCK2_H)
  IF (NOT HAVE_WINSOCK2_H)
    MESSAGE("You are missing winsock2.h.
      Correct this if you want full network functionality.")
  ENDIF(NOT HAVE_WINSOCK2_H)
  # ws2tcpip.h needs winsock2.h for various definitions.
  # Found it really difficult to pass the list of files as a
  # 1st argument.  This is what worked.
  SET(WS2FILES winsock2.h ws2tcpip.h)
  CHECK_INCLUDE_FILES("${WS2FILES}" HAVE_WS2TCPIP_H)
  IF (NOT HAVE_WS2TCPIP_H)
    MESSAGE("You are missing ws2tcpip.h.
      Correct this if you want full network functionality.")
  ENDIF(NOT HAVE_WS2TCPIP_H)

  # Seems like the ws2_32 stuff should be up here, not down below.

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
  SET(MORE_LIBS ${MORE_LIBS} ws2_32)
  SET(MORE_STATIC_LIBS ${MORE_STATIC_LIBS} ws2_32)
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
  SET(MORE_LIBS ${MORE_LIBS} m)
  SET(MORE_STATIC_LIBS ${MORE_STATIC_LIBS} m)
ENDIF(UNIX)

# Solaris doesn't have socket calls in its libc
IF(CMAKE_SYSTEM_NAME STREQUAL "SunOS")
  SET(MORE_LIBS ${MORE_LIBS} socket rt)
  SET(MORE_STATIC_LIBS ${MORE_STATIC_LIBS} socket rt)
ENDIF(CMAKE_SYSTEM_NAME STREQUAL "SunOS")

# Tack on -l for chicken-defaults.h
SET(L_MORE_LIBS ${MORE_LIBS})
SET(L_MORE_STATIC_LIBS ${MORE_STATIC_LIBS})
ADD_PREFIX(-l L_MORE_LIBS)
ADD_PREFIX(-l L_MORE_STATIC_LIBS)
LIST2STRING(L_MORE_LIBS L_MORE_LIBS_STRING)
LIST2STRING(L_MORE_STATIC_LIBS L_MORE_STATIC_LIBS_STRING)
SET(C_INSTALL_MORE_LIBS \"${L_MORE_LIBS_STRING}\")
SET(C_INSTALL_MORE_STATIC_LIBS \"${L_MORE_STATIC_LIBS_STRING}\")


####################################################################
#  DEFINE C_STACK_GROWS_DOWNWARD                                   #
####################################################################

TRY_RUN(C_STACK_GROWS_DOWNWARD
  STACK_TEST_COMPILED
  ${CMAKE_CURRENT_BINARY_DIR}
  ${Chicken_SOURCE_DIR}/StackGrowsDownward.c
)
IF(STACK_TEST_COMPILED)
  IF(C_STACK_GROWS_DOWNWARD STREQUAL "FAILED_TO_RUN")
    MESSAGE(SEND_ERROR "Compiled stack growth test, but unable to run it.  Using default.")
    SET(C_STACK_GROWS_DOWNWARD -1)
  ENDIF(C_STACK_GROWS_DOWNWARD STREQUAL "FAILED_TO_RUN")
ELSE(STACK_TEST_COMPILED)
  MESSAGE(SEND_ERROR "Unable to compile stack growth test.  Using default.")
  SET(C_STACK_GROWS_DOWNWARD -1)
ENDIF(STACK_TEST_COMPILED)
MESSAGE(STATUS "C_STACK_GROWS_DOWNWARD=${C_STACK_GROWS_DOWNWARD}")


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


####################################################################
#  CHICKEN_CONFIG_H                                                #
####################################################################

CONFIGURE_FILE(${Chicken_SOURCE_DIR}/chicken-config-cmake.h.in
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-config.h)
# ./configure just plops chicken-config.h in the source tree.
# Try to avoid collisions with it by searching 
# ${CMAKE_CURRENT_BINARY_DIR} first.
INCLUDE_DIRECTORIES(BEFORE ${CMAKE_CURRENT_BINARY_DIR})


####################################################################
#  CHICKEN_DEFAULTS_H                                              #
####################################################################

# get C flags as late as possible so they're all available
GET_DIRECTORY_PROPERTY(RAW_CFLAGS
  DIRECTORY ${Chicken_SOURCE_DIR}
  DEFINITIONS
)
SET(C_INSTALL_CFLAGS \"${RAW_CFLAGS}\")

# ./configure does the substitutions at make time, not
# configuration time.  Will this cause problems if
# people are using both builds?
CONFIGURE_FILE(${Chicken_SOURCE_DIR}/chicken-defaults.h.in
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-defaults.h)


####################################################################
#  LISTS OF .C SOURCES                                             #
####################################################################


# Library files that are not generated at build time.
# Either they are read-only sources,
# or they are hardwired at configuration time.

MACRO(GET_LIB_CSOURCE out)
  SET(${out}
    ${CMAKE_CURRENT_BINARY_DIR}/chicken-config.h
    ${Chicken_SOURCE_DIR}/chicken.h
    ${CMAKE_CURRENT_BINARY_DIR}/chicken-defaults.h
    ${Chicken_SOURCE_DIR}/runtime.c
  )
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
  utils
)
MACRO(GET_LIB_EXPORTS out)
  SET(${out} ${LIB_EXPORTS})
  ADD_SUFFIX(${out} .exports)
  ADD_PREFIX(${CMAKE_CURRENT_BINARY_DIR}/ ${out})
ENDMACRO(GET_LIB_EXPORTS)

# Localize the .c sources to a particular build subdirectory.
# This is needed because we're doing a multiple stage bootstrap.
# The 1st crop of generated .c files is not going to be the
# same as the 2nd crop.

MACRO(GET_LIBCHICKEN_C_SOURCES out)
  GET_LIB_CSOURCE(csource)
  GET_LIB_COMMON_CGEN(common)
  GET_LIB_SAFE_CGEN(safe)
  SET(${out} ${csource} ${common} ${safe})
ENDMACRO(GET_LIBCHICKEN_C_SOURCES)

MACRO(GET_LIBUCHICKEN_C_SOURCES out)
  GET_LIB_CSOURCE(csource)
  GET_LIB_COMMON_CGEN(common)
  GET_LIB_UNSAFE_CGEN(unsafe)
  SET(${out} ${csource} ${common} ${unsafe})
ENDMACRO(GET_LIBUCHICKEN_C_SOURCES)

# libchicken-gui just compiles libchicken with -DC_WINDOWS_GUI.
# There are no .c files specific to it.


# Files generated for Chicken.  No SAFE or UNSAFE distinctions.

SET(CHICKEN_CGEN
  batch-driver
  c-backend
  c-platform
  chicken
  compiler
  optimizer
  support
)
MACRO(GET_CHICKEN_CGEN out)
  SET(${out} ${CHICKEN_CGEN})
  ADD_SUFFIX(${out} .c)
  ADD_PREFIX(${CMAKE_CURRENT_BINARY_DIR}/ ${out})
ENDMACRO(GET_CHICKEN_CGEN)

MACRO(GET_CHICKEN_C_SOURCES out)
  GET_CHICKEN_CGEN(cgen)
  SET(${out}
    ${CMAKE_CURRENT_BINARY_DIR}/chicken-config.h
    ${Chicken_SOURCE_DIR}/chicken.h
    ${CMAKE_CURRENT_BINARY_DIR}/chicken-defaults.h
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
  srfi-1.c
  srfi-4.c
  srfi-13.c
  srfi-14.c
  srfi-18.c
  stub.c
  support.c
  tcp.c
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

LIBRARY_SCM_TO_C_EXPORTS(eval)
UNSAFE_LIBRARY_SCM_TO_C(eval)
LIBRARY_SCM_TO_C_EXPORTS(extras)
UNSAFE_LIBRARY_SCM_TO_C(extras)
LIBRARY_SCM_TO_C_EXPORTS(library build)
UNSAFE_LIBRARY_SCM_TO_C(library build)
LIBRARY_SCM_TO_C_EXPORTS(lolevel)
UNSAFE_LIBRARY_SCM_TO_C(lolevel)
LIBRARY_SCM_TO_C(match)
SIMPLE_SCM_TO_C(optimizer tweaks)
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
SIMPLE_SCM_TO_C(support banner tweaks)
LIBRARY_SCM_TO_C_EXPORTS(tcp)
UNSAFE_LIBRARY_SCM_TO_C(tcp)
LIBRARY_SCM_TO_C_EXPORTS(utils)
UNSAFE_LIBRARY_SCM_TO_C(utils)


GET_LIBCHICKEN_C_SOURCES(CHICKEN_LIB_SOURCES)
GET_LIBUCHICKEN_C_SOURCES(CHICKEN_UNSAFE_LIB_SOURCES)

ADD_LIBRARY(libchicken SHARED ${CHICKEN_LIB_SOURCES})
SET_TARGET_PROPERTIES(libchicken PROPERTIES
  COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN ${SHARED_FLAGS}")
TARGET_LINK_LIBRARIES(libchicken ${MORE_LIBS})
ADD_DEPENDENCIES(libchicken chicken-boot)

ADD_LIBRARY(libchicken-static STATIC ${CHICKEN_LIB_SOURCES})
SET_TARGET_PROPERTIES(libchicken-static PROPERTIES
  COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN ${STATIC_FLAGS}")
TARGET_LINK_LIBRARIES(libchicken-static ${MORE_STATIC_LIBS})
ADD_DEPENDENCIES(libchicken-static chicken-boot)

ADD_LIBRARY(libuchicken SHARED ${CHICKEN_UNSAFE_LIB_SOURCES})
SET_TARGET_PROPERTIES(libuchicken PROPERTIES
  COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN -DC_UNSAFE_RUNTIME ${SHARED_FLAGS}")
TARGET_LINK_LIBRARIES(libuchicken ${MORE_LIBS})
ADD_DEPENDENCIES(libuchicken chicken-boot)

ADD_LIBRARY(libuchicken-static STATIC ${CHICKEN_UNSAFE_LIB_SOURCES})
SET_TARGET_PROPERTIES(libuchicken-static PROPERTIES
  COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN -DC_UNSAFE_RUNTIME ${STATIC_FLAGS}")
TARGET_LINK_LIBRARIES(libuchicken-static ${MORE_STATIC_LIBS})
ADD_DEPENDENCIES(libuchicken-static chicken-boot)

IF(WIN32 AND NOT CYGWIN)

  # Should these be in MORE_LIBS way up above?
  SET(GUI_LIBS kernel32 user32 gdi32)

  ADD_LIBRARY(libchickengui SHARED ${CHICKEN_LIB_SOURCES})
  SET_TARGET_PROPERTIES(libchickengui PROPERTIES
    COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN -DC_WINDOWS_GUI ${SHARED_FLAGS}")
  TARGET_LINK_LIBRARIES(libchickengui ${GUI_LIBS} ${MORE_LIBS})
  ADD_DEPENDENCIES(libchickengui chicken-boot)

  ADD_LIBRARY(libchickengui-static STATIC ${CHICKEN_LIB_SOURCES})
  SET_TARGET_PROPERTIES(libchickengui-static PROPERTIES
    COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN -DC_WINDOWS_GUI ${STATIC_FLAGS}")
  TARGET_LINK_LIBRARIES(libchickengui-static ${GUI_LIBS} ${MORE_STATIC_LIBS})
  ADD_DEPENDENCIES(libchickengui-static chicken-boot)

ENDIF(WIN32 AND NOT CYGWIN)

# We want the library to be named 'libchicken' on all platforms.
# But, Cygwin conspires against us.  Their convention is
# cygname-0.dll and so forth.  Whether it's a "best practice" is
# debateable, but in the Cygwin FAQ and Googling about, it's the
# practice.  
# Default Unix, Cygwin, and MinGW behavior is to add 'lib' as a 
# prefix onto every library.  For cross-platform stuff,
# this often results in the 'liblibfoo" problem.

# Let CMake do the prefixing magic on Unix, Cygwin, and MinGW.

IF(CYGWIN)
  SET_TARGET_PROPERTIES(libchicken PROPERTIES OUTPUT_NAME chicken-0)
  SET_TARGET_PROPERTIES(libuchicken PROPERTIES OUTPUT_NAME uchicken-0)
ELSE(CYGWIN)
  SET_TARGET_PROPERTIES(libchicken PROPERTIES OUTPUT_NAME chicken)
  SET_TARGET_PROPERTIES(libuchicken PROPERTIES OUTPUT_NAME uchicken)
ENDIF(CYGWIN)
IF(MSVC OR MINGW)
  SET_TARGET_PROPERTIES(libchickengui PROPERTIES OUTPUT_NAME chickengui)
ENDIF(MSVC OR MINGW)

# CMake 2.4.3 has a bug where libchicken.a and libchicken.dll.a
# clobber each other during the build.  This will be fixed in
# CMake 2.4.4 and then we can have both at the same time.
# Meanwhile as a workaround we need to distinguish the library names
# and do a symlink at install time.

IF(MINGW)
  SET_TARGET_PROPERTIES(libchicken-static PROPERTIES OUTPUT_NAME chicken-s)
  SET_TARGET_PROPERTIES(libuchicken-static PROPERTIES OUTPUT_NAME uchicken-s)
  SET_TARGET_PROPERTIES(libchickengui-static PROPERTIES OUTPUT_NAME chickengui-s)
ELSE(MINGW)
  SET_TARGET_PROPERTIES(libchicken-static PROPERTIES OUTPUT_NAME chicken)
  SET_TARGET_PROPERTIES(libuchicken-static PROPERTIES OUTPUT_NAME uchicken)
  IF(MSVC)
    SET_TARGET_PROPERTIES(libchickengui-static PROPERTIES OUTPUT_NAME chickengui)
  ENDIF(MSVC)
ENDIF(MINGW)

# Unix-y compilers tack "lib" onto the front of library names.  MSVC doesn't.
# Also, MSVC uses the indistinguishable suffix .lib for both dlls and static
# libraries.  So we need postfixes to distinguish our library names.
# Can't find any canonical postfixing conventions, so these will do for now.
# Don't wish to use "-m" for MINSIZEREL_POSTFIX because "mt" typically means
# "multithreaded, static linkage" for Windows native libraries.

IF(MSVC)
  SET_TARGET_PROPERTIES(libchicken PROPERTIES PREFIX "lib" IMPORT_PREFIX "lib"
    RELEASE_POSTFIX "" MINSIZEREL_POSTFIX "-z" DEBUG_POSTFIX "-d" RELWITHDEBINFO_POSTFIX "-g")
  SET_TARGET_PROPERTIES(libuchicken PROPERTIES PREFIX "lib" IMPORT_PREFIX "lib"
    RELEASE_POSTFIX "" MINSIZEREL_POSTFIX "-z" DEBUG_POSTFIX "-d" RELWITHDEBINFO_POSTFIX "-g")
  SET_TARGET_PROPERTIES(libchickengui PROPERTIES PREFIX "lib" IMPORT_PREFIX "lib"
    RELEASE_POSTFIX "" MINSIZEREL_POSTFIX "-z" DEBUG_POSTFIX "-d" RELWITHDEBINFO_POSTFIX "-g")
  SET_TARGET_PROPERTIES(libuchicken-static PROPERTIES PREFIX "lib"
    RELEASE_POSTFIX "-s" MINSIZEREL_POSTFIX "-sz" DEBUG_POSTFIX "-sd" RELWITHDEBINFO_POSTFIX "-sg")
  SET_TARGET_PROPERTIES(libchicken-static PROPERTIES PREFIX "lib"
    RELEASE_POSTFIX "-s" MINSIZEREL_POSTFIX "-sz" DEBUG_POSTFIX "-sd" RELWITHDEBINFO_POSTFIX "-sg")
  SET_TARGET_PROPERTIES(libchickengui-static PROPERTIES PREFIX "lib"
    RELEASE_POSTFIX "-s" MINSIZEREL_POSTFIX "-sz" DEBUG_POSTFIX "-sd" RELWITHDEBINFO_POSTFIX "-sg")
ENDIF(MSVC)


####################################################################
#  CREATE CSC                                                      #
####################################################################

SIMPLE_SCM_TO_C(csc)

ADD_EXECUTABLE(csc
  ${Chicken_SOURCE_DIR}/chicken.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-config.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-defaults.h
  ${CMAKE_CURRENT_BINARY_DIR}/csc.c
)
SET_TARGET_PROPERTIES(csc PROPERTIES COMPILE_FLAGS "${SHARED_FLAGS}")
TARGET_LINK_LIBRARIES(csc libchicken)
ADD_DEPENDENCIES(csc chicken-boot)


####################################################################
#  CREATE CSI                                                      #
####################################################################

SIMPLE_SCM_TO_C(csi banner chicken-more-macros)

ADD_EXECUTABLE(csi
  ${Chicken_SOURCE_DIR}/chicken.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-config.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-defaults.h
  ${CMAKE_CURRENT_BINARY_DIR}/csi.c
)
SET_TARGET_PROPERTIES(csi PROPERTIES COMPILE_FLAGS "${SHARED_FLAGS}")
TARGET_LINK_LIBRARIES(csi libchicken)
ADD_DEPENDENCIES(csi chicken-boot)

ADD_EXECUTABLE(csi-static
  ${Chicken_SOURCE_DIR}/chicken.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-config.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-defaults.h
  ${CMAKE_CURRENT_BINARY_DIR}/csi.c
)
SET_TARGET_PROPERTIES(csi-static PROPERTIES COMPILE_FLAGS "${STATIC_FLAGS}")
TARGET_LINK_LIBRARIES(csi-static libchicken-static)
ADD_DEPENDENCIES(csi-static chicken-boot)


####################################################################
#  CREATE CHICKEN-PROFILE                                          #
####################################################################

SIMPLE_SCM_TO_C(chicken-profile build)

ADD_EXECUTABLE(chicken-profile
  ${Chicken_SOURCE_DIR}/chicken.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-config.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-defaults.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-profile.c
)
SET_TARGET_PROPERTIES(chicken-profile PROPERTIES COMPILE_FLAGS "${SHARED_FLAGS}")
TARGET_LINK_LIBRARIES(chicken-profile libchicken)
ADD_DEPENDENCIES(chicken-profile chicken-boot)


####################################################################
#  CREATE CHICKEN-SETUP                                            #
####################################################################

SIMPLE_SCM_TO_C(chicken-setup build)

ADD_EXECUTABLE(chicken-setup
  ${Chicken_SOURCE_DIR}/chicken.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-config.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-defaults.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-setup.c
)
SET_TARGET_PROPERTIES(chicken-setup PROPERTIES COMPILE_FLAGS "${SHARED_FLAGS}")
TARGET_LINK_LIBRARIES(chicken-setup libchicken)
ADD_DEPENDENCIES(chicken-setup chicken-boot)


####################################################################
#  CREATE CHICKEN.EXE                                              #
####################################################################

SIMPLE_SCM_TO_C(batch-driver tweaks)
SIMPLE_SCM_TO_C(c-backend tweaks)
SIMPLE_SCM_TO_C(c-platform tweaks)
SIMPLE_SCM_TO_C(chicken build chicken-ffi-macros chicken-more-macros tweaks)
SIMPLE_SCM_TO_C(compiler tweaks)

GET_CHICKEN_C_SOURCES(CHICKEN_EXE_SOURCES)

ADD_EXECUTABLE(chicken ${CHICKEN_EXE_SOURCES})
SET_TARGET_PROPERTIES(chicken PROPERTIES COMPILE_FLAGS "${SHARED_FLAGS}")
TARGET_LINK_LIBRARIES(chicken libchicken)
ADD_DEPENDENCIES(chicken chicken-boot)

ADD_EXECUTABLE(chicken-static ${CHICKEN_EXE_SOURCES})
SET_TARGET_PROPERTIES(chicken-static PROPERTIES COMPILE_FLAGS "${STATIC_FLAGS}")
TARGET_LINK_LIBRARIES(chicken-static libchicken-static)
ADD_DEPENDENCIES(chicken-static chicken-boot)


####################################################################
#  DOCUMENTATION                                                   #
####################################################################

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
    # Windows native Darcs doesn't handle Cygwin pathnames.
    # Corner case: Windows native Darcs is in system path,
    # but user is building under Cygwin.  Darcs chokes on Cygwin path.
    MESSAGE(STATUS "Testing whether Darcs actually works.")
    EXECUTE_PROCESS(COMMAND ${DARCS_EXE} changes --last=1 --repodir=${Chicken_SOURCE_DIR}
      RESULT_VARIABLE DARCS_FAILED)
    IF(DARCS_FAILED)
      ECHO_TARGET(darcs_changelog "Darcs does not work.  Cannot create ChangeLog.")
    ELSE(DARCS_FAILED)
      MESSAGE(STATUS "Darcs works.  Can create ChangeLog.")
      SET(HAVE_CHANGELOG TRUE)
      ADD_CUSTOM_TARGET(darcs_changelog
        ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/ChangeLog
      )
    ENDIF(DARCS_FAILED)
  ELSE(EXISTS ${Chicken_SOURCE_DIR}/_darcs)
    ECHO_TARGET(darcs_changelog "${Chicken_SOURCE_DIR}/_darcs does not exist, cannot create ChangeLog.")
  ENDIF(EXISTS ${Chicken_SOURCE_DIR}/_darcs)
ELSE(DARCS_EXE)
  ECHO_TARGET(darcs_changelog "Cannot find darcs, cannot create ChangeLog.")
ENDIF(DARCS_EXE)


####################################################################
#  INSTALLATION                                                    #
####################################################################


INSTALL(TARGETS
  chicken
  chicken-profile
  chicken-setup
  chicken-static
  csc
  csi
  csi-static
  libchicken
  libchicken-static
  libuchicken
  libuchicken-static
  RUNTIME DESTINATION ${BIN_HOME}
  LIBRARY DESTINATION ${LIB_HOME}
  ARCHIVE DESTINATION ${LIB_HOME}
)

GET_LIB_EXPORTS(exports)
INSTALL(FILES ${exports} DESTINATION ${SHARE_HOME})

IF(WIN32)
  INSTALL(FILES csibatch.bat DESTINATION ${BIN_HOME})
  IF(MSVC OR MINGW)
    INSTALL(TARGETS
      libchickengui
      libchickengui-static
      RUNTIME DESTINATION ${BIN_HOME}
      LIBRARY DESTINATION ${LIB_HOME}
      ARCHIVE DESTINATION ${LIB_HOME}
    )
  ENDIF(MSVC OR MINGW)
ENDIF(WIN32)

# CMake 2.4.3 will collide libchicken.dll.a and libchicken.a.
# We symlink around the problem until CMake 2.4.4 ships.

# CMake 2.4.3 can't produce both cygchicken-0.dll and libchicken.dll.a
# Currently Autoconf must produce the cygchicken-0.dll nomenclature,
# and that name is needed internally by the libraries for things to
# work correctly, so we can't ditch it in the source pool.  So we
# symlink it here.  This workaround courtesy of John Cowan.

# Cygwin CMake 2.4.3 has "cmake -E create_symlink"
# MinGW CMake 2.4.3 does not, as it is Windows native.
# MSYS has "ln -s" but it only does a copy, not an actual
# symlink.  Also MinGW isn't necessarily using MSYS.  All
# said and done, MinGW gets a straight copy and we hope it works.

# Brad King demonstrated \$ENV{DESTDIR} as the preferred method
# of handling ${CMAKE_INSTALL_PREFIX} when doing INSTALL(CODE ...)
# But I don't think this can be supported, as the paths must be
# hardwired into Chicken itself.

IF(MINGW)
  INSTALL(CODE "
    MESSAGE(STATUS \"Copying libchicken-s.a to libchicken.a\")
    EXECUTE_PROCESS(COMMAND \${CMAKE_COMMAND} -E remove
      \"${LIB_HOME}/libchicken.a\")
    EXECUTE_PROCESS(COMMAND \${CMAKE_COMMAND} -E copy
      \"${LIB_HOME}/libchicken-s.a\"
      \"${LIB_HOME}/libchicken.a\")
    MESSAGE(STATUS \"Copying libuchicken-s.a to libuchicken.a\")
    EXECUTE_PROCESS(COMMAND \${CMAKE_COMMAND} -E remove
      \"${LIB_HOME}/libuchicken.a\")
    EXECUTE_PROCESS(COMMAND \${CMAKE_COMMAND} -E copy
      \"${LIB_HOME}/libuchicken-s.a\"
      \"${LIB_HOME}/libuchicken.a\")
    MESSAGE(STATUS \"Copying libchickengui-s.a to libchickengui.a\")
    EXECUTE_PROCESS(COMMAND \${CMAKE_COMMAND} -E remove
      \"${LIB_HOME}/libchickengui.a\")
    EXECUTE_PROCESS(COMMAND \${CMAKE_COMMAND} -E copy
      \"${LIB_HOME}/libchickengui-s.a\"
      \"${LIB_HOME}/libchickengui.a\")
  ")
ENDIF(MINGW)

IF(CYGWIN)
  INSTALL(CODE "
    MESSAGE(STATUS \"Symlinking libchicken-0.dll.a to libchicken.dll.a\")
    EXECUTE_PROCESS(COMMAND \${CMAKE_COMMAND} -E remove
      \"${LIB_HOME}/libchicken.dll.a\")
    EXECUTE_PROCESS(COMMAND \${CMAKE_COMMAND} -E create_symlink
      \"${LIB_HOME}/libchicken-0.dll.a\"
      \"${LIB_HOME}/libchicken.dll.a\")
    MESSAGE(STATUS \"Symlinking libuchicken-0.dll.a to libuchicken.dll.a\")
    EXECUTE_PROCESS(COMMAND \${CMAKE_COMMAND} -E remove
      \"${LIB_HOME}/libuchicken.dll.a\")
    EXECUTE_PROCESS(COMMAND \${CMAKE_COMMAND} -E create_symlink
      \"${LIB_HOME}/libuchicken-0.dll.a\"
      \"${LIB_HOME}/libuchicken.dll.a\")
  ")
ENDIF(CYGWIN)

# Destination for eggs.  No eggs to install though.
INSTALL(CODE "
  MESSAGE(STATUS \"Installing ${EGG_HOME}\")
  MAKE_DIRECTORY(\"${EGG_HOME}\")
")

INSTALL(FILES chicken-ffi-macros.scm chicken-more-macros.scm
  DESTINATION ${SHARE_HOME})

INSTALL(FILES
  chicken.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-config.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-defaults.h
  DESTINATION ${INCLUDE_HOME}
)
INSTALL(FILES chicken.1 csi.1 csc.1 chicken-setup.1 chicken-profile.1 
  DESTINATION ${MAN_HOME})
INSTALL(FILES README LICENSE INSTALL-CMake.txt DESTINATION ${DOC_HOME})

# It's confusing to install ChangeLog.0-20040412 without installing
# ChangeLog.  It makes Chicken looks like it hasn't been updated in a long time.
# So, either install both, or neither.
IF(HAVE_CHANGELOG)
  INSTALL(FILES
    ${CMAKE_CURRENT_BINARY_DIR}/ChangeLog
    ${Chicken_SOURCE_DIR}/ChangeLog.0-20040412
    DESTINATION ${DOC_HOME}
  )
ENDIF(HAVE_CHANGELOG)


####################################################################
#   UNINSTALLATION                                                 #
####################################################################

# Policy is to wipe out any versions of binaries and libraries that
# Chicken has been building, not just the ones we installed.  This
# allows, say, MSVC and MinGW to be uninstalled and reinstalled in
# the same directory.
#
# We leave installed eggs alone.  Good idea or not?

SET(INSTALLED_BINARIES
  chicken
  chicken.exe
  chicken-profile
  chicken-profile.exe
  chicken-setup
  chicken-setup.exe
  chicken-static
  chicken-static.exe
  csc
  csc.exe
  csi
  csi.exe
  csibatch.bat
  csi-static
  csi-static.exe
)
ADD_PREFIX(${BIN_HOME}/ INSTALLED_BINARIES)

SET(INSTALLED_INCLUDES
  chicken.h
  chicken-config.h
  chicken-defaults.h  
)
ADD_PREFIX(${INCLUDE_HOME}/ INSTALLED_INCLUDES)

SET(INSTALLED_MANPAGES
  chicken.1
  chicken-profile.1
  chicken-setup.1
  csc.1
  csi.1
)
ADD_PREFIX(${MAN_HOME}/ INSTALLED_MANPAGES)

SET(INSTALLED_DOCS
  ChangeLog
  ChangeLog.0-20040412
  INSTALL-CMake.txt
  LICENSE
  README
)
ADD_PREFIX(${DOC_HOME}/ INSTALLED_DOCS)

ADD_CUSTOM_TARGET(uninstall
  COMMAND ${CMAKE_COMMAND} -E echo Completely removing all traces of Chicken installation from ${CMAKE_INSTALL_PREFIX}
  COMMAND ${CMAKE_COMMAND} -E echo Eggs in ${EGG_HOME} will be left alone.
  COMMAND ${CMAKE_COMMAND} -E remove ${INSTALLED_BINARIES}
  COMMAND ${CMAKE_COMMAND} -E remove ${INSTALLED_INCLUDES}
  COMMAND ${CMAKE_COMMAND} -E remove ${INSTALLED_MANPAGES}
  COMMAND ${CMAKE_COMMAND} -E remove ${INSTALLED_DOCS}
  COMMAND ${CMAKE_COMMAND} -E remove ${INFO_HOME}/chicken.info
  COMMAND ${CMAKE_COMMAND} -E remove ${BIN_HOME}/libchicken*
  COMMAND ${CMAKE_COMMAND} -E remove ${BIN_HOME}/libuchicken*
  COMMAND ${CMAKE_COMMAND} -E remove ${BIN_HOME}/cygchicken*
  COMMAND ${CMAKE_COMMAND} -E remove ${BIN_HOME}/cyguchicken*
  COMMAND ${CMAKE_COMMAND} -E remove ${LIB_HOME}/libchicken*
  COMMAND ${CMAKE_COMMAND} -E remove ${LIB_HOME}/libuchicken*
  COMMAND ${CMAKE_COMMAND} -E remove ${SHARE_HOME}/*.exports
  COMMAND ${CMAKE_COMMAND} -E remove ${SHARE_HOME}/*.scm
)


####################################################################
#   DISTRIBUTION                                                   #
####################################################################

SET(BOOT_CFILES_DEPS ${BOOT_CFILES})
ADD_PREFIX(${CMAKE_CURRENT_BINARY_DIR}/ BOOT_CFILES_DEPS)
ADD_CUSTOM_TARGET(boot_cfiles DEPENDS ${BOOT_CFILES_DEPS})

# dist.cmake contains the list of all files that should be present
# in the distribution.  It also contains a script for creating a
# distribution tree and verifying its contents.

# Distinguish CMake tarballs from "standard" ./configure tarballs
# using a -c- for CMake.  Don't put it as part of the buildversion
# and buildnumber because automated packaging systems would interpret
# it as superseding the "standard" builds.
SET(DIST_DIR chicken-c-${VERSION})

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
