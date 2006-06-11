CMAKE_MINIMUM_REQUIRED(VERSION 2.4)
INCLUDE(CheckIncludeFile)
INCLUDE(CheckSymbolExists)

PROJECT(Chicken)
INCLUDE_DIRECTORIES(${Chicken_SOURCE_DIR})


####################################################################
#  PASSING VARIABLES TO SUBDIRECTORIES                             #
####################################################################

# In CMake, if a variable or macro is defined before a subdirectory
# is added, then the variable will be trivially communicated to the
# subdirectory.  This is fine for passing information from the
# toplevel downwards.
#
# To pass up from directories, or across them, the variable needs
# CACHE INTERNAL "A documentation string" at the end of it.
# All 3 elements are required, i.e.
# SET(MY_ANSWER 42 CACHE INTERNAL "Any questions?")
# Otherwise you'll get a list of gobbledygoo.
#
# So, perform ADD_SUBDIRECTORY rather late!


####################################################################
#  CHICKEN VERSION MACROS                                          #
####################################################################

# Canonize the version and build into 1 number that can be
# easily compared.  We assume the build number has no more
# than 6 digits of precision.  If Chicken ever exceeds that,
# watch out!

MACRO(CANONIZE_VERSION version build canonized)
  MATH(EXPR ${canonized} "${version} * 1000000 + ${build}")
ENDMACRO(CANONIZE_VERSION)

# Pretty much all the various Chicken tools accept a -version
# argument and display some variation of "Version X, Build Y"
# as the result.  

MACRO(GET_TOOL_VERSION absolute_exe_name canonized)
  EXEC_PROGRAM(${absolute_exe_name}
    ARGS -version
    OUTPUT_VARIABLE exe_output
    RETURN_VALUE exe_return
  )
  IF(exe_return EQUAL 0)
    # Parse the output to extract the version and build numbers.
    # If the output format changes significantly, problems will ensue.
    SET(VERSION_BUILD_REGEX ".*Version ([0-9]+).*Build ([0-9]+).*")
    STRING(REGEX REPLACE ${VERSION_BUILD_REGEX} "\\1"
      version ${exe_output})
    STRING(REGEX REPLACE ${VERSION_BUILD_REGEX} "\\2"
      build ${exe_output})
    CANONIZE_VERSION(${version} ${build} ${canonized})

  ELSE(exe_return EQUAL 0)
    MESSAGE(FATAL_ERROR "Bogus result when invoking ${absolute_exe_name} -version
      Dump of output:
      ${exe_output}"
    )
  ENDIF(exe_return EQUAL 0)
ENDMACRO(GET_TOOL_VERSION)


####################################################################
#  CHICKEN VERSION REQUIREMENTS                                    #
####################################################################

# A previously installed Chicken can be used to build a new Chicken.
# The previously installed Chicken must have minimum Version and Build numbers.

SET(MINIMUM_CHICKEN_VERSION 2)
SET(MINIMUM_CHICKEN_BUILD 3)

CANONIZE_VERSION(${MINIMUM_CHICKEN_VERSION} ${MINIMUM_CHICKEN_BUILD}
  MINIMUM_CANONICAL_CHICKEN
)

# This is the minimum build necessary to generate .exports.

SET(EXPORTS_CHICKEN_VERSION 2)
SET(EXPORTS_CHICKEN_BUILD 310)

CANONIZE_VERSION(${EXPORTS_CHICKEN_VERSION} ${EXPORTS_CHICKEN_BUILD}
  MINIMUM_CANONICAL_EXPORTS
)


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

# Quite often, we need to specify where a source or
# generated file exists.  In principle, CMake should
# automagically find any file in ${Chicken_SOURCE_DIR}
# that is simply named.  myfile.c should always be
# equivalent to ${Chicken_SOURCE_DIR}/myfile.c
# In practice, we are paranoid.

MACRO(GLOBALIZE sourcelist)
  ADD_PREFIX(${Chicken_SOURCE_DIR}/ ${sourcelist})
ENDMACRO(GLOBALIZE)

# Generated files typically need to exist in their own local directory.
# This is important for multi-stage bootstrapping.

MACRO(LOCALIZE sourcelist)
  ADD_PREFIX(${CMAKE_CURRENT_BINARY_DIR}/ ${sourcelist})
ENDMACRO(LOCALIZE)


##############################################################
#                                                            #
#  FIND CHICKEN                                              #
#                                                            #
#  Outputs:  EXTANT_CHICKEN    EXTANT_CSI                    #
#                                                            #
#  Absolute paths of chicken executable and interpreter.     #
#  If none exists, evaluates to -NOTFOUND, which is          #
#  equivalent to FALSE in conditional tests.  Thus they are  #
#  both the path and the boolean about whether the path is   #
#  usable.  Isn't CMake clever?                              #
#                                                            #
##############################################################


# Is CHICKEN_HOME set in the environment?
# CHICKEN_HOME need not be set, but it could be a configuration error.
# Warn the user if it isn't set, and state the exact error.

STRING(COMPARE NOTEQUAL "$ENV{CHICKEN_HOME}" "" HAVE_CHICKEN_HOME)
IF(HAVE_CHICKEN_HOME)
  MESSAGE(STATUS "CHICKEN_HOME is $ENV{CHICKEN_HOME}")
  IF(NOT EXISTS $ENV{CHICKEN_HOME})
    MESSAGE(STATUS "$ENV{CHICKEN_HOME} does not exist!")
  ENDIF(NOT EXISTS $ENV{CHICKEN_HOME})
ELSE(HAVE_CHICKEN_HOME)
  MESSAGE(STATUS "CHICKEN_HOME is NOT set in your environment.")
  MESSAGE(STATUS "Searching system PATHs for Chicken.")
ENDIF(HAVE_CHICKEN_HOME)

# Do we have a Chicken installed already that we can use for the build?
# Did the user specify where to find Chicken via CMakeSetup or CCMake?

IF(NOT EXTANT_CHICKEN)
  FIND_PROGRAM(EXTANT_CHICKEN
    NAMES chicken-static chicken
    PATHS $ENV{CHICKEN_HOME} $ENV{CHICKEN_HOME}/bin
  )
  IF(EXTANT_CHICKEN)
    MESSAGE(STATUS "Found ${EXTANT_CHICKEN}")
  ELSE(EXTANT_CHICKEN)
    MESSAGE(SEND_ERROR "Can't find Chicken.")
  ENDIF(EXTANT_CHICKEN)
ENDIF(NOT EXTANT_CHICKEN)

IF(NOT EXTANT_CSI)
  FIND_PROGRAM(EXTANT_CSI
    NAMES csi-static csi
    PATHS $ENV{CHICKEN_HOME} $ENV{CHICKEN_HOME}/bin
  )
  IF(EXTANT_CSI)
    MESSAGE(STATUS "Found ${EXTANT_CSI}")
  ELSE(EXTANT_CSI)
    MESSAGE(SEND_ERROR "Can't find csi.")
  ENDIF(EXTANT_CSI)
ENDIF(NOT EXTANT_CSI)

# Chicken and csi must be from the same installation!

IF(EXTANT_CHICKEN AND EXTANT_CSI)
  GET_FILENAME_COMPONENT(EXTANT_CHICKEN_PATH ${EXTANT_CHICKEN} PATH)
  GET_FILENAME_COMPONENT(EXTANT_CSI_PATH ${EXTANT_CHICKEN} PATH)
  IF(NOT EXTANT_CHICKEN_PATH EQUAL EXTANT_CSI_PATH)
    MESSAGE(FATAL_ERROR "Your Chicken and csi are not in the same directory!  That is bad.  Giving up.")
  ENDIF(NOT EXTANT_CHICKEN_PATH EQUAL EXTANT_CSI_PATH)
ELSE(EXTANT_CHICKEN AND EXTANT_CSI)
  MESSAGE(FATAL_ERROR "Can't find a complete Chicken installation.  Giving up.")
ENDIF(EXTANT_CHICKEN AND EXTANT_CSI)


##############################################################
#                                                            #
#  VALIDATE VERSION REQIREMENTS                              #
#                                                            #
#  Outputs:  VALID_CHICKEN    VALID_CSI                      #
#                                                            #
##############################################################

SET(VALID_CHICKEN "-NOTFOUND")
SET(VALID_CSI "-NOTFOUND")
GET_TOOL_VERSION(${EXTANT_CHICKEN} chicken_version)
GET_TOOL_VERSION(${EXTANT_CSI} csi_version)
IF(chicken_version EQUAL csi_version)
  IF(chicken_version LESS MINIMUM_CANONICAL_CHICKEN)
    MESSAGE(FATAL_ERROR "Chicken ${MINIMUM_CANONICAL_CHICKEN} required.
      Available Chicken is only ${chicken_version}.  Giving up."
    )
  ELSE(chicken_version LESS MINIMUM_CANONICAL_CHICKEN)
    MESSAGE(STATUS "Chicken ${MINIMUM_CANONICAL_CHICKEN} required; using ${chicken_version}.")
    SET(VALID_CHICKEN ${EXTANT_CHICKEN})
    SET(VALID_CSI ${EXTANT_CSI})
  ENDIF(chicken_version LESS MINIMUM_CANONICAL_CHICKEN)

ELSE(chicken_version EQUAL csi_version)
  MESSAGE(FATAL_ERROR "Chicken and csi version numbers do not match!  That's bad.  Giving up.")
ENDIF(chicken_version EQUAL csi_version)

# The bootstrap compiler doesn't need to generate .exports.
# Our final compiler will ultimately generate .exports.


####################################################################
#  .SCM TO .C BUILD RULES                                          #
####################################################################


# The generated .c files are valid on all platforms, not just the
# ones they are built on.  In particular, this means the POSIX
# units (posix.c posixwin.c) and the REGEX units
# (pcre.c pregexp.c regex.c) will all be built.  By building all
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

# csc.c needs its own custom rule because csc.scm is generated.
# Will need to create csc.scm multiple times during bootstrap?

MACRO(GEN_CSC_C)
  CONFIGURE_FILE(${Chicken_SOURCE_DIR}/csc.scm.in ${CMAKE_CURRENT_BINARY_DIR}/csc.scm)
  MESSAGE(STATUS "Configuring ${Chicken_SOURCE_DIR}/csc.scm.in to ${CMAKE_CURRENT_BINARY_DIR}/csc.scm"
  )
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/csc.c
    MAIN_DEPENDENCY ${CMAKE_CURRENT_BINARY_DIR}/csc.scm
    DEPENDS ${Chicken_SOURCE_DIR}/banner.scm
    COMMAND ${CURRENT_CHICKEN} ${CMAKE_CURRENT_BINARY_DIR}/csc.scm -output-file ${CMAKE_CURRENT_BINARY_DIR}/csc.c ${CHICKEN_FLAGS}
    COMMENT "Compiling ${CMAKE_CURRENT_BINARY_DIR}/csc.scm to ${CMAKE_CURRENT_BINARY_DIR}/csc.c"
  )
ENDMACRO(GEN_CSC_C)

# csi gets special treatment with a prologue.

MACRO(GEN_CSI_C)
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
ENDMACRO(GEN_CSI_C)

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
    DEPENDS ${Chicken_SOURCE_DIR}/silex.scm
    COMMAND ${CURRENT_CSI} -script ${Chicken_SOURCE_DIR}/silex.scm ${Chicken_SOURCE_DIR}/easyffi.l ${CMAKE_CURRENT_BINARY_DIR}/easyffi.l.silex counters none
    COMMENT "Lexing ${Chicken_SOURCE_DIR}/easyffi.l to ${CMAKE_CURRENT_BINARY_DIR}/easyffi.l.silex"
  )
ENDMACRO(GEN_EASYFFI_L_SILEX)

# Sometimes we need, or are worried that we need ;-) a local copy of a file.
# We do it this way instead of with CONFIGURE_FILE so that there's a dependency,
# and so that it happens at the point in the build we expect.

MACRO(LOCAL_COPY filename)
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${filename}
    MAIN_DEPENDENCY ${Chicken_SOURCE_DIR}/${filename}
    COMMAND ${CMAKE_COMMAND} -E copy_if_different ${Chicken_SOURCE_DIR}/${filename} ${CMAKE_CURRENT_BINARY_DIR}/${filename}
    COMMENT "Copying ${Chicken_SOURCE_DIR}/${filename} to ${CMAKE_CURRENT_BINARY_DIR}/${filename}"
  )
ENDMACRO(LOCAL_COPY)


####################################################################
#  DEDUCE OS SPECIFIC BUILD OPTIONS                                #
####################################################################


# Be careful about whether NOT CYGWIN or NOT CMAKE_COMPILER_IS_GNUCC
# is really intended.  GCC has many similarities of operation across
# Linux, Cygwin, and MinGW, but there are also differences.


#All our various optional libraries go in here.
SET(EXTRA_LIBS )


# This flag is definitely needed for MSVC.
# Does MinGW need it?
IF(MSVC)
  ADD_DEFINITIONS(-DC_DEFAULT_TARGET_STACK_SIZE=300000)
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
    SET(REGEX_UNIT regex)
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
  SET(POSIX_UNIT posix)
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
#  USER OPTIONS                                                    #
####################################################################

# These appear as checkbox items in the CMakeSetup and CCMake GUIs.

# give the user an option to enable support for procedure-serialiazation
OPTION(ENABLE_PROCEDURE_TABLES "enable support for serialization of procedures" TRUE)
IF(ENABLE_PROCEDURE_TABLES)
  ADD_DEFINITIONS(-DC_ENABLE_PTABLES)
ENDIF(ENABLE_PROCEDURE_TABLES)

# give the user an option to enable support for symbol-GC
OPTION(ENABLE_SYMBOL_GC "enable support for GC of unused symbols" FALSE)
IF(ENABLE_SYMBOL_GC)
  ADD_DEFINITIONS(-DC_COLLECT_ALL_SYMBOLS)
ENDIF(ENABLE_SYMBOL_GC)

# give the user an option to enable support for an extra slot in symbols
OPTION(ENABLE_EXTRA_SYMBOL_SLOT "enable extra data slot for symbols" FALSE)
IF(ENABLE_EXTRA_SYMBOL_SLOT)
  ADD_DEFINITIONS(-DC_EXTRA_SYMBOL_SLOT)
ENDIF(ENABLE_EXTRA_SYMBOL_SLOT)

# give the user an option to disable support for the apply-hook (single stepping)
OPTION(DISABLE_APPLY_HOOK "disable apply hook" FALSE)
IF(DISABLE_APPLY_HOOK)
  ADD_DEFINITIONS(-DC_NO_APPLY_HOOK)
ENDIF(DISABLE_APPLY_HOOK)


####################################################################
#  DEFINE C_STACK_GROWS_DOWNWARD                                   #
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
ADD_DEFINITIONS(-DC_STACK_GROWS_DOWNWARD=${STACK_GROWS_DOWNWARD})
MESSAGE(STATUS "C_STACK_GROWS_DOWNWARD=${STACK_GROWS_DOWNWARD}")


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
  GLOBALIZE(${out})
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
  LOCALIZE(${out})
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
  LOCALIZE(${out})
ENDMACRO(GET_LIB_SAFE_CGEN)

# The UNSAFE library names have a 'u' prefix.

SET(LIB_UNSAFE_CGEN
  ${LIB_SAFE_CGEN}
)
ADD_PREFIX(u LIB_UNSAFE_CGEN)

MACRO(GET_LIB_UNSAFE_CGEN out)
  SET(${out} ${LIB_UNSAFE_CGEN})
  ADD_SUFFIX(${out} .c)
  LOCALIZE(${out})
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
  LOCALIZE(${out})
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
  partition
  support
)
MACRO(GET_CHICKEN_CGEN out)
  SET(${out} ${CHICKEN_CGEN})
  ADD_SUFFIX(${out} .c)
  LOCALIZE(${out})
ENDMACRO(GET_CHICKEN_CGEN)

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

MACRO(GET_CHICKEN_C_SOURCES out)
  GET_CHICKEN_CGEN(cgen)
  SET(${out}
    ${Chicken_SOURCE_DIR}/chicken.h
    ${Chicken_SOURCE_DIR}/chicken.rc
    ${cgen}
  )
ENDMACRO(GET_CHICKEN_C_SOURCES)


####################################################################
#  BOOTSTRAP                                                       #
####################################################################

# Now that we've determined everything we need to know to build,
# we can transfer control to a subdirectory and create a 1st
# stage boot compiler.

SET(CURRENT_CHICKEN ${VALID_CHICKEN})
SET(CURRENT_CSI ${VALID_CSI})

ADD_SUBDIRECTORY(boot)

GET_TARGET_PROPERTY(CHICKEN_BOOT_EXE chicken-boot LOCATION)
SET(CURRENT_CHICKEN ${CHICKEN_BOOT_EXE})
GET_TARGET_PROPERTY(CSI_BOOT_EXE csi-boot LOCATION)
SET(CURRENT_CSI ${CSI_BOOT_EXE})

# Now we're compiling with the up-to-date version of Chicken. Yippie!


####################################################################
#  .C GENERATION                                                   #
####################################################################

# Do these after we've created a 1st stage compiler via the bootstrap.

SIMPLE_SCM_TO_C(batch-driver parameters)
SIMPLE_SCM_TO_C(c-backend tweaks)
SIMPLE_SCM_TO_C(c-platform)
SIMPLE_SCM_TO_C(chicken build chicken-ffi-macros chicken-more-macros tweaks)
SIMPLE_SCM_TO_C(chicken-profile)
SIMPLE_SCM_TO_C(chicken-setup build parameters)
SIMPLE_SCM_TO_C(compiler easyffi parameters tweaks)
GEN_CSC_C()
GEN_CSI_C()
GEN_EASYFFI_C()
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
SIMPLE_SCM_TO_C(partition)
LIBRARY_SCM_TO_C_EXPORTS(pcre regex-common)
UNSAFE_LIBRARY_SCM_TO_C(pcre regex-common)
LIBRARY_SCM_TO_C_EXPORTS(posix)
UNSAFE_LIBRARY_SCM_TO_C(posix)
LIBRARY_SCM_TO_C_EXPORTS(posixwin)
UNSAFE_LIBRARY_SCM_TO_C(posixwin)
LIBRARY_SCM_TO_C_EXPORTS(pregexp regex-common)
UNSAFE_LIBRARY_SCM_TO_C(pregexp regex-common)
LIBRARY_SCM_TO_C(profiler)
LIBRARY_SCM_TO_C_EXPORTS(regex regex-common)
UNSAFE_LIBRARY_SCM_TO_C(regex regex-common)
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
SIMPLE_SCM_TO_C(support banner parameters tweaks)
LIBRARY_SCM_TO_C_EXPORTS(tcp)
UNSAFE_LIBRARY_SCM_TO_C(tcp)
LIBRARY_SCM_TO_C_EXPORTS(tinyclos)
UNSAFE_LIBRARY_SCM_TO_C(tinyclos)
LIBRARY_SCM_TO_C_EXPORTS(utils)
UNSAFE_LIBRARY_SCM_TO_C(utils)

# Make sure all .c files are generated before trying to build
# libraries.  Otherwise the dependencies tend to clobber each
# other.  It seems that CMake 2.4.2's file level dependencies
# aren't sufficient to make toplevel targets sequence properly,
# especially under Visual Studio .NET 2003.

GET_LIB_COMMON_CGEN(common)
ADD_CUSTOM_TARGET(common_cgen DEPENDS ${common})
ADD_DEPENDENCIES(common_cgen boot)

GET_LIB_SAFE_CGEN(safe)
ADD_CUSTOM_TARGET(safe_cgen DEPENDS ${safe})
ADD_DEPENDENCIES(safe_cgen boot)

GET_LIB_UNSAFE_CGEN(unsafe)
ADD_CUSTOM_TARGET(unsafe_cgen DEPENDS ${unsafe})
ADD_DEPENDENCIES(unsafe_cgen boot)

GET_CHICKEN_CGEN(hatch)
ADD_CUSTOM_TARGET(chicken_cgen DEPENDS ${hatch})
ADD_DEPENDENCIES(chicken_cgen boot)


####################################################################
#  CREATE LIBCHICKEN                                               #
####################################################################

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
ADD_DEPENDENCIES(libchicken boot common_cgen safe_cgen)

ADD_LIBRARY(libchicken-static STATIC ${CHICKEN_LIB_SOURCES})
SET_TARGET_PROPERTIES(libchicken-static PROPERTIES
  PREFIX "" IMPORT_PREFIX ""
  COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN ${STATIC_FLAGS}"
)
TARGET_LINK_LIBRARIES(libchicken-static ${EXTRA_LIBS})
ADD_DEPENDENCIES(libchicken-static boot common_cgen safe_cgen)

ADD_LIBRARY(libuchicken SHARED ${CHICKEN_UNSAFE_LIB_SOURCES})
SET_TARGET_PROPERTIES(libuchicken PROPERTIES
  PREFIX "" IMPORT_PREFIX ""
  COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN -DC_UNSAFE_RUNTIME ${SHARED_FLAGS}"
)
TARGET_LINK_LIBRARIES(libuchicken ${EXTRA_LIBS})
ADD_DEPENDENCIES(libuchicken boot common_cgen unsafe_cgen)

ADD_LIBRARY(libuchicken-static STATIC ${CHICKEN_UNSAFE_LIB_SOURCES})
SET_TARGET_PROPERTIES(libuchicken-static PROPERTIES
  PREFIX "" IMPORT_PREFIX ""
  COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN -DC_UNSAFE_RUNTIME ${STATIC_FLAGS}"
)
TARGET_LINK_LIBRARIES(libuchicken-static ${EXTRA_LIBS})
ADD_DEPENDENCIES(libuchicken-static boot common_cgen unsafe_cgen)

IF(WIN32 AND NOT CYGWIN)

  ADD_LIBRARY(libchicken_gui SHARED ${CHICKEN_LIB_SOURCES})
  SET_TARGET_PROPERTIES(libchicken_gui PROPERTIES
    PREFIX "" IMPORT_PREFIX ""
    COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN -DC_WINDOWS_GUI ${SHARED_FLAGS}"
  )
  TARGET_LINK_LIBRARIES(libchicken_gui kernel32 user32 gdi32 ${EXTRA_LIBS})
  ADD_DEPENDENCIES(libchicken_gui boot)

  ADD_LIBRARY(libchicken_gui-static STATIC ${CHICKEN_LIB_SOURCES})
  SET_TARGET_PROPERTIES(libchicken_gui-static PROPERTIES
    PREFIX "" IMPORT_PREFIX ""
    COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN -DC_WINDOWS_GUI ${STATIC_FLAGS}"
  )
  TARGET_LINK_LIBRARIES(libchicken_gui-static ${GUI_LIBS} ${EXTRA_LIBS})
  ADD_DEPENDENCIES(libchicken_gui-static boot common_cgen safe_cgen)

ENDIF(WIN32 AND NOT CYGWIN)


####################################################################
#  NURSERY                                                         #
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
ADD_DEPENDENCIES(nsample boot)


####################################################################
#  CREATE CSC, CSI, CHICKEN-PROFILE, CHICKEN-SETUP                 #
####################################################################

# These only depend on libchicken so build 'em earlier.

ADD_EXECUTABLE(csi ${CMAKE_CURRENT_BINARY_DIR}/csi.c)
SET_TARGET_PROPERTIES(csi PROPERTIES COMPILE_FLAGS "${SHARED_FLAGS}")
TARGET_LINK_LIBRARIES(csi libchicken)
ADD_DEPENDENCIES(csi boot)

ADD_EXECUTABLE(csi-static ${CMAKE_CURRENT_BINARY_DIR}/csi.c)
SET_TARGET_PROPERTIES(csi-static PROPERTIES COMPILE_FLAGS "${STATIC_FLAGS}")
TARGET_LINK_LIBRARIES(csi-static libchicken-static)
ADD_DEPENDENCIES(csi-static boot)

ADD_EXECUTABLE(csc ${CMAKE_CURRENT_BINARY_DIR}/csc.c)
SET_TARGET_PROPERTIES(csc PROPERTIES COMPILE_FLAGS "${SHARED_FLAGS}")
TARGET_LINK_LIBRARIES(csc libchicken)
ADD_DEPENDENCIES(csc boot)
# Felix says csc-static isn't necessary.

ADD_EXECUTABLE(chicken-profile ${CMAKE_CURRENT_BINARY_DIR}/chicken-profile.c)
SET_TARGET_PROPERTIES(chicken-profile PROPERTIES COMPILE_FLAGS "${SHARED_FLAGS}")
TARGET_LINK_LIBRARIES(chicken-profile libchicken)
ADD_DEPENDENCIES(chicken-profile boot)

ADD_EXECUTABLE(chicken-setup ${CMAKE_CURRENT_BINARY_DIR}/chicken-setup.c)
SET_TARGET_PROPERTIES(chicken-setup PROPERTIES COMPILE_FLAGS "${SHARED_FLAGS}")
TARGET_LINK_LIBRARIES(chicken-setup libchicken)
ADD_DEPENDENCIES(chicken-setup boot)


####################################################################
#  CREATE easyffi.l.silex                                          #
####################################################################

# generate easyffi.l.silex, which is needed by easyffi.c,GET_LIB_UNSAFE_CGEN(unsafe)
ADD_CUSTOM_TARGET(unsafe_cgen DEPENDS ${unsafe})


# which is needed by chicken.exe.

GEN_EASYFFI_L_SILEX()


####################################################################
#  CREATE CHICKEN.EXE                                              #
####################################################################


GET_CHICKEN_C_SOURCES(CHICKEN_EXE_SOURCES)

ADD_EXECUTABLE(chicken ${CHICKEN_EXE_SOURCES})
SET_TARGET_PROPERTIES(chicken PROPERTIES COMPILE_FLAGS "${SHARED_FLAGS}")
TARGET_LINK_LIBRARIES(chicken libchicken)
ADD_DEPENDENCIES(chicken boot chicken_cgen)

ADD_EXECUTABLE(chicken-static ${CHICKEN_EXE_SOURCES})
SET_TARGET_PROPERTIES(chicken-static PROPERTIES COMPILE_FLAGS "${STATIC_FLAGS}")
TARGET_LINK_LIBRARIES(chicken-static libchicken-static)
ADD_DEPENDENCIES(chicken-static boot chicken_cgen)


####################################################################
#  DOCUMENTATION                                                   #
####################################################################

# build the html documentation if makeinfo is available
FIND_PROGRAM(MAKEINFO_EXE NAMES makeinfo)
IF(NOT MAKEINFO_EXE)
  MESSAGE("makeinfo not found.  If you want chicken.html, either add
    makeinfo to your PATH, or specify it in CMakeSetup or CCMake.")
  # Hope that the user does so.  Don't bug 'em about re-running CMakeSetup.
  SET(MAKEINFO_EXE makeinfo)
ENDIF(NOT MAKEINFO_EXE)

ADD_CUSTOM_COMMAND(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/chicken.html
  MAIN_DEPENDENCY ${Chicken_SOURCE_DIR}/chicken.texi
  COMMAND ${MAKEINFO_EXE} --no-split --html --output=${CMAKE_CURRENT_BINARY_DIR}/chicken.html ${Chicken_SOURCE_DIR}/chicken.texi
  COMMENT "Compiling ${Chicken_SOURCE_DIR}/chicken.texi to ${CMAKE_CURRENT_BINARY_DIR}/chicken.html"
)
  
ADD_CUSTOM_TARGET(html ALL DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/chicken.html)


####################################################################
#  INSTALLATION                                                    #
####################################################################

# Haven't really dealt with all the subdirectories!
# Is this subdirectory structure hardwired for ALL platforms?
# Or do I have to specify it in chicken-defaults.h or something?

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
INSTALL(FILES ${exports} DESTINATION lib)

IF(WIN32)
  INSTALL(FILES csibatch.bat DESTINATION bin)
  IF(NOT CYGWIN)
    INSTALL(TARGETS
      libchicken_gui
      libchicken_gui-static
      RUNTIME DESTINATION bin
      LIBRARY DESTINATION lib
      ARCHIVE DESTINATION lib
    )
  ENDIF(NOT CYGWIN)
ENDIF(WIN32)

INSTALL(FILES chicken-ffi-macros.scm chicken-more-macros.scm
  DESTINATION share)

INSTALL(FILES chicken.h DESTINATION include)
INSTALL(FILES chicken.1 csi.1 csc.1 chicken-setup.1 chicken-profile.1 
  DESTINATION man/man1)
INSTALL(FILES ${CMAKE_CURRENT_BINARY_DIR}/chicken.html DESTINATION doc)
