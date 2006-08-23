# Copyright (c) 2006 by Brandon J. Van Every under MIT-style license.
# See LICENSE section at end of file for full license text.

####################################################################
#  MINIMAL BUILD                                                   #
####################################################################

# Files built here are only used for the bootstrap.  They are never installed.
# Consequently, we don't need to build everything.  We only build static
# libraries and executables, no need to fool with anything more complicated.
# We build:
#  libchicken-boot
#  chicken-boot
#
# chicken-boot is then used by the rest of the build.


####################################################################
#  CHICKEN VERSION MACROS                                          #
####################################################################

# Canonize the version and build into 1 number that can be
# easily compared.  We assume the build number has no more
# than 6 digits of precision.  If Chicken ever exceeds that,
# watch out!

# Chicken has been using single digits for major stable build
# releases.  However, interim releases in Darcs have triple
# digits for the build.  This means that Chicken 2.216 would
# canonize as greater than 2.3, unless we promote 2.3 to 2.300.

MACRO(CANONIZE_VERSION version build canonized)
  IF(build LESS 100)
    MATH(EXPR ${canonized} "${version} * 1000000 + ${build} * 100")
  ELSE(build LESS 100)
    MATH(EXPR ${canonized} "${version} * 1000000 + ${build}")
  ENDIF(build LESS 100)
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
# The bootstrap compiler doesn't need to generate .exports.
# Our final compiler will ultimately generate .exports.

SET(EXPORTS_CHICKEN_VERSION 2)
SET(EXPORTS_CHICKEN_BUILD 310)

CANONIZE_VERSION(${EXPORTS_CHICKEN_VERSION} ${EXPORTS_CHICKEN_BUILD}
  MINIMUM_CANONICAL_EXPORTS
)

##############################################################
#                                                            #
#  FIND CHICKEN                                              #
#                                                            #
#  Output:  EXTANT_CHICKEN                                   #
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
    MESSAGE("Can't find Chicken, which is required to generate bootstrap .c files.")
  ENDIF(EXTANT_CHICKEN)
ENDIF(NOT EXTANT_CHICKEN)


##############################################################
#                                                            #
#  VALIDATE VERSION REQIREMENTS                              #
#                                                            #
#  Output:  VALID_CHICKEN                                    #
#                                                            #
##############################################################

SET(VALID_CHICKEN "-NOTFOUND")
IF(EXTANT_CHICKEN)
  GET_TOOL_VERSION(${EXTANT_CHICKEN} chicken_version)
  IF(chicken_version LESS MINIMUM_CANONICAL_CHICKEN)
    MESSAGE("Chicken ${MINIMUM_CANONICAL_CHICKEN} required to generate bootstrap .c files.
      Available Chicken is only ${chicken_version}."
    )
  ELSE(chicken_version LESS MINIMUM_CANONICAL_CHICKEN)
    MESSAGE(STATUS "Chicken ${MINIMUM_CANONICAL_CHICKEN} required; using ${chicken_version}.")
    SET(VALID_CHICKEN ${EXTANT_CHICKEN})
  ENDIF(chicken_version LESS MINIMUM_CANONICAL_CHICKEN)
ENDIF(EXTANT_CHICKEN)

# it may or may not be valid, but this is what we have to work with.
SET(CURRENT_CHICKEN ${VALID_CHICKEN})


####################################################################
#  BOOT_CFILES                                                     #
####################################################################

# BOOT_CFILES is a list of all .c files necessary to build a bootstrap
# Chicken compiler, without a valid Chicken compiler being available.
# A Chicken build tree can come either with or without these files.
# Typically, when downloaded as a tarball distribution, these files
# will be present.  But when pulled straight from Darcs, these files
# will be absent.  If they are present, then we don't really need a
# Chicken compiler to do the build, although we may choose to use one
# anyways.

# Check if all BOOT_CFILES are present in the build tree.
SET(HAVE_BOOT_CFILES TRUE)
IF(NOT VALID_CHICKEN)
  MESSAGE("You have no valid Chicken available.
    If you have all the boot/*.c.in files that a bootstrap needs,
    then I can still build Chicken for you."
  )
ENDIF(NOT VALID_CHICKEN)
FOREACH(f ${BOOT_CFILES})
  IF(NOT EXISTS ${Chicken_SOURCE_DIR}/boot/${f}.in)
    SET(HAVE_BOOT_CFILES FALSE)
    IF(VALID_CHICKEN)
      MESSAGE(STATUS "Missing ${Chicken_SOURCE_DIR}/boot/${f}.in
        Not an error, can generate if needed.")
    ELSE(VALID_CHICKEN)
      MESSAGE(FATAL_ERROR "${Chicken_SOURCE_DIR}/boot/${f}.in is missing.
        Giving up, terminating build."
      )
    ENDIF(VALID_CHICKEN)
  ENDIF(NOT EXISTS ${Chicken_SOURCE_DIR}/boot/${f}.in)
ENDFOREACH(f)
IF(HAVE_BOOT_CFILES)
  IF(VALID_CHICKEN)
    # Just give status
    MESSAGE(STATUS "All boot/*.c.in files are present.")
  ELSE(VALID_CHICKEN)
    # Pester the user with a dialog box that everything will be ok.
    MESSAGE("All boot/*.c.in files are present.")
  ENDIF(VALID_CHICKEN)
ENDIF(HAVE_BOOT_CFILES)


####################################################################
#  .C GENERATION                                                   #
####################################################################

# Do not use LIBRARY_SCM_TO_C_EXPORTS to build the bootstrap compiler.
# We don't need the exports to build the next compiler stage, and
# Chicken 2.3 doesn't generate them.  We're worried that extra
# dependencies may cause CMake 2.4.2 to always build everything.
# If EXTANT_CHICKEN is a high enough version number to generate .exports,
# we'll have stray .exports junk in the directory.  Do we care about
# cleaning it up?

CONFIGURE_FILE(${Chicken_SOURCE_DIR}/chicken-paths.h.in
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-paths.h)

IF(HAVE_BOOT_CFILES)
  # We will simply copy boot/*.c.in files
  FOREACH(f ${BOOT_CFILES})
    ADD_CUSTOM_COMMAND(
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${f}
      MAIN_DEPENDENCY ${Chicken_SOURCE_DIR}/boot/${f}.in
      COMMAND ${CMAKE_COMMAND} -E copy_if_different "${Chicken_SOURCE_DIR}/boot/${f}.in" "${CMAKE_CURRENT_BINARY_DIR}/${f}"
      COMMENT "Copying ${Chicken_SOURCE_DIR}/boot/${f}.in to ${CMAKE_CURRENT_BINARY_DIR}/${f}"
    )
  ENDFOREACH(f)

ELSE(HAVE_BOOT_CFILES)
  # We will generate the .c files using the available Chicken compiler.

  SIMPLE_SCM_TO_C(batch-driver)
  SIMPLE_SCM_TO_C(c-backend tweaks)
  SIMPLE_SCM_TO_C(c-platform)
  SIMPLE_SCM_TO_C(chicken build chicken-ffi-macros chicken-more-macros tweaks)
  SIMPLE_SCM_TO_C(compiler tweaks)
  LIBRARY_SCM_TO_C(eval)
  LIBRARY_SCM_TO_C(extras)
  LIBRARY_SCM_TO_C(library build)
  LIBRARY_SCM_TO_C(lolevel)
  LIBRARY_SCM_TO_C(match)
  SIMPLE_SCM_TO_C(optimizer tweaks)
  LIBRARY_SCM_TO_C(pcre regex-common)
  LIBRARY_SCM_TO_C(posixunix)
  LIBRARY_SCM_TO_C(posixwin)
  LIBRARY_SCM_TO_C(pregexp regex-common)
  LIBRARY_SCM_TO_C(profiler)
  LIBRARY_SCM_TO_C(regexunix regex-common)
  LIBRARY_SCM_TO_C(scheduler)
  LIBRARY_SCM_TO_C(srfi-1)
  LIBRARY_SCM_TO_C(srfi-4)
  LIBRARY_SCM_TO_C(srfi-13)
  LIBRARY_SCM_TO_C(srfi-14)
  LIBRARY_SCM_TO_C(srfi-18)
  LIBRARY_SCM_TO_C(stub)
  SIMPLE_SCM_TO_C(support banner tweaks)
  LIBRARY_SCM_TO_C(tcp)
  LIBRARY_SCM_TO_C(utils)

ENDIF(HAVE_BOOT_CFILES)


####################################################################
#  CREATE LIBCHICKEN-BOOT                                          #
####################################################################

GET_LIBCHICKEN_C_SOURCES(LIBCHICKEN_BOOT_SOURCES)

ADD_LIBRARY(libchicken-boot STATIC ${LIBCHICKEN_BOOT_SOURCES})
SET_TARGET_PROPERTIES(libchicken-boot PROPERTIES
  COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN ${STATIC_FLAGS}"
  OUTPUT_NAME chicken-boot)
IF(WIN32 AND NOT CMAKE_COMPILER_IS_GNUCC)
  SET_TARGET_PROPERTIES(libchicken-boot PROPERTIES PREFIX "lib")
ENDIF(WIN32 AND NOT CMAKE_COMPILER_IS_GNUCC)

TARGET_LINK_LIBRARIES(libchicken-boot ${EXTRA_LIBS})


####################################################################
#  STACK SIZE                                                      #
####################################################################

# We don't do any nursery sampling for the bootstrap compiler.
CONFIGURE_FILE(${Chicken_SOURCE_DIR}/stack-size.h.in
  ${CMAKE_CURRENT_BINARY_DIR}/stack-size.h)


####################################################################
#  CREATE CHICKEN-BOOT                                             #
####################################################################

GET_CHICKEN_C_SOURCES(CHICKEN_BOOT_SOURCES)
ADD_EXECUTABLE(chicken-boot ${CHICKEN_BOOT_SOURCES})
SET_TARGET_PROPERTIES(chicken-boot PROPERTIES COMPILE_FLAGS "${STATIC_FLAGS}")
TARGET_LINK_LIBRARIES(chicken-boot libchicken-boot)


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
