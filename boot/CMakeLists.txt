# Copyright (c) 2006..2007 by Brandon J. Van Every under MIT-style license.
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
#  CHICKEN_CONFIG_H                                                #
####################################################################

CONFIGURE_FILE(${Chicken_SOURCE_DIR}/chicken-config-cmake.h.in
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-config.h)


####################################################################
#  CHICKEN_DEFAULTS_H                                              #
####################################################################

# ./configure does the substitutions at make time, not
# configuration time.  Will this cause problems if
# people are using both builds?
CONFIGURE_FILE(${Chicken_SOURCE_DIR}/chicken-defaults.h.in
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-defaults.h)


####################################################################
#  .C GENERATION                                                   #
####################################################################

# ./configure just plops chicken-config.h in the source tree.
# Try to avoid collisions with it by searching 
# ${CMAKE_CURRENT_BINARY_DIR} first.

INCLUDE_DIRECTORIES(BEFORE ${CMAKE_CURRENT_BINARY_DIR})

# Do not use LIBRARY_SCM_TO_C_EXPORTS to build the bootstrap compiler.
# We don't need the exports to build the next compiler stage, and
# Chicken 2.3 doesn't generate them.
# If EXTANT_CHICKEN is a high enough version number to generate .exports,
# we'll have stray .exports junk in the directory.  Do we care about
# cleaning it up?

IF(HAVE_BOOT_CFILES)
  # We will simply copy boot/cfiles/*.c files
  FOREACH(f ${BOOT_CFILES})
    ADD_CUSTOM_COMMAND(
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${f}
      MAIN_DEPENDENCY ${Chicken_SOURCE_DIR}/boot/cfiles/${f}
      COMMAND ${CMAKE_COMMAND} -E copy_if_different "${Chicken_SOURCE_DIR}/boot/cfiles/${f}" "${CMAKE_CURRENT_BINARY_DIR}/${f}"
      COMMENT "Copying ${Chicken_SOURCE_DIR}/boot/cfiles/${f} to ${CMAKE_CURRENT_BINARY_DIR}/${f}"
    )
  ENDFOREACH(f)

ELSE(HAVE_BOOT_CFILES)
  # We will generate the .c files using the available Chicken compiler.

  # files for libchicken
  LIBRARY_SCM_TO_C(eval)
  LIBRARY_SCM_TO_C(extras)
  LIBRARY_SCM_TO_C(library build)
  LIBRARY_SCM_TO_C(lolevel)
  LIBRARY_SCM_TO_C(match)
  SIMPLE_SCM_TO_C(optimizer tweaks)
  LIBRARY_SCM_TO_C(posixunix)
  LIBRARY_SCM_TO_C(posixwin)
  LIBRARY_SCM_TO_C(profiler)
  LIBRARY_SCM_TO_C(regex)
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

  # files for chicken
  SIMPLE_SCM_TO_C(batch-driver tweaks)
  SIMPLE_SCM_TO_C(c-backend tweaks)
  SIMPLE_SCM_TO_C(c-platform tweaks)
  SIMPLE_SCM_TO_C(chicken build chicken-ffi-macros chicken-more-macros tweaks)
  SIMPLE_SCM_TO_C(compiler tweaks)

ENDIF(HAVE_BOOT_CFILES)


####################################################################
#  CREATE LIBCHICKEN-BOOT                                          #
####################################################################

GET_LIBCHICKEN_C_SOURCES(LIBCHICKEN_BOOT_SOURCES)
ADD_CUSTOM_TARGET(libchicken-boot-c DEPENDS ${LIBCHICKEN_BOOT_SOURCES})

ADD_LIBRARY(libchicken-boot STATIC ${LIBCHICKEN_BOOT_SOURCES} ${PCRE_HEADER})
SET_TARGET_PROPERTIES(libchicken-boot PROPERTIES
  COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN ${STATIC_FLAGS}"
  OUTPUT_NAME chicken-boot)
IF(MSVC)
  SET_TARGET_PROPERTIES(libchicken-boot PROPERTIES PREFIX "lib")
ENDIF(MSVC)
TARGET_LINK_LIBRARIES(libchicken-boot ${MORE_STATIC_LIBS})
ADD_DEPENDENCIES(libchicken-boot libchicken-boot-c)


####################################################################
#  CREATE CHICKEN-BOOT                                             #
####################################################################

GET_CHICKEN_C_SOURCES(CHICKEN_BOOT_SOURCES)
ADD_CUSTOM_TARGET(chicken-boot-c DEPENDS ${CHICKEN_BOOT_SOURCES})

ADD_EXECUTABLE(chicken-boot ${CHICKEN_BOOT_SOURCES})
SET_TARGET_PROPERTIES(chicken-boot PROPERTIES COMPILE_FLAGS "${STATIC_FLAGS}")
TARGET_LINK_LIBRARIES(chicken-boot libchicken-boot libpcre-for-static)
ADD_DEPENDENCIES(chicken-boot chicken-boot-c)


####################################################################
#   LICENSE                                                        #
####################################################################

# Copyright (c) 2006..2007 by Brandon J. Van Every
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
