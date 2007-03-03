# Copyright (c) 2006..2007 by Brandon J. Van Every under MIT-style license.
# See LICENSE section at end of file for full license text.


####################################################################
#  LIBRARY COLLISION BUG                                           #
####################################################################

# CMake 2.4.3 has a bug where shared and static libraries in the same
# directory clobber each other if they have the same rootname.  Also,
# if static and shared libraries are in the same directory, static
# exes will prefer to link with the shared libraries.  To work around
# these problems, we build all static libs and exes in a subdirectory.

# The .c files we need are actually in a parent directory.
# Fortunately, we've already specified
#
#   CHICKEN_LIB_SOURCES
#   CHICKEN_UNSAFE_LIB_SOURCES
#   CSI_EXE_SOURCES
#   CHICKEN_EXE_SOURCES
#
# at the correct locations for fully qualifying the names of the
# .c files.  This makes target generation pretty easy.
#
# Doing static library generation in a subdirectory allows us to
# avoid pain at installation time.  Previously, I had solved the
# problem on MinGW by postfixing -s to the static library name, then
# copying the library to the correct name at install time.  This
# works fine when you know what your library suffixes are supposed
# to be, but it becomes a real PITA when you're dealing with unknown
# OSes that you can't test.  It's simpler to avoid the collision
# problem, build in separate namespaces, and not have to do anything
# special at INSTALL time.


####################################################################
#  CREATE LIBCHICKEN                                               #
####################################################################

# When in a subdirectory, CMake won't pick up that our
# .c files are generated.  So we fudge around it; note that
# GET_LIB_CSOURCE files are part of these files and they're not generated.
# Also there are duplicate files in the list; hope
# that doesn't cause problems.  But if it works, close enough.

SET_SOURCE_FILES_PROPERTIES(
  ${CHICKEN_LIB_SOURCES}
  ${CHICKEN_UNSAFE_LIB_SOURCES}
  ${CSI_EXE_SOURCES}
  ${CHICKEN_EXE_SOURCES}
  ${PCRE_FOR_STATIC_OBJS}
  PROPERTIES GENERATED true
)

IF(MSVC)

  # Eventually, we wish to support different postfixes for debug, release builds etc.
  # For now, the user has to install the 1 configuration she actually wants.

  ADD_LIBRARY(libchicken-static STATIC ${CHICKEN_LIB_SOURCES} ${PCRE_HEADER} ${PCRE_FOR_STATIC_OBJS})
  SET_TARGET_PROPERTIES(libchicken-static PROPERTIES
    COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN ${STATIC_FLAGS}"
    OUTPUT_NAME "chicken" PREFIX "lib"
    RELEASE_POSTFIX "-s" MINSIZEREL_POSTFIX "-s" DEBUG_POSTFIX "-s" RELWITHDEBINFO_POSTFIX "-s")
  TARGET_LINK_LIBRARIES(libchicken-static ${MORE_STATIC_LIBS})
  ADD_DEPENDENCIES(libchicken-static libchicken-c libpcre-for-static)

  ADD_LIBRARY(libuchicken-static STATIC ${CHICKEN_UNSAFE_LIB_SOURCES} ${PCRE_HEADER} ${PCRE_FOR_STATIC_OBJS})
  SET_TARGET_PROPERTIES(libuchicken-static PROPERTIES
    COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN -DC_UNSAFE_RUNTIME ${STATIC_FLAGS}"
    OUTPUT_NAME "uchicken" PREFIX "lib"
    RELEASE_POSTFIX "-s" MINSIZEREL_POSTFIX "-s" DEBUG_POSTFIX "-s" RELWITHDEBINFO_POSTFIX "-s")
  TARGET_LINK_LIBRARIES(libuchicken-static ${MORE_STATIC_LIBS})
  ADD_DEPENDENCIES(libuchicken-static libuchicken-c libpcre-for-static)

  ADD_LIBRARY(libchickengui-static STATIC ${CHICKEN_LIB_SOURCES} ${PCRE_HEADER} ${PCRE_FOR_STATIC_OBJS})
  SET_TARGET_PROPERTIES(libchickengui-static PROPERTIES
    COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN -DC_WINDOWS_GUI ${STATIC_FLAGS}"
    OUTPUT_NAME "chickengui" PREFIX "lib"
    RELEASE_POSTFIX "-s" MINSIZEREL_POSTFIX "-s" DEBUG_POSTFIX "-s" RELWITHDEBINFO_POSTFIX "-s")
  TARGET_LINK_LIBRARIES(libchickengui-static ${GUI_LIBS} ${MORE_STATIC_LIBS})
  ADD_DEPENDENCIES(libchickengui-static libchicken-c libpcre-for-static)

ELSE(MSVC)

  ADD_LIBRARY(libchicken-static STATIC ${CHICKEN_LIB_SOURCES} ${PCRE_HEADER} ${PCRE_FOR_STATIC_OBJS})
  SET_TARGET_PROPERTIES(libchicken-static PROPERTIES
    COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN ${STATIC_FLAGS}"
    OUTPUT_NAME "chicken")
  TARGET_LINK_LIBRARIES(libchicken-static ${MORE_STATIC_LIBS})
  ADD_DEPENDENCIES(libchicken-static libchicken-c libpcre-for-static)

  ADD_LIBRARY(libuchicken-static STATIC ${CHICKEN_UNSAFE_LIB_SOURCES} ${PCRE_HEADER} ${PCRE_FOR_STATIC_OBJS})
  SET_TARGET_PROPERTIES(libuchicken-static PROPERTIES
    COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN -DC_UNSAFE_RUNTIME ${STATIC_FLAGS}"
    OUTPUT_NAME "uchicken")
  TARGET_LINK_LIBRARIES(libuchicken-static ${MORE_STATIC_LIBS})
  ADD_DEPENDENCIES(libuchicken-static libuchicken-c libpcre-for-static)

  IF(WIN32 AND NOT CYGWIN)
    ADD_LIBRARY(libchickengui-static STATIC ${CHICKEN_LIB_SOURCES} ${PCRE_HEADER} ${PCRE_FOR_STATIC_OBJS})
    SET_TARGET_PROPERTIES(libchickengui-static PROPERTIES
      COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN -DC_WINDOWS_GUI ${STATIC_FLAGS}"
      OUTPUT_NAME "chickengui")
    TARGET_LINK_LIBRARIES(libchickengui-static ${GUI_LIBS} ${MORE_STATIC_LIBS})
    ADD_DEPENDENCIES(libchickengui-static libchicken-c libpcre-for-static)
  ENDIF(WIN32 AND NOT CYGWIN)

ENDIF(MSVC)


####################################################################
#  CREATE CSI-STATIC                                               #
####################################################################

IF(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  SET(MACOSX TRUE)
ELSE(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  SET(MACOSX FALSE)
ENDIF(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")

SET(MACOSX TRUE)

IF(NOT MACOSX)
  ADD_EXECUTABLE(csi-static ${CSI_EXE_SOURCES})
  SET_TARGET_PROPERTIES(csi-static PROPERTIES COMPILE_FLAGS "${STATIC_FLAGS}")
  TARGET_LINK_LIBRARIES(csi-static libchicken-static)
  ADD_DEPENDENCIES(csi-static csi-c)
ENDIF(NOT MACOSX)


####################################################################
#  CREATE CHICKEN-STATIC.EXE                                       #
####################################################################

IF(NOT MACOSX)
  ADD_EXECUTABLE(chicken-static ${CHICKEN_EXE_SOURCES})
  SET_TARGET_PROPERTIES(chicken-static PROPERTIES COMPILE_FLAGS "${STATIC_FLAGS}")
  TARGET_LINK_LIBRARIES(chicken-static libchicken-static)
  ADD_DEPENDENCIES(chicken-static chicken-c)
ENDIF(NOT MACOSX)


####################################################################
#  INSTALLATION                                                    #
####################################################################

INSTALL(TARGETS
  libchicken-static
  libuchicken-static
  LIBRARY DESTINATION ${LIB_HOME}
  ARCHIVE DESTINATION ${LIB_HOME})
IF(WIN32 AND NOT CYGWIN)
  INSTALL(TARGETS
    libchickengui-static
    RUNTIME DESTINATION ${BIN_HOME}
    LIBRARY DESTINATION ${LIB_HOME}
    ARCHIVE DESTINATION ${LIB_HOME})
ENDIF(WIN32 AND NOT CYGWIN)

IF(NOT MACOSX)
  INSTALL(TARGETS
    chicken-static
    csi-static
    RUNTIME DESTINATION ${BIN_HOME})
ENDIF(NOT MACOSX)


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
