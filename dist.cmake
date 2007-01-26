# Copyright (c) 2006 by Brandon J. Van Every under MIT-style license.
# See LICENSE section at end of file for full license text.


# inputs:
#   Chicken_SOURCE_DIR
#   DIST_DIR
#   CHANGELOG_FILE
#   BOOT_CFILES

# Turn BOOT_CFILES back into a list.
# This is not safe for filenames with spaces in them,
# but we don't have any of those.
SEPARATE_ARGUMENTS(BOOT_CFILES)

# Files in the Chicken_SOURCE_DIR
SET(SOURCE_DIR_FILES
  autogen.sh
  banner.scm
  batch-driver.scm
  build.scm
  buildversion
  c-backend.scm
  c-platform.scm
  ChangeLog.0-20040412
  chicken-config-cmake.h.in
  chicken-defaults.h.in
  chicken-ffi-macros.scm
  chicken-more-macros.scm
  chicken-profile.1
  chicken-profile.scm
  chicken-setup.1
  chicken-setup.scm
  chicken.1
  chicken.h
  chicken.ico
  chicken.rc
  chicken.scm
  chicken.spec.in
  CMakeLists.txt
  compiler.scm
  configure.in
  csc.1
  csc.scm
  csi.1
  csi.scm
  csibatch.bat
  dist.cmake
  eval.scm
  extras.scm
  hen.el
  INSTALL
  INSTALL-CMake.txt
  library.scm
  LICENSE
  lolevel.scm
  Makefile.am
  match.scm
  NEWS
  optimizer.scm
  regex.scm
  posixunix.scm
  posixwin.scm
  profiler.scm
  README
  runtime.c
  scheduler.scm
  srfi-1.scm
  srfi-4.scm
  srfi-13.scm
  srfi-14.scm
  srfi-18.scm
  StackGrowsDownward.c
  stub.scm
  support.scm
  tcp.scm
  tweaks.scm
  utils.scm
  pcre/pcre_compile.c
  pcre/pcre_get.c
  pcre/pcre_printint.c
  pcre/pcre_ucp_findchar.c
  pcre/pcre_config.c
  pcre/pcre_globals.c
  pcre/pcre_refcount.c
  pcre/pcre_valid_utf8.c
  pcre/pcre_dfa_exec.c
  pcre/pcre_info.c
  pcre/pcre_study.c
  pcre/pcre_version.c
  pcre/pcre_exec.c
  pcre/pcre_maketables.c
  pcre/pcre_tables.c
  pcre/pcre_xclass.c
  pcre/pcre_fullinfo.c
  pcre/pcre_ord2utf8.c
  pcre/pcre_try_flipped.c
  pcre/chartables.c
  pcre/AUTHORS
  pcre/COPYING
  pcre/LICENCE
  pcre/config.h
  pcre/pcre.h
  pcre/pcre_internal.h
  pcre/ucpinternal.h
  pcre/ucp.h
  pcre/ucptable.c
  pcre/ucp_findchar.c
  static/CMakeLists.txt
  benchmarks/boyer.scm
  benchmarks/browse.scm
  benchmarks/conform.scm
  benchmarks/cpstak.scm
  cscbench.scm
  benchmarks/ctak.scm
  benchmarks/dderiv.scm
  benchmarks/deriv.scm
  benchmarks/destructive.scm
  benchmarks/div-iter.scm
  benchmarks/div-rec.scm
  benchmarks/dynamic.scm
  benchmarks/earley.scm
  benchmarks/fft.scm
  benchmarks/fib.scm
  benchmarks/fibc.scm
  benchmarks/fprint.scm
  benchmarks/fread.scm
  benchmarks/hanoi.scm
  benchmarks/lattice.scm
  benchmarks/maze.scm
  benchmarks/nqueens.scm
  benchmarks/plists.scm
  benchmarks/puzzle.scm
  benchmarks/scheme.scm
  benchmarks/stack-size.cmake
  benchmarks/tak.scm
  benchmarks/takl.scm
  benchmarks/takr.scm
  benchmarks/traverse.scm
  benchmarks/travinit.scm
  benchmarks/triangl.scm
  benchmarks/others/except.scm
  benchmarks/others/except2.scm
  benchmarks/others/exception.cpp
  benchmarks/others/Makefile
  benchmarks/others/results.txt
  benchmarks/others/setlongjmp.c
  boot/CMakeLists.txt
  tests/library-tests.scm
  tests/locative-stress-test.scm
  tests/r4rstest.scm
  tests/runtests.sh
  tests/srfi-18-tests.scm
)

# The BOOT_CFILES also need to be copied
# from ${CMAKE_CURRENT_BINARY_DIR}/*.c
# to ${CMAKE_CURRENT_BINARY_DIR}/boot/cfiles/*.c
# so that we're booting from the most current Chicken.


####################################################################
#  INTEGRITY CHECK                                                 #
####################################################################

# Check if all distribution files are present.

IF(NOT EXISTS ${CHANGELOG_FILE})
  MESSAGE(FATAL_ERROR "ChangeLog file is missing: ${CHANGELOG_FILE}")
ENDIF(NOT EXISTS ${CHANGELOG_FILE})
FOREACH(f ${SOURCE_DIR_FILES})
  IF(NOT EXISTS ${Chicken_SOURCE_DIR}/${f})
    MESSAGE(FATAL_ERROR "Distribution file is missing: ${Chicken_SOURCE_DIR}/${f}")
  ENDIF(NOT EXISTS ${Chicken_SOURCE_DIR}/${f})
ENDFOREACH(f)
FOREACH(f ${BOOT_CFILES})
  IF(NOT EXISTS ${CMAKE_CURRENT_BINARY_DIR}/${f})
    MESSAGE(FATAL_ERROR "Distribution file is missing: ${CMAKE_CURRENT_BINARY_DIR}/${f}")
  ENDIF(NOT EXISTS ${CMAKE_CURRENT_BINARY_DIR}/${f})
ENDFOREACH(f)
MESSAGE(STATUS "All distribution files are present.")


####################################################################
#  CREATE DISTRIBUTION TREE                                        #
####################################################################

# Start with an empty tree.
IF(EXISTS ${CMAKE_CURRENT_BINARY_DIR}/${DIST_DIR})
  FILE(REMOVE_RECURSE ${CMAKE_CURRENT_BINARY_DIR}/${DIST_DIR})
ENDIF(EXISTS ${CMAKE_CURRENT_BINARY_DIR}/${DIST_DIR})

EXECUTE_PROCESS(COMMAND ${CMAKE_COMMAND} -E copy "${CHANGELOG_FILE}" "${CMAKE_CURRENT_BINARY_DIR}/${DIST_DIR}/ChangeLog")
FOREACH(f ${SOURCE_DIR_FILES})
  EXECUTE_PROCESS(COMMAND ${CMAKE_COMMAND} -E copy "${Chicken_SOURCE_DIR}/${f}" "${CMAKE_CURRENT_BINARY_DIR}/${DIST_DIR}/${f}")
ENDFOREACH(f)
FOREACH(f ${BOOT_CFILES})
  EXECUTE_PROCESS(COMMAND ${CMAKE_COMMAND} -E copy "${CMAKE_CURRENT_BINARY_DIR}/${f}" "${CMAKE_CURRENT_BINARY_DIR}/${DIST_DIR}/boot/cfiles/${f}")
ENDFOREACH(f)


####################################################################
#   GNU AUTOMAKE                                                   #
####################################################################

# Since CMake is used to generate the canonical Chicken distribution,
# it must invoke Automake so that ./configure et al are created.
# This adds files to the distribution that we haven't previously checked above.
# We do it now, rather than earlier, because "cmake -E copy" changes the timestamp.
# This can cause Automake to run twice, which is annoying.
# Also, doing it here, we can keep ${Chicken_SOURCE_DIR} free of Automake pollution.
#
# A Bourne shell "sh"
# must be available, so that Automake can run.  We shouldn't have
# to run CMakeLists.txt from a Bourne shell; we just need to have,
# say, a sh.exe available in our path.  We aren't currently
# doing any error checking on whether "sh" is available or working.
# Nor whether Automake is installed, for that matter.  Not many people
# need to build distros.
#
# As of Sept. 3rd, 2006, the various Autoconfs + Automakes
# distributed with MSYS are complete garbage.  Don't waste your
# time; I wasted an entire day before giving up.  The GNU Win32
# distros din't help either.  A project called mingw-install
# finally did the trick.  http://sourceforge.net/projects/mingw-install
# Haibin Zhang has solved quite a number of irritating integration
# issues.  Although, caveat emptor, it'll nuke your entire MinGW
# installation including GCC, without warning you!  Proper steps:
#
# - install MinGW-5.0.3.exe
# - install mingw-install-20060210/setup/MSYS-1.0.11-2004.04.30-1.exe
# - install mingw-install-20060210/msys/install.sh
# - comment out the nasty "rm -rf" statements in
#   mingw-install-20060210/mingw/install.sh
# - install mingw-install-20060210/mingw/install.sh

EXECUTE_PROCESS(
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${DIST_DIR}
  COMMAND sh autogen.sh
  RESULT_VARIABLE AUTOGEN_ERROR
)

IF(AUTOGEN_ERROR)
  MESSAGE(FATAL_ERROR "autogen.sh failed")
ENDIF(AUTOGEN_ERROR)

SET(AUTOMAKE_FILES aclocal.m4 compile config.guess config.sub configure install-sh Makefile.in missing ltmain.sh)
FOREACH(f ${AUTOMAKE_FILES})
  IF(NOT EXISTS ${CMAKE_CURRENT_BINARY_DIR}/${DIST_DIR}/${f})
    MESSAGE(FATAL_ERROR "Automake file failed to generate: ${CMAKE_CURRENT_BINARY_DIR}/${DIST_DIR}/${f}")
  ENDIF(NOT EXISTS ${CMAKE_CURRENT_BINARY_DIR}/${DIST_DIR}/${f})
ENDFOREACH(f)


####################################################################
#  CREATE TARBALL                                                  #
####################################################################

SET(DIST_TAR_NAME ${DIST_DIR}.tar.gz)
MESSAGE(STATUS "Creating ${CMAKE_CURRENT_BINARY_DIR}/${DIST_TAR_NAME}")
IF(EXISTS ${DIST_TAR_NAME})
  FILE(REMOVE ${DIST_TAR_NAME})
ENDIF(EXISTS ${DIST_TAR_NAME})
EXECUTE_PROCESS(
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  COMMAND ${CMAKE_COMMAND} -E tar cfz ${DIST_TAR_NAME} ${DIST_DIR}
)

####################################################################
#  CREATE ZIP                                                      #
####################################################################

FIND_PROGRAM(ZIP_EXE zip)
IF(ZIP_EXE)
  SET(DIST_ZIP_NAME ${DIST_DIR}.zip)
  MESSAGE(STATUS "Creating ${CMAKE_CURRENT_BINARY_DIR}/${DIST_ZIP_NAME}")
  IF(EXISTS ${DIST_ZIP_NAME})
    FILE(REMOVE ${DIST_ZIP_NAME})
  ENDIF(EXISTS ${DIST_ZIP_NAME})
  EXECUTE_PROCESS(
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMAND ${ZIP_EXE} -9 -q -r ${DIST_ZIP_NAME} ${DIST_DIR}
  )
ELSE(ZIP_EXE)
  MESSAGE(SEND_ERROR "No zip available to make .zip distribution.")
ENDIF(ZIP_EXE)


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
