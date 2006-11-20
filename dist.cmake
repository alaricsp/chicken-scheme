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
  aclocal.m4
  autogen.sh
  banner.scm
  batch-driver.scm
  build.scm
  buildversion
  c-backend.scm
  c-platform.scm
  ChangeLog.0-20040412
  chicken-config.h.in
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
  compile
  compiler.scm
  config.guess
  config.sub
  configure
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
  install-sh
  library.scm
  LICENSE
  lolevel.scm
  ltmain.sh
  Makefile.am
  Makefile.in
  match.scm
  missing
  NEWS
  optimizer.scm
  pcre.scm
  posixunix.scm
  posixwin.scm
  pregexp.scm
  profiler.scm
  README
  regex-common.scm
  regexunix.scm
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
  benchmarks/shootout/ackermann.chicken
  benchmarks/shootout/ary.chicken
  benchmarks/shootout/bench
  benchmarks/shootout/echo.chicken
  benchmarks/shootout/fibo.chicken
  benchmarks/shootout/hash.chicken
  benchmarks/shootout/hash2.chicken
  benchmarks/shootout/heapsort.chicken
  benchmarks/shootout/hello.chicken
  benchmarks/shootout/lists.chicken
  benchmarks/shootout/lists1.chicken
  benchmarks/shootout/matrix.chicken
  benchmarks/shootout/methcall.chicken
  benchmarks/shootout/moments.chicken
  benchmarks/shootout/nbody.chicken
  benchmarks/shootout/nestedloop.chicken
  benchmarks/shootout/nsieve-bits.scm
  benchmarks/shootout/objinst.chicken
  benchmarks/shootout/plugin.chicken
  benchmarks/shootout/procinst.chicken
  benchmarks/shootout/prodcons.chicken
  benchmarks/shootout/random.chicken
  benchmarks/shootout/regexmatch.chicken
  benchmarks/shootout/reversefile.chicken
  benchmarks/shootout/ringmsg.chicken
  benchmarks/shootout/sieve.chicken
  benchmarks/shootout/spellcheck.chicken
  benchmarks/shootout/strcat.chicken
  benchmarks/shootout/sumcol.chicken
  benchmarks/shootout/threads-new.chicken
  benchmarks/shootout/wc.chicken
  benchmarks/shootout/wordfreq.chicken
  boot/CMakeLists.txt
  tests/library-tests.scm
  tests/locative-stress-test.scm
  tests/r4rstest.scm
  tests/runtests.sh
  tests/srfi-18-tests.scm
  html/accessing-external-objects.html
  html/acknowledgements.html
  html/basic-mode-of-operation.html
  html/bibliography.html
  html/bugs-and-limitations.html
  html/c-interface.html
  html/callbacks.html
  html/data-representation.html
  html/declarations.html
  html/deviations-from-the-standard.html
  html/embedding.html
  html/extensions-to-the-standard.html
  html/foreign-type-specifiers.html
  html/interface-to-external-functions-and-variables.html
  html/locations.html
  html/non-standard-macros-and-special-forms.html
  html/non-standard-read-syntax.html
  html/other-support-procedures.html
  html/parameters.html
  html/pattern-matching.html
  html/supported-language.html
  html/index.html
  html/unit-eval.html
  html/unit-extras.html
  html/unit-library.html
  html/unit-lolevel.html
  html/unit-match.html
  html/unit-posix.html
  html/unit-regex.html
  html/unit-srfi-1.html
  html/unit-srfi-13.html
  html/unit-srfi-14.html
  html/unit-srfi-18.html
  html/unit-srfi-4.html
  html/unit-tcp.html
  html/unit-utils.html
  html/using-the-compiler.html
  html/using-the-interpreter.html
  html/chicken-setup.html
  html/faq.html
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
