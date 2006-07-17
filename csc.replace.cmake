# Copyright (c) 2006 by Brandon J. Van Every under MIT-style license.
# See LICENSE section at end of file for full license text.


# CMake needs this to perform substitutions upon csc.scm.in .
# CMake's CONFIGURE_FILE doesn't have a regex replace capability,
# but this can be worked around by running a CMake script.

FILE(READ ${Chicken_SOURCE_DIR}/csc.scm.in input)

STRING(REPLACE "%pkgdatadir%" "${INSTALL_HOME}" input "${input}")
STRING(REPLACE "%bindir%" "${INSTALL_BIN_HOME}" input "${input}")
STRING(REPLACE "%includedir%" "${INSTALL_INCLUDE_HOME}" input "${input}")
STRING(REPLACE "%libdir%" "${INSTALL_LIB_HOME}" input "${input}")
STRING(REPLACE "%cc%" "${CC_PATH}" input "${input}")
STRING(REPLACE "%cxx%" "${CXX_PATH}" input "${input}")

FILE(READ ${CMAKE_CURRENT_BINARY_DIR}/cscflags CSCFLAGS)

STRING(REPLACE "%cflags%" "${CSCFLAGS}" input "${input}")
STRING(REPLACE "%morelibs%" "${MORE_LIBS}" input "${input}")
STRING(REPLACE "%morestaticlibs%" "${MORE_STATIC_LIBS}" input "${input}")

FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/csc.scm "${input}")


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
