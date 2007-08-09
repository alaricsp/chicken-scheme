# Copyright (c) 2006..2007 by Brandon J. Van Every under MIT-style license.
# See LICENSE section at end of file for full license text.
####################################################################


ADD_LIBRARY(libpcre-for-shared STATIC ${PCRE_SOURCES})
IF(MSVC)
  SET_TARGET_PROPERTIES(libpcre-for-shared PROPERTIES
    COMPILE_FLAGS "${SHARED_FLAGS}"
    OUTPUT_NAME pcre-for-shared)
ELSE(MSVC)
  SET_TARGET_PROPERTIES(libpcre-for-shared PROPERTIES
    COMPILE_FLAGS "${SHARED_FLAGS} -fPIC"
    OUTPUT_NAME pcre-for-shared)
ENDIF(MSVC)

ADD_LIBRARY(libpcre-for-static STATIC ${PCRE_SOURCES})
SET_TARGET_PROPERTIES(libpcre-for-static PROPERTIES
  COMPILE_FLAGS "${STATIC_FLAGS}"
  OUTPUT_NAME pcre-for-static)

IF(MSVC)
  SET_TARGET_PROPERTIES(libpcre-for-shared PROPERTIES PREFIX "lib")
  SET_TARGET_PROPERTIES(libpcre-for-static PROPERTIES PREFIX "lib")
ENDIF(MSVC)


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
