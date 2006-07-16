# Copyright (c) 2006 by Brandon J. Van Every under MIT-style license.
# See LICENSE section at end of file for full license text.


# CMake needs this to perform substitutions upon chicken-defaults.h.in .
# CMake's CONFIGURE_FILE doesn't have a regex replace capability,
# but this can be worked around by running a CMake script.

# CMake can't return values from scripts.  When a script determines
# a value, it has to write it to a file.  Then the consumer can
# read the file.  nursery.cmake produces the value of
# DEFAULT_TARGET_STACK_SIZE and writes it to the file StackSize.

IF(EXISTS ${CMAKE_CURRENT_BINARY_DIR}/StackSize)
  FILE(READ ${CMAKE_CURRENT_BINARY_DIR}/StackSize DEFAULT_TARGET_STACK_SIZE)
ELSE(EXISTS ${CMAKE_CURRENT_BINARY_DIR}/StackSize)
  MESSAGE(FATAL_ERROR "${CMAKE_CURRENT_BINARY_DIR}/StackSize should have been created.
    Aborting ${CMAKE_CURRENT_BINARY_DIR}/chicken-defaults.h substitutions.")
ENDIF(EXISTS ${CMAKE_CURRENT_BINARY_DIR}/StackSize)


FILE(READ ${Chicken_SOURCE_DIR}/chicken-defaults.h.in input)
STRING(REPLACE "%INSTALL_HOME%" "${INSTALL_HOME}" input "${input}")
STRING(REPLACE "%INSTALL_BIN_HOME%" "${INSTALL_BIN_HOME}" input "${input}")
STRING(REPLACE "%INSTALL_LIB_HOME%" "${INSTALL_LIB_HOME}" input "${input}")
STRING(REPLACE "%CMAKE_C_COMPILER%" "${CMAKE_C_COMPILER}" input "${input}")
STRING(REPLACE "%STACK_GROWS_DOWNWARD%" "${STACK_GROWS_DOWNWARD}" input "${input}")
STRING(REPLACE "%DEFAULT_TARGET_STACK_SIZE%" "${DEFAULT_TARGET_STACK_SIZE}" input "${input}")
FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/chicken-defaults.h "${input}")


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
