# CMake needs this to perform substitutions upon csc.scm.in .
# CMake's CONFIGURE_FILE doesn't have a regex replace capability,
# but this can be worked around by running a CMake script.

# Note that in CMake 2.4.2, "cmake -P" is rather dumb.  It won't take any input arguments,
# and it doesn't receive any variables from the caller.  "cmake -P" does know the directory
# it was called from; it is set in CMAKE_CURRENT_BINARY_DIR.  So we put a bunch of SET commands 
# in a file called vars.cmake, make sure it's in the same directory we're calling from, and
# include it here.  Bleh!

INCLUDE(${CMAKE_CURRENT_BINARY_DIR}/vars.cmake)

# Now we have some of the usual variables we're expecting to have.

FILE(READ ${Chicken_SOURCE_DIR}/csc.scm.in input)

STRING(REPLACE "%pkgdatadir%" "${INSTALL_HOME}" input "${input}")
STRING(REPLACE "%bindir%" "${INSTALL_BIN_HOME}" input "${input}")
STRING(REPLACE "%includedir%" "${INSTALL_INCLUDE_HOME}" input "${input}")
STRING(REPLACE "%libdir%" "${INSTALL_LIB_HOME}" input "${input}")
STRING(REPLACE "%cc%" "${CMAKE_C_COMPILER}" input "${input}")
STRING(REPLACE "%cxx%" "${CMAKE_CXX_COMPILER}" input "${input}")
STRING(REPLACE "%cflags%" "${EXCESSIVE_CPPFLAGS}" input "${input}")
STRING(REPLACE "%morelibs%" "${MORE_LIBS}" input "${input}")
STRING(REPLACE "%morestaticlibs%" "${MORE_STATIC_LIBS}" input "${input}")

FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/csc.scm "${input}")
