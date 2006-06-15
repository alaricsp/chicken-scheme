# CMake needs this to perform substitutions upon chicken-defaults.h.in .
# CMake's CONFIGURE_FILE doesn't have a regex replace capability,
# but this can be worked around by running a CMake script.

# Note that in CMake 2.4.2, "cmake -P" is rather dumb.  It won't take any input arguments,
# and it doesn't receive any variables from the caller.  "cmake -P" does know the directory
# it was called from; it is set in CMAKE_CURRENT_BINARY_DIR.  So we put a bunch of SET commands 
# in a file called vars.cmake, make sure it's in the same directory we're calling from, and
# include it here.  Bleh!

INCLUDE(${CMAKE_CURRENT_BINARY_DIR}/vars.cmake)

# Now we have some of the usual variables we're expecting to have.

FILE(READ ${Chicken_SOURCE_DIR}/chicken-defaults.h.in input)
STRING(REPLACE "%INSTALL_HOME%" "${INSTALL_HOME}" input "${input}")
STRING(REPLACE "%INSTALL_BIN_HOME%" "${INSTALL_BIN_HOME}" input "${input}")
STRING(REPLACE "%INSTALL_LIB_HOME%" "${INSTALL_LIB_HOME}" input "${input}")
STRING(REPLACE "%CMAKE_C_COMPILER%" "${CMAKE_C_COMPILER}" input "${input}")
STRING(REPLACE "%STACK_GROWS_DOWNWARD%" "${STACK_GROWS_DOWNWARD}" input "${input}")
STRING(REPLACE "%DEFAULT_TARGET_STACK_SIZE%" "${DEFAULT_TARGET_STACK_SIZE}" input "${input}")
FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/chicken-defaults.h "${input}")
