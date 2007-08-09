# CMake 2.4.3 lacks a "CMake -E rmdir" command.
# It does have a FILE(REMOVE_RECURSE ) command, but we cannot run it from
# inside a custom rule or target.  So, we wrap it in a script, which we
# can then call from a custom command or target.
#
# INPUT:
#
#   DIR - absolute pathname of directory to be removed
#
# TYPICAL USAGE, from inside a custom target or rule:
#
#   COMMAND ${CMAKE_COMMAND}
#      -D DIR=${mydirectory}
#      -P ${Chicken_SOURCE_DIR}/rmdir.cmake

MESSAGE(STATUS "Removing directory ${DIR}")
FILE(REMOVE_RECURSE ${DIR})
