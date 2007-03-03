# Copyright (c) 2006..2007 by Brandon J. Van Every under MIT-style license.
# See LICENSE section at end of file for full license text.

####################################################################
#  TUTORIAL                                                        #
####################################################################

# This is working code for building the Chicken Scheme-to-C compiler.
# It is also a tutorial for building non-trivial CMake projects.
# Please realize that the 1st mission of this code is always to
# build Chicken properly.  If a section is under-commented or under-
# explained, that's because nobody has had time to explain it.  It
# may also be deemed an unstable part of the build design, and
# thus not worthy of being documented, as it will likely change.
#
# Chicken developers: do not remove tutorial comments from the code.
# Brevity is not the goal here.  Aside from making both Chicken and
# CMake more accessible to newbies, it is important to have good
# documentation in case Brandon J. Van Every is hit by a truck.
#
# If you notice inaccuracies, inconsistencies, or ways that the 
# explanations can be improved, please report them on the
# Chicken mailing list.  You can always access the mailing list from
# the Chicken homepage:
#
#   http://www.call-with-current-continuation.org


####################################################################
#  CMAKE SPECIFIC ISSUES                                           #
####################################################################

# As this is also a tutorial for building non-trivial CMake projects,
# some words on how to utilize the CMake community.  Here are the
# basic resources:
#
#   read the docs that ship with CMake
#
#   CMake FAQ
#   http://www.cmake.org/Wiki/CMake_FAQ
#
#   CMake Useful Variables
#   http://www.cmake.org/Wiki/CMake_Useful_Variables
#   This is really important because the CMake 2.4.5 documentation
#   does not document the variables that CMake uses, and there are
#   quite a few of them.
#
#   CMake mailing list
#   http://www.cmake.org/mailman/listinfo/cmake
#   Once you've made a reasonable effort to read the docs, the FAQ,
#   and the Useful Variables, go straight to the mailing list and
#   ask questions.  The docs are terse and omit some important
#   things, like the CMake variables.  People on the mailing list
#   are very helpful and responsive.  This is the main way that
#   you'll get the best quality of support from the CMake community.
#
#   CMake mailing list archive
#   http://public.kitware.com/pipermail/cmake/
#   If you're a newbie, there's a good chance your questions have
#   been asked already.  Although people don't mind newbie questions,
#   you can often get your answers faster if you just look them up in
#   the archive.  There's no search facility on this page, so using
#   Google Advanced Search is a good idea.  Goto
#   http://www.google.com/advanced_search and for the Domain, enter
#   the URL of the mailing list archive.
#
#   CMake wiki
#   http://www.cmake.org/Wiki/CMake
#   This has tutorials and lots more info that you may find useful.
#
#   CMake Bug Tracker
#   http://www.cmake.org/Bug/
#   Use the bug tracker both to report bugs in CMake, and to make
#   Feature Requests.  You may want to discuss the bug or feature
#   request on the mailing list before making an official entry.
#   For instance, whether something is a bug or a feature request
#   may be in the realm of debate.  :-)  When filing bugs, it is
#   best to provide a trivial CMakeLists.txt that reproduces the
#   problem.  You can get help on more complicated pieces of code,
#   but if you've already isolated the problem, quick action is
#   more likely.


####################################################################
#  CMAKE VERSION                                                   #
####################################################################

# CMake undergoes rapid development.  Open source projects such as
# Chicken tend to stress CMake's capabilities, and bugs in CMake are
# frequently found.  Kitware is very good at reacting quickly to
# bugs and adding new features.  So good, that even minor releases
# can have significant differences in features available.  Do not
# expect old CMake binaries to simply work!  If your project is
# non-trivial, you are probably exploring non-trivial CMake
# capabilities somewhere, and backwards compatibility is not assured.

CMAKE_MINIMUM_REQUIRED(VERSION 2.4.5 FATAL_ERROR)

# Bugs typically show up in the current version of CMake you're using.
# Then you make a bug report to http://www.kitware.com/Bug , and
# often the bug gets fixed in the CVS repository.  Then you want to
# verify that the bug was actually fixed.  But, you want to keep
# running your old code, because the next version of CMake hasn't
# shipped and you don't expect people to be compiling CMake from CVS.
#
# To handle this, we need to know what version of CMake we're using.
# We already issue a fatal error for any CMake less than 2.4.5.
# So this is sufficient for distinguishing whether we've got
# CMake 2.4.5, or something greater.  We would ideally like to
# make lexical comparisons on CMake version numbers, and be able
# to say things like "if it's greater than version 2.x.y, do this."
# But that's work to implement, and this is easy and sufficient for now.

SET(IS_CMAKE_245 false)
IF(CMAKE_MAJOR_VERSION EQUAL 2)
  IF(CMAKE_MINOR_VERSION EQUAL 4)
    IF(CMAKE_PATCH_VERSION EQUAL 4)
      SET(IS_CMAKE_245 true)
    ENDIF(CMAKE_PATCH_VERSION EQUAL 4)
  ENDIF(CMAKE_MINOR_VERSION EQUAL 4)
ENDIF(CMAKE_MAJOR_VERSION EQUAL 2) 


####################################################################
#  CHICKEN PROJECT                                                 #
####################################################################

# All CMake projects need a name.  The name is global to all
# CMakeLists.txt in subdirectories.  Once you define a project
# named "myproject", the variable "myproject_SOURCE_DIR" exists.
# This is the toplevel source directory of your project, i.e.
# wherever your topmost CMakeLists.txt resides.
#
# You can also specify support for languages other than C, using
# additional arguments.  Chicken only needs C, so we don't bother.

PROJECT(Chicken)

# You can specify the order in which directories are searched for
# header files.  Chicken is a multi-directory project, so we do
# need to tell subdirectories where to find our toplevel *.h files.
# Also, the user may already have Chicken installed on their system.
# To avoid any possibility of picking up wrong header files from
# the system, we search our own directories BEFORE searching
# system directories.
#
# CMake generates chicken-config.h in ${CMAKE_BINARY_DIR}
# ./configure just plops chicken-config.h in the source tree.
# Avoid collisions by searching ${CMAKE_BINARY_DIR} first.
# Also, libpcre needs chicken-config.h, so this passes the
# needed directory.

INCLUDE_DIRECTORIES(BEFORE ${CMAKE_BINARY_DIR} ${Chicken_SOURCE_DIR} ${Chicken_SOURCE_DIR}/pcre)


####################################################################
#  CHECKING HEADER FILES                                           #
####################################################################

# Typically we do not know if a header file, library, function, or
# other symbol is available on the system.  CMake has standard
# "modules" for checking their availability.  CMake "modules" are
# not available by default; if we want them, we have to explicitly
# INCLUDE them.
#
# CheckIncludeFile and CheckIncludeFiles do not do quite the same thing.
# CheckIncludeFile is the simpler case, when you only need 1 *.h file
# and it can be compiled in a straightforward manner into a trivial
# C test program.  Sometimes, however, multiple *.h files must be
# included together, in a specific sequence, and cannot be tested in
# isolation from one another.  Such is the case for WINSOCK *.h files,
# for instance.
#
# The interface for CheckIncludeFiles is more awkward.  Don't use it
# unless you really need it.  In particular, you will likely scratch
# your head about how exactly to specify a list of *.h files, so that
# CheckIncludeFiles actually takes it.

INCLUDE(CheckIncludeFile)
INCLUDE(CheckIncludeFiles)

# When you want to know if a specific function is available on your
# system, CheckFunctionExists is the module that people are likely to
# think of.  However, some "functions" are implemented as macros!
# CheckFunctionExists won't handle that.  CheckSymbolExists will.

INCLUDE(CheckSymbolExists)


####################################################################
#  DART DASHBOARD                                                  #
####################################################################

# CMake is integrated with a testing program called CTest.  It can
# create so-called "Dart Dashboards" on external websites.  These
# list the number of warnings and errors in a build.  If you write
# unit tests, the dashboard will list whether those tests pass or
# fail.  Information about your build can be dumped nightly, 
# continuously, or "experimentally" i.e. at any time you feel like.
#
# Chicken does not yet take full advantage of Dart Dashboard
# capabilities.  It is here as proof-of-concept.  This 1 line of
# code was sufficient to allow build results to be posted on the
# default Kitware public dashboard.  Over time, unit tests will be
# implemented and a canonical Dashboard of Chicken's status will
# be specified.

INCLUDE(Dart)


####################################################################
#  MACROS                                                          #
####################################################################

# CMake macros save work.  They are best employed:
#
# - to wrap up tasks that occur over and over again, i.e. as a
#   kind of function or subroutine.
#
# - to localize build rules in subdirectories, i.e. so you don't
#   have to type ${CMAKE_CURRENT_BINARY_DIR} all the time.
#
# CMake macros are also a way to obfuscate your code, make it
# unmaintainable, and render it illegible to others.  Don't get
# macro happy!  Programmers often think that abstraction and
# gratuitous refactoring is their job.  But, it's not the job of
# a Build Engineer.  Above all else, a Build Engineer needs to 
# Keep It Simple Stupid so that other people can understand,
# maintain, and spot problems with the build.
#
# This is why dumb lists of needed *.c files are often preferred to
# fancy build rules and macro wrap-ups.  It's easy for a programmer
# who doesn't know CMake to look through a simple list and spot files
# that need to be added, changed, or deleted.  They can use text
# editor search tools on such lists; they can't find weird production
# rules that do magical things to input strings.
#
# Prefer the straightforward CMake way to specify things, even if it
# is longer.   Some day, you're going to be asking someone on the CMake
# mailing list, "What's wrong with my code?"  If it's straightforward
# CMake code, you'll get snappy answers.  If you've wrapped it in 3
# bazillion layers of macros, nobody's going to be able to help you.  If
# you want someone to join your open source project as a build engineer,
# and they have to spend forever just figuring out how you
# macrotized everything, they're not going to bother.


####################################################################
#  TEXT UTILITIES                                                  #
####################################################################

# The text utilities evolved because handling lists properly in
# CMake can get confusing.  A list in CMake is a string of elements
# separated by semicolons.  So when is it a string, and when is it
# a list?  ${mylist} and "${mylist}" will give you completely different
# results; the quotes work as a sort of evaluator.
#
# Currently I am too lazy to explain all the nuances.
#
# Once you've figured out how to process your lists and strings properly,
# it is best to wrap up what you did and use it forever.  Otherwise you
# will make lotsa trivial mistakes over and over again.  I use FOREACH to
# iterate through lists because it's a conservative, safe way to do it.
# Generally when I've tried to operate directly on semicolons, I've
# gotten into trouble.
#
# Note the double evaluators such as ${${rootlist}}.  This is typical
# of macros:
#
# - rootlist is a formal name, a placeholder in the macro definition.
#
# - ${rootlist} is the name of the actual variable when the macro is
#   invoked.  i.e. If I call ADD_SUFFIX(mylistofroots .c), then
#   ${rootlist} == mylistofroots
#
# - ${${rootlist}} is the VALUE of the actual variable when the macro
#   is invoked.  This is often what you're actually interested in.
#   So if you did:
#
#     SET(mymonstermoves crush crumble chomp)
#     ADD_SUFFIX(mymonstermoves .c)
#
#   then within ADD_SUFFIX,
#   ${${rootlist}} == a list containing the elements
#
#     crush crumble chomp
#
# Note that within ADD_SUFFIX, you have single evaluations such as
# SET(${rootlist} ${outlist}).  This is because when doing a SET,
# the 1st argument is the NAME of variable you're setting, not its VALUE.
#
# Also notice that
#
#  outlist
#
# is a variable INSIDE the macro.  It has local scope; it is not a formal
# parameter.  You use it just like an ordinary variable within the macro;
# you do not ever double-evaluate it.  In our example above, at the end of
# ADD_SUFFIX, ${outlist} evaluates to
#
#  crush.c crumble.c chomp.c


MACRO(ADD_SUFFIX rootlist suffix)
  SET(outlist )
  FOREACH(root ${${rootlist}})
    LIST(APPEND outlist ${root}${suffix})
  ENDFOREACH(root)
  SET(${rootlist} ${outlist})
ENDMACRO(ADD_SUFFIX)

MACRO(ADD_PREFIX prefix rootlist)
  SET(outlist )
  FOREACH(root ${${rootlist}})
    LIST(APPEND outlist ${prefix}${root})
  ENDFOREACH(root)
  SET(${rootlist} ${outlist})
ENDMACRO(ADD_PREFIX)

# Returns an unquoted string.  Note that CMake will readily turn such
# strings back into lists, due to the duality of lists and
# semicolon-separated strings.  So be careful how you use it.

MACRO(LIST2STRING alist astring)
  FOREACH(elem ${${alist}})
   SET(${astring} "${${astring}} ${elem}")
  ENDFOREACH(elem)
ENDMACRO(LIST2STRING)

MACRO(UNESCAPE_WHITESPACE in out)
  # Remove escaped whitespace.  It causes some shells to choke.
  STRING(REPLACE "\\ " " " ${out} "${${in}}")
ENDMACRO(UNESCAPE_WHITESPACE)

MACRO(ESCAPE_BACKSLASHES in out)
  STRING(REPLACE "\\" "\\\\" ${out} "${${in}}")
ENDMACRO(ESCAPE_BACKSLASHES)

MACRO(ESCAPE_QUOTES in out)
  STRING(REPLACE "\"" "\\\"" ${out} "${${in}}")
ENDMACRO(ESCAPE_QUOTES)

# Sometimes a standard Windows path is required, even on non-native
# system.  Use this to force it.
#
# NOTE: this code doesn't clean up MSYS or Cygwin drive letter
# conventions.  It will blithely turn /e/path or /cydrive/e/path
# into \e\path or \cygdrive\e\path.  If it were "done right," it
# would change both to E:\path.  But I'm too lazy right now.
# There are some corner cases, like /cygdrive, /cygdrive/, /e,
# e:/, e:, and so forth.
#
# Consequently, this code isn't actually useful.

MACRO(UNQUOTED_WINDOWS_PATH cmake_path_in windows_path_out)  
  UNESCAPE_WHITESPACE(${cmake_path_in} ${windows_path_out})
  # Use backslashes for directory separation.
  STRING(REPLACE "/" "\\" ${windows_path_out} "${${windows_path_out}}")
ENDMACRO(UNQUOTED_WINDOWS_PATH)

MACRO(WINDOWS_PATH cmake_path_in windows_path_out)
  UNQUOTED_WINDOWS_PATH(${cmake_path_in} ${windows_path_out})
  SET(${windows_path_out} \"${${windows_path_out}}\")
ENDMACRO(WINDOWS_PATH)

# In CMake 2.4.5 under the MSYS generator, FILE(TO_NATIVE_PATH ...)
# generates Unix style pathnames.  Although this may be correct behavior
# under the MSYS shell itself, Chicken probably won't be used under the
# MSYS shell.  Generally MSYS is just there for build assistance.
# So, manually convert to Windows native pathnames.

MACRO(UNQUOTED_NATIVE_PATH cmake_path_in native_path_out)
  # MSYS should be treated like a Windows native path, generally,
  # even though it understands Unix-style paths.
  # Cygwin should generally be treated like a Unix system.
  IF(WIN32 AND NOT CYGWIN)
    UNQUOTED_WINDOWS_PATH(${cmake_path_in} ${native_path_out})
  ELSE(WIN32 AND NOT CYGWIN)
    UNESCAPE_WHITESPACE(${cmake_path_in} ${native_path_out})    
  ENDIF(WIN32 AND NOT CYGWIN)
ENDMACRO(UNQUOTED_NATIVE_PATH)

MACRO(NATIVE_PATH cmake_path_in native_path_out)
  UNQUOTED_NATIVE_PATH(${cmake_path_in} ${native_path_out})
  SET(${native_path_out} \"${${native_path_out}}\")
ENDMACRO(NATIVE_PATH)

# This is what you need for paths put into C *.h files.
# If you don't escape your backslashes, your C compiler will complain.

MACRO(NATIVE_C_PATH cmake_path_in native_path_out)
  NATIVE_PATH(${cmake_path_in} ${native_path_out})
  # Don't escape quotes.  C compilers don't want that.
  ESCAPE_BACKSLASHES(${native_path_out} ${native_path_out})
ENDMACRO(NATIVE_C_PATH)


####################################################################
#  STUB TARGETS FOR MISSING TOOLS                                  #
####################################################################

# It is common to look for tools at configuration time, and if they are
# not found, to provide make targets that give error messages.  We also
# typically want to know at configuration time that such a target has
# been created.  This keeps us from having to type the same error
# strings twice, and from making mistakes about STATUS vs. dialog boxes.

MACRO(ECHO_TARGET target quoted_message)
  MESSAGE(STATUS ${quoted_message})
  ADD_CUSTOM_TARGET(${target}
    COMMAND ${CMAKE_COMMAND} -E echo ${quoted_message})
ENDMACRO(ECHO_TARGET)


####################################################################
#  CHICKEN VERSION MACROS                                          #
####################################################################

FILE(READ ${Chicken_SOURCE_DIR}/buildversion BUILDVERSION)

# Canonize the version and build into 1 number that can be
# easily compared.  We assume the build number has no more
# than 6 digits of precision.  If Chicken ever exceeds that,
# watch out!

# Chicken has been using single digits for major stable build
# releases.  However, interim releases in Darcs have triple
# digits for the build.  This means that Chicken 2.216 would
# canonize as greater than 2.3, unless we promote 2.3 to 2.300.

MACRO(CANONIZE_VERSION version build canonized)
  IF(build LESS 100)
    IF(build LESS 10)
      # Chicken 2.4 should canonize as 2.400
      MATH(EXPR ${canonized} "${version} * 1000000 + ${build} * 100")
    ELSE(build LESS 10)
      # Chicken 2.41 should canonize as 2.410
      MATH(EXPR ${canonized} "${version} * 1000000 + ${build} * 10")
    ENDIF(build LESS 10)
  ELSE(build LESS 100)
    # Chicken 2.430 should canonize as 2.430
    MATH(EXPR ${canonized} "${version} * 1000000 + ${build}")
  ENDIF(build LESS 100)
ENDMACRO(CANONIZE_VERSION)

# "chicken -release" works on Chicken 2.3, but it is not
# documented by "chicken -help" .  It produces the
# Chicken banner, i.e. it's just like "chicken -version" .
# With Chicken 2.41 and onwards, "chicken -release" produces
# a simple BUILDVERSION.  So the problem is, if we allow
# older compilers, we don't know if we're going to get a
# banner or a BUILDVERSION.
#
# The other tools (csc, csi, etc.) have a " -release" option
# as of Chicken 2.430.  Earlier versions do not have it
# however, so if you use GET_CHICKEN_VERSION on a tool
# other than chicken.exe, be careful.

MACRO(GET_CHICKEN_VERSION absolute_exe_name canonized)
  EXEC_PROGRAM(${absolute_exe_name}
    ARGS -release
    OUTPUT_VARIABLE exe_output
    RETURN_VALUE exe_return
  )
  IF(exe_return EQUAL 0)
    # If the output has Felix L. Winkelmann's name in it,
    # then it must be a banner.
    IF("${exe_output}" MATCHES ".*(Felix L[.] Winkelmann).*")    
      # Parse the output to extract legacy version and build numbers.
      # If the output format changes significantly, problems will ensue.
      SET(LEGACY_BANNER_REGEX ".*Version ([0-9]+).*Build ([0-9]+).*")
      STRING(REGEX REPLACE ${LEGACY_BANNER_REGEX} "\\1"
        version ${exe_output})
      STRING(REGEX REPLACE ${LEGACY_BANNER_REGEX} "\\2"
        build ${exe_output})
    ELSE("${exe_output}" MATCHES ".*(Felix L[.] Winkelmann).*")
      # This is not a banner, but a plain BUILDNUMBER.
      # Parse the BUILDNUMBER into version and build.
      SET(VERSION_BUILD_REGEX "^([0-9]+)[.]([0-9]+).*$")
      STRING(REGEX REPLACE ${VERSION_BUILD_REGEX} "\\1"
        version ${exe_output})
      STRING(REGEX REPLACE ${VERSION_BUILD_REGEX} "\\2"
        build ${exe_output})
    ENDIF("${exe_output}" MATCHES ".*(Felix L[.] Winkelmann).*")
    CANONIZE_VERSION(${version} ${build} ${canonized})

  ELSE(exe_return EQUAL 0)
    MESSAGE(FATAL_ERROR "Bogus result when invoking ${absolute_exe_name} -release
      Dump of output:
      ${exe_output}"
    )
  ENDIF(exe_return EQUAL 0)
ENDMACRO(GET_CHICKEN_VERSION)


####################################################################
#  CHICKEN VERSION REQUIREMENTS                                    #
####################################################################

# A previously installed Chicken can be used to build a new Chicken.
# The previously installed Chicken must have minimum Version and Build numbers.

SET(MINIMUM_CHICKEN_VERSION 2)
SET(MINIMUM_CHICKEN_BUILD 3)

CANONIZE_VERSION(${MINIMUM_CHICKEN_VERSION} ${MINIMUM_CHICKEN_BUILD}
  MINIMUM_CANONICAL_CHICKEN
)

# This is the minimum build necessary to generate .exports.
# The bootstrap compiler doesn't need to generate .exports.
# Our final compiler will ultimately generate .exports.

SET(EXPORTS_CHICKEN_VERSION 2)
SET(EXPORTS_CHICKEN_BUILD 310)

CANONIZE_VERSION(${EXPORTS_CHICKEN_VERSION} ${EXPORTS_CHICKEN_BUILD}
  MINIMUM_CANONICAL_EXPORTS
)

##############################################################
#                                                            #
#  FIND CHICKEN                                              #
#                                                            #
#  Output:  EXTANT_CHICKEN                                   #
#                                                            #
#  Absolute paths of chicken executable and interpreter.     #
#  If none exists, evaluates to -NOTFOUND, which is          #
#  equivalent to FALSE in conditional tests.  Thus they are  #
#  both the path and the boolean about whether the path is   #
#  usable.  Isn't CMake clever?                              #
#                                                            #
##############################################################


# Is CHICKEN_HOME set in the environment?
# CHICKEN_HOME need not be set, but it could be a configuration error.
# Warn the user if it isn't set, and state the exact error.

STRING(COMPARE NOTEQUAL "$ENV{CHICKEN_HOME}" "" HAVE_CHICKEN_HOME)
IF(HAVE_CHICKEN_HOME)
  MESSAGE("Found CHICKEN_HOME=$ENV{CHICKEN_HOME} .
    This is only used by legacy MSVC compilers.")
  # Too lazy to test if it's a legacy MSVC Chicken
ENDIF(HAVE_CHICKEN_HOME)

# Do we have a Chicken installed already that we can use for the build?
# Did the user specify where to find Chicken via CMakeSetup or CCMake?

IF(NOT EXTANT_CHICKEN)
  FIND_PROGRAM(EXTANT_CHICKEN
    NAMES chicken-static chicken
    PATHS $ENV{CHICKEN_HOME} $ENV{CHICKEN_HOME}/bin
  )
  IF(EXTANT_CHICKEN)
    MESSAGE(STATUS "Found ${EXTANT_CHICKEN}")
  ELSE(EXTANT_CHICKEN)
    MESSAGE("Can't find Chicken, which is required to generate bootstrap .c files.")
  ENDIF(EXTANT_CHICKEN)
ENDIF(NOT EXTANT_CHICKEN)


##############################################################
#                                                            #
#  VALIDATE VERSION REQIREMENTS                              #
#                                                            #
#  Output:  VALID_CHICKEN                                    #
#                                                            #
##############################################################

SET(VALID_CHICKEN "-NOTFOUND")
IF(EXTANT_CHICKEN)
  GET_CHICKEN_VERSION(${EXTANT_CHICKEN} chicken_version)
  IF(chicken_version LESS MINIMUM_CANONICAL_CHICKEN)
    MESSAGE("Chicken ${MINIMUM_CANONICAL_CHICKEN} required to generate bootstrap .c files.
      Available Chicken is only ${chicken_version}."
    )
  ELSE(chicken_version LESS MINIMUM_CANONICAL_CHICKEN)
    MESSAGE(STATUS "Chicken ${MINIMUM_CANONICAL_CHICKEN} required; using ${chicken_version}.")
    SET(VALID_CHICKEN ${EXTANT_CHICKEN})
  ENDIF(chicken_version LESS MINIMUM_CANONICAL_CHICKEN)
ENDIF(EXTANT_CHICKEN)

# it may or may not be valid, but this is what we have to work with.
SET(CURRENT_CHICKEN ${VALID_CHICKEN})


####################################################################
#  BOOT_CFILES                                                     #
####################################################################

# BOOT_CFILES is a list of all .c files necessary to build a bootstrap
# Chicken compiler, without a valid Chicken compiler being available.
# A Chicken build tree can come either with or without these files.
# Typically, when downloaded as a tarball distribution, these files
# will be present.  But when pulled straight from Darcs, these files
# will be absent.  If they are present, then we don't use a Chicken
# compiler to do the build, we just compile the .c sources.
#
# It would be desireable to implement a uniform MANIFEST.txt file so
# that we don't have to edit these both here and in configure.in .

SET(BOOT_CFILES
  batch-driver.c
  c-backend.c
  c-platform.c
  chicken.c
  chicken-profile.c
  chicken-setup.c
  compiler.c
  csc.c
  csi.c
  eval.c
  eval.exports
  extras.c
  extras.exports
  library.c
  library.exports
  lolevel.c
  lolevel.exports
  match.c
  optimizer.c
  regex.c
  regex.exports
  posixunix.c
  posix.exports
  posixwin.c
  profiler.c
  scheduler.c
  scheduler.exports
  srfi-1.c
  srfi-1.exports
  srfi-4.c
  srfi-4.exports
  srfi-13.c
  srfi-13.exports
  srfi-14.c
  srfi-14.exports
  srfi-18.c
  srfi-18.exports
  stub.c
  support.c
  tcp.c
  tcp.exports
  ueval.c
  uextras.c
  ulibrary.c
  ulolevel.c
  uregex.c
  uposixunix.c
  uposixwin.c
  usrfi-1.c
  usrfi-4.c
  usrfi-13.c
  usrfi-14.c
  usrfi-18.c
  utcp.c
  utils.c
  utils.exports
  uutils.c
)

# Check if all BOOT_CFILES are present in the build tree.
SET(HAVE_BOOT_CFILES TRUE)
IF(NOT VALID_CHICKEN)
  MESSAGE("You have no valid Chicken available.
    If you have all the boot/cfiles/*.c files that a bootstrap needs,
    then I can still build Chicken for you."
  )
ENDIF(NOT VALID_CHICKEN)
FOREACH(f ${BOOT_CFILES})
  IF(NOT EXISTS ${Chicken_SOURCE_DIR}/boot/cfiles/${f})
    SET(HAVE_BOOT_CFILES FALSE)
    IF(VALID_CHICKEN)
      MESSAGE(STATUS "Missing ${Chicken_SOURCE_DIR}/boot/cfiles/${f}
        Not an error, can generate if needed.")
    ELSE(VALID_CHICKEN)
      MESSAGE(FATAL_ERROR "${Chicken_SOURCE_DIR}/boot/cfiles/${f} is missing.
        Giving up, terminating build."
      )
    ENDIF(VALID_CHICKEN)
  ENDIF(NOT EXISTS ${Chicken_SOURCE_DIR}/boot/cfiles/${f})
ENDFOREACH(f)
IF(HAVE_BOOT_CFILES)
  IF(VALID_CHICKEN)
    # Just give status
    MESSAGE(STATUS "All boot/cfiles/*.c files are present.")
  ELSE(VALID_CHICKEN)
    # Pester the user with a dialog box that everything will be ok.
    MESSAGE("All boot/cfiles/*.c files are present.")
  ENDIF(VALID_CHICKEN)
ENDIF(HAVE_BOOT_CFILES)


####################################################################
#  .SCM TO .C BUILD RULES                                          #
####################################################################


# The generated .c files are valid on all platforms, not just the
# ones they are built on.  In particular, this means the POSIX
# units (posixunix.c posixwin.c) will all be built.  By building all
# .c files, new platforms can be targeted.

# .o files are of
# course platform dependent.  If there are no matching .h files
# or link libraries available on a given platform, then the .o
# files won't be built.  That's the platform dependent part of
# a Chicken build.

# SAFE and UNSAFE builds need different flags.

SET(CHICKEN_FLAGS -quiet -no-trace -optimize-level 2 -include-path ${Chicken_SOURCE_DIR})
SET(CHICKEN_UFLAGS ${CHICKEN_FLAGS} -no-lambda-info -unsafe -feature unsafe)

# We're stuck with using DEPENDS to
# specify our dependencies, there's no other way.  
# In CMake 2.4.2, ADD_FILE_DEPENDENCIES will *not* work in
# conjunction with ADD_CUSTOM_COMMAND.  It is meant to be
# used for .o file dependencies on .c files, not for
# source-file-to-source-file dependencies.  

MACRO(SIMPLE_SCM_TO_C root)
  SET(deproots ${ARGN})
  ADD_PREFIX(${Chicken_SOURCE_DIR}/ deproots)
  ADD_SUFFIX(deproots .scm)
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${root}.c
    MAIN_DEPENDENCY ${Chicken_SOURCE_DIR}/${root}.scm
    DEPENDS ${deproots}
    COMMAND ${CURRENT_CHICKEN} ${Chicken_SOURCE_DIR}/${root}.scm -output-file ${CMAKE_CURRENT_BINARY_DIR}/${root}.c ${CHICKEN_FLAGS}
    COMMENT "Compiling ${Chicken_SOURCE_DIR}/${root}.scm to ${CMAKE_CURRENT_BINARY_DIR}/${root}.c"
  )
ENDMACRO(SIMPLE_SCM_TO_C)

# Library units must be compiled with -explicit-use

MACRO(LIBRARY_SCM_TO_C root)
  SET(deproots ${ARGN})
  ADD_PREFIX(${Chicken_SOURCE_DIR}/ deproots)
  ADD_SUFFIX(deproots .scm)
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${root}.c
    MAIN_DEPENDENCY ${Chicken_SOURCE_DIR}/${root}.scm
    DEPENDS ${deproots}
    COMMAND ${CURRENT_CHICKEN} ${Chicken_SOURCE_DIR}/${root}.scm -output-file ${CMAKE_CURRENT_BINARY_DIR}/${root}.c -explicit-use ${CHICKEN_FLAGS}
    COMMENT "Compiling ${Chicken_SOURCE_DIR}/${root}.scm to ${CMAKE_CURRENT_BINARY_DIR}/${root}.c"
  )
ENDMACRO(LIBRARY_SCM_TO_C)

# Some library units have .exports, others don't.

MACRO(LIBRARY_SCM_TO_C_EXPORTS root)
  SET(deproots ${ARGN})
  ADD_PREFIX(${Chicken_SOURCE_DIR}/ deproots)
  ADD_SUFFIX(deproots .scm)
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${root}.c ${CMAKE_CURRENT_BINARY_DIR}/${root}.exports
    MAIN_DEPENDENCY ${Chicken_SOURCE_DIR}/${root}.scm
    DEPENDS ${deproots}
    COMMAND ${CURRENT_CHICKEN} ${Chicken_SOURCE_DIR}/${root}.scm -output-file ${CMAKE_CURRENT_BINARY_DIR}/${root}.c -explicit-use ${CHICKEN_FLAGS}
    COMMENT "Compiling ${Chicken_SOURCE_DIR}/${root}.scm to ${CMAKE_CURRENT_BINARY_DIR}/${root}.c Exporting ${CMAKE_CURRENT_BINARY_DIR}/${root}.exports"
  )
ENDMACRO(LIBRARY_SCM_TO_C_EXPORTS)

# Unsafe library units get a 'u' prefix and must be compiled with -explicit-use
# NOTE: UNSAFE libraries do not emit .exports as they don't need them.

MACRO(UNSAFE_LIBRARY_SCM_TO_C saferoot)
  SET(deproots ${ARGN})
  ADD_PREFIX(${Chicken_SOURCE_DIR}/ deproots)
  ADD_SUFFIX(deproots .scm)
  ADD_CUSTOM_COMMAND(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/u${saferoot}.c
    MAIN_DEPENDENCY ${Chicken_SOURCE_DIR}/${saferoot}.scm
    DEPENDS ${deproots}
    COMMAND ${CURRENT_CHICKEN} ${Chicken_SOURCE_DIR}/${saferoot}.scm -output-file ${CMAKE_CURRENT_BINARY_DIR}/u${saferoot}.c -explicit-use ${CHICKEN_UFLAGS}
    COMMENT "Compiling ${Chicken_SOURCE_DIR}/${saferoot}.scm to ${CMAKE_CURRENT_BINARY_DIR}/u${saferoot}.c"
  )
ENDMACRO(UNSAFE_LIBRARY_SCM_TO_C)

####################################################################
#  USER OPTIONS                                                    #
####################################################################

# These appear as checkbox items in the CMakeSetup and CCMake GUIs.

OPTION(ENABLE_PROCEDURE_TABLES "enable support for serialization of procedures" TRUE)
IF(ENABLE_PROCEDURE_TABLES)
  ADD_DEFINITIONS(-DC_ENABLE_PTABLES)
ENDIF(ENABLE_PROCEDURE_TABLES)

OPTION(ENABLE_SYMBOL_GC "enable support for GC of unused symbols" FALSE)
IF(ENABLE_SYMBOL_GC)
  ADD_DEFINITIONS(-DC_COLLECT_ALL_SYMBOLS)
ENDIF(ENABLE_SYMBOL_GC)

OPTION(ENABLE_EXTRA_SYMBOL_SLOT "enable extra data slot for symbols" FALSE)
IF(ENABLE_EXTRA_SYMBOL_SLOT)
  ADD_DEFINITIONS(-DC_EXTRA_SYMBOL_SLOT)
ENDIF(ENABLE_EXTRA_SYMBOL_SLOT)

OPTION(DISABLE_APPLY_HOOK "disable apply hook" FALSE)
IF(DISABLE_APPLY_HOOK)
  ADD_DEFINITIONS(-DC_NO_APPLY_HOOK)
ENDIF(DISABLE_APPLY_HOOK)

OPTION(ENABLE_DEBUG_BUILD "select debugging options for C compiler" FALSE)

# Should this be on by default?

IF(MSVC)
  OPTION(USE_DYNAMIC_C_RUNTIME "use dynamically linked multithreaded C runtime" FALSE)
  IF(USE_DYNAMIC_C_RUNTIME)
    ADD_DEFINITIONS(/MD)
  ENDIF(USE_DYNAMIC_C_RUNTIME)
ELSE(MSVC)
  OPTION(WITHOUT_LIBFFI "disable use if libffi, even if installed" FALSE)
ENDIF(MSVC)


####################################################################
#  DEDUCE OS SPECIFIC BUILD OPTIONS                                #
####################################################################


# Be careful about whether NOT CYGWIN or NOT CMAKE_COMPILER_IS_GNUCC
# is really intended.  GCC has many similarities of operation across
# Linux, Cygwin, and MinGW, but there are also differences.

# ****** TODO ******
# What about OS, compiler, and CPU specific optimizations?
# Make VC++ have Release or MinSizeRel as default build?
# I want to see benchmarks integrated as a test suite before bothering.
# Need proof regarding what's worth it, and also proof that the
# benchmarks measure something useful.

#All our various optional libraries go in here.
SET(MORE_LIBS )
SET(MORE_STATIC_LIBS )

# Any CMake-specific defines are in chicken-config.h
ADD_DEFINITIONS(-DHAVE_CHICKEN_CONFIG_H)

ADD_DEFINITIONS(-DC_NO_PIC_NO_DLL)

# Compiler optimizations.  Beware that these must be
# passed to C_INSTALL_CFLAGS somehow.

IF(CMAKE_COMPILER_IS_GNUCC)
  # Why is -Os believed to produce better code?  Why not -O2?
  # Really should implement some benchmarking instead of just guessing.
  IF(ENABLE_DEBUG_BUILD)
    ADD_DEFINITIONS(-g -Wall -Wno-unused -Wno-uninitialized)
  ELSE(ENABLE_DEBUG_BUILD)
    ADD_DEFINITIONS(-Os -fno-strict-aliasing -Wall -Wno-unused -Wno-uninitialized)
  ENDIF(ENABLE_DEBUG_BUILD)
  # -fomit-frame-pointer causes chicken-boot to crash on MinGW 3.4.5
  # Unknown whether it affects other MinGW versions.
  # This points out that testing builds with different optimization flags
  # could get complicated.
  IF(NOT MINGW AND NOT ENABLE_DEBUG_BUILD)
    ADD_DEFINITIONS(-fomit-frame-pointer)
  ENDIF(NOT MINGW AND NOT ENABLE_DEBUG_BUILD)
ENDIF(CMAKE_COMPILER_IS_GNUCC)

# Chicken uses the INSTALL_* paths in various tools to generate shell
# commands; for instance, under a Windows command prompt.  So they need
# to be native paths.

# First specify the paths relative to each other in CMake style.
SET(SHARE_HOME   ${CMAKE_INSTALL_PREFIX}/share/chicken)
SET(BIN_HOME     ${CMAKE_INSTALL_PREFIX}/bin)
SET(INCLUDE_HOME ${CMAKE_INSTALL_PREFIX}/include)
SET(LIB_HOME     ${CMAKE_INSTALL_PREFIX}/lib)
SET(STATIC_LIB_HOME     ${CMAKE_INSTALL_PREFIX}/lib)
SET(INFO_HOME    ${CMAKE_INSTALL_PREFIX}/info)
SET(MAN_HOME     ${CMAKE_INSTALL_PREFIX}/man/man1)
SET(EGG_HOME     ${LIB_HOME}/chicken/1)
SET(DOC_HOME     ${SHARE_HOME}/doc)

# Then make them native after the path logic is done.
NATIVE_C_PATH(SHARE_HOME         C_INSTALL_SHARE_HOME)
NATIVE_C_PATH(BIN_HOME           C_INSTALL_BIN_HOME)
NATIVE_C_PATH(INCLUDE_HOME       C_INSTALL_INCLUDE_HOME)
NATIVE_C_PATH(LIB_HOME           C_INSTALL_LIB_HOME)
NATIVE_C_PATH(STATIC_LIB_HOME    C_INSTALL_STATIC_LIB_HOME)
NATIVE_C_PATH(EGG_HOME           C_INSTALL_EGG_HOME)

# Do we actually want absolute pathnames for the compilers,
# or just simple definitions like "gcc" or "cl" ?
GET_FILENAME_COMPONENT(THE_CC ${CMAKE_C_COMPILER} NAME_WE)
GET_FILENAME_COMPONENT(THE_CXX ${CMAKE_CXX_COMPILER} NAME_WE)
SET(C_INSTALL_CC \"${THE_CC}\")
SET(C_INSTALL_CXX \"${THE_CXX}\")
SET(C_TARGET_CC ${C_INSTALL_CC})
SET(C_TARGET_CXX ${C_INSTALL_CXX})

# Historically, MSVC was given a stack size of 300000.
# Not convinced this is necessary, but noted in case of problems.
SET(C_DEFAULT_TARGET_STACK_SIZE \(128*1024\))

CHECK_INCLUDE_FILE(alloca.h HAVE_ALLOCA_H)
# Is passing multiple .h files going to be troublesome like for WS2TCPIP below?
CHECK_SYMBOL_EXISTS(alloca "alloca.h malloc.h" HAVE_ALLOCA)
CHECK_INCLUDE_FILE(crt_externs.h HAVE_CRT_EXTERNS_H)
CHECK_INCLUDE_FILE(direct.h HAVE_DIRECT_H)
CHECK_SYMBOL_EXISTS(gcvt stdlib.h HAVE_GCVT)
CHECK_SYMBOL_EXISTS(memmove string.h HAVE_MEMMOVE)
CHECK_SYMBOL_EXISTS(strerror string.h HAVE_STRING)
CHECK_INCLUDE_FILE(grp.h HAVE_GRP_H)
CHECK_INCLUDE_FILE(stdint.h HAVE_STDINT_H)
CHECK_INCLUDE_FILE(sysexits.h HAVE_SYSEXITS_H)

CHECK_INCLUDE_FILE(dlfcn.h HAVE_DLFCN_H)
IF(HAVE_DLFCN_H)
  # The presence of dlfcn.h does NOT mean that -ldl is used.
  # For instance, Cygwin has dlfcn.h and does not use -ldl.
  FIND_LIBRARY(DL_LIBRARY dl)
  IF(DL_LIBRARY)
    SET(MORE_LIBS ${MORE_LIBS} dl)
    SET(MORE_STATIC_LIBS ${MORE_STATIC_LIBS} dl)
  ENDIF(DL_LIBRARY)
ENDIF(HAVE_DLFCN_H)

CHECK_INCLUDE_FILE(dl.h HAVE_DL_H)
IF(HAVE_DL_H)
  SET(MORE_LIBS ${MORE_LIBS} ldl)
  SET(MORE_STATIC_LIBS ${MORE_STATIC_LIBS} ldl)
ENDIF(HAVE_DL_H)

CHECK_INCLUDE_FILE(ffi.h HAVE_FFI_H)

IF(NOT MSVC)
  IF(WITHOUT_LIBFFI)
    SET(HAVE_FFI_H FALSE)
  ELSE(WITHOUT_LIBFFI)
    IF(HAVE_FFI_H)
      SET(MORE_LIBS ${MORE_LIBS} ffi)
      SET(MORE_STATIC_LIBS ${MORE_STATIC_LIBS} ffi)
    ENDIF(HAVE_FFI_H)
  ENDIF(WITHOUT_LIBFFI)
ENDIF(NOT MSVC)
  
# check for windows.h
CHECK_INCLUDE_FILE(windows.h HAVE_WINDOWS_H)
IF(HAVE_WINDOWS_H)

  # NOTE: Don't assume we're on Windows.  We could be cross-compiling.
  CHECK_SYMBOL_EXISTS(LoadLibrary windows.h HAVE_LOADLIBRARY)
  CHECK_SYMBOL_EXISTS(GetProcAddress windows.h HAVE_GETPROCADDRESS)

  CHECK_INCLUDE_FILE(winsock2.h HAVE_WINSOCK2_H)
  IF (NOT HAVE_WINSOCK2_H)
    MESSAGE("You are missing winsock2.h.
      Correct this if you want full network functionality.")
  ENDIF(NOT HAVE_WINSOCK2_H)
  # ws2tcpip.h needs winsock2.h for various definitions.
  # Found it really difficult to pass the list of files as a
  # 1st argument.  This is what worked.
  SET(WS2FILES winsock2.h ws2tcpip.h)
  CHECK_INCLUDE_FILES("${WS2FILES}" HAVE_WS2TCPIP_H)
  IF (NOT HAVE_WS2TCPIP_H)
    MESSAGE("You are missing ws2tcpip.h.
      Correct this if you want full network functionality.")
  ENDIF(NOT HAVE_WS2TCPIP_H)

  # Seems like the ws2_32 stuff should be up here, not down below.

ENDIF(HAVE_WINDOWS_H)

# We can create POSIX .c files for all platforms when
# generating .scm --> c. code.  However, we can only
# generate the .o file appropriate to this platform.
# If no POSIX is available, don't generate any POSIX_UNIT
# at all.

# Note that in practice, right now we're not generating
# all the POSIX files we could.  Will need to address
# this when preparing a .c snapshot for other platforms.

SET(POSIX_UNIT )
IF(WIN32 AND NOT CYGWIN)
  SET(POSIX_UNIT posixwin)
  SET(MORE_LIBS ${MORE_LIBS} ws2_32)
  SET(MORE_STATIC_LIBS ${MORE_STATIC_LIBS} ws2_32)
ENDIF(WIN32 AND NOT CYGWIN)
IF(UNIX)
  SET(POSIX_UNIT posixunix)
ENDIF(UNIX)

# Shared and static libraries are built with different flags.
# In CMake, SET(x a b c) produces x="a;b;c"
# SET(x "a b c") produces x="a b c", which is what we want.

SET(SHARED_FLAGS "-DPIC")

IF(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
  SET(SHARED_FLAGS "${SHARED_FLAGS} -fno-common -no-cpp-precomp")
ENDIF(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")

# Unix platforms can get into trouble if they don't have -lm.
# Windows platforms generally don't need it.
# Uncertain if this is strictly necessary, but we're paranoid.
IF(UNIX)
  SET(MORE_LIBS ${MORE_LIBS} m)
  SET(MORE_STATIC_LIBS ${MORE_STATIC_LIBS} m)
ENDIF(UNIX)

# Solaris doesn't have socket calls in its libc
IF(CMAKE_SYSTEM_NAME STREQUAL "SunOS")
  SET(MORE_LIBS ${MORE_LIBS} socket rt)
  SET(MORE_STATIC_LIBS ${MORE_STATIC_LIBS} socket rt)
ENDIF(CMAKE_SYSTEM_NAME STREQUAL "SunOS")

# Tack on -l for chicken-defaults.h
SET(L_MORE_LIBS ${MORE_LIBS})
SET(L_MORE_STATIC_LIBS ${MORE_STATIC_LIBS})
ADD_PREFIX(-l L_MORE_LIBS)
ADD_PREFIX(-l L_MORE_STATIC_LIBS)
LIST2STRING(L_MORE_LIBS L_MORE_LIBS_STRING)
LIST2STRING(L_MORE_STATIC_LIBS L_MORE_STATIC_LIBS_STRING)
SET(C_INSTALL_MORE_LIBS \"${L_MORE_LIBS_STRING}\")
SET(C_INSTALL_MORE_STATIC_LIBS \"${L_MORE_STATIC_LIBS_STRING}\")


####################################################################
#  DEFINE C_STACK_GROWS_DOWNWARD                                   #
####################################################################

# Sometimes you want to deduce a system property by compiling a piece
# of code, running it, and seeing what the output does.  For instance,
# you can detect the Endianness of a platform that way.  In fact,
# running a code snippet is the only way to get it in general.  Some
# definitions might be available in a *.h file somewhere, but that's
# a platform specific solution.  Endianness is not actually a problem
# in CMake as the TestBigEndian macro is available, but for other
# odd things, you'll need to write your own tests.
#
# Such as the direction of stack growth!  The easiest approach is to
# write a code snippet that returns a boolean value, 1 or 0.  Give it
# a logical name like DoesWhatIWant.c.  Return 1 if it does what you
# want, 0 if it doesn't.  Do a TRY_RUN, and receive this value through
# the RUN_RESULT_VAR.  Use the RUN_RESULT_VAR later in your build,
# to control whatever you need to control.
#
# When you want your TRY_RUN to just return a simple numerical result
# code, you don't want to bother with OUTPUT_VARIABLE.  That's stdout;
# you're going to get a lot of junk from the compiler reportage and
# have to parse it.  That's a PITA and typically not what you want.
#
# The COMPILE_RESULT_VAR is just for diagnostics.  Like, does your
# code even compile?  Is your code fine, but your shell is toast?
# You use COMPILE_RESULT_VAR for bulletproofing, not for the results
# you're actually interested in.

TRY_RUN(C_STACK_GROWS_DOWNWARD
  STACK_TEST_COMPILED
  ${CMAKE_CURRENT_BINARY_DIR}
  ${Chicken_SOURCE_DIR}/StackGrowsDownward.c
)
IF(STACK_TEST_COMPILED)
  IF(C_STACK_GROWS_DOWNWARD STREQUAL "FAILED_TO_RUN")
    MESSAGE(SEND_ERROR "Compiled stack growth test, but unable to run it.  Using default.")
    SET(C_STACK_GROWS_DOWNWARD -1)
  ENDIF(C_STACK_GROWS_DOWNWARD STREQUAL "FAILED_TO_RUN")
ELSE(STACK_TEST_COMPILED)
  MESSAGE(SEND_ERROR "Unable to compile stack growth test.  Using default.")
  SET(C_STACK_GROWS_DOWNWARD -1)
ENDIF(STACK_TEST_COMPILED)
MESSAGE(STATUS "C_STACK_GROWS_DOWNWARD=${C_STACK_GROWS_DOWNWARD}")


####################################################################
#  CHICKEN_SPEC                                                    #
####################################################################

# I don't really know what chicken.spec is for.
CONFIGURE_FILE(${Chicken_SOURCE_DIR}/chicken.spec.in
  ${CMAKE_CURRENT_BINARY_DIR}/chicken.spec)


####################################################################
#  CHICKEN_DEFAULTS_H                                              #
####################################################################

# get C flags as late as possible so they're all available
GET_DIRECTORY_PROPERTY(RAW_CFLAGS
  DIRECTORY ${Chicken_SOURCE_DIR}
  DEFINITIONS
)

SET(C_INSTALL_CFLAGS \"${RAW_CFLAGS}\")
SET(C_TARGET_CFLAGS ${C_INSTALL_CFLAGS})
SET(C_TARGET_LIB_HOME ${C_INSTALL_LIB_HOME})
SET(C_TARGET_SHARE_HOME ${C_INSTALL_SHARE_HOME})
SET(C_TARGET_INCLUDE_HOME ${C_INSTALL_INCLUDE_HOME})
SET(C_TARGET_STATIC_LIB_HOME ${C_INSTALL_STATIC_HOME})
SET(C_TARGET_MORE_LIBS ${C_INSTALL_MORE_LIBS})
SET(C_TARGET_MORE_STATIC_LIBS ${C_INSTALL_MORE_LIBS})
SET(C_CROSS_CHICKEN 0)

####################################################################
#  CHICKEN_CONFIG_H                                                #
####################################################################

CONFIGURE_FILE(${Chicken_SOURCE_DIR}/chicken-config-cmake.h.in
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-config.h)


# ./configure does the substitutions at make time, not
# configuration time.  Will this cause problems if
# people are using both builds?
CONFIGURE_FILE(${Chicken_SOURCE_DIR}/chicken-defaults.h.in
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-defaults.h)


####################################################################
#  LISTS OF .C SOURCES                                             #
####################################################################


# Library files that are not generated at build time.
# Either they are read-only sources,
# or they are hardwired at configuration time.

MACRO(GET_LIB_CSOURCE out)
  SET(${out}
    ${CMAKE_CURRENT_BINARY_DIR}/chicken-config.h
    ${Chicken_SOURCE_DIR}/chicken.h
    ${CMAKE_CURRENT_BINARY_DIR}/chicken-defaults.h
    ${Chicken_SOURCE_DIR}/runtime.c
  )
ENDMACRO(GET_LIB_CSOURCE)

# Library files that are always generated the same way and named the
# same thing regardless of whether the library is SAFE or UNSAFE.

SET(LIB_COMMON_CGEN
  match
  profiler
  scheduler
  stub
) 
MACRO(GET_LIB_COMMON_CGEN out)
  SET(${out} ${LIB_COMMON_CGEN})
  ADD_SUFFIX(${out} .c)
  ADD_PREFIX(${CMAKE_CURRENT_BINARY_DIR}/ ${out})
ENDMACRO(GET_LIB_COMMON_CGEN)

# Library files that are generated and named differently depending on
# whether the library is SAFE or UNSAFE.  These are the SAFE
# names.

SET(LIB_SAFE_CGEN
  eval
  extras
  library
  lolevel
  ${POSIX_UNIT}
  regex
  srfi-1
  srfi-4
  srfi-13
  srfi-14
  srfi-18
  tcp
  utils
)
MACRO(GET_LIB_SAFE_CGEN out)
  SET(${out} ${LIB_SAFE_CGEN})
  ADD_SUFFIX(${out} .c)
  ADD_PREFIX(${CMAKE_CURRENT_BINARY_DIR}/ ${out})
ENDMACRO(GET_LIB_SAFE_CGEN)

# The UNSAFE library names have a 'u' prefix.

SET(LIB_UNSAFE_CGEN
  ${LIB_SAFE_CGEN}
)
ADD_PREFIX(u LIB_UNSAFE_CGEN)

MACRO(GET_LIB_UNSAFE_CGEN out)
  SET(${out} ${LIB_UNSAFE_CGEN})
  ADD_SUFFIX(${out} .c)
  ADD_PREFIX(${CMAKE_CURRENT_BINARY_DIR}/ ${out})
ENDMACRO(GET_LIB_UNSAFE_CGEN)

# libchicken files that emit .exports
# Wouldn't it be clever to automagically deduce these?
# Note: it's the SAFE list + 'scheduler'.

SET(LIB_EXPORTS
  eval
  extras
  library
  lolevel
  posix
  regex
  scheduler
  srfi-1
  srfi-4
  srfi-13
  srfi-14
  srfi-18
  tcp
  utils
)
MACRO(GET_LIB_EXPORTS out)
  SET(${out} ${LIB_EXPORTS})
  ADD_SUFFIX(${out} .exports)
  ADD_PREFIX(${CMAKE_CURRENT_BINARY_DIR}/ ${out})
ENDMACRO(GET_LIB_EXPORTS)

# Localize the .c sources to a particular build subdirectory.
# This is needed because we're doing a multiple stage bootstrap.
# The 1st crop of generated .c files is not going to be the
# same as the 2nd crop.

MACRO(GET_LIBCHICKEN_C_SOURCES out)
  GET_LIB_CSOURCE(csource)
  GET_LIB_COMMON_CGEN(common)
  GET_LIB_SAFE_CGEN(safe)
  SET(${out} ${csource} ${common} ${safe})
ENDMACRO(GET_LIBCHICKEN_C_SOURCES)

MACRO(GET_LIBUCHICKEN_C_SOURCES out)
  GET_LIB_CSOURCE(csource)
  GET_LIB_COMMON_CGEN(common)
  GET_LIB_UNSAFE_CGEN(unsafe)
  SET(${out} ${csource} ${common} ${unsafe})
ENDMACRO(GET_LIBUCHICKEN_C_SOURCES)

# libchicken-gui just compiles libchicken with -DC_WINDOWS_GUI.
# There are no .c files specific to it.


# Files generated for Chicken.  No SAFE or UNSAFE distinctions.

SET(CHICKEN_CGEN
  batch-driver
  c-backend
  c-platform
  chicken
  compiler
  optimizer
  support
)
MACRO(GET_CHICKEN_CGEN out)
  SET(${out} ${CHICKEN_CGEN})
  ADD_SUFFIX(${out} .c)
  ADD_PREFIX(${CMAKE_CURRENT_BINARY_DIR}/ ${out})
ENDMACRO(GET_CHICKEN_CGEN)

MACRO(GET_CHICKEN_C_SOURCES out)
  GET_CHICKEN_CGEN(cgen)
  SET(${out}
    ${CMAKE_CURRENT_BINARY_DIR}/chicken-config.h
    ${Chicken_SOURCE_DIR}/chicken.h
    ${CMAKE_CURRENT_BINARY_DIR}/chicken-defaults.h
    ${Chicken_SOURCE_DIR}/chicken.rc
    ${cgen}
  )
ENDMACRO(GET_CHICKEN_C_SOURCES)


####################################################################
#  PCRE LIBRARY                                                     #
####################################################################

# The PCRE code is used several times by the various Chicken libraries.  We statically compile it for
# convenience, so that the user doesn't have to type -lpcre when linking.  Also, we don't want the
# user linking against a system PCRE library, as we may have our own bugfixes and customizations.
#
# PCRE is compiled statically for both our shared and static libraries.   pcre.h needs these definitions
# so that dll imports are correct, and for UTF8 and UCP support.

ADD_DEFINITIONS(-DPCRE_STATIC -DSUPPORT_UTF8 -DSUPPORT_UCP)

SET(PCRE_HEADER ${Chicken_SOURCE_DIR}/pcre/pcre.h)
SET(PCRE_ROOTS
  chartables
  pcre_compile
  pcre_config
  pcre_dfa_exec
  pcre_exec
  pcre_fullinfo
  pcre_get
  pcre_globals
  pcre_maketables
  pcre_ord2utf8
  pcre_printint
  pcre_refcount
  pcre_study
  pcre_tables
  pcre_try_flipped
  pcre_ucp_findchar
  pcre_valid_utf8
  pcre_version
  pcre_xclass
)
SET(PCRE_SOURCES ${PCRE_ROOTS})
ADD_SUFFIX(PCRE_SOURCES .c)
SET(PCRE_SOURCES
  config.h
  pcre.h
  pcre_internal.h
  ucp.h
  ${PCRE_SOURCES}
)
ADD_PREFIX(${Chicken_SOURCE_DIR}/pcre/ PCRE_SOURCES)

# The shared libraries
#    libchicken, libuchicken, libchicken-gui, libuchicken-gui
# and the static libraries
#    libchicken-boot, libchicken-static, libuchicken-static, libchicken-gui-static, libuchicken-gui-static
# all need the static PCRE library.
#
# We can easily include a static PCRE library in shared Chicken library. Unfortunately, we
# cannot include a static PCRE library in a *static* Chicken library.  "ar" is the underlying library
# tool on several platforms, and it can't put static libraries in static libraries.  
#
# One workaround is to compile the PCRE sources straight into the static Chicken libraries.  
# This is relatively simple to implement, and has the advantage of depending on 100% well defined
# CMake interfaces.  But, it leads to compilation redundancy in 5 static libraries.
#
# Another workaround is to deduce where the PCRE object files live, and include the object files
# directly into the static Chicken libraries.  With this approach we can eliminate the compilation
# redundancy.  However, it relies on some assumptions about what directories CMake uses,
# and these directories could change in the future.  For now though, it is the most efficient route.
# 
# Once this code is proven good, I'll make a CMake feature request, for a standard way to get the
# object directories and possibly the objects.  This would take the black magic out of the approach.
#
# libchicken-boot is consumed only by chicken-boot.  Thus it's no big deal to manually link
# libchicken-boot as a dependency.  We build an appropriate library in the /pcre subdirectory for
# this purpose.  This also produces all of the object files we want.  

ADD_SUBDIRECTORY(pcre)

# Then we use the following voodoo to reuse the objects.
#
# CMake generators are currently of 2 types: those which build single configurations, and those
# which build multiple configurations.  These 2 types use 2 different directory structures for where
# they put their object files.  The currently recommended way to deduce which type of generator
# we're using, is to see if CMAKE_CONFIGURATION_TYPES is empty or not.  If it's empty, then it's
# single configuration.  If it's non-empty, then it's multiple configuration, and contains a list of all
# the configurations available.  We're not interested in that list, only whether it's empty or non-empty.

IF(CMAKE_CONFIGURATION_TYPES)
  # We have a multiple configuration generator.  Use this directory structure.
  #
  # Note that CMAKE_BUILD_TYPE has no value when Visual Studio .sln files are generated.
  # This is because on MSVC, no build type is actually selected at generation time.  The MSVC
  # user typically selects her build type after opening the .sln file.  CMAKE_CFG_INTDIR expands
  # to a Visual Studio macro that will contain the right value, once Visual Studio is opened and
  # a build type is selected.
  SET(PCRE_FOR_STATIC_OBJ_DIR
    ${CMAKE_BINARY_DIR}/pcre/libpcre-for-static.dir/${CMAKE_CFG_INTDIR})
ELSE(CMAKE_CONFIGURATION_TYPES)
  # We have a single configuration generator.  Use this directory structure:
  SET(PCRE_FOR_STATIC_OBJ_DIR
    ${CMAKE_BINARY_DIR}/pcre${CMAKE_FILES_DIRECTORY}/libpcre-for-static.dir)
ENDIF(CMAKE_CONFIGURATION_TYPES)

# Now we know what directory the objects live in.  Construct the actual list of objects:

SET(PCRE_FOR_STATIC_OBJS ${PCRE_ROOTS})
ADD_SUFFIX(PCRE_FOR_STATIC_OBJS ${CMAKE_C_OUTPUT_EXTENSION})
ADD_PREFIX(${PCRE_FOR_STATIC_OBJ_DIR}/ PCRE_FOR_STATIC_OBJS)


####################################################################
#  BOOTSTRAP                                                       #
####################################################################

# Now that we've determined everything we need to know to build,
# we can transfer control to a subdirectory and create a 1st
# stage boot compiler.  

ADD_SUBDIRECTORY(boot)

GET_TARGET_PROPERTY(CHICKEN_BOOT_EXE chicken-boot LOCATION)
SET(CURRENT_CHICKEN ${CHICKEN_BOOT_EXE})

# Now we're compiling with the up-to-date version of Chicken. Yippie!


####################################################################
#  CREATE LIBCHICKEN                                               #
####################################################################

LIBRARY_SCM_TO_C_EXPORTS(eval)
UNSAFE_LIBRARY_SCM_TO_C(eval)
LIBRARY_SCM_TO_C_EXPORTS(extras)
UNSAFE_LIBRARY_SCM_TO_C(extras)
LIBRARY_SCM_TO_C_EXPORTS(library build)
UNSAFE_LIBRARY_SCM_TO_C(library build)
LIBRARY_SCM_TO_C_EXPORTS(lolevel)
UNSAFE_LIBRARY_SCM_TO_C(lolevel)
LIBRARY_SCM_TO_C(match)
SIMPLE_SCM_TO_C(optimizer tweaks)
LIBRARY_SCM_TO_C_EXPORTS(regex)
UNSAFE_LIBRARY_SCM_TO_C(regex)

# posixunix.scm and posixwin.scm both generate a file called posix.exports.
# Thus there is a file collision.  In principle the files should export the same
# functions, but in practice, Windows often lags behind Unix development.
# The quick 'n' dirty solution is to compile both at the same time and just
# take the results of the 2nd compilation as the correct results.  We make
# the Windows version of posix.exports the "correct" results because it is
# more likely to be missing stuff.  Thus it'll break someone's code at
# compile time and we'll know there's a problem.
#
# If this quick 'n' dirty approach is deemed insufficient, then the contents
# of the posix.exports files need to be sorted and compared, to ensure
# that they are indeed identical.  The sorting could be done with CMake,
# but sorted output from Chicken itself would be better.  It would be more
# readable and allow Autoconf to generate the same output.

# Only posixwin.scm is responsible for posix.exports.
# posixunix.scm emits it anyways, but posixwin.scm will re-emit it.
# Side effect: posixwin.c is compiled every time posixunix.c is compiled.
# If we had a command line option to suppress the emit code, we could avoid this.

ADD_CUSTOM_COMMAND(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/posixunix.c
  MAIN_DEPENDENCY ${Chicken_SOURCE_DIR}/posixunix.scm
  COMMAND ${CURRENT_CHICKEN} ${Chicken_SOURCE_DIR}/posixunix.scm -output-file ${CMAKE_CURRENT_BINARY_DIR}/posixunix.c -explicit-use ${CHICKEN_FLAGS}
  COMMENT "Compiling ${Chicken_SOURCE_DIR}/posixunix.scm to ${CMAKE_CURRENT_BINARY_DIR}/posixwin.c"
)
ADD_CUSTOM_COMMAND(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/posixwin.c ${CMAKE_CURRENT_BINARY_DIR}/posix.exports
  MAIN_DEPENDENCY ${Chicken_SOURCE_DIR}/posixwin.scm
  COMMAND ${CURRENT_CHICKEN} ${Chicken_SOURCE_DIR}/posixwin.scm -output-file ${CMAKE_CURRENT_BINARY_DIR}/posixwin.c -explicit-use ${CHICKEN_FLAGS}
  COMMENT "Compiling ${Chicken_SOURCE_DIR}/posixwin.scm to ${CMAKE_CURRENT_BINARY_DIR}/posixwin.c Exporting ${CMAKE_CURRENT_BINARY_DIR}/posix.exports"
)
# Unsafe libraries don't emit anything.
UNSAFE_LIBRARY_SCM_TO_C(posixunix)
UNSAFE_LIBRARY_SCM_TO_C(posixwin)

LIBRARY_SCM_TO_C(profiler)
LIBRARY_SCM_TO_C_EXPORTS(scheduler)
LIBRARY_SCM_TO_C_EXPORTS(srfi-1)
UNSAFE_LIBRARY_SCM_TO_C(srfi-1)
LIBRARY_SCM_TO_C_EXPORTS(srfi-4)
UNSAFE_LIBRARY_SCM_TO_C(srfi-4)
LIBRARY_SCM_TO_C_EXPORTS(srfi-13)
UNSAFE_LIBRARY_SCM_TO_C(srfi-13)
LIBRARY_SCM_TO_C_EXPORTS(srfi-14)
UNSAFE_LIBRARY_SCM_TO_C(srfi-14)
LIBRARY_SCM_TO_C_EXPORTS(srfi-18)
UNSAFE_LIBRARY_SCM_TO_C(srfi-18)
LIBRARY_SCM_TO_C(stub)
SIMPLE_SCM_TO_C(support banner tweaks)
LIBRARY_SCM_TO_C_EXPORTS(tcp)
UNSAFE_LIBRARY_SCM_TO_C(tcp)
LIBRARY_SCM_TO_C_EXPORTS(utils)
UNSAFE_LIBRARY_SCM_TO_C(utils)

# exports should be part of the target dependencies, somehow.
# Need to rework this stuff; it's the most confusing part of
# the build.  Blowing it off for now.

ADD_CUSTOM_TARGET(all-posix-c DEPENDS
  ${CMAKE_CURRENT_BINARY_DIR}/posixunix.c
  ${CMAKE_CURRENT_BINARY_DIR}/uposixunix.c
  ${CMAKE_CURRENT_BINARY_DIR}/posixwin.c
  ${CMAKE_CURRENT_BINARY_DIR}/uposixwin.c
)
ADD_DEPENDENCIES(all-posix-c chicken-boot)

GET_LIB_COMMON_CGEN(COMMON_C)
ADD_CUSTOM_TARGET(lib-common-c DEPENDS ${COMMON_C})
ADD_DEPENDENCIES(lib-common-c chicken-boot)

GET_LIB_SAFE_CGEN(SAFE_C)
ADD_CUSTOM_TARGET(lib-safe-c DEPENDS ${SAFE_C})
ADD_DEPENDENCIES(lib-safe-c chicken-boot)

GET_LIB_UNSAFE_CGEN(UNSAFE_C)
ADD_CUSTOM_TARGET(lib-unsafe-c DEPENDS ${UNSAFE_C})
ADD_DEPENDENCIES(lib-unsafe-c chicken-boot)

GET_LIB_CSOURCE(SOURCE_C)
ADD_CUSTOM_TARGET(libchicken-c DEPENDS ${SOURCE_C})
ADD_DEPENDENCIES(libchicken-c lib-common-c lib-safe-c)
ADD_CUSTOM_TARGET(libuchicken-c DEPENDS ${SOURCE_C})
ADD_DEPENDENCIES(libuchicken-c lib-common-c lib-unsafe-c)


GET_LIBCHICKEN_C_SOURCES(CHICKEN_LIB_SOURCES)
GET_LIBUCHICKEN_C_SOURCES(CHICKEN_UNSAFE_LIB_SOURCES)

IF(WIN32 AND NOT CYGWIN)
  SET(GUI_LIBS kernel32 user32 gdi32)
ENDIF(WIN32 AND NOT CYGWIN)

# Unix-y compilers tack "lib" onto the front of library names.  MSVC doesn't.
# Also, MSVC uses the indistinguishable suffix .lib for both dlls and static
# libraries.  So we need postfixes to distinguish our library names.
# Can't find any canonical postfixing conventions, so these will do for now.
# Don't wish to use "-m" for MINSIZEREL_POSTFIX because "mt" typically means
# "multithreaded, static linkage" for Windows native libraries.

IF(MSVC)

  # Eventually, we wish to support different postfixes for debug, release builds etc.
  # For now, the user has to install the 1 configuration she actually wants.

  ADD_LIBRARY(libchicken SHARED ${CHICKEN_LIB_SOURCES} ${PCRE_HEADER})
  SET_TARGET_PROPERTIES(libchicken PROPERTIES
    COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN ${SHARED_FLAGS}"
    OUTPUT_NAME "chicken" PREFIX "lib" IMPORT_PREFIX "lib"
    RELEASE_POSTFIX "" MINSIZEREL_POSTFIX "" DEBUG_POSTFIX "" RELWITHDEBINFO_POSTFIX "")
  TARGET_LINK_LIBRARIES(libchicken libpcre-for-shared ${MORE_LIBS})
  ADD_DEPENDENCIES(libchicken libchicken-c)

  ADD_LIBRARY(libuchicken SHARED ${CHICKEN_UNSAFE_LIB_SOURCES} ${PCRE_HEADER})
  SET_TARGET_PROPERTIES(libuchicken PROPERTIES
    COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN -DC_UNSAFE_RUNTIME ${SHARED_FLAGS}"
    OUTPUT_NAME "uchicken" PREFIX "lib" IMPORT_PREFIX "lib"
    RELEASE_POSTFIX "" MINSIZEREL_POSTFIX "" DEBUG_POSTFIX "" RELWITHDEBINFO_POSTFIX "")
  TARGET_LINK_LIBRARIES(libuchicken libpcre-for-shared ${MORE_LIBS})
  ADD_DEPENDENCIES(libuchicken libuchicken-c)

  ADD_LIBRARY(libchickengui SHARED ${CHICKEN_LIB_SOURCES} ${PCRE_HEADER})
  SET_TARGET_PROPERTIES(libchickengui PROPERTIES
    COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN -DC_WINDOWS_GUI ${SHARED_FLAGS}"
    OUTPUT_NAME "chickengui" PREFIX "lib" IMPORT_PREFIX "lib"
    RELEASE_POSTFIX "" MINSIZEREL_POSTFIX "" DEBUG_POSTFIX "" RELWITHDEBINFO_POSTFIX "")
  TARGET_LINK_LIBRARIES(libchickengui libpcre-for-shared ${GUI_LIBS} ${MORE_LIBS})
  ADD_DEPENDENCIES(libchickengui libchicken-c)

ELSE(MSVC)

  # We want the library to be named 'libchicken' on all platforms.
  # But, Cygwin conspires against us.  Their convention is
  # cygname-0.dll and so forth.  Whether it's a "best practice" is
  # debateable, but in the Cygwin FAQ and Googling about, it's the
  # practice.  
  # Default Unix, Cygwin, and MinGW behavior is to add 'lib' as a 
  # prefix onto every library.  For cross-platform stuff,
  # this often results in the 'liblibfoo" problem.
    
  ADD_LIBRARY(libchicken SHARED ${CHICKEN_LIB_SOURCES} ${PCRE_HEADER})
  SET_TARGET_PROPERTIES(libchicken PROPERTIES
    COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN ${SHARED_FLAGS}")
  TARGET_LINK_LIBRARIES(libchicken libpcre-for-shared ${MORE_LIBS})
  ADD_DEPENDENCIES(libchicken libchicken-c)

  ADD_LIBRARY(libuchicken SHARED ${CHICKEN_UNSAFE_LIB_SOURCES} ${PCRE_HEADER})
  SET_TARGET_PROPERTIES(libuchicken PROPERTIES
    COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN -DC_UNSAFE_RUNTIME ${SHARED_FLAGS}")
  TARGET_LINK_LIBRARIES(libuchicken libpcre-for-shared ${MORE_LIBS})
  ADD_DEPENDENCIES(libuchicken libuchicken-c)

  SET_TARGET_PROPERTIES(libchicken PROPERTIES OUTPUT_NAME "chicken")
  SET_TARGET_PROPERTIES(libuchicken PROPERTIES OUTPUT_NAME "uchicken")
  IF(CYGWIN)
    SET_TARGET_PROPERTIES(libchicken PROPERTIES VERSION 0)
    SET_TARGET_PROPERTIES(libuchicken PROPERTIES VERSION 0)
  ENDIF(CYGWIN)

  IF(WIN32 AND NOT CYGWIN)
    ADD_LIBRARY(libchickengui SHARED ${CHICKEN_LIB_SOURCES} ${PCRE_HEADER})
    SET_TARGET_PROPERTIES(libchickengui PROPERTIES
      COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN -DC_WINDOWS_GUI ${SHARED_FLAGS}"
      OUTPUT_NAME "chickengui")
    TARGET_LINK_LIBRARIES(libchickengui libpcre-for-shared ${GUI_LIBS} ${MORE_LIBS})
    ADD_DEPENDENCIES(libchickengui libchicken-c)
  ENDIF(WIN32 AND NOT CYGWIN)

ENDIF(MSVC)


####################################################################
#  CREATE CSC                                                      #
####################################################################

SIMPLE_SCM_TO_C(csc)
ADD_CUSTOM_TARGET(csc-c DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/csc.c)
ADD_DEPENDENCIES(csc-c chicken-boot)

ADD_EXECUTABLE(csc
  ${Chicken_SOURCE_DIR}/chicken.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-config.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-defaults.h
  ${CMAKE_CURRENT_BINARY_DIR}/csc.c
)
SET_TARGET_PROPERTIES(csc PROPERTIES COMPILE_FLAGS "${SHARED_FLAGS}")
SET_TARGET_PROPERTIES(csc PROPERTIES INSTALL_RPATH "${LIB_HOME}")
TARGET_LINK_LIBRARIES(csc libchicken)
ADD_DEPENDENCIES(csc csc-c)


####################################################################
#  CREATE CSI                                                      #
####################################################################

SIMPLE_SCM_TO_C(csi banner chicken-more-macros)
ADD_CUSTOM_TARGET(csi-c DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/csi.c)
ADD_DEPENDENCIES(csi-c chicken-boot)

SET(CSI_EXE_SOURCES
  ${Chicken_SOURCE_DIR}/chicken.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-config.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-defaults.h
  ${CMAKE_CURRENT_BINARY_DIR}/csi.c
)
ADD_EXECUTABLE(csi ${CSI_EXE_SOURCES})
SET_TARGET_PROPERTIES(csi PROPERTIES COMPILE_FLAGS "${SHARED_FLAGS}")
SET_TARGET_PROPERTIES(csi PROPERTIES INSTALL_RPATH "${LIB_HOME}")
TARGET_LINK_LIBRARIES(csi libchicken)
ADD_DEPENDENCIES(csi csi-c)


####################################################################
#  CREATE CHICKEN-PROFILE                                          #
####################################################################

SIMPLE_SCM_TO_C(chicken-profile build)
ADD_CUSTOM_TARGET(chicken-profile-c DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/chicken-profile.c)
ADD_DEPENDENCIES(chicken-profile-c chicken-boot)

ADD_EXECUTABLE(chicken-profile
  ${Chicken_SOURCE_DIR}/chicken.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-config.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-defaults.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-profile.c
)
SET_TARGET_PROPERTIES(chicken-profile PROPERTIES COMPILE_FLAGS "${SHARED_FLAGS}")
SET_TARGET_PROPERTIES(chicken-profile PROPERTIES INSTALL_RPATH "${LIB_HOME}")
TARGET_LINK_LIBRARIES(chicken-profile libchicken)
ADD_DEPENDENCIES(chicken-profile chicken-profile-c)


####################################################################
#  CREATE CHICKEN-SETUP                                            #
####################################################################

SIMPLE_SCM_TO_C(chicken-setup build)
ADD_CUSTOM_TARGET(chicken-setup-c DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/chicken-setup.c)
ADD_DEPENDENCIES(chicken-setup-c chicken-boot)

ADD_EXECUTABLE(chicken-setup
  ${Chicken_SOURCE_DIR}/chicken.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-config.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-defaults.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-setup.c
)
SET_TARGET_PROPERTIES(chicken-setup PROPERTIES COMPILE_FLAGS "${SHARED_FLAGS}")
SET_TARGET_PROPERTIES(chicken-setup PROPERTIES INSTALL_RPATH "${LIB_HOME}")
TARGET_LINK_LIBRARIES(chicken-setup libchicken)
ADD_DEPENDENCIES(chicken-setup chicken-boot chicken-setup-c)


####################################################################
#  CREATE CHICKEN.EXE                                              #
####################################################################

SIMPLE_SCM_TO_C(batch-driver tweaks)
SIMPLE_SCM_TO_C(c-backend tweaks)
SIMPLE_SCM_TO_C(c-platform tweaks)
SIMPLE_SCM_TO_C(chicken build chicken-ffi-macros chicken-more-macros tweaks)
SIMPLE_SCM_TO_C(compiler tweaks)

GET_CHICKEN_C_SOURCES(CHICKEN_EXE_SOURCES)
ADD_CUSTOM_TARGET(chicken-c DEPENDS ${CHICKEN_EXE_SOURCES})
ADD_DEPENDENCIES(chicken-c chicken-boot)

ADD_EXECUTABLE(chicken ${CHICKEN_EXE_SOURCES})
SET_TARGET_PROPERTIES(chicken PROPERTIES COMPILE_FLAGS "${SHARED_FLAGS}")
SET_TARGET_PROPERTIES(chicken PROPERTIES INSTALL_RPATH "${LIB_HOME}")
TARGET_LINK_LIBRARIES(chicken libchicken)
ADD_DEPENDENCIES(chicken chicken-c)


####################################################################
#  CREATE STATIC LIBS AND EXES                                     #
####################################################################

# CMake 2.4.5, by default, only allows a shared or a static library
# of the same OUTPUT_NAME to be built in the same directory.  This is
# enforced by clobbering all the .o files before building; I think
# the last library specified is the winner.  This behavior can be
# modified by using SET_TARGET_PROPERTIES to set CLEAN_DIRECT_OUTPUT
# to 1.  However, this incurs a risk of linking against shared libs
# when static libs are intended, and vice versa.  That's why the
# default behavior is to clobber.
# 
# To work around these problems, we build all static libs and exes
# in a subdirectory.  Then there are no issues.

ADD_SUBDIRECTORY(static)


####################################################################
#  CHANGELOG                                                       #
####################################################################

# A distribution archive (aka a tarball) will have ChangeLog already.
# A Darcs repository tree will not, however.  We have to generate it.
#
# Note that as of Nov. 22nd, 2006, a Darcs package that understands
# Cygwin paths is not readily available.  It is possible to compile
# Darcs from Haskell sources, but that requires GHC, which can be
# difficult to get working.  The upshot is it's a PITA to access
# Darcs from Cygwin and hence to create a ChangeLog.  It's possible,
# but one has to proceed carefully.
#
# Different shells can cause Darcs to fail.  For instance, running a
# Windows native Darcs under a Cygwin shell can fail, because the
# Windows native Darcs doesn't understand Cygwin paths.  A workaround
# is to avoid issuing any Cygwin path to Darcs, and instead use a
# WORKING_DIRECTORY, so that CMake handles some of its own paths and
# not Darcs.
#
# In principle, if we need to use a Darcs command, we should test
# whether Darcs is available and actually works.  In practice, writing
# reliable tool tests in CMake 2.4.5 is painful.
#
# You cannot use EXECUTE_PROCESS to write a tool test.  It executes in
# CMake's environment, not the actual build environment.  For instance,
# let's say Darcs is available at the Windows command prompt.
# EXECUTE_PROCESS will say it works.  However, it won't actually work
# under Visual Studio, because VS doesn't typically receive all the paths
# that the command prompt does.
#
# To write a tool test, Kitware expects one to emit a trivial CMakeLists.txt
# to a temporary subdirectory, and then TRY_COMPILE it.  In practice, this
# approach is exceedingly fragile, due to quote consumption problems with
# FILE(WRITE ...) and with shells.
#
# What's really needed is an entirely different / better mechanism for
# tool testing.  Something that's exactly parallel to the code we'd
# write here at the toplevel, so that there are no weird extraneous
# considerations.

SET(CHANGELOG_FILE -NOTFOUND)
IF(EXISTS ${Chicken_SOURCE_DIR}/_darcs)
  FIND_PROGRAM(DARCS_EXE darcs)
  IF(DARCS_EXE)
    FILE(REMOVE_RECURSE ${CMAKE_CURRENT_BINARY_DIR}/try-darcs)

    # Note the need to escape any quotes that are part of the file output.
    # I cannot figure out how to get code emitted in a file to quote properly.
    # Consequently, we use a WORKING_DIRECTORY to duck the issue.
    #
    # Note that the \"${DARCS_EXE}\" quotes are necessary here, even though
    # they are not generally necessary in the toplevel CMakeLists.txt, i.e.
    # this file you're reading now.  At this level, whitespace is escaped, i.e.
    #   ${DARCS_EXE} = E:/Program\ Files/darcsdir-w32
    # But once emitted, the whitespace escapes are lost.  We get
    #   ${DARCS_EXE} = E:/Program Files/darcsdir-w32
    # and of course Cygwin dies, as E:/Program isn't a valid command.
    #
    # Possibly this emission code should be replaced with a CONFIGURE_FILE
    # template.  Or else substitutions should be performed with
    # STRING(CONFIGURE ...).  Anything to get the quote / escape problems
    # under control.

    FILE(WRITE ${CMAKE_CURRENT_BINARY_DIR}/try-darcs/CMakeLists.txt "
      PROJECT(try-darcs)
      ADD_CUSTOM_TARGET(try
        WORKING_DIRECTORY \"${Chicken_SOURCE_DIR}\"
        COMMAND \"${DARCS_EXE}\" changes --last=0
      )
    ")
    TRY_COMPILE(DARCS_WORKS
       ${CMAKE_CURRENT_BINARY_DIR}/try-darcs
       ${CMAKE_CURRENT_BINARY_DIR}/try-darcs
       try-darcs try
    )
    IF(DARCS_WORKS)
      SET(CHANGELOG_FILE ${CMAKE_CURRENT_BINARY_DIR}/ChangeLog)

      # Custom commands have the format:
      #
      #   COMMAND command1 [args...]
      #
      # command1 has to be a CMake path.  This is not documented in CMake 2.4.5.
      # Think of command1 as receiving "special interpretation" and not really
      # being a "custom" command, i.e. you're not free to do what you like.
      #
      # Also, if command1 is an absolute pathname, it must have whitespace
      # handled properly.  The results of CMake Find* commands always have
      # whitespace handled properly, but if you're homebrewing your own
      # pathnames, be careful.
      #
      # Other path statements in [args...] need to be paths that your
      # tool understands.  This means you'll need native paths, unless your
      # tool happens to like CMake paths.
      #
      # As of November 22, 2006, there is no Cygwin version of Darcs.
      # Instead, one typically has a Windows native version of
      # Darcs running under a Cygwin shell.  This Windows native Darcs
      # does not understand Cygwin paths, i.e. --repodir=/cygdrive/c/whatever
      # will fail.
      #
      # Ideally, we'd complete the implementation of WINDOWS_PATH and provide
      # proper paths.  Pragmatically, this is a PITA.  We adopt the expedient
      # of using a working directory so that we don't have to bother with
      # --repodir.
      #
      # stdout is redirected to the CHANGELOG_FILE.  Redirection is a shell
      # operation, so we need a path that the shell understands.  Generally,
      # keeping what a CMake comamnd1 needs, separate from what a tool needs,
      # separate from what a shell needs, is confusing.  But that's the drill.

      NATIVE_PATH(CHANGELOG_FILE NATIVE_CHANGELOG_FILE)

      # Always build the ChangeLog unconditionally.  It doesn't take long,
      # and we need to make sure it stays up to date.  There is no easy
      # way to determine if the Darcs repository has changed since we last
      # looked at it, so brute force is the workaround.

      ADD_CUSTOM_TARGET(darcs-changelog ALL
        COMMENT "Generating ${NATIVE_CHANGELOG_FILE} from Darcs repository."
        WORKING_DIRECTORY ${Chicken_SOURCE_DIR}
        COMMAND ${DARCS_EXE} changes > ${NATIVE_CHANGELOG_FILE}
      )
    ELSE(DARCS_WORKS)
      ECHO_TARGET(darcs-changelog "Darcs does not work.  Cannot create ChangeLog.")
    ENDIF(DARCS_WORKS)
  ELSE(DARCS_EXE)
    ECHO_TARGET(darcs-changelog "Cannot find Darcs.  Cannot create ChangeLog.")    
  ENDIF(DARCS_EXE)
ELSE(EXISTS ${Chicken_SOURCE_DIR}/_darcs)
  IF(EXISTS ${Chicken_SOURCE_DIR}/ChangeLog)
    SET(CHANGELOG_FILE ${Chicken_SOURCE_DIR}/ChangeLog)
    ECHO_TARGET(darcs-changelog "No Darcs repository.  ChangeLog already exists as part of archive distribution.")
  ELSE(EXISTS ${Chicken_SOURCE_DIR}/ChangeLog)
    ECHO_TARGET(darcs-changelog "No Darcs repository.  ChangeLog is missing from archive distribution.")
  ENDIF(EXISTS ${Chicken_SOURCE_DIR}/ChangeLog)
ENDIF(EXISTS ${Chicken_SOURCE_DIR}/_darcs)


####################################################################
#    HTML documentation                                            #
####################################################################

IF(EXISTS html/index.html)
  SET(HTMLDOC_FILES 
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
    html/faq.html)
ENDIF(EXISTS html/index.html)


####################################################################
#  INSTALLATION                                                    #
####################################################################


INSTALL(TARGETS
  chicken
  chicken-profile
  chicken-setup
  csc
  csi
  libchicken
  libuchicken
  RUNTIME DESTINATION ${BIN_HOME}
  LIBRARY DESTINATION ${LIB_HOME}
  ARCHIVE DESTINATION ${LIB_HOME}
)
IF(WIN32 AND NOT CYGWIN)
  INSTALL(TARGETS
    libchickengui
    RUNTIME DESTINATION ${BIN_HOME}
    LIBRARY DESTINATION ${LIB_HOME}
    ARCHIVE DESTINATION ${LIB_HOME}
  )
ENDIF(WIN32 AND NOT CYGWIN)

GET_LIB_EXPORTS(exports)
INSTALL(FILES ${exports} DESTINATION ${SHARE_HOME})

IF(WIN32)
  INSTALL(FILES csibatch.bat DESTINATION ${BIN_HOME})
ENDIF(WIN32)

# Brad King demonstrated \$ENV{DESTDIR} as the preferred method
# of handling ${CMAKE_INSTALL_PREFIX} when doing INSTALL(CODE ...)
# But I don't think this can be supported, as the paths must be
# hardwired into Chicken itself.

# Destination for eggs.  No eggs to install though.
INSTALL(CODE "
  MESSAGE(STATUS \"Installing ${EGG_HOME}\")
  MAKE_DIRECTORY(\"${EGG_HOME}\")
")

INSTALL(FILES chicken-ffi-macros.scm chicken-more-macros.scm
  DESTINATION ${SHARE_HOME})

INSTALL(FILES
  chicken.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-config.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-defaults.h
  DESTINATION ${INCLUDE_HOME}
)
INSTALL(FILES chicken.1 csi.1 csc.1 chicken-setup.1 chicken-profile.1 
  DESTINATION ${MAN_HOME})
INSTALL(FILES README LICENSE INSTALL-CMake.txt DESTINATION ${DOC_HOME})

# It is confusing to install ChangeLog.0-20040412 without ChangeLog.
# It makes Chicken look like it hasn't been updated in a long time.
# Either install both, or none at all.
IF(CHANGELOG_FILE)
  INSTALL(FILES
    ${CHANGELOG_FILE}
    ${Chicken_SOURCE_DIR}/ChangeLog.0-20040412
    DESTINATION ${DOC_HOME}
  )
ENDIF(CHANGELOG_FILE)

INSTALL(FILES ${HTMLDOC_FILES} DESTINATION ${DOC_HOME}/html)


####################################################################
#   UNINSTALLATION                                                 #
####################################################################

# Policy is to wipe out any versions of binaries and libraries that
# Chicken has been building, not just the ones we installed.  This
# allows, say, MSVC and MinGW to be uninstalled and reinstalled in
# the same directory.
#
# We leave installed eggs alone.  Good idea or not?

SET(INSTALLED_BINARIES
  chicken
  chicken.exe
  chicken-profile
  chicken-profile.exe
  chicken-setup
  chicken-setup.exe
  chicken-static
  chicken-static.exe
  csc
  csc.exe
  csi
  csi.exe
  csibatch.bat
  csi-static
  csi-static.exe
)
ADD_PREFIX(${BIN_HOME}/ INSTALLED_BINARIES)

SET(INSTALLED_INCLUDES
  chicken.h
  chicken-config.h
  chicken-defaults.h  
)
ADD_PREFIX(${INCLUDE_HOME}/ INSTALLED_INCLUDES)

SET(INSTALLED_MANPAGES
  chicken.1
  chicken-profile.1
  chicken-setup.1
  csc.1
  csi.1
)
ADD_PREFIX(${MAN_HOME}/ INSTALLED_MANPAGES)

SET(INSTALLED_DOCS
  ChangeLog
  ChangeLog.0-20040412
  INSTALL-CMake.txt
  LICENSE
  README
)
ADD_PREFIX(${DOC_HOME}/ INSTALLED_DOCS)

ADD_CUSTOM_TARGET(uninstall
  COMMAND ${CMAKE_COMMAND} -E echo Completely removing all traces of Chicken installation from ${CMAKE_INSTALL_PREFIX}
  COMMAND ${CMAKE_COMMAND} -E echo Eggs in ${EGG_HOME} will be left alone.
  COMMAND ${CMAKE_COMMAND} -E remove ${INSTALLED_BINARIES}
  COMMAND ${CMAKE_COMMAND} -E remove ${INSTALLED_INCLUDES}
  COMMAND ${CMAKE_COMMAND} -E remove ${INSTALLED_MANPAGES}
  COMMAND ${CMAKE_COMMAND} -E remove ${INSTALLED_DOCS}
  COMMAND ${CMAKE_COMMAND} -E remove ${INFO_HOME}/chicken.info
  COMMAND ${CMAKE_COMMAND} -E remove ${BIN_HOME}/libchicken*
  COMMAND ${CMAKE_COMMAND} -E remove ${BIN_HOME}/libuchicken*
  COMMAND ${CMAKE_COMMAND} -E remove ${BIN_HOME}/cygchicken*
  COMMAND ${CMAKE_COMMAND} -E remove ${BIN_HOME}/cyguchicken*
  COMMAND ${CMAKE_COMMAND} -E remove ${LIB_HOME}/libchicken*
  COMMAND ${CMAKE_COMMAND} -E remove ${LIB_HOME}/libuchicken*
  COMMAND ${CMAKE_COMMAND} -E remove ${SHARE_HOME}/*.exports
  COMMAND ${CMAKE_COMMAND} -E remove ${SHARE_HOME}/*.scm
)


####################################################################
#   DISTRIBUTION                                                   #
####################################################################

# dist.cmake contains the list of all files that should be present
# in the distribution.  It also contains a script for creating a
# distribution tree and verifying its contents.  dist.cmake
# produces a distribution that works for both CMake and ./configure,
# and is now the canonical method for creating a distribution.
#
# We still support the GNU Autoconf / Automake build.  Thus, to
# make a distro, Automake has to be run.  Thus, a Bourne shell "sh"
# must be available, so that Automake can run.  We shouldn't have
# to run CMakeLists.txt from a Bourne shell; we just need to have,
# say, a sh.exe available in our path.  We aren't currently
# doing any error checking on whether "sh" is available or working.
# Nor whether Automake is installed, for that matter.  Not many people
# need to build distros.
#
# dist.cmake handles all the Automake magic.  It's not visible here.

SET(BOOT_CFILES_DEPS ${BOOT_CFILES})
ADD_PREFIX(${CMAKE_CURRENT_BINARY_DIR}/ BOOT_CFILES_DEPS)
ADD_CUSTOM_TARGET(boot-c DEPENDS ${BOOT_CFILES_DEPS})
ADD_DEPENDENCIES(boot-c
  all-posix-c lib-common-c lib-safe-c lib-unsafe-c
  chicken-c csc-c csi-c chicken-profile-c chicken-setup-c
)

SET(DIST_DIR chicken-${BUILDVERSION})

# We wrap ADD_CUSTOM_COMMAND with ADD_CUSTOM_TARGET so that if a
# build step fails, CMake cannot falsely report a success.  We
# need OUTPUT files from an ADD_CUSTOM_COMMAND to correctly track our
# progress.  If an OUTPUT file doesn't get built, then we have failed.
# If we just use a chain of ADD_CUSTOM_TARGET commands, they will always
# execute unconditionally and will always report success.

ADD_CUSTOM_COMMAND(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${DIST_DIR}.tar.gz ${CMAKE_CURRENT_BINARY_DIR}/${DIST_DIR}.zip
  DEPENDS ${AUTOMAKE_FILES} ${CHANGELOG_FILE}
  COMMAND ${CMAKE_COMMAND} -DChicken_SOURCE_DIR=${Chicken_SOURCE_DIR} -DDIST_DIR=${DIST_DIR} -DCHANGELOG_FILE="${CHANGELOG_FILE}" -DBOOT_CFILES="${BOOT_CFILES}" -P ${Chicken_SOURCE_DIR}/dist.cmake
)
ADD_CUSTOM_TARGET(dist DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${DIST_DIR}.tar.gz ${CMAKE_CURRENT_BINARY_DIR}/${DIST_DIR}.zip)
ADD_DEPENDENCIES(dist boot-c darcs-changelog)


####################################################################
#    OPENGL INSTALLATION AND TESTING                               #
####################################################################

# As a byproduct of OpenGL testing, the OpenGL egg and all its
# dependencies are installed on the user's system.  It would be
# nice to test without installing, but this is not possible so long
# as Chicken's installation paths are hardwired into chicken-defaults.h
#
# We need to invoke chicken-setup from a temp directory that doesn't contain
# the chicken-setup we just built.  The installed chicken-setup is supposed
# to know how to find all the other needed tools in the Chicken installation.
# Thus if this works, it is testing a whole lot of the Chicken installation.
#
# We wrap ADD_CUSTOM_COMMAND with ADD_CUSTOM_TARGET so that if a
# build step fails, CMake cannot falsely report a success.  We
# need OUTPUT files from an ADD_CUSTOM_COMMAND to correctly track our
# progress.  If an OUTPUT file doesn't get built, then we have failed.
# If we just use a chain of ADD_CUSTOM_TARGET commands, they will always
# execute unconditionally and will always report success.
#
# The ADD_CUSTOM_TARGET bulletproofing is supposed to work, but doesn't!
# In CMake 2.4.5, when file
# dependencies of ADD_CUSTOM_TARGET are not built, the target nevertheless
# reports success.  So you can use these targets, but don't trust 'em when
# they tell you they've succeeded.

ADD_CUSTOM_COMMAND(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/opengl-temp
  COMMAND ${CMAKE_COMMAND}
    -D DIR=${CMAKE_CURRENT_BINARY_DIR}/opengl-temp
    -P ${Chicken_SOURCE_DIR}/mkdir.cmake
)
ADD_CUSTOM_TARGET(opengl-temp-dir DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/opengl-temp)

ADD_CUSTOM_COMMAND(
  OUTPUT ${EGG_HOME}/opengl.dll ${EGG_HOME}/opengl.setup-info
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/opengl-temp
  COMMAND ${BIN_HOME}/chicken-setup -dont-ask opengl
)
ADD_CUSTOM_TARGET(install-opengl-egg
  DEPENDS ${EGG_HOME}/opengl.dll ${EGG_HOME}/opengl.setup-info)
ADD_DEPENDENCIES(install-opengl-egg opengl-temp-dir chicken-setup)

# Kitware says "install" is not available as a first-class dependency,
# and should not be used as such.
#
# ADD_DEPENDENCIES(install-opengl-egg install)


####################################################################
#    BENCHMARKING                                                  #
####################################################################

# SIMPLE_SCM_TO_C(cscbench)
# ADD_CUSTOM_TARGET(cscbench-c DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/cscbench.c)
# ADD_DEPENDENCIES(cscbench-c chicken-boot)
# ADD_EXECUTABLE(cscbench
#   ${Chicken_SOURCE_DIR}/chicken.h
#   ${CMAKE_CURRENT_BINARY_DIR}/chicken-config.h
#   ${CMAKE_CURRENT_BINARY_DIR}/chicken-defaults.h
#   ${CMAKE_CURRENT_BINARY_DIR}/cscbench.c
# )
# SET_TARGET_PROPERTIES(cscbench PROPERTIES COMPILE_FLAGS "${STATIC_FLAGS}")
# TARGET_LINK_LIBRARIES(cscbench libchicken-static)
# ADD_DEPENDENCIES(cscbench cscbench-c)
# ADD_CUSTOM_TARGET(bench 
#   DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/cscbench
#   COMMAND ${CMAKE_COMMAND} -E chdir ${Chicken_SOURCE_DIR}/benchmarks ${CMAKE_CURRENT_BINARY_DIR}/cscbench)


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
