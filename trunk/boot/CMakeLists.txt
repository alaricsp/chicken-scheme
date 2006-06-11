# Files built here are only used for the bootstrap.  They are never installed.
# Consequently, we don't need to build everything.  We only build static
# libraries and executables, no need to fool with anything more complicated.
# We build:
#  libchicken-boot
#  easyffi.l.silex
#  chicken-boot
#  csi-boot
#
# chicken-boot and csi-boot are then used by the rest of the build.

# Do not use LIBRARY_SCM_TO_C_EXPORTS to build the bootstrap compiler.
# We don't need the exports to build the next compiler stage, and
# Chicken 2.3 doesn't generate them.  We're worried that extra
# dependencies may cause CMake 2.4.2 to always build everything.
# If EXTANT_CHICKEN is a high enough version number to generate .exports,
# we'll have stray .exports junk in the directory.  Do we care about
# cleaning it up?

SIMPLE_SCM_TO_C(batch-driver parameters)
SIMPLE_SCM_TO_C(c-backend tweaks)
SIMPLE_SCM_TO_C(c-platform)
SIMPLE_SCM_TO_C(chicken build chicken-ffi-macros chicken-more-macros tweaks)
SIMPLE_SCM_TO_C(chicken-profile)
SIMPLE_SCM_TO_C(chicken-setup build parameters)
SIMPLE_SCM_TO_C(compiler easyffi parameters tweaks)
GEN_CSI_C()
GEN_EASYFFI_C()
LIBRARY_SCM_TO_C(eval parameters)
LIBRARY_SCM_TO_C(extras)
LIBRARY_SCM_TO_C(library build parameters)
LIBRARY_SCM_TO_C(lolevel)
LIBRARY_SCM_TO_C(match)
SIMPLE_SCM_TO_C(optimizer parameters tweaks)
SIMPLE_SCM_TO_C(partition)
LIBRARY_SCM_TO_C(pcre regex-common)
LIBRARY_SCM_TO_C(posix)
LIBRARY_SCM_TO_C(posixwin)
LIBRARY_SCM_TO_C(pregexp regex-common)
LIBRARY_SCM_TO_C(profiler)
LIBRARY_SCM_TO_C(regex regex-common)
LIBRARY_SCM_TO_C(scheduler)
LIBRARY_SCM_TO_C(srfi-1)
LIBRARY_SCM_TO_C(srfi-4)
LIBRARY_SCM_TO_C(srfi-13)
LIBRARY_SCM_TO_C(srfi-14)
LIBRARY_SCM_TO_C(srfi-18)
LIBRARY_SCM_TO_C(stub)
SIMPLE_SCM_TO_C(support banner parameters tweaks)
LIBRARY_SCM_TO_C(tcp)
LIBRARY_SCM_TO_C(tinyclos)
LIBRARY_SCM_TO_C(utils)

# Make sure all .c files are generated before trying to build
# libraries.  Otherwise the dependencies tend to clobber each
# other.  It seems that CMake 2.4.2's file level dependencies
# aren't sufficient to make toplevel targets sequence properly,
# especially under Visual Studio .NET 2003.

GET_LIB_COMMON_CGEN(common)
ADD_CUSTOM_TARGET(boot_common_cgen DEPENDS ${common})
  
GET_LIB_SAFE_CGEN(safe)
ADD_CUSTOM_TARGET(boot_safe_cgen DEPENDS ${safe})

GET_CHICKEN_CGEN(hatch)
ADD_CUSTOM_TARGET(boot_chicken_cgen DEPENDS ${hatch})

####################################################################
#  CREATE LIBCHICKEN-BOOT                                          #
####################################################################

GET_LIBCHICKEN_C_SOURCES(LIBCHICKEN_BOOT_SOURCES)

ADD_LIBRARY(libchicken-boot STATIC ${LIBCHICKEN_BOOT_SOURCES})
SET_TARGET_PROPERTIES(libchicken-boot PROPERTIES
  PREFIX "" IMPORT_PREFIX ""
  COMPILE_FLAGS "-DC_BUILDING_LIBCHICKEN ${STATIC_FLAGS}"
)
TARGET_LINK_LIBRARIES(libchicken-boot ${EXTRA_LIBS})
ADD_DEPENDENCIES(libchicken-boot boot_common_cgen boot_safe_cgen)


####################################################################
#  CREATE easyffi.l.silex                                          #
####################################################################

GEN_EASYFFI_L_SILEX()


####################################################################
#  CREATE CHICKEN-BOOT                                             #
####################################################################

GET_CHICKEN_C_SOURCES(CHICKEN_BOOT_SOURCES)
ADD_EXECUTABLE(chicken-boot ${CHICKEN_BOOT_SOURCES})
SET_TARGET_PROPERTIES(chicken-boot PROPERTIES COMPILE_FLAGS "${STATIC_FLAGS}")
TARGET_LINK_LIBRARIES(chicken-boot libchicken-boot)
ADD_DEPENDENCIES(chicken-boot boot_chicken_cgen)


####################################################################
#  CREATE CSI-BOOT                                                 #
####################################################################

ADD_EXECUTABLE(csi-boot ${CMAKE_CURRENT_BINARY_DIR}/csi.c)
SET_TARGET_PROPERTIES(csi-boot PROPERTIES COMPILE_FLAGS "${STATIC_FLAGS}")
TARGET_LINK_LIBRARIES(csi-boot libchicken-boot)


####################################################################
#  BOOT TARGET                                                     #
####################################################################

# Feeling paranoid about the these needing to be in the same directory
# for the boostrap compiler and/or interpreter to be usable.
# However, it hasn't solved any build problems.  May not be needed.

LOCAL_COPY(chicken.h)
LOCAL_COPY(chicken-ffi-macros.scm)
LOCAL_COPY(chicken-more-macros.scm)

ADD_CUSTOM_TARGET(boot
  DEPENDS
  ${CMAKE_CURRENT_BINARY_DIR}/chicken.h
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-ffi-macros.scm
  ${CMAKE_CURRENT_BINARY_DIR}/chicken-more-macros.scm
)
ADD_DEPENDENCIES(boot chicken-boot csi-boot)
