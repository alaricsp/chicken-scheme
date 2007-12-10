# defaults.make - default settings -*- Makefile -*-
#
# Copyright (c) 2007, Felix L. Winkelmann
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following
# conditions are met:
#
#   Redistributions of source code must retain the above copyright notice, this list of conditions and the following
#     disclaimer. 
#   Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
#     disclaimer in the documentation and/or other materials provided with the distribution. 
#   Neither the name of the author nor the names of its contributors may be used to endorse or promote
#     products derived from this software without specific prior written permission. 
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS
# OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
# AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
# OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#
# Send bugs, suggestions and ideas to: 
#
# felix@call-with-current-continuation.org
#
# Felix L. Winkelmann
# Unter den Gleichen 1
# 37130 Gleichen
# Germany


# basic parameters

BINARYVERSION ?= 3
NURSERY ?= (128*1024)
STACKDIRECTION ?= 1
CROSS_CHICKEN ?= 0

# directories

ifeq ($(PLATFORM),mingw)
PREFIX ?= c:\\devtools
DESTDIR =
BINDIR = $(DESTDIR)$(PREFIX)\\bin
LIBDIR = $(DESTDIR)$(PREFIX)\\lib
SHAREDIR = $(DESTDIR)$(PREFIX)\\share
DATADIR = $(SHAREDIR)\\chicken
TOPMANDIR = $(SHAREDIR)\\man
MANDIR = $(TOPMANDIR)\\man1
INFODIR = $(SHAREDIR)\\info
INCDIR = $(DESTDIR)$(PREFIX)\\include
DOCDIR = $(DATADIR)\\doc
CHICKENLIBDIR = $(LIBDIR)\\chicken
EGGDIR = $(CHICKENLIBDIR)\\$(BINARYVERSION)
else
PREFIX ?= /usr/local
DESTDIR =
BINDIR = $(DESTDIR)$(PREFIX)/bin
LIBDIR = $(DESTDIR)$(PREFIX)/lib
SHAREDIR = $(DESTDIR)$(PREFIX)/share
DATADIR = $(SHAREDIR)/chicken
TOPMANDIR = $(SHAREDIR)/man
MANDIR = $(TOPMANDIR)/man1
INFODIR = $(SHAREDIR)/info
INCDIR = $(DESTDIR)$(PREFIX)/include
DOCDIR = $(DATADIR)/doc
CHICKENLIBDIR = $(LIBDIR)/chicken
EGGDIR = $(CHICKENLIBDIR)/$(BINARYVERSION)
endif

# commands

ifdef HOSTSYSTEM
C_COMPILER ?= $(HOSTSYSTEM)-gcc
CXX_COMPILER ?= $(HOSTSYSTEM)-g++
LIBRARIAN ?= $(HOSTSYSTEM)-ar
else
C_COMPILER ?= gcc
CXX_COMPILER ?= g++
LIBRARIAN ?= ar
endif
LINKER ?= $(C_COMPILER)
ifeq ($(PLATFORM),mingw)
REMOVE_COMMAND ?= del
else
REMOVE_COMMAND ?= rm
endif
ASSEMBLER ?= $(C_COMPILER)
MAKEINFO_PROGRAM ?= -makeinfo
ifeq ($(PLATFORM),mingw)
INSTALL_PROGRAM ?= copy
MAKEDIR_COMMAND ?= -mkdir
else
INSTALL_PROGRAM ?= install
MAKEDIR_COMMAND ?= mkdir
endif
POSTINSTALL_STATIC_LIBRARY ?= true
POSTINSTALL_PROGRAM ?= true
INSTALLINFO_PROGRAM ?= -install-info
UNINSTALLINFO_PROGRAM ?= -install-info

# target variables

ifdef TARGETSYSTEM
TARGET_C_COMPILER ?= $(TARGETSYSTEM)-$(C_COMPILER)
TARGET_CXX_COMPILER ?= $(TARGETSYSTEM)-$(CXX_COMPILER)
else
TARGET_C_COMPILER ?= $(C_COMPILER)
TARGET_CXX_COMPILER ?= $(CXX_COMPILER)
endif

TARGET_C_COMPILER_OPTIONS ?= $(C_COMPILER_OPTIONS)
TARGET_C_COMPILER_OPTIMIZATION_OPTIONS ?= $(C_COMPILER_OPTIMIZATION_OPTIONS)
TARGET_PREFIX ?= $(PREFIX)
TARGET_RUN_PREFIX ?= $(TARGET_PREFIX)
TARGET_LIBRARIES ?= $(LIBRARIES)
TARGET_LINKER_OPTIONS ?= $(LINKER_OPTIONS)

ifneq ($(TARGET_C_COMPILER),$(C_COMPILER))
CROSS_CHICKEN = 1
else
CROSS_CHICKEN = 0
endif


# options

ifndef NOPTABLES
C_COMPILER_PTABLES_OPTIONS = -DC_ENABLE_PTABLES
endif
INCLUDES ?= -I.
PCRE_INCLUDES ?= $(INCLUDES) -Ipcre
C_COMPILER_COMPILE_OPTION ?= -c
C_COMPILER_OUTPUT_OPTION ?= -o
ifdef DEBUGBUILD
C_COMPILER_OPTIMIZATION_OPTIONS ?= -g -Wall -Wno-unused
endif
C_COMPILER_BUILD_RUNTIME_OPTIONS ?= -DC_BUILDING_LIBCHICKEN
C_COMPILER_BUILD_UNSAFE_RUNTIME_OPTIONS ?= $(C_COMPILER_BUILD_RUNTIME_OPTIONS) -DNDEBUG -DC_UNSAFE_RUNTIME
C_COMPILER_PCRE_OPTIONS ?= -DPCRE_STATIC
C_COMPILER_SHARED_OPTIONS ?= -fPIC -DPIC
LINKER_EXECUTABLE_OPTIONS ?= -L.
LINKER_STATIC_OPTIONS ?= $(LINKER_EXECUTABLE_OPTIONS)
LINKER_OUTPUT_OPTION ?= -o
LINKER_LIBRARY_OPTION ?= -l
LINKER_LINK_SHARED_LIBRARY_OPTIONS ?= -shared
LIBRARIAN_OPTIONS ?= cru
LIBRARIES ?= -lm
ifeq ($(PLATFORM),mingw)
REMOVE_COMMAND_OPTIONS ?= /f /q
REMOVE_COMMAND_RECURSIVE_OPTIONS ?= /f /s /q
MAKE_WRITABLE_COMMAND ?= rem
else
REMOVE_COMMAND_OPTIONS ?= -f
REMOVE_COMMAND_RECURSIVE_OPTIONS ?= -fr
MAKE_WRITABLE_COMMAND ?= chmod a+rw
endif
MAKEINFO_PROGRAM_OPTIONS ?= --no-split 
ifneq ($(PLATFORM),mingw)
INSTALL_PROGRAM_SHARED_LIBRARY_OPTIONS ?= -m755
INSTALL_PROGRAM_STATIC_LIBRARY_OPTIONS ?= -m644
INSTALL_PROGRAM_EXECUTABLE_OPTIONS ?= -m755
INSTALL_PROGRAM_FILE_OPTIONS ?= -m644
MAKEDIR_COMMAND_OPTIONS ?= -p
endif
ASSEMBLER_OPTIONS ?= $(C_COMPILER_OPTIONS)
ASSEMBLER_OUTPUT_OPTION ?= -o
ASSEMBLER_COMPILE_OPTION ?= -c
ifdef STATICBUILD
PRIMARY_LIBCHICKEN ?= libchicken$(A)
else
ifeq ($(PLATFORM),cygwin)
PRIMARY_LIBCHICKEN = cygchicken-0.dll
LIBCHICKEN_SO_FILE = cygchicken-0.dll
LIBUCHICKEN_SO_FILE = cyguchicken-0.dll
else
PRIMARY_LIBCHICKEN ?= libchicken$(SO)
LIBCHICKEN_SO_FILE ?= libchicken$(SO)
LIBUCHICKEN_SO_FILE ?= libuchicken$(SO)
endif
endif
UNINSTALLINFO_PROGRAM_OPTIONS ?= --delete
LIBCHICKEN_SO_LIBRARIES ?= $(LIBRARIES)
LIBUCHICKEN_SO_LIBRARIES ?= $(LIBRARIES)
LIBCHICKENGUI_SO_LIBRARIES ?= $(LIBRARIES)

# other settings

HOSTNAME ?= $(shell hostname)
ifeq ($(PLATFORM),mingw)
BUILD_TIME ?= $(shell date /t)
UNAME_SYS ?= MinGW
else
BUILD_TIME ?= $(shell date +%Y-%m-%d)
UNAME_SYS ?= $(shell uname)
endif
BUILD_TAG ?= compiled $(BUILD_TIME) on $(HOSTNAME) ($(UNAME_SYS))

ifdef LOCKTOSPACE
C_COMPILER_BUILD_RUNTIME_OPTIONS += -DC_LOCK_TOSPACE
endif

# file extensions

O ?= .o
A ?= .a
# EXE =
SO ?= .so

# special files

POSIXFILE ?= posixunix
CHICKEN_CONFIG_H = chicken-config.h
PCRE_OBJECT_FILES ?= pcre/*.o

ifneq ($(ARCH),)
HACKED_APPLY = 1
APPLY_HACK_OBJECT = apply-hack.$(ARCH)$(O)
endif

# bootstrapping compiler

CHICKEN = $(PREFIX)/bin/chicken$(EXE)

# Scheme compiler flags

CHICKEN_OPTIONS = -quiet -no-trace -optimize-level 2 -include-path .
CHICKEN_LIBRARY_OPTIONS = $(CHICKEN_OPTIONS) -explicit-use
CHICKEN_PROGRAM_OPTIONS = $(CHICKEN_OPTIONS) -no-lambda-info
CHICKEN_COMPILER_OPTIONS = $(CHICKEN_PROGRAM_OPTIONS) -extend private-namespace.scm
CHICKEN_UNSAFE_OPTIONS = -unsafe -no-lambda-info

# targets

CHICKEN_PROGRAM = $(PROGRAM_PREFIX)chicken$(PROGRAM_SUFFIX)
CSC_PROGRAM = $(PROGRAM_PREFIX)csc$(PROGRAM_SUFFIX)
CSI_PROGRAM = $(PROGRAM_PREFIX)csi$(PROGRAM_SUFFIX)
CHICKEN_PROFILE_PROGRAM = $(PROGRAM_PREFIX)chicken-profile$(PROGRAM_SUFFIX)
CHICKEN_SETUP_PROGRAM = $(PROGRAM_PREFIX)chicken-setup$(PROGRAM_SUFFIX)
CHICKEN_BUG_PROGRAM = $(PROGRAM_PREFIX)chicken-bug$(PROGRAM_SUFFIX)

ifdef STATICBUILD
CHICKEN_STATIC_EXECUTABLE = $(CHICKEN_PROGRAM)$(EXE)
CSI_STATIC_EXECUTABLE = $(CSI_PROGRAM)$(EXE)
CHICKEN_SHARED_EXECUTABLE = $(CHICKEN_PROGRAM)-shared$(EXE)
CSI_SHARED_EXECUTABLE = $(CSI_PROGRAM)-shared$(EXE)
TARGETLIBS ?= libchicken$(A) libuchicken$(A)
TARGETS ?= $(TARGETLIBS) $(CHICKEN_STATIC_EXECUTABLE) \
	$(CSI_STATIC_EXECUTABLE) $(CHICKEN_PROFILE_PROGRAM)$(EXE) \
	$(CSC_PROGRAM)$(EXE) \
	chicken.info $(CHICKEN_BUG_PROGRAM)$(EXE)
else
CHICKEN_STATIC_EXECUTABLE = $(CHICKEN_PROGRAM)-static$(EXE)
CSI_STATIC_EXECUTABLE = $(CSI_PROGRAM)-static$(EXE)
CHICKEN_SHARED_EXECUTABLE = $(CHICKEN_PROGRAM)$(EXE)
CSI_SHARED_EXECUTABLE = $(CSI_PROGRAM)$(EXE)
TARGETLIBS ?= libchicken$(A) libuchicken$(A) \
	$(LIBCHICKEN_SO_FILE) $(LIBUCHICKEN_SO_FILE)
TARGETS ?= $(TARGETLIBS) $(CHICKEN_SHARED_EXECUTABLE) \
	$(CSI_SHARED_EXECUTABLE) $(CHICKEN_PROFILE_PROGRAM)$(EXE) \
	$(CSC_PROGRAM)$(EXE) $(CHICKEN_SETUP_PROGRAM)$(EXE) chicken.info \
	$(CHICKEN_BUG_PROGRAM)$(EXE)
endif

# main rule

.PHONY: all

all: $(TARGETS)

# generic part of chicken-config.h

ifndef CUSTOM_CHICKEN_DEFAULTS
chicken-defaults.h:
	echo "/* generated */" >$@
	echo "#define C_BUILD_TAG \"$(BUILD_TAG)\"" >>$@
	echo "#ifndef C_INSTALL_CC" >>$@
	echo "# define C_INSTALL_CC \"$(C_COMPILER)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_INSTALL_CXX" >>$@
	echo "# define C_INSTALL_CXX \"$(CXX_COMPILER)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_INSTALL_CFLAGS" >>$@
	echo "# define C_INSTALL_CFLAGS \"$(C_COMPILER_OPTIONS) $(C_COMPILER_OPTIMIZATION_OPTIONS)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_INSTALL_LDFLAGS" >>$@
	echo "# define C_INSTALL_LDFLAGS \"$(LINKER_OPTIONS) $(LINKER_OPTIMIZATION_OPTIONS)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_INSTALL_SHARE_HOME" >>$@
	echo "# define C_INSTALL_SHARE_HOME \"$(DATADIR)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_INSTALL_BIN_HOME" >>$@
	echo "# define C_INSTALL_BIN_HOME \"$(BINDIR)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_INSTALL_EGG_HOME" >>$@
	echo "# define C_INSTALL_EGG_HOME \"$(EGGDIR)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_INSTALL_LIB_HOME" >>$@
	echo "# define C_INSTALL_LIB_HOME \"$(LIBDIR)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_INSTALL_STATIC_LIB_HOME" >>$@
	echo "# define C_INSTALL_STATIC_LIB_HOME \"$(LIBDIR)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_INSTALL_INCLUDE_HOME" >>$@
	echo "# define C_INSTALL_INCLUDE_HOME \"$(INCDIR)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_INSTALL_MORE_LIBS" >>$@
	echo "# define C_INSTALL_MORE_LIBS \"$(LIBRARIES)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_INSTALL_MORE_STATIC_LIBS" >>$@
	echo "# define C_INSTALL_MORE_STATIC_LIBS \"$(LIBRARIES)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_DEFAULT_TARGET_STACK_SIZE" >>$@
	echo "# define C_DEFAULT_TARGET_STACK_SIZE $(NURSERY)" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_STACK_GROWS_DOWNWARD" >>$@
	echo "# define C_STACK_GROWS_DOWNWARD $(STACKDIRECTION)" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_TARGET_MORE_LIBS" >>$@
	echo "# define C_TARGET_MORE_LIBS \"$(TARGET_LIBRARIES)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_TARGET_MORE_STATIC_LIBS" >>$@
	echo "# define C_TARGET_MORE_STATIC_LIBS \"$(TARGET_LIBRARIES)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_TARGET_CC" >>$@
	echo "# define C_TARGET_CC \"$(TARGET_C_COMPILER)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_TARGET_CXX" >>$@
	echo "# define C_TARGET_CXX \"$(TARGET_CXX_COMPILER)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_TARGET_CFLAGS" >>$@
	echo "# define C_TARGET_CFLAGS \"$(TARGET_C_COMPILER_OPTIONS) $(TARGET_C_COMPILER_OPTIMIZATION_OPTIONS)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_TARGET_LDFLAGS" >>$@
	echo "# define C_TARGET_LDFLAGS \"$(TARGET_LINKER_OPTIONS) $(TARGET_LINKER_OPTIMIZATION_OPTIONS)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_CROSS_CHICKEN" >>$@
	echo "# define C_CROSS_CHICKEN $(CROSS_CHICKEN)" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_TARGET_LIB_HOME" >>$@
	echo "# define C_TARGET_LIB_HOME \"$(TARGET_PREFIX)/lib\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_TARGET_RUN_LIB_HOME" >>$@
	echo "# define C_TARGET_RUN_LIB_HOME \"$(TARGET_RUN_PREFIX)/lib\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_TARGET_SHARE_HOME" >>$@
	echo "# define C_TARGET_SHARE_HOME \"$(TARGET_PREFIX)/share\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_TARGET_INCLUDE_HOME" >>$@
	echo "# define C_TARGET_INCLUDE_HOME \"$(TARGET_PREFIX)/include\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_TARGET_STATIC_LIB_HOME" >>$@
	echo "# define C_TARGET_STATIC_LIB_HOME \"$(TARGET_PREFIX)/lib\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_CHICKEN_PROGRAM" >>$@
	echo "# define C_CHICKEN_PROGRAM \"$(CHICKEN_PROGRAM)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_CSC_PROGRAM" >>$@
	echo "# define C_CSC_PROGRAM \"$(CSC_PROGRAM)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_CSI_PROGRAM" >>$@
	echo "# define C_CSI_PROGRAM \"$(CSI_PROGRAM)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_CHICKEN_PROFILE_PROGRAM" >>$@
	echo "# define C_CHICKEN_PROFILE_PROGRAM \"$(CHICKEN_PROFILE_PROGRAM)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_CHICKEN_SETUP_PROGRAM" >>$@
	echo "# define C_CHICKEN_SETUP_PROGRAM \"$(CHICKEN_SETUP_PROGRAM)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_CHICKEN_BUG_PROGRAM" >>$@
	echo "# define C_CHICKEN_BUG_PROGRAM \"$(CHICKEN_BUG_PROGRAM)\"" >>$@
	echo "#endif" >>$@
endif
#
