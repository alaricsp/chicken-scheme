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

BINARYVERSION = 1
NURSERY = (128*1024)
STACKDIRECTION = 1
CROSS_CHICKEN = 0

# directories

PREFIX = /usr/local
DESTDIR = $(PREFIX)
BINDIR = $(DESTDIR)/bin
LIBDIR = $(DESTDIR)/lib
SHAREDIR = $(DESTDIR)/share
MANDIR = $(SHAREDIR)/man/man1
INFODIR = $(SHAREDIR)/info
INCDIR = $(DESTDIR)/include
DOCDIR = $(SHAREDIR)/chicken/doc
EGGDIR = $(DESTDIR)/lib/chicken/$(BINARYVERSION)

# commands

C_COMPILER ?= gcc
CXX_COMPILER ?= g++
LINKER ?= $(C_COMPILER)
LIBRARIAN ?= ar
REMOVE_COMMAND ?= rm
ASSEMBLER ?= $(C_COMPILER)
MAKEINFO_PROGRAM ?= -makeinfo
INSTALL_PROGRAM ?= install
MAKEDIR_COMMAND ?= mkdir
POSTINSTALL_STATIC_LIBRARY ?= true
POSTINSTALL_PROGRAM ?= true
INSTALLINFO_PROGRAM ?= -install-info
UNINSTALLINFO_PROGRAM ?= -install-info
HOST_C_COMPILER ?= $(C_COMPILER)

# target variables

TARGET_C_COMPILER ?= $(C_COMPILER)
TARGET_CXX_COMPILER ?= $(CXX_COMPILER)
TARGET_C_COMPILER_OPTIONS ?= $(C_COMPILER_OPTIONS)
TARGET_C_COMPILER_OPTIMIZATION_OPTIONS ?= $(C_COMPILER_OPTIMIZATION_OPTIONS)
TARGET_PREFIX ?= $(PREFIX)
TARGET_LIBRARIES ?= $(LIBRARIES)


# options

ifndef NOPTABLES
C_COMPILER_PTABLES_OPTIONS = -DENABLE_PTABLES
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
LINKER_OPTIONS ?= -L.
LINKER_STATIC_OPTIONS ?= $(LINKERFLAGS)
LINKER_OUTPUT_OPTION ?= -o
LINKER_LIBRARY_OPTION ?= -l
LINKER_LINK_SHARED_LIBRARY_OPTIONS ?= -shared
LIBRARIAN_OPTIONS ?= cru
LIBRARIES ?= -lm
REMOVE_COMMAND_OPTIONS ?= -f
REMOVE_COMMAND_RECURSIVE_OPTIONS ?= -fr
MAKEINFO_PROGRAM_OPTIONS ?= --no-split
INSTALL_PROGRAM_SHARED_LIBRARY_OPTIONS ?= -m755
INSTALL_PROGRAM_STATIC_LIBRARY_OPTIONS ?= -m644
INSTALL_PROGRAM_EXECUTABLE_OPTIONS ?= -m755
INSTALL_PROGRAM_FILE_OPTIONS ?= -m644
MAKEDIR_COMMAND_OPTIONS ?= -p
ASSEMBLER_OPTIONS ?= $(C_COMPILER_OPTIONS)
ASSEMBLER_OUTPUT_OPTION ?= -o
ASSEMBLER_COMPILE_OPTION ?= -c
PRIMARY_LIBCHICKEN ?= libchicken$(SO)
UNINSTALLINFO_PROGRAM_OPTIONS ?= --delete
HOST_C_COMPILER_OUTPUT_OPTION ?= $(C_COMPILER_OUTPUT_OPTION)
LIBCHICKEN_SO_LIBRARIES ?= $(LIBRARIES)
LIBUCHICKEN_SO_LIBRARIES ?= $(LIBRARIES)
LIBCHICKENGUI_SO_LIBRARIES ?= $(LIBRARIES)

# file extensions

O ?= .o
A ?= .a
# EXE =
SO ?= .so

# special files

POSIXFILE ?= posixunix
# CHICKEN_CONFIG_H = chicken-config.h
PCRE_OBJECT_FILES ?= pcre/*.o

ifdef ARCH
HACKED_APPLY = 1
APPLY_HACK_OBJECT = apply-hack.$(ARCH)$(O)
endif

# bootstrapping compiler

CHICKEN = $(PREFIX)/bin/chicken

# Scheme compiler flags

CHICKEN_OPTIONS = -quiet -no-trace -optimize-level 2 -include-path .
CHICKEN_LIBRARY_OPTIONS = $(CHICKEN_OPTIONS) -explicit-use
CHICKEN_PROGRAM_OPTIONS = $(CHICKEN_OPTIONS) -no-lambda-info
CHICKEN_UNSAFE_OPTIONS = -unsafe -no-lambda-info

# various settings

.PHONY: all everything

everything: all

# generic part of chicken-config.h

ifndef CUSTOM_CHICKEN_DEFAULTS
chicken-defaults.h:
	echo "/* generated */" >$@
	echo "#ifndef C_INSTALL_CC" >>$@
	echo "# define C_INSTALL_CC \"$(C_COMPILER)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_INSTALL_CXX" >>$@
	echo "# define C_INSTALL_CXX \"$(CXX_COMPILER)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_INSTALL_CFLAGS" >>$@
	echo "# define C_INSTALL_CFLAGS \"$(C_COMPILER_OPTIONS) $(C_COMPILER_OPTIMIZATION_OPTIONS)\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_INSTALL_SHARE_HOME" >>$@
	echo "# define C_INSTALL_SHARE_HOME \"$(SHAREDIR)\"" >>$@
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
	echo "# define C_TARGET_MORE_LIBS \"$(TAARGET_LIBRARIES)\"" >>$@
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
	echo "#ifndef C_CROSS_CHICKEN" >>$@
	echo "# define C_CROSS_CHICKEN $(CROSS_CHICKEN)" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_TARGET_LIB_HOME" >>$@
	echo "# define C_TARGET_LIB_HOME \"$(TARGET_PREFIX)/lib\"" >>$@
	echo "#endif" >>$@
	echo "#ifndef C_TARGET_RUN_LIB_HOME" >>$@
	echo "# define C_TARGET_RUN_LIB_HOME \"$(TARGET_PREFIX)/lib\"" >>$@
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
endif
