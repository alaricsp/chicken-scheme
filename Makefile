# Makefile - toplevel makefile
#
# Copyright (c) 2007, Felix L. Winkelmann
# Copyright (c) 2008-2009, The Chicken Team
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

STANDARD_TARGETS \
	= all clean distclean spotless install uninstall confclean check \
	  fullcheck dist libs install-libs bootstrap

SRCDIR = .

.PHONY: $(STANDARD_TARGETS)

ifndef PLATFORM
$(STANDARD_TARGETS):
	@echo "no PLATFORM given."
	@echo ""
	@echo "Please select your target platform by running one of the following commands:"
	@echo ""
	@echo "  $(MAKE) PLATFORM=linux"
	@echo "  $(MAKE) PLATFORM=bsd"
	@echo "  $(MAKE) PLATFORM=macosx"
	@echo "  $(MAKE) PLATFORM=mingw-msys"
	@echo "  $(MAKE) PLATFORM=mingw"
	@echo "  $(MAKE) PLATFORM=cygwin"
	@echo "  $(MAKE) PLATFORM=solaris"
	@echo "  $(MAKE) PLATFORM=cross-linux-mingw"
	@echo ""
	@echo "For more information, consult the README file."
	@exit 1
else
all:
	$(MAKE) -f $(SRCDIR)/Makefile.$(PLATFORM) all
clean:
	$(MAKE) -f $(SRCDIR)/Makefile.$(PLATFORM) clean
distclean:
	$(MAKE) -f $(SRCDIR)/Makefile.$(PLATFORM) distclean
spotless:
	$(MAKE) -f $(SRCDIR)/Makefile.$(PLATFORM) spotless
install:
	$(MAKE) -f $(SRCDIR)/Makefile.$(PLATFORM) install
uninstall:
	$(MAKE) -f $(SRCDIR)/Makefile.$(PLATFORM) uninstall
confclean:
	$(MAKE) -f $(SRCDIR)/Makefile.$(PLATFORM) confclean
check:
	$(MAKE) -f $(SRCDIR)/Makefile.$(PLATFORM) check
fullcheck:
	$(MAKE) -f $(SRCDIR)/Makefile.$(PLATFORM) fullcheck
dist:
	$(MAKE) -f $(SRCDIR)/Makefile.$(PLATFORM) distfiles
	csi -s scripts/makedist.scm
libs:
	$(MAKE) -f $(SRCDIR)/Makefile.$(PLATFORM) libs
install-libs:
	$(MAKE) -f $(SRCDIR)/Makefile.$(PLATFORM) install-libs
bootstrap:
	$(MAKE) -f $(SRCDIR)/Makefile.$(PLATFORM) bootstrap
bench:
	$(MAKE) -f $(SRCDIR)/Makefile.$(PLATFORM) bench
endif
