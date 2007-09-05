# Makefile - toplevel makefile
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


.PHONY: all clean distclean spotless install uninstall

ifndef PLATFORM
all clean spotless distclean install uninstall:
	@echo "no PLATFORM given."
	@echo ""
	@echo "please select your target platform by running one of the following commands:"
	@echo ""
	@echo "  $(MAKE) PLATFORM=linux"
	@echo "  $(MAKE) PLATFORM=bsd"
	@echo "  $(MAKE) PLATFORM=macosx"
	@echo "  $(MAKE) PLATFORM=mingw"
	@echo "  $(MAKE) PLATFORM=cross-linux-mingw"
	@echo ""
	@exit 1
else
all:
	$(MAKE) -f Makefile.$(PLATFORM) all
clean:
	$(MAKE) -f Makefile.$(PLATFORM) clean
distclean:
	$(MAKE) -f Makefile.$(PLATFORM) distclean
spotless:
	$(MAKE) -f Makefile.$(PLATFORM) spotless
install:
	$(MAKE) -f Makefile.$(PLATFORM) install
uninstall:
	$(MAKE) -f Makefile.$(PLATFORM) uninstall
endif