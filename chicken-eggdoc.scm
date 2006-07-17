(use eggdoc)

(define license
"Copyright (c) 2000-2006, Felix Winkelmann.  All rights reserved.

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the Software),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED ASIS, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.")

(define email "mailto:felix@call-with-current-continuation.org")

(define doc
  `((eggdoc:begin
     (name "chicken")
     (description (p "The CHICKEN Scheme system"))
     
     (author (url ,email "felix"))

     (history
      (version "2.41" "release version")
      (version "2.325" "install step is done via process-execute to allow replacement of running chicken-setup instance")
      (version "2.321" "IEEE float constant fix")
      (version "2.320" "Removed MSVC support, better prefix handling")
      (version "2.319" "Some changes and a fix in the setup script again [Thanks to John Cowan again]")
      (version "2.318" "Fix in setup script [Thanks to John Cowan]")
      (version "2.317" "Initial release as egg"))

     (download "chicken.egg")

     (documentation
      (p "This extension packages the newest development snapshot of the CHICKEN base system as an egg.")
      (p "When installed via " (tt "chicken-setup") ", the development snapshot will be configured, built and"
	 " installed with the same settings as the previously installed (the current) version of CHICKEN."
	 " To pass extra options to " (tt "configure") ", invoke " (tt "chicken-setup") " with the configuration"
	 " options following " (tt "--") ", like this:"
	 (pre "% chicken-setup chicken -- --without-XYZ ...") )
      (p "You will need " (tt "autoconf") ", " (tt "automake") " and " (tt "libtool") ".") )
     (section "License" (pre ,license)))))

(eggdoc->html doc)
