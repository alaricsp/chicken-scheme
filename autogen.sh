#! /bin/sh
set -x
libtoolize --force --copy --automake
aclocal
autoheader
automake --foreign -a --copy
autoconf
