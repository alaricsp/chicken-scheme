#!/bin/sh
### test-dist.sh - test distribution tarball
#
# usage: test-dist.sh [-bootstrap] PLATFORM [TARBALL]

set -e

pwdopts=
bootstrap=

if test "$1" == "-bootstrap"; then
    bootstrap=1
    shift
fi

case $# in
    1|2) ;;
    *) 
	echo "usage: test-dist.sh [-bootstrap] PLATFORM [TARBALL]"
	exit 1;;
esac

platform="$1"
tarball="$2"
makeprg=gmake

# use gmake, if available
if test -z `which gmake`; then
    makeprg=make
fi

# need Windows-style drive letter on mingw/msys
if test -n "$MSYSTEM"; then
    pwdopts=-W
fi

# bootstrap, if desired
prefix=`pwd $pwdopts`/tmp-test-dist

if test \! -x "$prefix/bin/csi"; then
    echo "no csi at ${prefix} - please build and install chicken first"
    exit 1
fi

for ext in htmlprag matchable; do
    if test `$prefix/bin/csi -p "(extension-information '${ext})"` == "#f"; then
	$prefix/bin/chicken-install $ext
    fi
done

if test -n "$bootstrap"; then
    $makeprg PLATFORM=$platform PREFIX=$prefix DEBUGBUILD=1 bootstrap
    $makeprg PLATFORM=$platform PREFIX=$prefix DEBUGBUILD=1 CHICKEN=./chicken-boot confclean all install
fi

# if no tarball given, create one
if test -z "$tarball"; then
    $prefix/bin/csi -s scripts/makedist.scm --make=$makeprg --platform=$platform
    tarball=chicken-`cat buildversion`.tar.gz
fi

# prepare testing directory
if test -d tmp-test-dist; then
    rm -fr tmp-test-dist/*
fi

mkdir -p tmp-test-dist
cp "$tarball" tmp-test-dist

# unpack and enter
cd tmp-test-dist
tar xvfz "$tarball"
cd `basename "$tarball" .tar.gz`

# build #1
$makeprg PLATFORM=$platform PREFIX=$prefix DEBUGBUILD=1 all install
# check #1
$makeprg PLATFORM=$platform PREFIX=$prefix DEBUGBUILD=1 check
# build once again with freshly built compiler
touch *.scm
$makeprg PLATFORM=$platform PREFIX=$prefix DEBUGBUILD=1 CHICKEN=$prefix/bin/chicken all install
# and check...
$makeprg PLATFORM=$platform PREFIX=$prefix DEBUGBUILD=1 check

# Install a few eggs
$prefix/bin/chicken-install -test prometheus
$prefix/bin/chicken-install opengl

echo "looks good."
