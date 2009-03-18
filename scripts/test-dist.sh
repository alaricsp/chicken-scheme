#!/bin/sh
### test-dist.sh - test distribution tarball
#
# usage: test-dist.sh [-bootstrap] PLATFORM TARBALL

set -e
set -x

pwdopts=
bootstrap=

if test "$1" == "-bootstrap"; then
    bootstrap=1
    shift
fi

if test $# \!= 2; then
    echo "usage: test-dist.sh [-bootstrap] PLATFORM TARBALL"
    exit 1
fi

platform="$1"
tarball="$2"

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

if test -n "$bootstrap"; then
    $makeprg PLATFORM=$platform PREFIX=`pwd $pwdopts` DEBUGBUILD=1 bootstrap
    $makeprg PLATFORM=$platform PREFIX=`pwd $pwdopts` DEBUGBUILD=1 CHICKEN=./chicken-boot confclean all install
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

echo "looks good."
