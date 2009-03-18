#!/bin/sh
### guess-platform.sh - guess correct setting for PLATFORM


if test "$MSYSTEM" == MINGW32; then
    echo mingw-msys
    exit
fi

case `uname` in
    *Linux*) 
	echo "linux";;
    *BSD*)
	echo "bsd";;
    Darwin)
	echo "macosx";;
    *)
	echo "cannot figure out correct PLATFORM"
	exit 1;;
    # missing: solaris, cygwin
esac
