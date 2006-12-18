#!/bin/sh
# runtests.sh 

set -e
export DYLD_LIBRARY_PATH=`pwd`/..
export LD_LIBRARY_PATH=`pwd`/..
compile="../csc -compiler ../chicken -o a.out"

echo "======================================== library tests ..."
../csi -w -s library-tests.scm

echo "======================================== srfi-18 tests ..."
../csi -w -s srfi-18-tests.scm

echo "======================================== ffi tests ..."
$compile ffi-test.scm && ./a.out

echo "======================================== r4rstest ..."
../csi -i -s r4rstest.scm >/dev/null

echo "======================================== locative stress test ..."
$compile locative-stress-test.scm && ./a.out

echo "======================================== benchmarks ..."
pushd ../benchmarks
for x in `ls *.scm`; do
    case $x in
	"cscbench.scm");;
	"plists.scm");;
	*)
	    echo $x
	    ../csc $x -O2 -d0 -prologue plists.scm && ./`basename $x .scm` >/dev/null;;
    esac
done
popd

echo "======================================== done."
