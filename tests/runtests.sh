#!/bin/sh
# runtests.sh

set -e
TEST_DIR=`pwd`
export DYLD_LIBRARY_PATH=${TEST_DIR}/..
export LD_LIBRARY_PATH=${TEST_DIR}/..
compile="../csc -compiler ../chicken-static -o a.out"

echo "======================================== runtime tests ..."
../csi -s apply-test.scm

echo "======================================== library tests ..."
../csi -w -s library-tests.scm

echo "======================================== fixnum tests ..."
$compile fixnum-tests.scm && ./a.out

echo "======================================== srfi-18 tests ..."
../csi -w -s srfi-18-tests.scm

echo "======================================== path tests ..."
$compile path-tests.scm && ./a.out

echo "======================================== r4rstest ..."
../csi -i -s r4rstest.scm >/dev/null

echo "======================================== locative stress test ..."
$compile locative-stress-test.scm && ./a.out

echo "======================================== benchmarks ..."
cd ../benchmarks
for x in `ls *.scm`; do
    case $x in
	"cscbench.scm");;
	"plists.scm");;
	*)
	    echo $x
	    ../csc $x -compiler ../chicken-static -O2 -d0 -prologue plists.scm && ./`basename $x .scm` >/dev/null;;
    esac
done
cd ${TEST_DIR}

echo "======================================== done."
