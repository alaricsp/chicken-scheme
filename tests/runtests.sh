#!/bin/sh
# runtests.sh

set -e
TEST_DIR=`pwd`
export DYLD_LIBRARY_PATH=${TEST_DIR}/..
export LD_LIBRARY_PATH=${TEST_DIR}/..
compile="../csc -compiler ../chicken -C -I.. -L.. -o a.out"

echo "======================================== runtime tests ..."
../csi -s apply-test.scm
$compile test-gc-hooks.scm && ./a.out

echo "======================================== library tests ..."
../csi -w -s library-tests.scm

echo "======================================== syntax tests ..."
../csi -w -s syntax-tests.scm
../csi -w matchable.scm -s match-test.scm
../csi -w -s loopy-test.scm

echo "======================================== module tests ..."
../csi -w -s module-tests.scm

echo "======================================== hash-table tests ..."
../csi -w -s hash-table-tests.scm

echo "======================================== port tests ..."
../csi -w -s port-tests.scm

echo "======================================== fixnum tests ..."
$compile fixnum-tests.scm && ./a.out

echo "======================================== srfi-18 tests ..."
../csi -w -s srfi-18-tests.scm

echo "======================================== path tests ..."
$compile path-tests.scm && ./a.out

echo "======================================== r4rstest ..."
../csi -e '(set! ##sys#procedure->string (constantly "#<procedure>"))' \
  -i -s r4rstest.scm >r4rstest.log
diff -u r4rstest.out r4rstest.log

echo "======================================== locative stress test ..."
$compile locative-stress-test.scm && ./a.out

echo "======================================== embedding (1) ..."
$compile embedded1.c && ./a.out

echo "======================================== embedding (2) ..."
$compile -e embedded2.scm && ./a.out

echo "======================================== benchmarks ..."
cd ../benchmarks
for x in `ls *.scm`; do
    case $x in
	"cscbench.scm");;
	"plists.scm");;
	*)
	    echo $x
	    ../csc $x -compiler ../chicken -C -I.. -L.. -O2 -d0 && \
		./`basename $x .scm`;;
    esac
done
cd ${TEST_DIR}

echo "======================================== done."
