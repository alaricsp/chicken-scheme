#!/bin/sh
# runtests.sh

set -e
TEST_DIR=`pwd`
export DYLD_LIBRARY_PATH=${TEST_DIR}/..
export LD_LIBRARY_PATH=${TEST_DIR}/..
compile="../csc -compiler ../chicken -v -C -I.. -L.. -o a.out"
compile_s="../csc -s -compiler ../chicken -v -C -I.. -L.."

echo "======================================== runtime tests ..."
../csi -s apply-test.scm
$compile test-gc-hooks.scm && ./a.out

echo "======================================== library tests ..."
../csi -w -s library-tests.scm

echo "======================================== syntax tests ..."
../csi -w -s syntax-tests.scm
$compile syntax-tests.scm && ./a.out
../csi -w matchable.scm -s match-test.scm
../csi -w -s loopy-test.scm

echo "======================================== module tests ..."
../csi -w -s module-tests.scm
rm -f ec.so ec.import.*
../csi -wbqn ec.scm ec-tests.scm
$compile_s ec.scm -emit-import-library ec -o ec.so
$compile_s ec.import.scm -o ec.import.so
../csi -wbnq ec.so ec-tests.scm
# $compile ec-tests.scm && ./a.out

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
