#!/bin/sh
# runtests.sh

set -e
TEST_DIR=`pwd`
export DYLD_LIBRARY_PATH=${TEST_DIR}/..
export LD_LIBRARY_PATH=${TEST_DIR}/..
compile="../csc -compiler ../chicken -v -I.. -L.. -include-path .. -o a.out"
compile_s="../csc -s -compiler ../chicken -v -I.. -L.. -include-path .."
interpret="../csi -include-path .."

echo "======================================== runtime tests ..."
$interpret -s apply-test.scm
$compile test-gc-hooks.scm && ./a.out

echo "======================================== library tests ..."
$interpret -w -s library-tests.scm

echo "======================================== syntax tests ..."
$interpret -w -s syntax-tests.scm

echo "======================================== syntax tests (compiled) ..."
$compile syntax-tests.scm && ./a.out

echo "======================================== syntax tests (matchable) ..."
$interpret -w matchable.scm -s match-test.scm

echo "======================================== syntax tests (loopy-loop) ..."
$interpret -w -s loopy-test.scm

echo "======================================== syntax tests (r5rs_pitfalls) ..."
$interpret -iw -s r5rs_pitfalls.scm

echo "======================================== module tests ..."
$interpret -include-path .. -w -s module-tests.scm

echo "======================================== module tests (compiled) ..."
$compile module-tests-compiled.scm && ./a.out

echo "======================================== module tests (chained) ..."
rm -f m*.import.* test-chained-modules.so
$interpret -bnq test-chained-modules.scm
$compile_s test-chained-modules.scm -j m3
$compile_s m3.import.scm
$interpret -bn test-chained-modules.so
$interpret -bn test-chained-modules.so -e '(import m3) (s3)'

echo "======================================== module tests (ec) ..."
rm -f ec.so ec.import.*
$interpret -wbqn ec.scm ec-tests.scm
$compile_s ec.scm -emit-import-library ec -o ec.so
$compile_s ec.import.scm -o ec.import.so 
$interpret -wbnq ec.so ec-tests.scm
# $compile ec-tests.scm && ./a.out        # takes ages to compile

echo "======================================== hash-table tests ..."
$interpret -w -s hash-table-tests.scm

echo "======================================== port tests ..."
$interpret -w -s port-tests.scm

echo "======================================== fixnum tests ..."
$compile fixnum-tests.scm && ./a.out

echo "======================================== srfi-18 tests ..."
$interpret -w -s srfi-18-tests.scm

echo "======================================== path tests ..."
$compile path-tests.scm && ./a.out

echo "======================================== r4rstest ..."
$interpret -e '(set! ##sys#procedure->string (constantly "#<procedure>"))' \
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
	    ../csc $x -compiler ../chicken -C -I.. -L.. -O2 -d0
	    ./`basename $x .scm`;;
    esac
done
cd ${TEST_DIR}

echo "======================================== done."
