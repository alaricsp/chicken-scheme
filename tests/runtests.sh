#!/bin/sh
# runtests.sh

set -e
TEST_DIR=`pwd`
export DYLD_LIBRARY_PATH=${TEST_DIR}/..
export LD_LIBRARY_PATH=${TEST_DIR}/..
compile="../csc -compiler ../chicken -v -I.. -L.. -include-path .. -o a.out"
compile_s="../csc -s -compiler ../chicken -v -I.. -L.. -include-path .."
interpret="../csi -include-path .."

echo "======================================== compiler tests ..."
$compile compiler-tests.scm && ./a.out

echo "======================================== compiler tests (2) ..."
$compile compiler-tests.scm -lambda-lift && ./a.out

echo "======================================== runtime tests ..."
$interpret -s apply-test.scm
$compile test-gc-hooks.scm && ./a.out

echo "======================================== library tests ..."
$interpret -s library-tests.scm

echo "======================================== syntax tests ..."
$interpret -s syntax-tests.scm

echo "======================================== syntax tests (compiled) ..."
$compile syntax-tests.scm && ./a.out

echo "======================================== syntax tests (2, compiled) ..."
$compile syntax-tests-2.scm && ./a.out

#echo "======================================== meta-syntax tests ..."
#$interpret -bnq meta-syntax-test.scm -e '(import foo)' -e '(bar 1 2)'
#$compile_s -s meta-syntax-test.scm -j foo
#$compile_s -s foo.import.scm
#$interpret -bnq -e '(require-library meta-syntax-test)' -e '(import foo)' -e '(bar 1 2)'

echo "======================================== import library tests ..."
rm -f foo.import.*
$compile import-library-test1.scm -emit-import-library foo
$interpret -s import-library-test2.scm
$compile_s -s foo.import.scm -o foo.import.so
$interpret -s import-library-test2.scm
$compile import-library-test2.scm && ./a.out

echo "======================================== syntax tests (matchable) ..."
$interpret matchable.scm -s match-test.scm

echo "======================================== syntax tests (loopy-loop) ..."
$interpret -s loopy-test.scm

echo "======================================== syntax tests (r5rs_pitfalls) ..."
$interpret -i -s r5rs_pitfalls.scm

echo "======================================== module tests ..."
$interpret -include-path .. -s module-tests.scm

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
$interpret -bqn ec.scm ec-tests.scm
$compile_s ec.scm -emit-import-library ec -o ec.so
$compile_s ec.import.scm -o ec.import.so 
$interpret -bnq ec.so ec-tests.scm
# $compile ec-tests.scm && ./a.out        # takes ages to compile

echo "======================================== hash-table tests ..."
$interpret -s hash-table-tests.scm

echo "======================================== port tests ..."
$interpret -s port-tests.scm

echo "======================================== fixnum tests ..."
$compile fixnum-tests.scm && ./a.out

echo "======================================== srfi-18 tests ..."
$interpret -s srfi-18-tests.scm

echo "======================================== path tests ..."
$compile path-tests.scm && ./a.out

echo "======================================== regular expression tests ..."
$interpret -bnq test-irregex.scm

echo "======================================== r4rstest ..."
$interpret -e '(set! ##sys#procedure->string (constantly "#<procedure>"))' \
  -i -s r4rstest.scm >r4rstest.log
diff -u r4rstest.out r4rstest.log

echo "======================================== finalizer tests ..."
$interpret -s test-finalizers.scm

echo "======================================== finalizer tests (2) ..."
$compile test-finalizers-2.scm && ./a.out

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
