#!/bin/sh
# runtests.sh

set -e
TEST_DIR=`pwd`
export DYLD_LIBRARY_PATH=${TEST_DIR}/..
export LD_LIBRARY_PATH=${TEST_DIR}/..

mkdir -p test-repository

# copy files into test-repository (by hand to avoid calling `chicken-install'):

for x in setup-api.so setup-api.import.so setup-download.so \
      setup-download.import.so chicken.import.so lolevel.import.so \
      srfi-1.import.so srfi-4.import.so data-structures.import.so \
      ports.import.so files.import.so posix.import.so \
      srfi-13.import.so srfi-69.import.so extras.import.so \
      regex.import.so srfi-14.import.so tcp.import.so \
      foreign.import.so scheme.import.so srfi-18.import.so \
      utils.import.so csi.import.so irregex.import.so types.db; do
  cp ../$x test-repository
done

$TEST_DIR/../chicken-install -init test-repository export
CHICKEN_REPOSITORY=$TEST_DIR/test-repository
CHICKEN=../chicken

if test -n "$MSYSTEM"; then
    CHICKEN="..\\chicken.exe"
fi

compile="../csc -compiler $CHICKEN -v -I.. -L.. -include-path .. -o a.out"
compile_s="../csc -s -compiler $CHICKEN -v -I.. -L.. -include-path .."
interpret="../csi -n -include-path .."

echo "======================================== compiler tests ..."
$compile compiler-tests.scm
./a.out

echo "======================================== compiler tests (2) ..."
$compile compiler-tests.scm -lambda-lift
./a.out

echo "======================================== scrutiny tests ..."
$compile scrutiny-tests.scm -scrutinize -analyze-only -ignore-repository -types ../types.db 2>scrutiny.out

if test -n "$MSYSTEM"; then
    dos2unix scrutiny.out
fi

# this is sensitive to gensym-names, so make it optional
if test \! -f scrutiny.expected; then
    cp scrutiny.out scrutiny.expected
fi

diff -u scrutiny.out scrutiny.expected || true

echo "======================================== runtime tests ..."
$interpret -s apply-test.scm
$compile test-gc-hooks.scm
./a.out

echo "======================================== library tests ..."
$interpret -s library-tests.scm

echo "======================================== syntax tests ..."
$interpret -s syntax-tests.scm

echo "======================================== syntax tests (compiled) ..."
$compile syntax-tests.scm
./a.out

echo "======================================== syntax tests (2, compiled) ..."
$compile syntax-tests-2.scm
./a.out

#echo "======================================== meta-syntax tests ..."
#$interpret -bnq meta-syntax-test.scm -e '(import foo)' -e '(bar 1 2)'
#$compile_s -s meta-syntax-test.scm -j foo
#$compile_s -s foo.import.scm
#$interpret -bnq -e '(require-library meta-syntax-test)' -e '(import foo)' -e '(bar 1 2)'

echo "======================================== compiler syntax tests ..."
$compile compiler-syntax-tests.scm
./a.out

echo "======================================== import library tests ..."
rm -f foo.import.*
$compile import-library-test1.scm -emit-import-library foo
$interpret -s import-library-test2.scm
$compile_s -s foo.import.scm -o foo.import.so
$interpret -s import-library-test2.scm
$compile import-library-test2.scm
./a.out

echo "======================================== syntax tests (matchable) ..."
$interpret matchable.scm -s match-test.scm

echo "======================================== syntax tests (loopy-loop) ..."
$interpret -s loopy-test.scm

echo "======================================== syntax tests (r5rs_pitfalls) ..."
$interpret -i -s r5rs_pitfalls.scm

echo "======================================== module tests ..."
$interpret -include-path .. -s module-tests.scm

echo "======================================== module tests (compiled) ..."
$compile module-tests-compiled.scm
./a.out

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
# $compile ec-tests.scm
# ./a.out        # takes ages to compile

echo "======================================== hash-table tests ..."
$interpret -s hash-table-tests.scm

echo "======================================== lolevel tests ..."
$interpret -s lolevel-tests.scm

echo "======================================== port tests ..."
$interpret -s port-tests.scm

echo "======================================== fixnum tests ..."
$compile fixnum-tests.scm
./a.out

echo "======================================== srfi-18 tests ..."
$interpret -s srfi-18-tests.scm
# $interpret -s feeley-dynwind.scm

echo "======================================== path tests ..."
$interpret -bnq path-tests.scm

echo "======================================== regular expression tests ..."
$interpret -bnq test-irregex.scm

echo "======================================== r4rstest ..."
$interpret -e '(set! ##sys#procedure->string (constantly "#<procedure>"))' \
  -i -s r4rstest.scm >r4rstest.log

if test -n "$MSYSTEM"; then
    # the windows runtime library prints flonums differently
    tail r4rstest.log
else
    diff -bu r4rstest.out r4rstest.log || true
fi

echo "======================================== compiler/nursery stress test ..."
for s in 100000 120000 200000 250000 300000 350000 400000 450000 500000; do
    echo "  $s"
    ../chicken ../utils.scm -:s$s -output-file tmp.c -include-path .. 
done

echo "======================================== finalizer tests ..."
$interpret -s test-finalizers.scm

echo "======================================== finalizer tests (2) ..."
$compile test-finalizers-2.scm
./a.out

echo "======================================== locative stress test ..."
$compile locative-stress-test.scm
./a.out

echo "======================================== embedding (1) ..."
$compile embedded1.c
./a.out

echo "======================================== embedding (2) ..."
$compile -e embedded2.scm
./a.out

echo "======================================== benchmarks ..."
cd ../benchmarks
for x in `ls *.scm`; do
    case $x in
	"cscbench.scm");;
	"plists.scm");;
	*)
	    echo $x
	    ../csc $x -compiler $CHICKEN -C -I.. -L.. -O3 -d0
	    ./`basename $x .scm`;;
    esac
done
cd ${TEST_DIR}

echo "======================================== done."
