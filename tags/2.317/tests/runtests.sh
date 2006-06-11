# runtests.sh 

set -e
export DYLD_LIBRARY_PATH=`pwd`/../.libs
export LD_LIBRARY_PATH=`pwd`/../.libs

echo "======================================== library-tests ..."
../csi -s library-tests.scm

echo "======================================== ffi tests ..."
../csc ffi-test.scm -compiler ../chicken -o a.out && ./a.out

echo "======================================== r4rstest ..."
../csi -i -s r4rstest.scm >/dev/null

echo "======================================== tinyclos-examples ...."
../csc -compiler ../chicken tinyclos-examples.scm -o a.out && ./a.out

echo "======================================== benchmarks ..."
pushd ..
make bench
popd

echo "======================================== done."
