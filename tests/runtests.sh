
# runtests.sh 

set -e
export DYLD_LIBRARY_PATH=`pwd`/../.libs
export LD_LIBRARY_PATH=`pwd`/../.libs
compile="../csc -compiler ../chicken -o a.out"

echo "======================================== library-tests ..."
../csi -w -s library-tests.scm

echo "======================================== ffi tests ..."
$compile ffi-test.scm && ./a.out

echo "======================================== r4rstest ..."
../csi -i -s r4rstest.scm >/dev/null

echo "======================================== tinyclos-examples ..."
$compile tinyclos-examples.scm && ./a.out

echo "======================================== locative stress test ..."
$compile locative-stress-test.scm && ./a.out

echo "======================================== benchmarks ..."
pushd ../benchmarks
for x in `ls *.scm`; do
  if test $x != "cscbench.scm"; then
    echo $x
    ../csc $x -O2 -d0 -prologue plists && ./`basename $x .scm` >/dev/null
  fi
done
popd

echo "======================================== done."
