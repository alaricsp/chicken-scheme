# build-osx.sh - usage: sh distribution/build-osx.sh

set -e

echo "======================================== bootstrapping ..."
darcs dist
rm -fr /tmp/chicken
cp chicken.tar.gz /tmp
pushd /tmp
tar xfz chicken.tar.gz
cd chicken
sh autogen.sh
./configure --prefix=/usr
installed_chicken=`which chicken`
make BOOTSTRAP_PATH=`dirname $installed_chicken`
popd
cd distribution
echo "======================================== copying files ..."
rm -fr files
mkdir -p files/usr/bin files/usr/lib files/usr/share/chicken/doc files/usr/man/man1 files/usr/include
bindir=/tmp/chicken/.libs
cp $bindir/csc $bindir/chicken $bindir/csi $bindir/chicken-setup $bindir/chicken-profile files/usr/bin
cp $bindir/libchicken.0.dylib files/usr/lib
cp $bindir/libuchicken.0.dylib files/usr/lib
cp $bindir/libchicken.a files/usr/lib
cp $bindir/libuchicken.a files/usr/lib
pushd files/usr/lib
ln -s libchicken.0.dylib libchicken.0.0.0.dylib
ln -s libchicken.0.dylib libchicken.dylib
ln -s libuchicken.0.dylib libuchicken.0.0.0.dylib
ln -s libuchicken.0.dylib libuchicken.dylib
popd
cp /usr/lib/libffi* files/usr/lib
cp /usr/include/ffi* files/usr/include
cp /tmp/chicken/*.exports files/usr/share/chicken
cp /tmp/chicken/chicken.h /tmp/chicken/chicken-config.h /tmp/chicken/chicken-defaults.h files/usr/include
cp /tmp/chicken/chicken-ffi-macros.scm /tmp/chicken/chicken-more-macros.scm files/usr/share/chicken
cp /tmp/chicken/*.1 files/usr/man/man1
# cp ???/chicken.html files/usr/share/chicken/doc
cp -r /tmp/chicken/html /tmp/chicken/README ../ChangeLog /tmp/chicken/NEWS files/usr/share/chicken/doc
#rm -fr /tmp/chicken

echo "======================================== done."
