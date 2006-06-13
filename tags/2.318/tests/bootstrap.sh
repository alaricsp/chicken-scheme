# bootstrap.sh 

set -e

echo "======================================== bootstrapping ..."
darcs dist
rm -fr /tmp/chicken
cp chicken.tar.gz /tmp
pushd /tmp
tar xfz chicken.tar.gz
cd chicken
sh autogen.sh
./configure --disable-shared --prefix=`pwd`/chicken-install
installed_chicken=`which chicken`
make BOOTSTRAP_PATH=`dirname $installed_chicken`
touch *.scm
make
rm -fr /tmp/chicken

echo "======================================== done."
