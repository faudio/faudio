
# VERSION=2.x.x

if [ $# -lt 1 ]
then
  echo "Usage: `basename $0` [version]"
  exit $E_BADARGS
else
    VERSION=$1
fi

echo "Uploading documentation"
make doc

pushd pages
git rm -rf docs
cp -R ../doc/build/html/ docs
mkdir -p versions/$VERSION
cp -R ../doc/build/html/ versions/$VERSION/docs

git add docs versions # -u does NOT work
git commit -m "Updated docs"
git push origin gh-pages
popd
