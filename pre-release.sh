
if [ $# -lt 1 ]
then
  echo "Usage: `basename $0` [version]"
  exit $E_BADARGS
else
    VERSION=$1
fi

make test
mate CMakeLists.txt bindings/lisp/faudio.asd doc/Doxyfile
git add -u
git commit -m "v$VERSION"
git tag "v$VERSION"
git push
git push --tags

