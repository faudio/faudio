
if [ $# -lt 1 ]
then
  echo "Usage: `basename $0` [version]"
  exit $E_BADARGS
else
    VERSION=$1
fi

make test
nano CMakeLists.txt bindings/lisp/faudio.asd doc/Doxyfile

echo git add -u
echo git commit -m "v$VERSION"
echo git tag "v$VERSION"
echo git push
echo git push --tags

