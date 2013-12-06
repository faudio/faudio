# VERSION=2.x.x

if [ $# -ne 1 ]
then
  echo "Usage: `basename $0` [version]"
  exit $E_BADARGS
else
    VERSION=$1
fi


if [[ $* != *--clean* ]]; then

    rm -rf distribute
    mkdir -p distribute
    mkdir -p distribute/bin
    mkdir -p distribute/include
    mkdir -p distribute/bindings
    # mkdir -p distribute/lib
    mkdir -p distribute/Frameworks

    cp -R include distribute
    rm distribute/include/config.h.in

    cp -R bindings/lisp distribute/bindings
    find distribute/bindings/lisp -type f -name '*~' -exec rm -f '{}' \;

    cp -R build/Frameworks/Faudio.framework distribute/Frameworks

    cp -R build/bin distribute
    rm distribute/bin/sbcl

    mv distribute faudio
    tar -pvczf faudio-$VERSION.tar.gz faudio
    rm -rf faudio
    
    make modules doc
    pushd pages

    git rm -rf docs
    cp -R ../doc/build/html/ docs
    mkdir -p versions/$VERSION
    cp -R ../doc/build/html/ versions/$VERSION/docs

    git add docs versions # -u does NOT work
    git commit -m "Updated docs"
    git push
    popd

	open -a "Firefox" "https://github.com/hanshoglund/faudio/releases/new"
else
    echo "Cleaning"
    rm -rf faudio-*.tar.gz
fi