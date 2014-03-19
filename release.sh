# VERSION=2.x.x

if [ $# -lt 1 ]
then
  echo "Usage: `basename $0` [version]"
  exit $E_BADARGS
else
    VERSION=$1
fi

echo "Checking distribution environment..."
command -v make >/dev/null 2>&1 	|| { echo "I require make but it's not installed. Aborting." >&2; exit 1; }
command -v modulo >/dev/null 2>&1   || { echo "I require modulo but it's not installed. Aborting." >&2; exit 1; }
command -v git >/dev/null 2>&1  	|| { echo "I require git but it's not installed. Aborting." >&2; exit 1; }


if [[ $* != *--clean* ]]; then

	echo "Generating headers from module files..."
	make modules

	echo "Building..."
	cmake -DCMAKE_BUILD_TYPE=Release build
	make
	
    echo "Running tests..."
	make test

    echo "Setting up distribution..."
    rm -rf distribute
    mkdir -p distribute
    mkdir -p distribute/bin
    mkdir -p distribute/include
    mkdir -p distribute/bindings
    # mkdir -p distribute/lib
    mkdir -p distribute/Frameworks

    echo "Copying headers..."
    cp -R include distribute
    rm distribute/include/config.h.in

    echo "Generating language bindings..."
	make bindings
    echo "Copying language bindings..."
    cp -R bindings/lisp distribute/bindings
    find distribute/bindings/lisp -type f -name '*~' -exec rm -f '{}' \;
	find distribute/bindings/lisp -type f -name '*.gitignore' -exec rm -f '{}' \;

    echo "Copying binaries..."
    cp -R build/Frameworks/Faudio.framework distribute/Frameworks

    cp -R build/bin distribute
    rm distribute/bin/sbcl

    mv distribute faudio
    tar -pvczf faudio-$VERSION.tar.gz faudio
    rm -rf faudio
    
	if [[ $* != *--no-upload* ]]; then
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
	fi

	echo "Distribution faudio-$VERSION.tar.gz created, please upload."
	open .
	open -a "Google Chrome" "https://github.com/hanshoglund/faudio/releases/new"
else
    echo "Cleaning"
    rm -rf faudio-*.tar.gz
    rm -rf faudio-*.zip
fi