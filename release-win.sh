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
command -v 7z >/dev/null 2>&1   	|| { echo "I require 7z but it's not installed. Aborting." >&2; exit 1; }
command -v find >/dev/null 2>&1 	|| { echo "I require find but it's not installed. Aborting." >&2; exit 1; }
command -v git >/dev/null 2>&1  	|| { echo "I require git but it's not installed. Aborting." >&2; exit 1; }


DLLS=`find build/bin/ -regex .*.dll`

if [[ $* != *--clean* ]]; then

	echo "The following files will be copied:

$DLLS

	"

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
    # mkdir -p distribute/lib
    mkdir -p distribute/include
    mkdir -p distribute/bindings

    echo "Copying headers..."
	cp -R include distribute
	rm distribute/include/config.h.in

    echo "Generating language bindings..."
	make bindings
    
	echo "Copying language bindings..."
	cp -R bindings/lisp distribute/bindings
	find distribute/bindings/lisp -type f -name '*~' -exec rm -f '{}' \;
	find distribute/bindings/lisp -type f -name '*.bak' -exec rm -f '{}' \;
	find distribute/bindings/lisp -type f -name '*.gitignore' -exec rm -f '{}' \;

    echo "Copying binaries..."
	cp -R build/bin distribute

	mv distribute faudio
	7z a -tzip faudio-$VERSION.zip faudio
	rm -rf faudio

	echo "Distribution faudio-$VERSION.zip created, please upload here:"
	echo "    https://github.com/faudio/faudio/releases/new"
	# open -a "Firefox" "https://github.com/faudio/faudio/releases/new"
else
    echo "Cleaning"
    rm -rf faudio-*.zip
fi