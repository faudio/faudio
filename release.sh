VERSION=2.x.x

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
    git add docs         
    git commit -m "Updated docs"
    git push
    popd
    

else
	echo "Cleaning"
	rm -rf faudio-*.tar.gz
fi