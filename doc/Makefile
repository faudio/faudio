
all : pdf html

html : lispdoc doxygen

lispdoc :
	# tool/lispdoc.hs language_bindings/lisp/*.lisp > Lisp.md;

pdf:
	mkdir -p build/book; \
	pandoc -S --table-of-contents -o "build/book/Audio Engine.pdf" \
		Reference.md \
		Basic\ concepts.md \
		Types\ of\ devices.md \
		Audio\ types.md \
		Midi\ types.md \
		Signals\ and\ processors.md \
		Build\ and\ install.md;

cat-build-scripts :
	;

doxygen :
	mkdir -p build/html; \
	cp include/styles.css build/html/; \
	doxygen Doxyfile	
