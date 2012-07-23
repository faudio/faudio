
main : lispdoc doxygen

lispdoc :
	# tool/lispdoc.hs language_bindings/lisp/*.lisp > Lisp.md;

book:
	mkdir -p build/book; \
	pandoc -S --table-of-contents -o "build/book/Audio Engine.pdf" *.md; \

cat-build-scripts :
	;

doxygen :
	mkdir -p build/html; \
	cp include/styles.css build/html/; \
	doxygen Doxyfile	
