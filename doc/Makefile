
main : lispdoc doxygen

lispdoc :
	doc/tool/lispdoc.hs language_bindings/lisp/audio.lisp > doc/Lisp.md;

doxygen :
	mkdir -p doc/build/html; \
	cp doc/include/styles.css doc/build/html/; \
	doxygen doc/Doxyfile	
