
main : lispdoc doxygen

lispdoc :
	# tool/lispdoc.hs language_bindings/lisp/*.lisp > Lisp.md;

doxygen :
	mkdir -p build/html; \
	cp include/styles.css build/html/; \
	doxygen Doxyfile	
