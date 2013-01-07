
# all : pdf html
# 
# 
# pdf:
# 	mkdir -p build/book; \
# 	pandoc -S --table-of-contents -o "build/book/Audio Engine.pdf" \
# 		Introduction.md \
# 		Overview.md \
# 		Sessions\ and\ devices.md \
# 		Signals\ and\ processors.md \
# 		Scheduling.md \
# 		Build\ and\ install.md;

html: graphs doxygen

graphs:
	make -f doc/graphs/Makefile;

doxygen:
	doxygen doc/Doxyfile \
	&& mkdir -p doc/build/html/images \
	&& cp doc/images/device_states.png doc/build/html/images/device_states.png \
	&& cp doc/images/device_states2.png doc/build/html/images/device_states2.png \
	&& cp doc/include/styles.css doc/build/html/; 

pdf: doxygen
	pushd doc/build/latex; \
	make; \
	cp refman.pdf "../book/Audio Engine.pdf"; \
	popd;
