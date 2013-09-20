
# all : html
all : html-fast
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

html: graphs plots doxygen dirs
html-fast: doxygen dirs

pdf: graphs plots doxygen dirs
	pushd doc/build/latex; \
	make; \
	cp refman.pdf "../book/Audio Engine.pdf"; \
	popd;

graphs:
	make -f doc/graphs/Makefile;

plots:
	make -f doc/plots/Makefile;

doxygen:
	doxygen doc/Doxyfile \
	&& mkdir -p doc/build/html/images \
	&& cp doc/include/styles.css doc/build/html/styles.css; 

dirs:
	mkdir -p doc/build/html \
    	&& mkdir -p doc/build/latex \
    	&& mkdir -p doc/build/book
clean:
	rm -rf doc/build/html \
    	&& rm -rf doc/build/latex \
    	&& rm -rf doc/build/book
