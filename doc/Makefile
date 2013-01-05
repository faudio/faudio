
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

doxygen :
	doxygen doc/Doxyfile; \
	cp doc/include/styles.css doc/build/html/; 

html : doxygen
pdf: doxygen
	pushd doc/build/latex; \
	make; \
	cp refman.pdf "../book/Audio Engine.pdf"; \
	popd;
