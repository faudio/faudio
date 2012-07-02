
all:   lw-gen examples
install: lw-gen
clean: clean-lw-gen clean-examples

INSTALL_DIR = ../../dist/audio-engine-lisp/audio-engine
install:
	rm -rf   $(INSTALL_DIR); \
	mkdir -p $(INSTALL_DIR); \
	find . -name "*.lisp" | cpio -pvdm $(INSTALL_DIR); \
	find . -name "README" | cpio -pvdm $(INSTALL_DIR); \
	echo "Successfully installed audio-engine to $(INSTALL_DIR)"

lw-gen:
	touch backends/lispworks-gen.lisp;
	tools/lwfli \
		-p "audio" \
		-I "../../include/" -x "-DSCL_UNICODE;-DSCL_LISPWORKS" \
		"../../include/sclaudio.h" \
		"backends/lispworks-gen.lisp";
	sleep 0.5;                         
	touch backends/lispworks-gen.lisp; \
    sed -e "s/\"audio\"/:audio/" -i "" backends/lispworks-gen.lisp;

clean-lw-gen:
		rm -f backends/lispworks-gen.lisp

.PHONY: examples
examples: lw-gen
		tools/lisp examples/playnotes/deliver.lisp;
		tools/lisp examples/list-au/deliver.lisp;

clean-examples:
		rm -f playnotes;
		rm -f list-au;