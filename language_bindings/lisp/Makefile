
all: lw-gen examples
clean: clean-lw-gen clean-examples

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

clean-examples:
		rm -f playnotes;