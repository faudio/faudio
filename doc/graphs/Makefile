
all: device_states device_states2 test_graph

show: all
	open doc/images/*.png;

PROC 	     = cpp -I"/Volumes/DoReMIR/development/osx/audio-engine"
DOT_TO_PDF   = dot -Tpdf
PDF_TO_PNG   = convert -density 300x300 -resize 25%

test_graph:
	$(PROC) <doc/graphs/test_graph.dot | $(DOT_TO_PDF) >doc/images/test_graph.pdf && \
	$(PDF_TO_PNG) doc/images/test_graph.pdf doc/images/test_graph.png;

device_states:
	$(PROC) <doc/graphs/device_states.dot | $(DOT_TO_PDF) >doc/images/device_states.pdf && \
	$(PDF_TO_PNG) doc/images/device_states.pdf doc/images/device_states.png;

device_states2:
	$(PROC) <doc/graphs/device_states2.dot | $(DOT_TO_PDF) >doc/images/device_states2.pdf && \
	$(PDF_TO_PNG) doc/images/device_states2.pdf doc/images/device_states2.png;

