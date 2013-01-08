
all: device_states device_states2 test_graph dsp seq par loop split unary binary ternary

show: all
	open $(OUTPUT)/*.png;

PROC 	     	= cpp -I"/Volumes/DoReMIR/development/osx/audio-engine"
DOT_TO_PDF   	= dot -Tpdf
PDF_TO_PNG   	= convert -density 300x300 -resize 25%
INPUT 	     	= doc/graphs
OUTPUT	     	= doc/images

CONVERT_GRAPH   = $(PROC) <$(INPUT)/$(1).dot | $(DOT_TO_PDF) >$(OUTPUT)/$(1).pdf \
	          && \
		  $(PDF_TO_PNG) $(OUTPUT)/$(1).pdf $(OUTPUT)/$(1).png;

dsp:
	$(call CONVERT_GRAPH,dsp)
seq:
	$(call CONVERT_GRAPH,dsp_seq)
par:
	$(call CONVERT_GRAPH,dsp_par)
loop:
	$(call CONVERT_GRAPH,dsp_loop)
split:
	$(call CONVERT_GRAPH,dsp_split)
unary:
	$(call CONVERT_GRAPH,dsp_unary)
binary:
	$(call CONVERT_GRAPH,dsp_binary)
ternary:
	$(call CONVERT_GRAPH,dsp_ternary)


test_graph:
	$(call CONVERT_GRAPH,test_graph)

device_states:
	$(call CONVERT_GRAPH,device_states)

device_states2:
	$(call CONVERT_GRAPH,device_states2)

clean:
	rm $(OUTPUT)/*.pdf; \
	rm $(OUTPUT)/*.png; \
