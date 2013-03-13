
all: images device_states device_states2 test_graph dsp seq par loop split unary binary ternary gen

show: all
	open $(OUTPUT)/*.png;

ROOT 	        = $(shell pwd)
PROC 	     	= cpp -I"$(ROOT)"
DOT_TO_PDF   	= dot -Tpdf
PDF_TO_PNG   	= convert -density 150x150 -resize 50%
INPUT 	     	= doc/graphs
OUTPUT	     	= doc/images

CONVERT_GRAPH   = $(PROC) <$(INPUT)/$(1).dot | $(DOT_TO_PDF) >$(OUTPUT)/$(1).pdf \
	          	&& \
		  $(PDF_TO_PNG) $(OUTPUT)/$(1).pdf $(OUTPUT)/$(1).png;

images:
	mkdir -p doc/images
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

	$(call CONVERT_GRAPH,dsp_binary)

test_gen:
	$(call CONVERT_GRAPH,test_gen)

gen:
	$(call CONVERT_GRAPH,gen)


clean:
	rm $(OUTPUT)/*.pdf; \
	rm $(OUTPUT)/*.png; \
