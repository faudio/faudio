
all: test_plot

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

test_plot:
	$(call CONVERT_GRAPH,test_plot)


clean:
	rm $(OUTPUT)/*.pdf; \
	rm $(OUTPUT)/*.png; \
