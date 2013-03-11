
all: test_plot

show: all
	open $(OUTPUT)/*.png;

ROOT 	        = $(shell pwd)
PROC 	     	= cpp -I"$(ROOT)"
PLOT_TO_PS   	= gnuplot
PS_TO_PDF	= ps2pdf
PDF_TO_PNG   	= convert -density 150x150 -resize 50%
INPUT 	     	= doc/plots
OUTPUT	     	= doc/images

CLEAN_PLOT	= rm -f $(OUTPUT)/$(1).ps \
			&& \
		  rm -f $(OUTPUT)/$(1).pdf \
			&& \
		  rm -f $(OUTPUT)/$(1).png

CONVERT_PLOT    = cat $(INPUT)/head.plot $(INPUT)/$(1).plot | $(PLOT_TO_PS) >$(OUTPUT)/$(1).ps \
			&& \
		  $(PS_TO_PDF) $(OUTPUT)/$(1).ps $(OUTPUT)/$(1).pdf \
			&& \
		  $(PDF_TO_PNG) $(OUTPUT)/$(1).pdf $(OUTPUT)/$(1).png;

head:
	echo $(HEAD)
test_plot:  
	$(call CLEAN_PLOT,test_data) && \
	$(call CONVERT_PLOT,test_data)


clean:
	rm $(OUTPUT)/*.pdf; \
	rm $(OUTPUT)/*.png; \
