# Makefile to use knitr for package vignettes

# put all PDF targets here, separated by spaces
PDFS= p0_ggbio.pdf p1_tracks.pdf p2_ideogram.pdf p3_overview.pdf p4_txdb.pdf p5_case.pdf

all: $(PDFS) 

clean:
	rm -f *.tex *.bbl *.blg *.aux *.out *.log *.spl *tikzDictionary *.toc
	rm -f figures/ggbio-*	ggbio-*
	
%.pdf: %.Rnw
	$(R_HOME)/bin/Rscript -e "library(knitr); knit2pdf('$*.Rnw')"

