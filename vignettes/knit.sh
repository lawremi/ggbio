#!/bin/bash
Rdevscript -e "library(knitr);knit('ggbio.Rnw')"
pdflatex ggbio.tex
pdflatex ggbio.tex
evince ggbio.pdf
