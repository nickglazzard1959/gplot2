#!/bin/bash
#
# Use GPLOT to make the Figures in EPS format.
./make-doc-figures.sh
./make-cheat-sheet.sh
#
# Use LaTeX to make gplot.pdf
rm -f gplot.aux	gplot.log gplot.out gplot.pdf
xelatex gplot.tex > /dev/null
xelatex gplot.tex > /dev/null
xelatex gplot.tex > /dev/null
rm -f gplot.aux	gplot.log gplot.out
