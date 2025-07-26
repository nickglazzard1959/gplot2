#!/bin/bash
rm -f gplot.aux	gplot.log gplot.out gplot.pdf
xelatex gplot.tex > /dev/null
xelatex gplot.tex > /dev/null
xelatex gplot.tex > /dev/null
rm -f gplot.aux	gplot.log gplot.out
