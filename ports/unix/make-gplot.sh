#!/bin/bash
#
# Process PLGPLOT source in gplot-library to create gfortran
# compatible code, then compile that code.
#

echo "Build GPLOT program."
#
if [ -z "${OPT}" ]; then
    OPT=0
fi

rm -rf gplot-source
mkdir gplot-source
#
echo "... getting ftgplot.f and pre-processing it"
asciify -d -m gplot-map.json ../../gplot-library/ftgplot.f gplot-source/ftgplot.f
#
cd gplot-source
echo "... compiling ftgplot.f"
gfortran -g -c -O${OPT} -Wall -Wno-unused-dummy-argument -std=legacy -cpp -DPORTF77 ftgplot.f
#
echo "... linking gplot"
gfortran -g -o ../gplot ftgplot.o ../lib/dimfm.a ../lib/grdev.a ../lib/support.a ../lib/utils.a
cd ..
#
echo "... done."
