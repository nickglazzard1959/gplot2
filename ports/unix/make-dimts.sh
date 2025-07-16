#!/bin/bash
#
# Process PLDIMTS source in dimts-library to create gfortran
# compatible code, then compile that code.
#

echo "Build DIMFILM test programs."
#
if [ -z "${OPT}" ]; then
    OPT=0
fi

rm -rf dimts-source
mkdir dimts-source
#
echo "... getting dimtest.f, lstdfon.f and pre-processing them"
asciify -d -m dimts-map.json ../../dimts-library/dimtest.f dimts-source/dimtest.f
asciify -d -m dimts-map.json ../../dimts-library/lstdfon.f dimts-source/lstdfon.f
#
cd dimts-source
echo "... compiling dimtest.f and lstdfon.f"
gfortran -g -c -O${OPT} -Wall -Wno-unused-dummy-argument -std=legacy -cpp -DPORTF77 dimtest.f
gfortran -g -c -O${OPT} -Wall -Wno-unused-dummy-argument -std=legacy -cpp -DPORTF77 lstdfon.f
#
echo "... linking dimtest and lstdfon"
gfortran -g -o ../dimtest dimtest.o ../lib/dimfm.a ../lib/grdev.a ../lib/support.a ../lib/utils.a
gfortran -g -o ../lstdfon lstdfon.o ../lib/dimfm.a ../lib/grdev.a ../lib/support.a ../lib/utils.a
cd ..
#
echo "... done."
