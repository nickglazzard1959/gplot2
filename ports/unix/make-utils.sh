#!/bin/bash
#
# Process PLUTILS source in utils-library to create gfortran
# compatible code, then compile that code and build a static
# library, utils.a
#

echo "Build UTILS library."
#
if [ -z "${OPT}" ]; then
    OPT=0
fi
if [ ! -d lib ]; then
    mkdir lib
fi
rm -rf utils-source
mkdir utils-source
# N.B. do NOT asciify chars.f.
cp ../../utils-library/chars.f utils-source/chars.f
cp ../../utils-library/cmchars.cmn utils-source/cmchars.cmn
#
cd utils-source
echo "... compiling chars.f"
gfortran -g -c -O${OPT} -Wall -std=legacy -cpp -DPORTF77 chars.f
#
echo "... creating static library utils.a"
ar rcs ../lib/utils.a chars.o
cd ..
#
echo "... done."
