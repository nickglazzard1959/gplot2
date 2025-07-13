#!/bin/bash
#
# Build a support function library, support.a
# for Unix-like systems.
#

echo "Build SUPPORT library."
#
if [ -z "${OPT}" ]; then
    OPT=0
fi
if [ ! -d lib ]; then
    mkdir lib
fi
#
echo "... compiling support.f"
gfortran -g -c -O${OPT} -Wall -Wno-unused-dummy-argument -std=legacy -cpp -DPORTF77 support.f
#
echo "... compiling support_c.c"
gcc -g -c support_c.c
#
echo "... creating static library support.a"
ar rcs lib/support.a support.o support_c.o
cd ..
#
echo "... done."
