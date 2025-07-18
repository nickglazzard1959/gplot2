#!/bin/bash
#
# Build a support function library, support.a
# for COS systems.
#

function compile() {
    echo "... compiling $1 ..."
    FNOF="${1%.f}"
    kftc -l ${FNOF}.lst -o ${FNOF}.cal -s -a static ${FNOF}.f
    cal -x -o ${FNOF}.o -l - -i ${FNOF}.cal >> ${FNOF}.lst
    return 0
}

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
compile support.f
#
echo "... creating static library support.a"
lib -l supportliblist.lst -o lib/support.lib support.o
#
echo "... done."
