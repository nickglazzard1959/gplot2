#!/bin/bash
#
# Process PLDIMTS source in dimts-library to create kftc
# compatible code, then cross-compile that code and build a static
# library, dimts.lib
#
function get_file() {
    echo "... getting $1"
    asciify -d -m dimts-map.json -c cosdefs.json ../../dimts-library/$1 dimts-source/$1
    return 0
}

function compile() {
    echo "... compiling $1 ..."
    FNOF="${1%.f}"
    kftc -l ${FNOF}.lst -o ${FNOF}.cal -s -a static ${FNOF}.f
    cal -x -o ${FNOF}.o -l - -i ${FNOF}.cal >> ${FNOF}.lst
    return 0
}

function link() {
    echo "... linking $1 ..."
    ldr -o $1.abs -m $1.map $1.o \
        $LIBDIR/dimfilm.lib \
        $LIBDIR/grdev.lib \
        $LIBDIR/support.lib \
        $FTNROOT/libio.lib \
        $FTNROOT/librt.lib \
        $FTNROOT/libintf.lib \
        $COSROOT/libem.a \
        $COSROOT/libsys.a \
        $COSROOT/libc.a
}

echo "Build DIMTS."
#
export ACKROOT=/usr/local/share/ack
export COSROOT=$ACKROOT/cos
export FTNROOT=$COSROOT/fortran
export LIBDIR=../lib
#
if [ -z "${OPT}" ]; then
    OPT=0
fi
if [ ! -d lib ]; then
    mkdir lib
fi
rm -rf dimts-source
mkdir dimts-source
#
F_LIST=(
    dimtest.f
    lstdfon.f
)
#
#echo ${F_LIST[@]}
#
# Get the source files, pre-processing them along the way.
for i in "${!F_LIST[@]}"; do
    get_file "${F_LIST[$i]}"
done
#
# Cross-compile with kFTC.
cd dimts-source
#
for i in "${!F_LIST[@]}"; do
    compile "${F_LIST[$i]}"
done
#
echo "... creating binaries ..."
#
# Link the binaries.
link dimtest
link lstdfon
#
cd ..
#
echo "... done."
