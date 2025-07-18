#!/bin/bash
#
# Process PLGRDEV source in grdev-library to create kftc
# compatible code, then cross-compile that code and build a static
# library, grdev.lib
#
function get_file() {
    echo "... getting $1"
    asciify -c cosdefs.json ../../grdev-library/$1 grdev-source/$1
    return 0
}

function compile() {
    echo "... compiling $1 ..."
    FNOF="${1%.f}"
    kftc -l ${FNOF}.lst -o ${FNOF}.cal -s -a static ${FNOF}.f
    cal -x -o ${FNOF}.o -l - -i ${FNOF}.cal >> ${FNOF}.lst
    return 0
}

echo "Build GRDEV library."
#
if [ -z "${OPT}" ]; then
    OPT=0
fi
if [ ! -d lib ]; then
    mkdir lib
fi
rm -rf grdev-source
mkdir grdev-source
#
F_LIST=(
    epsdim.f
    epsnam.f
    psbegn.f
    psend.f
    psclr.f
    psrgbc.f
    pswid.f
    psmove.f
)
#
#echo ${F_LIST[@]}
#
C_LIST=(
    dfxpcn.cmn
    dfxpsn.cmn
)
#
# Get the source files, pre-processing them along the way.
for i in "${!F_LIST[@]}"; do
    get_file "${F_LIST[$i]}"
done
#
for i in "${!C_LIST[@]}"; do
    get_file "${C_LIST[$i]}"
done
#
# Cross-compile with kFTC.
cd grdev-source
#
for i in "${!F_LIST[@]}"; do
    compile "${F_LIST[$i]}"
done
#
echo "... creating static library grdev.lib."
#
# Process the source file list to an object file list.
O_LIST=()
for i in "${!F_LIST[@]}"; do
    F_NAME="${F_LIST[$i]}"
    O_NAME="${F_NAME%.f}.o"
    O_LIST+=("${O_NAME}")
done
echo "${O_LIST[@]}"
#
# Create the library.
lib -l grdevliblist.lst -o ../lib/grdev.lib "${O_LIST[@]}"
#
cd ..
#
echo "... done."
