#!/bin/bash
#
# Process PLGRDEV source in grdev-library to create gfortran
# compatible code, then compile that code and build a static
# library, grdev.a
#
function get_file() {
    echo "... getting $1"
    asciify -m grdev-map.json ../../grdev-library/$1 grdev-source/$1
    return 0
}

function compile() {
    echo "... compiling $1"
    gfortran -g -c -O${OPT} -Wall -std=legacy -cpp -DPORTF77 -DUNIX $1
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
epblkd.f
epsdim.f
epsnam.f
epsrut.f
ftsvg.f
psbegn.f
psclr.f
psend.f
psmove.f
psrgbc.f
pswid.f
svgblkd.f
tekadd.f
tektst.f
)
#
#echo ${F_LIST[@]}
#
C_LIST=(
a12cmn.cmn    
dfxpcn.cmn
dfxpsn.cmn
gtcmn.cmn
svgcmn.cmn
tekcmn.cmn
)
#
for i in "${!F_LIST[@]}"; do
    get_file "${F_LIST[$i]}"
done
#
for i in "${!C_LIST[@]}"; do
    get_file "${C_LIST[$i]}"
done
#
cd grdev-source
for i in "${!F_LIST[@]}"; do
    compile "${F_LIST[$i]}"
done
#
echo "... creating static library grdev.a"
ar rcs ../lib/grdev.a *.o
cd ..
#
echo "... done."
