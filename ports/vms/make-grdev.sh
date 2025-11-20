#!/bin/bash
#
# Process PLGRDEV source in grdev-library to create VMS FORTRAN
# compatible code, then transfer that code to VMS.
#
function get_file() {
    echo "... getting $1"
    asciify -c vmsdefs.json -m grdev-map.json ../../grdev-library/$1 grdev-source/$1
    return 0
}

echo "Prepare GRDEV library."
#
if [ -z "${VMSUSER}" ]; then
    echo "Environment variable VMSUSER must be defined."
fi
if [ -z "${VMSPASSWORD}" ]; then
    echo "Environment variable VMSPASSWORD must be defined."
fi
if [ -z "${VMSHOST}" ]; then
    echo "Environment variable VMSHOST must be defined."
fi
if [ -z "${VMSDEBUG}" ]; then
    echo "*** Building for RELEASE ***"
    FOPT=
    LOPT=
else
    echo "*** Building for DEBUG ***"
    FOPT="/DEBUG/NOOPT"
    LOPT="/DEBUG"
fi
#
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
rm -f grdev-source/grdev-files
touch grdev-source/grdev-files
#
for i in "${!F_LIST[@]}"; do
    get_file "${F_LIST[$i]}"
    echo "${F_LIST[$i]}" >> grdev-source/grdev-files
done
#
for i in "${!C_LIST[@]}"; do
    get_file "${C_LIST[$i]}"
    echo "${C_LIST[$i]}" >> grdev-source/grdev-files
done
#
rm -f grdev-source/grdev-build.com
touch grdev-source/grdev-build.com
echo "\$ PURGE" >> grdev-source/grdev-build.com
for i in "${!F_LIST[@]}"; do
    echo "\$ FORT${FOPT} ${F_LIST[$i]%.*}" >> grdev-source/grdev-build.com
done
echo "\$ LIB/OBJ/CREATE GRDEV *.OBJ" >> grdev-source/grdev-build.com
echo "\$ PURGE" >> grdev-source/grdev-build.com
#
cd grdev-source
vmsftp -p ${VMSPASSWORD} -m ../vmsexts.json ${VMSUSER} ${VMSHOST} << EOF
cred gplot
cd gplot
cred lib
cred grdev-source
cd grdev-source
mput grdev-files
put grdev-build.com
bye
EOF
cd ..
#
echo "... done."
