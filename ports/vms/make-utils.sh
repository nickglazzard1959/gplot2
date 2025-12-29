#!/bin/bash
#
# Process PLUTILS source in utils-library to create VMS FORTRAN
# compatible code, then transfer that code to VMS.
#
set -x
echo "Construct UTILS library."
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
rm -rf utils-source
mkdir utils-source
#
# Apply conditional compilation but do not convert Display Code to ASCII for chars.f
asciify -c vmsdefs.json -d ../../utils-library/chars.f utils-source/chars.f
asciify -c vmsdefs.json ../../utils-library/cmchars.cmn utils-source/cmchars.cmn
#
cd utils-source
echo "... creating [gplot.utils-source] on VMS"
rm -f utils-files
(
    cat <<EOF
chars.f
cmchars.cmn
EOF
) > utils-files
#
rm -f utils-build
touch utils-build
echo "\$ PURGE" >> utils-build.com
echo "\$ FORT${FOPT} CHARS" >> utils-build.com
echo "\$ LIB/OBJ/CREATE UTILS *.OBJ" >> utils-build.com
echo "\$ PURGE" >> utils-build.com
#
if [ -z "${VMSUSERROOT}" ]; then
    UROOT="none"
else
    UROOT="${VMSUSERROOT}"
fi
vmsftp -p ${VMSPASSWORD} -m ../vmsexts.json -r ${UROOT} ${VMSUSER} ${VMSHOST} << EOF
cred gplot
cd gplot
cred lib
cred utils-source
cd utils-source
mput utils-files
put utils-build.com
bye
EOF
cd ..
#
echo "... done."
