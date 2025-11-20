#!/bin/bash
#
# Process PLGPLOT source in gplot-library to create VMS FORTRAN
# compatible code, then transfer that code to VMS.
#

echo "Prepare GPLOT program."
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
echo "... creating [gplot.gplot-source] on VMS"
rm -rf gplot-source
mkdir gplot-source
#
echo "... getting ftgplot.f and pre-processing it"
asciify -d -c vmsdefs.json -m gplot-map.json ../../gplot-library/ftgplot.f gplot-source/ftgplot.f
cp support.f gplot-source/support.f
cp ../../gplot-library/dadimfo.src gplot-source/dadimfo.dat
#
cd gplot-source
#
rm -f gplot-build.com
touch gplot-build.com
#
# WARNING: For VMS DEBUG to find the source code, you MUST link with the libraries in
# the same directory as the source code for them. Not sure why, but copying the
# libraries to [-.LIB] and linking with them leads to "source lines not found" in DEBUG
# (which is otherwise a pleasure to use).
echo "\$ PURGE" >> gplot-build.com
echo "\$ FORT${FOPT} FTGPLOT" >> gplot-build.com
echo "\$ FORT${FOPT} SUPPORT" >> gplot-build.com
echo "\$ LIB/OBJ/CRE SUPPORT SUPPORT" >> gplot-build.com
echo "\$ PURGE" >> gplot-build.com
echo "\$ LINK${LOPT} FTGPLOT,[-.DIMFM-SOURCE]DIMFM/LIB,[-.GRDEV-SOURCE]GRDEV/LIB,[]SUPPORT/LIB,[-.UTILS-SOURCE]UTILS/LIB,[]SUPPORT/LIB" >> gplot-build.com
echo "\$ PURGE" >> gplot-build.com 
#
(
    cat <<EOF
ftgplot.f
support.f
EOF
    ) > gplot-files
vmsftp -p ${VMSPASSWORD} -m ../vmsexts.json ${VMSUSER} ${VMSHOST} << EOF
cred gplot
cd gplot
cred gplot-source
cd gplot-source
mput gplot-files
put gplot-build.com
put dadimfo.dat
bye
EOF
cd ..
#
echo "... done."
