#!/bin/bash
#
# Generate a NOS batch job to extract the source code from
# a MODIFY program library in a form the library can be
# reconstructed from. Use SCOPY to obtain a version of
# the source with record boundaries in text form, so that
# it can be transferred by FTP without losing the record
# structure.
#
if [ "$#" != 4 ]; then
    echo "Usage: make-plget-job.sh plname nos-user-name nos-user-password jobfilename"
    exit 1
fi
LIBNAMEPL=$1
if [ "${LIBNAMEPL:0:2}" != "PL" ]; then
    echo "Expected NOS program library OPL name to start with PL"
    exit 1
fi
LIBNAME5="${LIBNAMEPL:2:7}"
if [[ "${#LIBNAME5}" -lt "1" ]]; then
    echo "Expected library name to have something after PL"
    exit 1
fi
JOBFILENAME=$4
(
sed -e "s/libname/${LIBNAME5}/g" <<EOF
BAlibname.
USER,nosuser,nospassword.
*
* GET SOURCE FROM A MODIFY LIBRARY PLlibname.
* THE RESULT IS SSlibname.
* THAT CAN BE FTP-D WITH RECORD STRUCTURE
* PRESERVED.
*
SETTL,*.
SETJSL,*. 
GET,PLlibname.
MODIFY,P=PLlibname,N=0,L=0,C=0,S=SRlibname,F.
REWIND,SRlibname.
SCOPY,SRlibname,SSlibname.
REPLACE,SSlibname.
EOF
) | sed -e "s/nosuser/${2}/g" | sed -e "s/nospassword/${3}/g" > ${JOBFILENAME}
