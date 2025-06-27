#!/bin/bash
#
# import-modify.sh - Move the contents of a MODIFY OPL on a NOS system to an
# equivalent in a form that Git can deal with. The OPL is transformed to a
# directory with multiple files, one per module.
#
# It is assumed that the OPL on NOS is called PLxxxxx where xxxxx is up to
# 5 characters and is the "library-name" supplied to this script.
#
# The modules in the OPL will be put in a directory called <library-name>-library.
# The "type" of each module is guessed and used to form a file extension:
#  .f for FORTRAN code.
#  .cmn for COMMON (included material, often FORTRAN common blocks)
#  .src for anything else (e.g. CCL procedures).
# The file name stem is the module name in the OPL.
#
# The contents of the directory (only .f, .cmn and .src files) can by used
# to make MODIFY source input that recreates the OPL or updates modules in an OPL.
#
if [ ! -f .modgitproject ]; then
    echo "Cannot find .modgitproject ... not in project root directory?"
    echo "Giving up."
    exit 1
fi
source .modgitproject
echo "Project = ${MODGITPROJECT}"
if [ "$#" -lt "1" ]; then
    echo "Usage: import-modify.sh library-name [extra-args]"
    exit 1
fi
if [ -z "${NOSUSER}" ]; then
    echo "The environment variable NOSUSER must be set to the NOS account name to be used."
    exit 1
fi
if [ -z "${NOSPW}" ]; then
    echo "The environment variable NOSPW must be set to the password for the NOS account name to be used."
    exit 1
fi
if [ -z "${NOSHOST}" ]; then
    echo "The environment variable NOSHOST must be set to the host name or IP of the machine running NOS on DtCyber."
    exit 1
fi
if [ "$#" -eq "2" ]; then
    EXTRA_ARGS=$2
else
    EXTRA_ARGS=" "
fi
#
LIBNAME_LOWER=$(echo "$1" | tr '[:upper:]' '[:lower:]')
LIBNAME_UPPER=$(echo "$1" | tr '[:lower:]' '[:upper:]')
if [ ${#LIBNAME_UPPER} -gt "5" ]; then
    echo "Supplied libname is too long. 5 characters max."
    exit 1
fi
if [ ! -d plget-jobs ]; then
    mkdir plget-jobs
fi
if [ ! -d plsources ]; then
    mkdir plsources
fi
JOBNAME="plget-jobs/get-pl${LIBNAME_LOWER}-src.job"
make-plget-job.sh PL${LIBNAME_UPPER} ${NOSUSER} ${NOSPW} ${JOBNAME}
runrbf.sh ${JOBNAME}
lastspool.sh cat
nosftp -p $NOSPW -e "get SS${LIBNAME_UPPER} plsources/pl${LIBNAME_LOWER}.src display" $NOSUSER $NOSHOST
rc=$?
if [ rc -ne 0 ]; then
    exit 1
fi
nosftp -p $NOSPW -e "del SS${LIBNAME_UPPER}" $NOSUSER $NOSHOST
rc=$?
if [ rc -ne 0 ]; then
    exit 1
fi
modsplit -u -d ${LIBNAME_LOWER}-library ${EXTRA_ARGS} plsources/pl${LIBNAME_LOWER}.src
rc=$?
if [ rc -ne 0 ]; then
    exit 1
fi
exit 0
