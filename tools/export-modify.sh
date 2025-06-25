#!/bin/bash
#
# export-modify.sh - Create or update a MODIFY PL (program library) on NOS
# using the contents (all or only those modified in or since the last Git commit)
# of a directory. This directory will normally be created/updated by
# import-modify.sh, followed by its contents being changed on Unix and
# being managed by Git on those systems.
#
# Only specific file extensions will be processed: .f, .cmn and .src.
#
if [ ! -f .modgitproject ]; then
    echo "Cannot find .modgitproject ... not in project root directory?"
    echo "Giving up."
    exit 1
fi
source .modgitproject
echo "Project = ${MODGITPROJECT}"
if [ "$#" -lt "1" ]; then
    echo "Usage: export-modify.sh library-name [extra-args]"
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
if [ ! -d plput-jobs ]; then
    mkdir plput-jobs
fi
JOBNAME="plput-jobs/put-pl${LIBNAME_LOWER}-src.job"
SRCNAME="plput-jobs/SR${LIBNAME_UPPER}"
LIBNAME="${LIBNAME_LOWER}-library"
if [ ! -d ${LIBNAME} ]; then
    echo "Library source directory ${LIBNAME} not found. Giving up."
    exit 1
fi
modjoin -f ${SRCNAME} ${EXTRA_ARGS} ${LIBNAME} ${JOBNAME} PL${LIBNAME_UPPER}
nosftp -p $NOSPW -e "put ${SRCNAME}.plsrc SR${LIBNAME_UPPER} display" $NOSUSER $NOSHOST
runrbf.sh ${JOBNAME}
lastspool.sh cat
