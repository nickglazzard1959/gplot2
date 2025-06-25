#!/bin/bash
#
# After all the MODIFY program library source fetch
# batch jobs have been run on NOS, this script will
# fetch them to UNIX for further processing.
#
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
#
nosftp -p $NOSPW -e "get SSPROCS plprocs.src display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "get SSDIMFM pldimfm.src display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "get SSGRDEV plgrdev.src display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "get SSUTILS plutils.src display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "get SSGPLOT plgplot.src display" $NOSUSER $NOSHOST
