#!/bin/bash
#
# Send example GPLOT obey scripts to a VMS system.
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
#
if [ -z "${VMSUSERROOT}" ]; then
    UROOT="none"
else
    UROOT="${VMSUSERROOT}"
fi
#
vmsftp -p $VMSPASSWORD -r ${UROOT} -e "mput required-list" $VMSUSER $VMSHOST
