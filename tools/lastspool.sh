#!/bin/bash
#
# View latest RJE spool file output.
if [ -z "${RJESPOOL}" ]; then
    echo "The environment variable RJESPOOL must be set to the RJE station spool directory."
    exit 1
fi
if [ "$#" -gt 0 ]; then
    CMD="cat"
else
    CMD="more"
fi
$CMD ${RJESPOOL}/`ls -t ${RJESPOOL} | head -n 1`
