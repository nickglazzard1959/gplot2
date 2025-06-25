#!/bin/bash
#
# Fire up an RJE station and run a single batch job.
# This is a pretty inefficient way to do this, but it works.
# Ideally, we'd keep RJE station active and send it job
# files as needed.
#
if [ -z "${RJEHOME}" ]; then
    echo "RJEHOME must be defined."
    exit 1
fi
if [ "$#" -ne "1" ]; then
    echo "Usage: runrbf.sh jobfilename"
    exit 1
fi
JOBFILE=$(realpath "$1")
echo $JOBFILE
if [ ! -f $JOBFILE ]; then
    echo "$JOBFILE not found or not a regular file."
    exit 1
fi
expect /usr/local/bin/runrbf.exp $JOBFILE
