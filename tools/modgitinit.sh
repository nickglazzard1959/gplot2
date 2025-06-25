#!/bin/bash
#
# modgitinit.sh - Initialise a MODIFY + Git project.
#
if [ "$#" -lt "1" ]; then
    echo "Usage: modgitinit.sh project-description"
    exit 1
fi
rm -f .modgitproject
echo "export MODGITPROJECT=\"${1}\"" > .modgitproject
echo "export NOSUSER=guest" >> .modgitproject
echo "export NOSPW=guest" >> .modgitproject
echo "export NOSHOST=unset" >> .modgitproject
echo 'export RJEHOME=~/rje-station' >> .modgitproject
echo 'export RJESPOOL=${RJEHOME}/spool' >> .modgitproject
mkdir -p plget-jobs
mkdir -p plput-jobs
mkdir -p plsources
