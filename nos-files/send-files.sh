#!/bin/bash
#
# Send selected files to the NOS system using the
# credentials set up in .modgitproject. These can
# be edited with modgitedit
#
if [ ! -f ../.modgitproject ]; then
    echo "Cannot find ../.modgitproject ... not in nos-files directory?"
    echo "Giving up."
    exit 1
fi
source ../.modgitproject
echo "Project = ${MODGITPROJECT}"
#
nosftp -p $NOSPW -e "put prlogin prlogin display" $NOSUSER $NOSHOST
