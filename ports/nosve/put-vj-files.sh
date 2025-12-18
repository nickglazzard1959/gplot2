#!/bin/bash
#
# Send NOS/VE build jobs to the NOS system using the
# credentials set up in .modgitproject. These can
# be edited with modgitedit
#
if [ ! -f ../../.modgitproject ]; then
    echo "Cannot find ../.modgitproject ... not in ports/nosve directory?"
    echo "Giving up."
    exit 1
fi
source ../../.modgitproject
echo "Project = ${MODGITPROJECT}"
#
nosftp -p $NOSPW -e "mput required-list ascii" $NOSUSER $NOSHOST
