#!/bin/bash
# Transfer extras to NOS side of RTR.
if [ ! -f ../../../.modgitproject ]; then
    echo "Cannot find .modgitproject ... not in ports/nosve/extras directory?"
    echo "Giving up."
    exit 1
fi
source ../../../.modgitproject
#
nosftp -p $NOSPW -e "mput extras-list ascii" $NOSUSER $NOSHOST
