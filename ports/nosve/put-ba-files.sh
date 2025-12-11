#!/bin/bash
#
# Send NOS MODIFY PL build jobs to the NOS system using
# the credentials set up in .modgitproject. These can
# be edited with modgitedit
#
# This must be run AFTER the export-modify.sh tool has
# been run for utils, grdev, dimfm, gplot (and partially
# failed) to transfer MODIFY source to the NOS part of the
# NOS / NOS/VE dual-state RTR.
#
if [ ! -f ../../.modgitproject ]; then
    echo "Cannot find ../.modgitproject ... not in ports/nosve directory?"
    echo "Giving up."
    exit 1
fi
source ../../.modgitproject
echo "Project = ${MODGITPROJECT}"
#
cp ../../plput-jobs/put-plutils-src.job bautils
cp ../../plput-jobs/put-plgrdev-src.job bagrdev
cp ../../plput-jobs/put-pldimfm-src.job badimfm
cp ../../plput-jobs/put-plgplot-src.job bagplot
#
nosftp -p $NOSPW -e "mput required-ba-list display" $NOSUSER $NOSHOST
#
rm -f bautils
rm -f bagrdev
rm -f badimfm
rm -f bagplot
