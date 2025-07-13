#!/bin/bash
#
# Send example GPLOT obey scripts to the NOS system using the
# credentials set up in .modgitproject. These can
# be edited with modgitedit
#
if [ ! -f ../.modgitproject ]; then
    echo "Cannot find ../.modgitproject ... not in obey-files directory?"
    echo "Giving up."
    exit 1
fi
source ../.modgitproject
echo "Project = ${MODGITPROJECT}"
#
nosftp -p $NOSPW -e "put obdemo obdemo display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obfuncs obfuncs display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obmysty obmysty display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obplant obplant display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obright obright display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obspiro obspiro display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obhitom obhitom display" $NOSUSER $NOSHOST
