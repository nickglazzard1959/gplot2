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
nosftp -p $NOSPW -e "put obblk1 obblk1 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obdiag obdiag display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obdiag2 obdiag2 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obeqn obeqn display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obfont obfont display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgran obgran display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obsamfo obsamfo display" $NOSUSER $NOSHOST
