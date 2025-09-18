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
nosftp -p $NOSPW -e "put obgran obgran display" $NOSUSER $NOSHOST
#
nosftp -p $NOSPW -e "put daeg1 daeg1 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put daeg2 daeg2 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put daeg3 daeg3 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put daeg4 daeg4 display" $NOSUSER $NOSHOST
#
nosftp -p $NOSPW -e "put obgf28a obgf28a display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgf28b obgf28b display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgf28c obgf28c display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgf28d obgf28d display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgf28m obgf28m display" $NOSUSER $NOSHOST
#
nosftp -p $NOSPW -e "put obgraf1 obgraf1 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgrf1a obgrf1a display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgraf2 obgraf2 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgraf3 obgraf3 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgraf4 obgraf4 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgraf5 obgraf5 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgraf7 obgraf7 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgraf8 obgraf8 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgraf9 obgraf9 display" $NOSUSER $NOSHOST
#
nosftp -p $NOSPW -e "put obgrf10 obgrf10 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgrf11 obgrf11 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgrf12 obgrf12 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgrf13 obgrf13 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgrf14 obgrf14 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgrf15 obgrf15 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgrf16 obgrf16 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgrf17 obgrf17 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgrf18 obgrf18 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgrf19 obgrf19 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgrf20 obgrf20 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgrf21 obgrf21 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgrf22 obgrf22 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgrf28 obgrf28 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgrf29 obgrf29 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgrf30 obgrf30 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgrf31 obgrf31 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgrf2a obgrf2a display" $NOSUSER $NOSHOST
#
nosftp -p $NOSPW -e "put gplproc gplproc display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obprtst obprtst display" $NOSUSER $NOSHOST
#
nosftp -p $NOSPW -e "put obfont obfont display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obsyms obsyms display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obmarks obmarks display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obsamfo obsamfo display" $NOSUSER $NOSHOST
#
nosftp -p $NOSPW -e "put obgd01 obgd01 display" $NOSUSER $NOSHOST
#
nosftp -p $NOSPW -e "put obcht1 obcht1 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obcht2 obcht2 display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obmodio obmodio display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obgrid obgrid display" $NOSUSER $NOSHOST
#
nosftp -p $NOSPW -e "put obaltst obaltst display" $NOSUSER $NOSHOST
nosftp -p $NOSPW -e "put obfin obfin display" $NOSUSER $NOSHOST
