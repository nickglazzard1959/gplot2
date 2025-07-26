#!/bin/bash
if [ ! -d /usr/local/share/dimfilm ]; then
    echo "Note: Creating /usr/local/share/dimfilm"
    sudo mkdir /usr/local/share/dimfilm
    sudo chmod a+rw /usr/local/share/dimfilm
fi
sudo cp ../../gplot-library/dadimfo.src /usr/local/share/dimfilm/DADIMFO
sudo cp ./gplot /usr/local/bin
sudo cp ./ugplot /usr/local/bin
