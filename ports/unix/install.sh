#!/bin/bash
if [ ! -d /usr/local/share/dimfilm ]; then
    echo "Note: Creating /usr/local/share/dimfilm"
    sudo mkdir /usr/local/share/dimfilm
    sudo chmod a+rw /usr/local/share/dimfilm
fi
sudo cp ../../gplot-library/dadimfo.src /usr/local/share/dimfilm/DADIMFO
sudo cp ./gplot /usr/local/bin
sudo cp ./ugplot /usr/local/bin
#
# Now this I can hardly believe. I had been using /usr/local/bin/gplot
# happily for a while on macOS ... until it started being killed immediately
# on startup. Running exactly the same binary in a development directory
# (e.g. here) worked. It seems that, at some point, macOS decided that
# gplot (in /usr/local/bin) had to be codesigned or it would be killed.
# This is insane - like much of the world right now.
#
# The following fixes this, at least until Apple further "improves
# security" ... which is likely to happen.
#
if [[ "$OSTYPE" == "darwin"* ]]; then
    sudo codesign --force --deep --sign - /usr/local/bin/gplot
fi
