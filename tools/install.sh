#!/bin/bash
#
# Install tools in /usr/local/bin
#
if [ ! -d /usr/local/bin ]; then
    echo "/usr/local/bin not found. Giving up."
    exit 1
fi

sudo cp runrbf.sh /usr/local/bin
sudo cp runrbf.exp /usr/local/bin
sudo cp runrje.sh /usr/local/bin
sudo cp lastspool.sh /usr/local/bin
sudo cp make-plget-job.sh /usr/local/bin
sudo cp import-modify.sh /usr/local/bin
sudo cp export-modify.sh /usr/local/bin
sudo cp modgitinit.sh /usr/local/bin
sudo cp modgitedit.sh /usr/local/bin
sudo cp epsview.sh /usr/local/bin
sudo cp svgviewall.sh /usr/local/bin
cd nostools
pip install .
cd ..

echo "Done."
