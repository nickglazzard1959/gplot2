#!/bin/bash
echo "Split PL sources to files in directories"
tools/modsplit.py -u -n -d procs-library plsources/plprocs.src
tools/modsplit.py -u -d utils-library plsources/plutils.src
tools/modsplit.py -u -d grdev-library plsources/plgrdev.src
tools/modsplit.py -u -d dimfm-library -s build-dim.sh --omit getbyt plsources/pldimfm.src
tools/modsplit.py -u -d gplot-library plsources/plgplot.src
