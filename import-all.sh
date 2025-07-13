#!/bin/bash
#
# Import all files for GPLOT + DIMFILM from MODIFY.
#
# The source code initially resides on a NOS system in
# in MODIFY program libraries (OPLs). It is to be transferred
# to this Unix system converted to a form Git can deal with.
#
# Although all modules are transferred from each MODIFY OPL,
# Git should detect which ones have actually changed correctly.
#
# CCL procedures PLPROCS --> procs-library
# Keep this as "pure" MODIFY source with no source code conversions.
# This code is only relevant to NOS.
IM_ALLOW_OVERWRITES=1 import-modify.sh procs -n
#
# Utilities library PLUTILS --> utils-library
IM_ALLOW_OVERWRITES=1 import-modify.sh utils
#
# Graphics devices library PLGRDEV --> grdev-library
IM_ALLOW_OVERWRITES=1 import-modify.sh grdev
#
# DIMFILM library PLDIMFM --> dimfm-library
# Create a Unix build script for this, as the code should compile
# on Unix with few exceptions.
IM_ALLOW_OVERWRITES=1 import-modify.sh dimfm "-s build-unix.sh --omit getbyt -l dimfilm.a"
#
# GPLOT program. PLGPLOT --> gplot-library
IM_ALLOW_OVERWRITES=1 import-modify.sh gplot
