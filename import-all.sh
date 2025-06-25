#!/bin/bash
#
# Import all files for GPLOT + DIMFILM from MODIFY.
#
# CCL procedures PLPROCS --> procs-library
# Keep this as "pure" MODIFY source with no source code conversions.
# This code is only relevant to NOS.
import-modify.sh procs -n
#
# Utilities library PLUTILS --> utils-library
import-modify.sh utils
#
# Graphics devices library PLGRDEV --> grdev-library
import-modify.sh grdev
#
# DIMFILM library PLDIMFM --> dimfm-library
# Create a Unix build script for this, as the code should compile
# on Unix with few exceptions.
import-modify.sh dimfm -s build-unix.sh --omit getbyt -l dimfilm.a
#
# GPLOT program.
import-modify.sh gplot
