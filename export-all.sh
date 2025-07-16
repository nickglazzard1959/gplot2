#!/bin/bash
#
# Export all files for GPLOT + DIMFILM to MODIFY
#
# The exports are done with the -a option, so all modules
# are exported and MODIFY OPLs will be completely rebuilt.
#
# procs-library --> PLPROCS
export-modify.sh procs "-n -a"
#
# utils-library --> PLUTILS
export-modify.sh utils -a
#
# grdev-library --> PLGRDEV
export-modify.sh grdev -a
#
# dimfm-library --> PLDIMFM
export-modify.sh dimfm -a
#
# gplot-library --> PLGPLOT
export-modify.sh gplot -a
#
# dimts-library --> PLDIMTS
export-modify.sh dimts -a
