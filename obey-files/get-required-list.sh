#!/bin/bash
grep "OB OB" obaltst | awk '{print $2}' | uniq > required-list
echo "OBALTST" >> required-list
echo "OBSPIRC" >> required-list
echo "OBGF28A" >> required-list
echo "OBGF28B" >> required-list
echo "OBGF28C" >> required-list
echo "OBGF28D" >> required-list
echo "DAEG1" >> required-list
echo "DAEG2" >> required-list
echo "DAEG3" >> required-list
echo "DAEG4" >> required-list
echo "GPLPROC" >> required-list
echo "OBALSVG" >> required-list
echo "OBALEPS" >> required-list
