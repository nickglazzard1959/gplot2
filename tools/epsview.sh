#!/bin/bash
if [ "$#" -ne "1" ]; then
    echo "Usage: epsview.sh file.eps (output file.pdf and file.png)"
    exit 1
fi
EPSNAME="$1"
EPSFILE="${EPSNAME%.*}"
#EPSEXT="${EPSNAME##*.}"
A="$(grep BoundingBox ${EPSNAME} | grep -v '(atend)' | awk '/%%BoundingBox:/ {printf("%dx%d",10 * $4,10 * $5); exit }' -)"
echo "Calculated output bounds = $A"
ps2pdf -g${A} ${EPSNAME} ${EPSFILE}.pdf
magick -density 192 ${EPSFILE}.pdf -quality 100 -alpha remove ${EPSFILE}.png
open ${EPSFILE}.pdf
