#!/bin/bash
#
# This started because macOS can no longer "just" display
# .eps files by double clicking on them. However, it is
# useful to get "the right size" output and decent quality.
# It also fixes up lower case being lost on COS to NOS
# transfers.
# This needs ghostview and imagemagick to be installed.
#
if [ "$#" -lt "1" ]; then
    echo "Usage: epsview.sh file.eps [fixup] (output file.pdf and file.png)"
    exit 1
fi
EPSNAME="$1"
#
# Ensure the input file exists.
if [ ! -f "${EPSNAME}" ]; then
    echo "File ${EPSNAME} does not exist."
    exit 1
fi
#
# Output from COS gets converted to all upper case with braces
# omitted by ... something. Try to fix that.  It should be
# possible to transfer files with lower case intact from COS
# to NOS, but I cannot find the right options to do it.
if [ "$#" -eq 2 ]; then
    sed -e '1,11d' ${EPSNAME} | \
        tr '[:upper:]' '[:lower:]' | \
        sed -e 's/boundingbox/BoundingBox/g' > zzzztemp
    (
        cat <<EOF
%%!PS-Adobe-3.0 EPSF-3.0    
%%Title: Dimfilm plot file.                  
%%BoundingBox: (atend)              
%%EndComments         
/l {moveto lineto              
currentpoint            
stroke moveto} def                
/m {newpath moveto} def                   
/c {setrgbcolor} def                 
/w {0.5 mul setlinewidth} def                     
%%EndProlog
EOF
    ) > zzzytemp
    cat zzzytemp zzzztemp > zzzxtemp
    rm -f zzzytemp zzzztemp
    rm -f ${EPSNAME}
    mv zzzxtemp ${EPSNAME}
fi
#
# Extract bounding box info and scale it by 10. This is needed to get
# decent quality output. 
A="$(grep BoundingBox ${EPSNAME} | grep -v '(atend)' | awk '/%%BoundingBox:/ {printf("%dx%d",10 * $4,10 * $5); exit }' -)"
echo "Calculated output bounds = $A"
EPSFILE="${EPSNAME%.*}"
ps2pdf -g${A} ${EPSNAME} ${EPSFILE}.pdf
magick -density 192 ${EPSFILE}.pdf -quality 100 -alpha remove ${EPSFILE}.png
#
# Open the PDF on macOS to display it.
open ${EPSFILE}.pdf
