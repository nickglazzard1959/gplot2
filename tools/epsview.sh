#!/bin/bash
#
# This started because macOS can no longer "just" display
# .eps files by double clicking on them. However, it is
# useful to get "the right size" output and decent quality
# on all systems.
#
# It also fixes up lower case being lost on COS to NOS
# transfers.
#
# This needs ghostview, imagemagick and pdfcrop to be installed.
#
# Also, on Linux ... it seems Imagemagick thinks Ghostview output
# is a deadly security problem! This make make it refuse to convert
# PDF to PNG (you could not make this stuff up). To make it do so,
# edit a security policies file, usually:
#  /etc/Image-Magick-6/policy.xml (as sudo, version dependent).
# Change:
#  <policy domain="coder" rights="none" pattern="PDF" />
# To:
#  <policy domain="coder" rights="read|write" pattern="PDF" />
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
# Ensure ps2pdf is available.
if ! command -v ps2pdf &>/dev/null; then
    echo "Cannot find ps2pdf. Install ghostscript."
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
ps2pdf -g${A} ${EPSNAME} ${EPSFILE}_zztemp.pdf
#
# Crop off borders that ps2pdf adds at the bottom and left. Add back small symmetric border.
pdfcrop --margins 5 ${EPSFILE}_zztemp.pdf ${EPSFILE}.pdf
rm -f ${EPSFILE}_zztemp.pdf
if command -v magick &>/dev/null; then
    magick -density 384 ${EPSFILE}.pdf -quality 100 -alpha remove ${EPSFILE}.png
else
    if command -v convert &>/dev/null; then
        convert -density 384 ${EPSFILE}.pdf -quality 100 -alpha remove ${EPSFILE}.png
    else
        echo "Cannot generate PNG version. Install Imagemagick."
    fi
fi
#
# Open the PDF on macOS to display it.
if [[ "$(uname -s)" == "Darwin" ]]; then
    open ${EPSFILE}.pdf
else
    xdg-open ${EPSFILE}.pdf
fi
