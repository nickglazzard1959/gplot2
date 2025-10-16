#!/bin/bash
pushd  ../../temp
epsview.sh -c cht1001.eps
epsview.sh -c cht2001.eps
gs -dNOPAUSE -sDEVICE=pdfwrite -sOUTPUTFILE=cheat-sheet.pdf -dBATCH cht1001.pdf cht2001.pdf
popd
cp ../../temp/cheat-sheet.pdf .
