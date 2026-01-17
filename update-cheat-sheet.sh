#!/bin/bash
rm -rf temp
mkdir -p temp
(
    cat <<EOF
PREFIX ../obey-files
DEV SVG cht1 2048,1280
OB OBCHT1
EOF
) > temp/obgenerate1
(
    cat <<EOF
PREFIX ../obey-files
DEV SVG cht2 2048,1280
OB OBCHT2
EOF
) > temp/obgenerate2
pushd temp
gplot obey=obgenerate1
gplot obey=obgenerate2
cp cht1001.svg ..
cp cht2001.svg ..
popd
