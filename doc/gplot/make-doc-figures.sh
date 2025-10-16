#!/bin/bash
rm -rf ../../temp
mkdir -p ../../temp
(
    cat <<EOF
PREFIX ../obey-files
OB OBALEPS
EOF
) > ../../temp/obgenerate
pushd ../../temp
ugplot obey=obgenerate
popd
