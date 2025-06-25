#!/bin/bash
#
# Edit .modgitproject
#
if [ ! -f .modgitproject ]; then
    echo "Cannot find .modgitproject ... not in project root directory?"
    echo "Giving up."
    exit 1
fi
nano .modgitproject
