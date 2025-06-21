#!/bin/bash
#
# Generate NOS batch jobs needed to get source code from
# MODIFY program libraries in a form from which the program
# libraries can be reconstructed *and* which can be
# transferred by FTP without losing the essential record
# structure information.
#
if [ -z "${NOSUSER}" ]; then
    echo "The environment variable NOSUSER must be set to the NOS account name to be used."
    exit 1
fi
if [ -z "${NOSPW}" ]; then
    echo "The environment variable NOSPW must be set to the password for the NOS account name to be used."
    exit 1
fi
./make-plget-job.sh PLPROCS ${NOSUSER} ${NOSPW}
./make-plget-job.sh PLDIMFM ${NOSUSER} ${NOSPW}
./make-plget-job.sh PLGRDEV ${NOSUSER} ${NOSPW}
./make-plget-job.sh PLUTILS ${NOSUSER} ${NOSPW}
./make-plget-job.sh PLGPLOT ${NOSUSER} ${NOSPW}
