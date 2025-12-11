Building all components of GPLOT and DIMFILM on VAX/VMS
=======================================================

This can be done as follows.

Transferring source to the VAX/VMS host
---------------------------------------

This uses the Python based tool `VMSFTP`. You will need to set environment variables specifying
the user on VMS where the build is to occur, their password and the name or IP address
of the VMS machine. If a debug version is to be built, a further environment
variable should be defined (if it is not defined, an optimised build will be
performed on VMS).

```
cd ports/vms

export VMSUSER=<user-name>
export VMSPASSWORD=<user-password>
export VMSHOST=<vms-host-name>
export VMSDEBUG=1 (optionally, for a debug build)

./make-utils.sh
./make-grdev.sh
./make-dimfm.sh
./make-gplot.sh
```

Note that these steps make a fully populated source tree on VMS. They
do not actually build the software, as that has to be done on VMS 
(of course).

Any error messages about failing to create sub-directories can be ignored.
These are expected on any but the first use of these scripts on a given
machine.


Building DIMFILM and GPLOT on VAX/VMS
-------------------------------------

Login to the VMS machine to which the source tree has been transferred (above),
using the user specified in the transfers.

Then:
```
set def [.gplot.utils-source]
@utils-build

set def [-.grdev-source]
@grdev-build

set def [-.dimfm-source]
@dimfm-build

set def [-.gplot-source]
@gplot-build

rename ftgplot.exe gplot.exe
```

There will be warnings issued in the compilation phases of these
builds, but they can all be ignored. They are mostly warnings about
possibly non-portable pieces of code.


Running GPLOT on VAX/VMS
------------------------

It is necessary to define a logical name pointing to the location of
the font definitions file. It is also convenient to set up a command
to run GPLOT. Those things can done with:
```
define DIMFONTS_LOGICAL DISK$USER:[<user>.GPLOT.GPLOT-SOURCE]DADIMFO.DAT

gplot := "run DISK$USER:[<user>.GPLOT.GPLOT-SOURCE]gplot.exe"
```

Note that the file paths here are case insensitive. It is possible that
user files are somewhere other than `DISK$USER:` depending on how your VMS
system is set up, so adjust as necessary.

Then `gplot` should run GPLOT from any location. The two commands above are best
added to your login command procedure if you are planning to use GPLOT much.

GPLOT uses "foreign command" argument parsing on VMS so that the command line arguments available
on "Unix-like" systems are also available on VMS using the same syntax.
