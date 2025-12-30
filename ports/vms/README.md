Building all components of GPLOT and DIMFILM on VAX/VMS
=======================================================

This can be done as follows. Please note that there is a section below
with some additional notes for people using VAX/VMS 4.7. These notes
are not needed if using VAX/VMS 7.3. They may be relevant for other versions
(only 7.3 and 4.7 have been tested).
Please read this section **first** if it is relevant to you.


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

gplot :== "run DISK$USER:[<user>.GPLOT.GPLOT-SOURCE]gplot.exe"
```

Note that the file paths here are case insensitive. It is possible that
user files are somewhere other than `DISK$USER:` depending on how your VMS
system is set up, so adjust as necessary.

Then `gplot` should run GPLOT from any location. The two commands above are best
added to your login command procedure if you are planning to use GPLOT much.

GPLOT uses "foreign command" argument parsing on VMS so that the command line arguments available
on "Unix-like" systems are also available on VMS using the same syntax.


Installing on VAX/VMS 4.7 (Telegraphics turnkey system)
-------------------------------------------------------

Probably the most readily available VAX/VMS "distribution" today (2025/2026) is
the [Telegraphics VAX/VMS 4.7 turnkey](http://cmg.telegraphics.net/classiccmp/vms/vms_tk.html). 
This is very easy to install (using "open" SIMH 4.1) and has no "licensing issues".

However, the FTP server on that system has some idiosyncracies which must be worked
around. The `VMSFTP` tool will do what is necessary if the environment variable
`VMSUSERROOT` is set to the home directory of the user for whom GPLOT is being
installed. For example (if additional user directories are being created in the
same location as the example users):
```
export VMSUSERROOT='disk$home:[users.<user>]'
```

If this environment variable is not defined, `VMSFTP` will not take additional steps
which are needed for the V4.7 FTP server. It should **not** be defined if using VAX/VMS 7.3
for example, as these additional steps may cause problems rather than fixing them. For
example, VMS 4.7 FORTRAN does not define the AND intrinsic and this is also fixed up
when `VMSUSERROOT` is defined in a way that might be incompatible with VMS 7.3 (and maybe other
versions).

**After** defining `VMSUSERROOT`, follow the steps above to install GPLOT/DIMFILM.

You should also adjust the paths used in the "Running GPLOT" section, of course.
For example:
```
define DIMFONTS_LOGICAL DISK$HOME:[USERS.<user>.GPLOT.GPLOT-SOURCE]DADIMFO.DAT
gplot :== "run DISK$HOME:[USERS.<user>.GPLOT.GPLOT-SOURCE]gplot.exe"
```
