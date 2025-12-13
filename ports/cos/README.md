Building DIMFILM on COS
=======================

Kevin Jordan succeeded in installing the original Cray Station software
on NOS 2.8.7 so that NOS running on a simulated Cyber mainframe can
communicate with COS running on a simulated Cray X-MP, giving exactly
the same user experience as with the original hardware in the mid-1980's.

Unfortunately, although the COS operating system proper was snatched 
from the jaws of death, the software development toolchain for COS 
appears to have been lost. Over a decade of searching for this has
found nothing.

Kevin Jordan has created a complete toolchain to replace this missing
critical software. This includes an assembler, loader and librarian that
can be used for cross-development on "Unix". After he added a Cray X-MP
back end to the Amsterdam Compiler Kit ("ack") C compiler, these tools
can also be cross-compiled and  run natively on COS. 
Finally, a newly written FORTRAN-77
compiler ("kFTC") can also be used either for cross-development on
"Unix" or run natively on COS.

The DIMFILM library can be built with kFTC and the other toolchain
components so that FORTRAN-77 programs running on COS can generate
graphical output. 

At present, only the DIMFILM library and test programs are built for
COS. GPLOT is not built. It should be fairly straightforward to add
GPLOT, but GPLOT is intended primarily for interactive use and while
COS can be used interactively (via the NOS ICF - Interactive Cray Facility),
almost all work was done via batch jobs back in the day. 

DIMFILM is cross-compiled on "Unix" using COS Tools, Kevin's fork
of "ack" and his "kFTC" FORTRAN compiler. 
The repository for COS Tools is [here](https://github.com/kej715/COS-Tools)
and the repository for the "ack" fork is [here](https://github.com/kej715/ack). The
READMEs for these are very thorough and the build and install instructions
should be followed carefully.

Once these software items have been built and installed, it is relatively 
straightforward to cross-compile FORTRAN-77 programs so that they can be run on
COS. The final output (from the linker) is a static binary (`.abs` file) which
is entirely self-contained.

Things inevitably get a little complicated, as there are three machines
involved running three different operating systems. 

- Once a `.abs` file has been built on a "Unix" system, it must be moved
  to a NOS account. This is easily done with FTP (e.g with the `bput` command
  in the `NOSFTP` tool).
- Once a binary for COS is on NOS, the file must be moved to an account
  on COS. As delivered, COS has a single user account set up and that is
  probably usually enough for running batch jobs on the system. Copying files can
  be done with the `REPLACE` procedure in the `CRAY` procedure library
  in the `INSTALL` account on NOS. This is installed when the Cray Station
  option is selected for NOS. Note, though, that there are two versions
  of this procedure library: a "minimal" version that is available to all
  users and one that has procedures that manipulate COS permanent files,
  which is only available from the NOS `INSTALL` account. This means we
  must use the NOS `INSTALL` account to transfer files to COS. The transfer
  is done by the Cray Station software running on NOS and COS.
- Once the binary file is on COS, we can construct and submit a batch job
  which runs that binary. We can also transfer data files needed for the job,
  or produced by it, to and from COS using features of COS and the NOS
  Cray Station software. A COS batch job created on NOS can be sent to COS using
  the NOS `CSUBMIT` command. This can return job output to a "wait queue" for
  retrieval with `QGET`. The progress of a job running on COS can be monitored
  from NOS using the `CSTATUS` command.


## Build the graphics device support library from PLGRDEV

```
./make-grdev.sh
```
This pre-processes the source in `grdev-library` to a newly created
source directory `grdev-source`, then compiles the contents with
`kFTC`, creating the library `lib/grdev.lib`

The only device currently supported on COS is EPSCOL (colour
Encapsulated PostScript).

The `asciify` program is used to apply conditional compilation rather
that the C Pre Processor, as `kFTC` doesn't use CPP internally. The 
`asciify` program "understands" only the CPP subset needed for conditional
compilation (`#ifdef`, `#if`, `#else`, `#endif`). Symbol definitions
are specified in a dictionary in JSON format (in the file `cosdefs.json`).
Conditionals can be nested, although that feature is not used as yet.


## Build the DIMFILM library

```
./make-dimfm.sh
```
This pre-processes the source in `dimfm-library` to a newly created
source directory `dimfm-source`, then compiles the contents with
`kFTC`, creating the library `lib/dimfilm.lib`

This library has identical functionality to the NOS version.
Other FORTRAN-77 programs can be compiled with `kFTC` and linked
with it to generate graphical output.


## Build the COS support library

```
./make-support.sh
```
This compiles the support library code (from `support.f`)
to create the library `lib/support.lib`. Any programs
using DIMFILM will have to link with this library in addition to
the others mentioned above.


## Build the DIMFILM test programs

```
./make-dimts.sh
```
This pre-processes the source in `dimts-library` to a newly created
source directory `dimts-source`, then compiles the contents with
`kFTC`, creating two relocateable object files which are
linked with all the above libraries to create the `dimtest.abs` and 
`lstdfon.abs` programs.


## Running cross-compiled programs on COS

The Cray X-MP (real or simulated) is not a totally independent computer.
It can only function as an "attached processor" communicating with a 
front-end machine. In our case, this front end is a (simulated) CDC Cyber
mainframe running NOS 2.8.7 equipped with Cray Station (simulated) hardware 
and software. The "hardware" implements the "Front End Interface" attachment
option in which the Cray machine is attached to a Cyber channel.

Any cross-compiled program (and required associated data files) must first
be moved to the `INSTALL` account on the NOS system. FTP is best for this.
An example is:
```
(genv) $ nosftp install nuc1
Password for NOS account: 
Contacting NOS FTP server on host: nuc1
230 USER LOGGED IN, PROCEED.
220 SERVICE READY FOR NEW USER.
Local cwd now: /Volumes/qemu-main/gitprojects/gplot2/ports/cos

NOS FTP> bput dimts-source/dimtest.abs dimtest
Sending file: dimts-source/dimtest.abs (binary) ...
... storing file as: dimtest
226 CLOSING DATA CONNECTION.

NOS FTP> bye

Exiting normally.
221 SERVICE CLOSING CONTROL CONNECTION. LOGGED OUT.
```

For DIMFILM, the font definition file must also be transferred.
```
(genv) $ nosftp install nuc1                  
Password for NOS account: 
Contacting NOS FTP server on host: nuc1
230 USER LOGGED IN, PROCEED.                              
220 SERVICE READY FOR NEW USER.                           
Local cwd now: /Volumes/qemu-main/gitprojects/gplot2/ports/cos

NOS FTP> put ../../gplot-library/dadimfo.src dadimfo display
Sending file: ../../gplot-library/dadimfo.src (text) ...
... storing file as: dadimfo
226 CLOSING DATA CONNECTION.                              

NOS FTP> bye

Exiting normally.
221 SERVICE CLOSING CONTROL CONNECTION. LOGGED OUT.   
```

Once the cross-compiled program has been transferred to NOS,
the next step is to move it to COS, where it needs to reside as
a permanent file.

The first step is to login as `INSTALL` (password `INSTALL`,
unless it has been changed ... which might be a good idea!).

The transfer to the COS machine can be done using CCL procedures in the 
`CRAY` procedure library. Proceed as follows (NOS commands are shown
in upper case, but case does not matter):
```
GET,CRAY.
BEGIN,SAVE,CRAY,DIMTEST.
```
This uses the `DIMTEST` program as an example, but other cross-compiled
programs could be used instead.

Note that `SAVE` should only be used if the program has not already been
transferred. It is is already present on COS, use `REPLACE` instead.

The procedures in the `CRAY` library work by creating COS batch jobs and
sending them to COS to be run using the NOS `CSUBMIT` command. While
running, their progress can be monitored using `CSTATUS`. Once completed,
their output will, by default, appear in the NOS "wait queue". The 
contents of the "wait queue" can be seen using:
```
ENQUIRE,JSN.
```
which lists all the account's running and completed jobs, showing their
"job sequence numbers" or JSNs. To retrieve the output from a job
to see whether it worked or not (and any other output) use:
```
QGET,JSN.
```

To transfer and save the font definitions file `DADIMFO` on COS, use:
```
BEGIN,SAVE,CRAY,DADIMFO,DF=CB.
```
In this case, we need to let COS know the file contains "coded characters"
and is not to be treated as "binary data" using "transparent" transfer
mode (`DF` denotes "data format" and `CB` stands for "character, blocked").

Note that the `CRAY` procedure library contains many more procedures and
features, and it can take care to preserve lower case data on COS when files
are transferred. For more information about these procedures,
[see here.](https://github.com/kej715/DtCyber/tree/main/NOS2.8.7#cos-tools)
We are using only a small subset of the features available.

Once the cross-compiled DIMFILM based program and `DADIMFO` are resident
on COS, the program can be run and its output retrieved by submitting
a batch job to the COS system.

The `CRAY` procedures library contains a very easy to use `RUN` procedure
that will transfer a cross-compiled program and an optional data file to COS,
run it and return the job output. If used with the `QGET` procedure in the
`CRAY` procedure library (*not* the NOS `QGET` command), the job output
will preserve lower case when viewed on NOS.

However, running a program that uses DIMFILM will hopefully produce output
files containing Encapsulated PostScript. No single procedure can deal with
returning arbitrary numbers of output files back to the front end.

Consequently, a COS batch file must be written for each program to be run
on COS. The batch file must also be customised so that the data files
are returned to the appropriate user.

As an example of how to run a program that uses DIMFILM on COS and get the
EPS files back, the shell script `make-cbdimts.job` creates a COS batch
file called `cbdimts.job` which runs the `DIMTEST` program previously saved
on COS. After running `DIMTEST`, it returns the EPS files created to the
NOS user whose name and password has been set in the `.modgitproject` file
(see above). Here is an example of the batch job generated for the `TESTER`
NOS account:
```
JOB,JN=CBDIMTS,T=2000.
ACCOUNT,US=SYSTEM,AC=CRAY,APW=XYZZY,UPW=QUASAR.
ECHO,ON=ALL.
OPTION,STAT=ON.
ACCESS,DN=DIMTEST,OWN=SYSTEM. 
ACCESS,DN=DADIMFO,OWN=SYSTEM. 
MEMORY,FL,USER.
DIMTEST.
*
* GET THE RESULT EPS FILES BACK.
* BY DEFAULT SOMETHING CONVERTS LOWER CASE TO UPPER, SADLY.
* IT SHOULD BE POSSIBLE TO GET BACK LOWER CASE CHARACTERS
* BUT I HAVE NOT SO FAR SUCCEEDED IN DOING SO.
* THE FIXUP OPTION OF EPSVIEW.SH CAN CONVERT THE DEFECTIVE
* OUTPUT TO A USABLE FORM FOR DIMFILM OUTPUT FOR NOW.
*
DISPOSE,DN=NW001,DC=ST,MF=FE,TEXT='USER,TESTER,TESTER.CTASK.REPLACE,NW001.'.
DISPOSE,DN=NW002,DC=ST,MF=FE,TEXT='USER,TESTER,TESTER.CTASK.REPLACE,NW002.'.
DISPOSE,DN=NW003,DC=ST,MF=FE,TEXT='USER,TESTER,TESTER.CTASK.REPLACE,NW003.'.
DISPOSE,DN=NW004,DC=ST,MF=FE,TEXT='USER,TESTER,TESTER.CTASK.REPLACE,NW004.'.
DISPOSE,DN=NW005,DC=ST,MF=FE,TEXT='USER,TESTER,TESTER.CTASK.REPLACE,NW005.'.
DISPOSE,DN=NW006,DC=ST,MF=FE,TEXT='USER,TESTER,TESTER.CTASK.REPLACE,NW006.'.
DISPOSE,DN=NW007,DC=ST,MF=FE,TEXT='USER,TESTER,TESTER.CTASK.REPLACE,NW007.'.
DISPOSE,DN=NW008,DC=ST,MF=FE,TEXT='USER,TESTER,TESTER.CTASK.REPLACE,NW008.'.
DISPOSE,DN=NW009,DC=ST,MF=FE,TEXT='USER,TESTER,TESTER.CTASK.REPLACE,NW009.'.
DISPOSE,DN=NW010,DC=ST,MF=FE,TEXT='USER,TESTER,TESTER.CTASK.REPLACE,NW010.'.
RELEASE,DN=DIMTEST. 
RELEASE,DN=DADIMFO.
```

The `cbdimts.job` file needs to be transferred to the appropriate account
on NOS (here `TESTER`) using FTP. The transfer should be done with
```
put cbdimts.job cbdimts display
```
with `nosftp` or an equivalent "ascii" transfer with other FTP clients.

To run the job, login to the NOS account and use:
```
CSUBMIT,CBDIMTS,TO.
```
As noted above, the progress of the job can be monitored with `CSTATUS`.
When the job has finished, its output can be retrieved from the "wait
queue" using:
```
ENQUIRE,JSN.
```
to identify the returned output, then:
```
QGET,jsn.
```
to obtain the output for viewing on the terminal (e.g. with `SCOPY` or `FSE`).
Hopefully, it will have worked, and output files will have been produced and returned
to NOS.

Once the output EPS files are on NOS, they can be transferred to a "Unix"
system using FTP. Using `nosftp`:
```
(genv) $ nosftp tester nuc1
Password for NOS account: 
Contacting NOS FTP server on host: nuc1
230 USER LOGGED IN, PROCEED.
220 SERVICE READY FOR NEW USER.
Local cwd now: /Volumes/qemu-main/gitprojects/gplot2/ports/cos

NOS FTP> get nw009 nw009.eps display
... retrieving file: nw001
226 CLOSING DATA CONNECTION. 

NOS FTP> bye

Exiting normally.
221 SERVICE CLOSING CONTROL CONNECTION. LOGGED OUT. 
```
Either `display` or `ascii` can be used as the data format type in
the `get` command. The EPS files are created with lower case characters
and braces as valid EPSF on COS, but the `DISPOSE` seems to convert them
to Display Code using the (default) `DF=CB` data format.

Unfortunately, none of the available `DF` options in `DISPOSE` seem to
transfer characters in a format that can be converted reliably to 
6/12 "ascii" with the NOS `FCOPY` command. `TR` does not work. The 
closest is `BB` ("blocked binary") but there are some errors even
with that (which doesn't sound like it is the right option anyway).
This will need more investigation.

Meanwhile, the `epsview.sh` shell script has a "fixup" option which
will convert the files obtained by FTP to valid EPSF. This will only
work with COS DIMFILM output files! For example:
```
$ epsview.sh -f -p nw009.eps
```
will convert the file to PDF after fixing the transfer issues,
and display it in Preview on macOS, as well as producing a decent resolution
PNG image version.
