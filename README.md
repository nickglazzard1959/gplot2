
GPLOT - A Graph Plotting Program based on DIMFILM
=================================================

GPLOT is a graph plotting program, similar in intent to Gnuplot but very
different in detail (and less capable -- although it also does some things
Gnuplot does not). It is intended to run primarily
on CDC NOS 2.8.7, but it will also run on "Unix-like" systems and the
Cray Operating System (COS 1.17). In addition to plotting graphs of
data (stored in files), the current version of GPLOT has some features
which can be useful for general drawing, especially of things such as
block diagrams.

GPLOT is based on the DIMFILM library (Descriptive Instructions for MicroFILM)
which was written by Dr. John Gilbert at ULCC between 1972 and the mid 1990s.
The version of DIMFILM used is the second major version, from around 1983.
The first version was CDC specific, while the second was much more easily 
ported and ran on IBM MVS, Cray COS and UNICOS as well as Convex machines.

DIMFILM source was obtained from Dr. Gilbert via Dr. Adrian Clark in 2005.
Unfortunately, due to a malfunctioning RAID controller, the ULCC production
code was no longer available by that point and, to some degree, things had
to be pieced together from multiple sources by Dr. Gilbert. Consequently,
I do not have the original fonts for DIMFILM and the source may be, at least 
partially, an early beta version. I did some work in 2008 to get
it to work using Hershey fonts, and some minor modifications have been made
then and since as needed to make it functional on NOS and "Unix". Only 
necessary changes have been made, though, and they have been few. The
original source as received is also included in this repository.

I am not aware of DIMFILM being preserved anywhere else. Although the code
does not contain any copyright notices, it is the work of John Gilbert.
I am reluctant to include it, but GPLOT is largely a "front end" to it, so
it isn't optional. Also, I fear DIMFILM might be entirely lost otherwise.

Both GPLOT and DIMFILM are written in FORTRAN-77, which was often the
best supported language prior to the dominance of "Unix-like" and Windows
systems. Fortunately, FORTRAN-77 remains well supported today on "Unix-like"
systems thanks to gfortran (along with modern Fortran, which is gfortran's
main focus, of course).

GPLOT and DIMFILM are developed and maintained primarily on CDC NOS 2.8.7.
NOS has its own way of managing source code, at the heart of which is 
a program called MODIFY. 

MODIFY is quite usable, but is radically different from anything seen on
today's mainstream systems. Source is kept in "program library" (PL) 
files which store many modules (often all those needed for a whole
project or a major subsystem) in a single file. Furthermore, those
files are binary and reside in a record oriented file system on what
is now a very obscure operating system.

Obviously, MODIFY and Git are about as incompatible as it is possible
to get for systems that both manage source code! Storing code in Git on
Github is very appealing as far as preservation and accessibility are
concerned, so getting these two systems to work together seemed to be
a useful sub-project.

The GPLOT project therefore establishes a fair amount of infrastructure that
allows MODIFY and Git to inter-operate without too much pain.

Please note that neither GPLOT nor DIMFILM have been built on any version
of Windows. 


MODIFY/Git Interoperability Infrastructure
------------------------------------------

The scheme to have Git and MODIFY cooperate with each other relies entirely
on the extensive network capabilities opened up by Kevin Jordan's fork of
Tom Hunter's DtCyber CDC Cyber 6000 machine simulator. This fork supports
transfer of files with FTP over TCP/IP and running batch jobs with RBF
(Remote Batch Facility) using RJE/HASP. The machine hosting the Git repo
must have Kevin's RJE Station installed so that `rjecli` in that package
works. 

At the highest level, the goals for this interoperability scheme are to:

- Allow the contents of a MODIFY PL on NOS to be transferred to a Git
  repository.
- Allow changes to source code made on NOS and stored in a MODIFY PL
  to update the contents of the corresponding Git repository.
- Allow changes made to sources in the Git repository to update the
  corresponding MODIFY PL on NOS.
  
The top level functionality (all that people using this need know
about) consists of two Bash shell scripts:

- `import-modify.sh`: This imports the source contained in a MODIFY PL
  into a directory, the contents of which can be put under Git management.
- `export-modify.sh`: This exports the source contained in a Git repo
  (with a directory structure created by `import-modify.sh`) to a MODIFY
  PL.
  
To simplify things as much as possible, some pretty rigid naming conventions
must be adhered to.

- The MODIFY PL must be called `PLabcde` on NOS. The first 2 characters 
  must be `PL` followed by 1 to 5 letters.
- The `import-modify.sh` script only needs to be given `abcde` as a parameter (another
  parameter can be given if necessary, as described below).
- The import script will create a directory called: `abcde-library` in which
  the contents of the MODIFY PL will be stored. Each MODIFY PL module will
  be written to a separate file. More details below.
- The export script also need only be given `abcde` as a parameter. 
- The export script will use the contents of the directory `abcde-library` to update
  the MODIFY PL, `PLabcde`. The PL can be created anew or an existing PL can be
  updated using information from Git about what has changed.
  
The `import-modify.sh` and `export-modify.sh` scripts should only be run from the 
root of the Git repo for a project. They both check for this.
  
Updates are made at the granularity of modules (files). MODIFY can track changes at
the source code line level, but this is too hard to do at present and may not
be a good idea anyway.

Git and MODIFY will be running on two different machines (the same physical machine
might be hosting the Git repo and running DtCyber for NOS, but there are still
two logical machines involved). Information about the machines and users must be
supplied so that the "Git machine" can communicate with the "MODIFY machine" and
vice versa.

The setup required for this is done by a file in the Git repo root directory:
```
.modgitproject
```
A boilerplate version of this is created by:
```
modgitinit.sh
```
This can be edited with the Nano editor (which is usually pre-installed on both
Linux and macOS) using:
```
modgitedit.sh "GPLOT and DIMFILM"
```
The contents of this file are:
```
export MODGITPROJECT="GPLOT and DIMFILM"
export NOSUSER=guest
export NOSPW=guest
export NOSHOST=noshostname
export RJEHOME=~/rje-station
export RJESPOOL=${RJEHOME}/spool
```

The contents must be set so that using FTP and submitting batch jobs to NOS with
`rjecli` in `rje-station` (using RBF - Remote Batch facility) will work.
The NOS account given via `NOSUSER`, `NOSPW` and `NOSHOST` must be the one
where the MODIFY PL lives, of course. `NOSHOST` will be the host name or
IP address of the physical machine running DtCyber.

### The `import-modify.sh` script

To populate or update the contents of `abcde-library` from `PLabcde` use:
```
import-modify.sh abcde
```
This is appropriate for MODIFY PLs that contain source code and documentation
modules. 

If a PL module is an ASCII module, it will be converted from 6/12 Display Code
to upper and lower case "normal" ASCII. This will normally only be the case
for documentation modules. In this case, lines may be up to 72 characters long
if they are entirely lower case or 144 characters if entirely upper case or
somewhere in between if mixed case! These modules are stored in files with the
`.asc` extension.

Otherwise, although the file will be "ASCII" (or UTF-8) on the Git repo host 
machine, it will contain 6/12 Display Code sequences unchanged if they
appear in the module source. I.e. no conversion of `^L` to lower case `l`
will be attempted. Using 6/12 Display Code for source code is fraught with
difficulties and in many languages (e.g. FORTRAN) lower or mixed case can
only appear in character (string) constants. It is best to just leave non-ASCII
modules alone and have strings such as `"Hello"` appear as `"H^E^L^L^O"` in the
code. The clinching argument for this (IMO) is that there is often a 72 
character limit on source line length, but if 6/12 codes are presented
as lower case and edited, you won't know when they have become too long!

Note that MODIFY by default will truncate lines at 72 (or 144) characters
itself. It can be told not to, but that tends to cause difficulties, as
you have to remember to "tell it not to" everywhere.

`COMMON` modules marked as such in MODIFY are written to files with the
`.cmn` extension. These are much the same as "include files". Source files
"include" them using `*CALL cmnname` in MODIFY. These `*CALL` lines are 
converted to `INCLUDE 'cmnname.cmn'` to get the same effect for compilers
that can include files on the Git repo host. This sort of file inclusion
can be nested.

An attempt is made to identify FORTRAN-77 source specifically from its
contents. If such source is identified, it is written to files with
the extension `.f`.

All other material (not ASCII, COMMON or "guessed" FORTRAN-77) is written
to files with the extension `.src`.

At present, we do not try to "guess" source code for other languages.

Note that source code files are explictly converted to upper case only
along the way. With the source languages we are currently using, and
the details of how data is transferred, lower case should not appear
anyway (except in "string" constants), but if it does, it is converted 
to upper case.

The entire contents of a MODIFY PL are always processed by 
`import-modify.sh`. All modules are fetched and converted to files every
time the script is run. If a file has been edited in the Git repo, the
*changes could be lost*. To prevent this happening accidentally,
`import-modify.sh` checks if any files in `abcde-library` have been
modified, as identified by Git. If they have, it will refuse to import
anything unless the environment variable `IM_ALLOW_OVERWRITES` is defined.
To allow overwrites for a specific use of `import-modify.sh` while preventing
them otherwise, the following Bash functionality is useful:
```
IM_ALLOW_OVERWRITES=1 import-modify.sh abcde
```

In general, changes should be committed to Git and 
`export-modify.sh` run to update the MODIFY PL to match before 
`import-modify.sh` is used.

MODIFY PLs used to store CCL (Cyber Control Language) procedures need
special treatment. None of the processing described above should be
performed. Instead, the source should be kept as MODIFY source, quite
unchanged. This is done with the `-n` or `--noprocess` option.
```
import-modify.sh abcde -n
```

There are additional options to help with building the source on
Unix-like systems. 

`-s scriptfile` or `--script scriptfile` will create a Bash
shell script to run gfortran on every `.f` file. 

`--omit a,b,c` will omit modules `a`, `b` and `c` from the
script file. This may be needed for modules that are non-standard
FORTRAN (e.g. include COMPASS assembler). These cannot be compiled
by gfortran.

`-l libname` or `--library libname` will add a section to the
script file to insert all the `.o` files from compiling the
`.f` modules into a static library (`libname.a`).

Note that all "extra arguments" must be combined into a 
single parameter with double quotation marks. For example:
```
import-modify.sh dimfm "-s build-unix.sh --omit getbyt -l dimfilm.a"
```

This script generation facility has proved less useful than anticipated,
but it may be helpful in other projects.

### The `export-modify.sh` script.

To populate or update the contents of `PLabcde` from `abcde-library` use:
```
export-modify.sh abcde
```

This will apply processing appropriate to the file type (`.f`, `.cmn`, `.src`
and `.asc`) by default, for example converting `INCLUDE 'x.cmn'` back to 
`*CALL x`. To prevent all processing (e.g. for CCL procedure libraries),
the `--noprocess` or `-n` option is available.

Also by default, only changed files will be processed and used to update
an existing PL. If all files are to be processed and/or a new PL needs to be
created on the NOS side, the `--all` or `-a` option can be used.

Changed files are detected by Git. The default method is to use (internally)
`git ls-files -m` which will find files that have been changed *since* the last 
commit. However, using the `--committed` or `-c` option will look instead at
files that were changed *in* the last Git commit.

### Behind the scenes

The `import-modify.sh` and `export-modify.sh` should do all that is needed to
keep MODIFY and Git versions of the same software "in sync". Those scripts
use some other tools which may be of interest and more generally useful.

MODIFY source code is extracted from and inserted into PLs by running
dynamically constructed batch jobs on NOS. The jobs are submitted via
RBF (Remote Batch Facility) using Kevin Jordan's `rje-station` software.
The Bash script `runrbf.sh` together with an Expect script called `runrbf.exp` 
run the jobs. To do that, they start an `rje-station`, submit the job by
loading the RJE station's card reader with the job, then wait for the return
of the job's output. The jobs are such that, once the output has been returned,
the job will have been fully processed. The `rje-station` program then exits.
Creating and destroying an RJE Station for each batch job is not great. In the
real world an RJE Station was a large chunk of hardware which would have been
logged in to NOS once and left running while it was used to submit many
jobs. At present, this is a convenient way to do things, but later versions
of these inter-operability tools will hopefully find a way to interact with a
single, long lived, RJE Station instance.

To provide feedback on how the batch jobs went, the `lastspool.sh` script
displays the contents of the latest job output file on `stdout`.

MODIFY source code is transferred to and from NOS by FTP. Although NOS can 
work with any FTP client (probably), there is a tool called `nosftp` provided
which simplifies working with NOS as much as possible. This is implemented by 
a Python program, `nosftp.py` which uses Python's `ftplib` FTP client
library.

Most of the "hard work" is done by two Python programs: `modsplit` for 
MODIFY source import and `modjoin` for MODIFY source export. 

### Installing the inter-operability tools

The tools can be found in the `tools` subdirectory. For the Python
programs, use of a Python "virtual environment" (venv) is highly 
recommended. Python 3.9 or later is needed.

A single venv may be shared between multiple projects,
but creation and use of a venv for these tools is shown for completeness. 
This example venv is called `genv`.

- Create the venv:
```
python3 -m venv ~/genv
```
- Activate the newly created venv:
```
source ~/genv/bin/activate
```

This venv must be active whenever the tools are used.

There are no additional Python libraries or system packages that
need to be installed.

Once the venv has been created and activated, just use:
```
./install.sh
```
in the `tools` directory to install all components. You will
need to be able to use `sudo` and you will be prompted for your
password when running this script.



Transferring all components of GPLOT and DIMFILM between Git and NOS
--------------------------------------------------------------------

Let's assume we start with a clone of the GPLOT Git repository on a "Unix"
system  and a NOS 2.8.7 account with some priliveges (you need to be able to
have a lot of CPU time and disk resources) which has not seen GPLOT before.

First, install the MODIFY/Git inter-operability tools as described above.

Next, configure these tools using:
```
modgitedit.sh
```
also as described above.

There are six major parts to GPLOT/DIMFILM and the environment on NOS
needed to build it.

1. A set of CCL procedures which establish new commands (in effect) which
   are used to build and maintain the source code. I also use these for my 
   other NOS software. They try to make using MODIFY and other NOS utilities
   more convenient and, perhaps, look a bit more like today's mainstream
   systems (although not much like them!). They trade efficiency for
   convenience, but it is unlikely that any NOS system is heavily loaded
   these days (apart from the two public PLATO installations, perhaps).
   This is `PLPROCS`.
2. A PL containing a "utilities library" (source for an object library with widely
   useful functions and subroutines for doing things with character variables
   and interfacing with NOS). There are also a few small "utility programs"
   in the PL that test and use the library. This is `PLUTILS`.
3. A PL containing graphics device output code. This is `PLGRDEV`. It provides
   basic functions that support Encapsulated PostScript (EPS), Scalable Vector
   Graphics (SVG), output to my GTerm "terminal" software and output to
   Tektronix 401x Direct View Storage Tube terminals (which dominated graphics
   devices in the 1970s and early 1980s. Some of this (especially the "serial line"
   output code for terminals) is quite NOS specific.
4. The DIMFILM graphics library. This is `PLDIMFM`. It contains over 500 modules
   with around 25,000 lines of code altogether.
5. The GPLOT program and vector fonts for DIMFILM. This is `PLGPLOT`. 
6. A pair of test and example programs for DIMFILM (separate from GPLOT).
   This is `PLDIMTS`.

To transfer all six PLs from "Unix" to NOS, the Bash shell script:
```
./export-all.sh
```
should be run. This uses `export-modify.sh` appropriately to transfer the full
contents of all the PLs to the NOS account set up with `modgitedit.sh`.

There is one other file which should be moved to the NOS system: `PRLOGIN`,
which is set up as a "login script" to be run when the account logs in.

To send that to NOS:
```
cd nos-tools
./send-files.sh
```
This uses `nosftp` to send the file `prlogin` to the account defined with
`modgitedit.sh`.


Building all components of GPLOT and DIMFILM on NOS.
----------------------------------------------------

The first step is to set up the CCL procedures in `PLPROCS` so they can be
used in subsequent steps. This only needs to be done once for an account,
unless the procedures are themselves changed by the user.

### Installing the CCL procedures in PLPROCS

Login to your NOS account (using an interactive session) in the usual way.
Then use these commands:
```
GET,PLPROCS.
MODIFY,P=PLPROCS,C=BUILD,L=0,S=0,Z./*EDIT BUILD
BEGIN,BUILD,BUILD.
LIBRARY,USERLIB.
UPROC,PRLOGIN.
```

This creates a CCL procedure library called `USERLIB` and sets it up in the
library search list (a bit like changing `$PATH` on "Unix" systems, but not
really). It also sets `PRLOGIN` to be the "script" run on login. This sets
up `USERLIB` on login so it is available and also lets the user pick from 
a list of terminals for the interactive session (so that the Full Screen Editor
- FSE - "just works"). You may want to modify this part, depending on which
terminal emulator you use. 

### Build the utilities library from PLUTILS

The PLs in this software contain procedures for building the "products" whose
source code is in the PL (or a related PL in some cases). This permits a way
of working which is a bit like using `make` on "Unix-like" systems (but,
again, not really). To build the utilities library and related programs use:
```
GET,PLUTILS.
MODEXEC,BUILD,PLUTILS,OPT=2.
```
This uses the `MODEXEC` procedure defined in `PLPROCS` to run the `BUILD` procedure
found in `PLUTILS`. This is how `PLUTILS` can contain the instructions needed to
build it and make running those instructions very easy.

The end result of this is an object library called `LBUTILS`, which GPLOT needs
to link with later. A secondary result is a library of executable binaries
called `BINLIB`. One way of making the programs in there available as if they were
commands is:
```
ATTACH,BINLIB.
LIBRARY,BINLIB/A.
```
Three programs in there that might be of some use are `DATE`, `UPTIME` and
`WHOAMI`, which more or less do what you might expect them to!

To list the contents of a "library" (object, procedure or exectable program),
the `PLPROCS` supplied `MODLIST` can be used. E.g.:
```
/modlist,binlib
          CATALOG OF BINLIB         FILE    1
    REC   NAME      TYPE          LENGTH   CKSUM     DATE

      1   BINLIB    ULIB              20    7550  25/06/26.
      2   DATE      ABS            12505    5125  25/06/26.
           DATE         125
      3   DECFL     ABS            23340    7725  25/06/26.
           DECFL        125
      4   ENCFL     ABS            21702    3672  25/06/26.
           ENCFL        125
      5   UPTIME    ABS            12535    1131  25/06/26.
           UPTIME       125
      6   WHOAMI    ABS            12643    0777  25/06/26.
           WHOAMI       125
      7   BINLIB    OPLD              15    6232  25/06/26.

      8   * EOF *       SUM =     105404
```

Note the `OPT=2` argument for full optimisation (more or less) when code is
compiled. If this is omitted, no optimisation is performed, which will also
be the case if `OPT=0` is given. Levels 1 and 3 are also available, with 3
running cross-procedure optimisation. Refer to the appropriate CDC documentation
for details.

### Build the graphics device support library from PLGRDEV

The build procedures for this library, DIMFILM and the GPLOT program all
reside in the `PLGPLOT` program library.

This creates the `LBGRDEV` object library:
```
GET,PLGPLOT.
GET,PLGRDEV.
MODEXEC,BUILD,PLGPLOT,ARG=DEVICES,OPT=2.
```

### Build the DIMFILM library

This creates the object library `LBDIMFM`:
```
GET,PLDIMFM.
MODEXEC,BUILD,PLGPLOT,ARG=DIMFILM,OPT=2.
```
This is likely to take several minutes.


### Build the GPLOT program and extract the font definition file

Now that all the required object libraries have been built, the GPLOT
program itself can be made. This will do that:
```
MODEXEC,BUILD,PLGPLOT,ARG=GPLOT,OPT=2.
```

There are two results. The executable binary is written to a file
called `GPLOT`. This can be used in later sessions (by this account)
with these commands:
```
ATTACH,GPLOT.
GPLOT. (etc., see below)
```

The second result is the creation of a file called `DADIMFO` which
contains data that defines the vector fonts used by DIMFILM.


### Build the DIMFILM test programs

It is possible to use DIMFILM to provide graphical output from programs
other than GPLOT, of course, by writing a program (usually in FORTRAN-77)
and linking it with the DIMFILM library and support libraries.

Two "test programs" are supplied both to help verify DIMFILM is working
correctly and to serve as examples of how to write programs that call it.

- DIMTEST produces 10 frames of output, exercising much of the graph
  plotting functionality of DIMFILM.
- LSTDFON produces 24 frames, one for each available font. Each frame
  is a font table and the full set is a complete font catalog.
  
To build these programs on NOS:
```
GET,PLDIMTS.
MODEXEC,BUILD,PLDIMTS,OPT=2.
```

To run them, first `ATTACH` the fonts file (GPLOT does this internally,
so the user doesn't have to remember to do this) then run them by name.
```
ATTACH,DADIMFO.
ATTACH,DIMTEST.
ATTACH,LSTDFON.
DIMTEST.
LSTDFON.
```
The output of DIMTEST is a set of EPS files named `NW001` to
`NW010`. The output of LSTDFON is also a set of EPS files called
`FON001` to `FON024`.


Using EPS and SVG files created on NOS
--------------------------------------

There is no direct way of viewing EPS or SVG files on NOS. They must
be viewed on systems which can display PostScript or PostScript converted
to PDF for EPS files, or in a browser for SVG files.

On NOS systems running the NOS HTTP server, the SVG files could be transferred
to the WWW account and viewed with any web browser on almost any other system.
There is another piece of NOS software which implements a Markdown-like scheme
for writing and posting "notes" on the NOS HTTP server which integrates with
GPLOT using SVG output from graphs and diagrams. This is not described here,
though.

However, it will often be useful to copy both EPS and SVG output as files
to "mainstream" systems for further use. This is most easily done with
`NOSFTP`. These files contain lower case characters and should be treated
as ASCII. For example:
```
$ nosftp tester nuc1
Password for NOS account: 
Contacting NOS FTP server on host: nuc1
230 USER LOGGED IN, PROCEED.
220 SERVICE READY FOR NEW USER.
Local cwd now: /Volumes/qemu-main/gitprojects/gplot2

NOS FTP> get nw009 nw009.eps ascii
... retrieving file: nw009
226 CLOSING DATA CONNECTION.

NOS FTP> bye
```

Further developing this software
--------------------------------

Once the above transfer and build procedures have been carried out,
further development work on GPLOT or any of its components can be 
carried out on NOS or the "Unix-like" Git host machine, and the
MODIFY and Git representations of the source can be kept in sync using
`import-modify.sh` and `export-modify.sh` as described above.


Building all components of GPLOT and DIMFILM on "Unix".
-------------------------------------------------------

It is straightforward to build DIMFILM and GPLOT on "Unix-like"
systems such as macOS and Linux. As far as we are concerned for
this project, all these systems are almost identical.

The main requirement is that `gfortran` is installed and working,
along with the usual C/C++ development environment that is always
available on these systems. As part of the GNU Compiler Collection,
`gfortran` is also readily available. 

Note that the "Unix" versions of DIMFILM and GPLOT are *ports* of
the NOS version. The NOS code is quite portable (well, to a 
`gfortran` environment -- some features such as 7 character variable
names might cause trouble with strict FORTRAN-77 compilers). It turns
out that a very small amount of conditional compilation (supported
by MODIFY and the C Pre-Processor) and a small "support library"
that provides some functions compatible with NOS and FTN5, is almost
all that is needed. A very small number of other incompatibilites
are handled by string substitution using the `asciify` tool. These
are primarily to cope with the highly non-standard `PROGRAM` statement
FTN5 uses and a few floating point constants (which need to be larger
on NOS than IEEE 32 bit floating point range). The string substitutions
are defined in `.json` files.

At present, the "Unix" version of GPLOT is intended to behave *identically*
to the NOS version. This includes limitations to 7 character file names
and all input being converted to upper case! Note that files to be read
will generally have to have UPPER CASE names.

The build procedure is very similar to that on NOS.

### Build the utilities library from PLUTILS

```
cd ports/unix
./make-utils.sh
```
This pre-processes the source in `utils-library` to a newly created
source directory `utils-source`, then compiles the contents with
`gfortran`, creating the library `lib/utils.a`

In fact, only the source file `chars.f` is needed for GPLOT/DIMFILM.

### Build the graphics device support library from PLGRDEV

```
./make-grdev.sh
```
This pre-processes the source in `grdev-library` to a newly created
source directory `grdev-source`, then compiles the contents with
`gfortran`, creating the library `lib/grdev.a`

All five devices supported on NOS are also supported on Unix. These
are: EPSBIN (binary Encapsulated PostScript), EPSCOL (colour
Encapsulated PostScript), TEK (Tektronix 401x DVST terminals),
GTERM (The GTerm terminal, supporting colour) and SVG (Scalable
Vector Graphics).

### Build the DIMFILM library

```
./make-dimfm.sh
```
This pre-processes the source in `dimfm-library` to a newly created
source directory `dimfm-source`, then compiles the contents with
`gfortran`, creating the library `lib/dimfm.a`

This library has identical functionality to the NOS version.
FORTRAN-77 (and maybe later version of Fortran?) programs can
be compiled with `gfortran` and linked with it (and the other
libraries noted above) to produce graphical output.

### Build the Unix support library

```
./make-support.sh
```
This compiles the support library code (from `support.f` and
`support_c.c`) to create the library `lib/support.a`. Any programs
using DIMFILM will have to link with this library in addition to
the others mentioned above.

### Build the GPLOT program and copy the font definition file

```
./make-gplot.sh
```
This pre-processes the source in `gplot-library` to a newly created
source directory `gplot-source`, then compiles the contents with
`gfortran`, creating a relocateable object file which is then
linked with all the above libraries to create the `gplot` program.
The DIMFILM font file, found in `gplot-library/dadimfo.src` is also
copied to `ports/unix/DADIMFO`. After this:
```
./gplot
```
will start GPLOT, which should function identically to the NOS
version.

Note that command line arguments are as described for NOS in the
GPLOT documentation, except that arguments are separated by spaces
rather than commas. The same `keyword=value` and `keyword` format
is retained, as used on NOS.

### Build the DIMFILM test programs

```
./make-dimts.sh
```
This pre-processes the source in `dimts-library` to a newly created
source directory `dimts-source`, then compiles the contents with
`gfortran`, creating two relocateable object files which are
linked with all the above libraries to create the `dimtest` and 
`lstdfon` programs.

These can then be run in the usual way:
```
./dimtest
./lstdfon
```

These should create the same EPS files described above for the 
NOS versions of these programs.

### Using EPS files on macOS

Unfortunately, recent versions of macOS can no longer view EPS
and other PostScript files just by opening them (double-clicking 
or whatever), for reasons best known to Apple. 

A script called `epsview.sh` is included in the `tools` directory
and installed along with the other tools by `install.sh` in that
location. This uses programs in the `ghostscript` package to
convert EPS to PDF and then open the PDF version. This displays
output in macOS `Preview`. It can also produces a `.png` image version 
using Imagemagick tools if the `-p` option is supplied. 
This script takes steps to ensure that
EPS files output by DIMFILM can be opened correctly and displayed
in good quality.

It is also possible to crop the output to match the true extents
of the EPS data rather than using the paper size specified when
the file was generated. This is done with the `-c` option.

For this to work, `ghostscript` and `imagemagick` must be installed
with Homebrew or MacPorts. Once this is done, EPS files can be displayed
from the command line:
```
$ epsview.sh NW009 
```
will open a new `Preview` window or tab displaying the contents of the
given EPS file (the name of which need not have the `.eps` extension).


Building DIMFILM on COS
-----------------------

Thanks to Chris Fenton and Andras Tantos, the Cray Operating System
(COS) that ran on early models of Cray supercomputers before UNICOS
(which is a version of Unix) was rescued from oblivion. The story of
how that was done is remarkable -- suffice it to say that COS was as
near to being totally lost as can be without actually vanishing
completely. Much more information on this can be found 
[here.](https://www.modularcircuits.com/blog/articles/the-cray-files/)

Andras Tantos has developed a very effective simulator for Cray PVP
machines, including the Cray X-MP and Cray SV1. The latter model can
be used to run UNICOS and the former is suitable for the recovered
COS V1.17 software. The simulator can be found
[here.](https://github.com/andrastantos/cray-sim).

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


### Build the graphics device support library from PLGRDEV

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


### Build the DIMFILM library

```
./make-dimfm.sh
```
This pre-processes the source in `dimfm-library` to a newly created
source directory `dimfm-source`, then compiles the contents with
`kFTC`, creating the library `lib/dimfilm.lib`

This library has identical functionality to the NOS version.
Other FORTRAN-77 programs can be compiled with `kFTC` and linked
with it to generate graphical output.


### Build the COS support library

```
./make-support.sh
```
This compiles the support library code (from `support.f`)
to create the library `lib/support.lib`. Any programs
using DIMFILM will have to link with this library in addition to
the others mentioned above.


### Build the DIMFILM test programs

```
./make-dimts.sh
```
This pre-processes the source in `dimts-library` to a newly created
source directory `dimts-source`, then compiles the contents with
`kFTC`, creating two relocateable object files which are
linked with all the above libraries to create the `dimtest.abs` and 
`lstdfon.abs` programs.


### Running cross-compiled programs on COS

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



GPLOT examples and mini tutorial
--------------------------------

### The simplest graph
Let's start by plotting a very simple graph. Normally, data to be
plotted exists in disk files, but, in this first example, the data
will be internally generated by the `MEMTEST` command, which dynamically
allocates data arrays on NOS (static arrays are used on "Unix-like" systems)
and fills X,Y values with a sine wave.

---

![](sveg001.svg)

---

The script file (obey file) to generate this contains:
```
C SIMPLEST GRAPH
C
RESET
MEMTEST
TITLE "A*L VERY SIMPLE GRAPH"
XLABEL "X*L AXIS"
YLABEL "Y*L AXIS"
XYLINE
```

Although very minimal, there are a few points worth noting.

- For maximum NOS / "Unix" compatibility, it is best to use only UPPER CASE in obey files. If you use lower case, it will work on both systems, but the results may be different. NOS (in `NORMAL` mode) will internally convert lower case to upper case. So "A very simple graph" will appear exactly as that on "Unix", but as "A VERY SIMPLE GRAPH" on NOS. Using upper case and DIMFILM "string markup" is portable and handles subscripts, superscripts, fractions and so on too. Note that GPLOT should *not* be used in `ASCII` mode on NOS! GPLOT does not attempt to deal with 6/12 Display Code, but will convert it (mostly!) to upper case. However, internal arrays are not sized for 6/12 representations, so there will be problems.
- The `RESET` command re-establishes a default state. If multiple obey files are used in one GPLOT session without using `RESET` at the top of each file, the results will be unpredictable (the state at the end on one obey file will be inherited by the next).
- This script is device independent. It can be used "as is" with any of the supported devices. To generate SVG from it, we can use:
```
/ GPLOT or $ UGPLOT
? dev svg sveg 1200,800
? prefix obey-files
? obey obgraf1
? ex
```
which will create an SVG file called `sveg001` (`sveg001.svg` on "Unix") using a "resolution" of 1200 by 800 pixels (with SVG, this is
really a "size" rather than a resolution, as the vector data can be scaled without losing detail). 

- The various annotations associated with a graph (title, axis labels, etc.) should appear before the command which plots the data
(`XYLINE` here).
- The ranges of the X and Y axes are automatically determined from the supplied data.
- The graph is drawn in a square area in the center of the output device "canvas". When plotting graphs, it is often better to fill the output canvas.
- Everything is drawn in a single colour (red).

We will address these last two deficiencies in the next example.

### Using different colours and filling the canvas
Here is the same graph, but with different classes of things in different
colours and filling the output canvas.

---

![](sv2g001.svg)

---

The code for this is:
```
C USING COLOUR/STYLE GROUPS
C
RESET
GRAPHMODE ON
MEMTEST
CSGROUP GENERAL
COL 1 0 0
CSG TEXT
COL 0 0 0
CSG ANNOT
COL 0 0.1 1
TITLE "A*L SIMPLE GRAPH WITH BETTER COLOURS"
XLABEL "X*L AXIS"
YLABEL "Y*L AXIS"
XYLINE
```

In GPLOT/DIMFILM there are 3 "different classes of things", namely:

- Text (`TEXT`): this refers to all types of "strings".
- Annotation (`ANNOT`): this refers to the various things that annotate graphs specifically.
- Other (`GENERAL`): All elements not in the above classes. This includes all lines and points.

All of them are members of the class `ALL`.

These 3 classes are called "colour/style groups" and the current class is selected with the
`CSGROUP` command. Following this, `COLOUR` and `STYLE` commands set the colour (as normalised RGB)
and line style (solid, dashed, etc.) for that `CSGROUP` only.

Note that any command can be abbreviated -- so long as the abbreviation remains unique, it will work.
(Note that this is *not* true of evaluator operators, as we will see).

### Adding a grid or graticule
Hmm


GPLOT cheat sheet
-----------------

GPLOT has quite a few commands and facilities. It is fully documented
in a PDF format manual (LaTeX source for this can be found in the 
`doc/gplot` directory).

A quick guide in "ASCII art" format is included here, and this may often
suffice.

```
=====================================================================================================================
GPLOT STORAGE UNITS
=====================================================================================================================
                  NDATA NUMBER OF GRAPH DATA POINTS
                  NELEM EVALUATION LENGTH
                    |
                    | MAXPOINTS (=1000 DEFL) 60 BIT REALS
            1       |     |                                                       1   2         8   9
          +---+---+ ... +---+                                                   +---+---+     +---+---+
 NSTACK-1 |   |   |     |   |                    SCALAR VALUE REGISTERS STO/RCL |   |   | ... |   |   |  60 BIT REALS
          +---+---+ ... +---+                                                   +---+---+     +---+---+
         ... NSTACK=8 DEFL ...  <-- TOS - TOP OF STACK
          +---+---+ ... +---+                                                   +---+---+     +---+---+
YEH,XE 3  |   |   |     |   | -- FOR ERROR       PROCEDURE REGISTERS PROC/@N    |   |   | ... |   |   |  80 CHARACTERS
          +---+---+ ... +---+    BARS IN                                        +---+---+     +---+---+
YEL,YE 2  |   |   |     |   | -- GRAPHS
          +---+---+ ... +---+                                                   +---+---+     +---+---+
   Y   1  |   |   |     |   | -- FOR POINTS      STRING REGISTERS STRING        |   |   | ... |   |   |  80 CHARACTERS
          +===+===+ ... +===+    IN                                             +---+---+     +---+---+
   X   0  |   |   |     |   | -- GRAPHS                                           
          +---+---+ ... +---+ ----- EVAL TREATS X/0 SPECIALLY
                                   
  STACK OF ARRAYS USED BOTH FOR GRAPH                                 PURPOSE SPECIFIC "REGISTERS"
  POINT DATA AND FOR THE RPN FUNCTION
  OR PROCEDURE EVALUATOR.

======================================================================================================================
GPLOT COMMANDS - ALL CAN BE ABBREVIATED SO LONG AS THEY REMAIN UNIQUE
======================================================================================================================
-------------------------------------------------------------------------------------------- SYSTEM ------------------
DEVICE .............. NAME [FILE] - SELECT OUTPUT DEVICE
CLEAR ............... CLEAR DRAWING AREA / END OUTPUT FILE                                          DEVICES
FILL ................ FILL SCREEN WITH COLOUR (DEV. DEP.)                                  --------------------------
OBEY ................ NAME [PARAMETERS ...] - READ COMMANDS FROM FILE (NESTABLE)           GTERM  - PYTHON TERMINAL
HELP ................ DISPLAY COMMAND HELP                                                 TEK4K  - TEKTRONIX 4014
STATUS .............. DISPLAY STATUS INFORMATION                                           EPSCOL - COLOUR EPSF FILES 
MEMTEST ............. TEST DYNAMIC MEMORY. GENERATE GRAPH PLOTTING TEST DATA               EPSBIN - BINARY EPSF FILES
GET ................. NAME - GET AN INDIRECT PERMANENT FILE                                SVG    - SVG FORMAT FILES
LOGFILE ............. NAME - OPEN A COMMAND LOG FILE
MAXPOINTS ........... NUMBER - SET MAX DATA POINTS IN INTERNAL ARRAYS (DEFL. 1000)
NSTACK .............. N - SET EVAL STACK SIZE (MIN. 4, DEFL. 8)
EXIT ................ EXIT GPLOT
-------------------------------------------------------------------------------------------- COMMON SETTINGS ---------
COLOUR .............. R G B - SET RGB COLOUR TO USE
WIDTH ............... WIDTH - SET LINE WIDTH
MARKER .............. NUMBER - SET POINT MARKER NUMBER
STYLE ............... "SOLID" "DASH" "DOT" "DASHDOT" - LINE STYLE
-------------------------------------------------------------------------------------------- GRAPH PLOTTING ----------
READ ................ NAME XCOL YCOL [YECOL [XECOL]] - READ A DATA FILE USING COLUMNS. NAME=HERE, USE INPUT TO EOF LINE 
XYPOINT ............. DRAW XY GRAPH WITH POINTS
XYLINE .............. DRAW XY GRAPH WITH LINES                                     MULTIPLE PLOTS, SAME AXES EXAMPLE
XYHISTOGRAM ......... DRAW XY HISTOGRAM                                            ---------------------------------
GRMOVE .............. X Y - MOVE TO GRAPH COORDS (X,Y)                             READ ...
GRDRAW .............. X Y - DRAW TO GRAPH COORDS (X,Y)                             COLOUR 0 0 0
XYAUTO .............. FIND BOTH AXIS RANGES AUTOMATICALLY                          TITLE "A *LTITLE"
XRANGE .............. XLO YHI - SET X AXIS RANGE                                   XLABEL "X *LAXIS"
YRANGE .............. YLO YHI - SET Y AXIS RANGE                                   YLABEL "Y *LAXIS"
XYSAME .............. KEEP PREVIOUS XY AXIS RANGES                                 XYLINE
XLINEAR ............. USE LINEAR X AXIS                                            ANNOTATE OFF
YLINEAR ............. USE LINEAR Y AXIS                                            XYSAME
XLOG ................ USE LOG X AXIS                                               READ ...
YLOG ................ USE LOG Y AXIS                                               COLOUR 1 0 0
GRID ................ "NONE" "X" "Y" "BOTH" - GRID GRAPH IN AXIS                   XYLINE  ( ... ETC ... )
INTERPOLATE ......... "LINEAR" "CUBIC" "QUINTIC" [N] - INTERPOLATE
ASYMYERRORBARS ...... "ON" "OFF" - ASYMMETRIC Y ERROR BARS (OFF FOR X AND Y ERROR BARS)
HISTSTYLE ........... "ABUT" "ABUT+SHADE" "LINES" "WIDE" "WIDE+SHADE" [WIDTH] - HISTOGRAM STYLE
ANNOTATE ............ "ON" "OFF" - TURN GRAPH ANNOTATION ON/OFF
RIGHTANNOT .......... "ON" "OFF" - TURN RIGHT EDGE ANNOTATION ON/OFF (DRAWN ONLY IF ANNOTATE OFF)
TITLE ............... "TEXT" - SET TITLE
XLABEL .............. "TEXT" - SET X AXIS LABEL
YLABEL .............. "TEXT" - SET Y AXIS LABEL
RYLABEL ............. "TEXT" - SET RIGHT EDGE Y AXIS LABEL
GSTYLE .............. "BOXED" "AXES" "OPEN" - OVERALL GRAPH DRAWING STYLE (AXES MUST BE IN RANGE TO DRAW, SEE AXCUT)
AXCUT ............... X0 Y0 - SET POINT THROUGH WHICH AXES PASS (DEFAULT: 0,0)
-------------------------------------------------------------------------------------------- GENERAL DRAWING ----------
BOUNDS .............. XL XH YL YH - SET PLOT BOUNDS (USER COORDINATE SYSTEM)
PANE ................ XL XH YL YH - SET PANE (CLIPPING) AREA. ALSO AREA IN WHICH GRAPH WILL BE PLOTTED.
CANVAS .............. XL XH YL YH - SET BOUNDS AND PANE (TO BE THE SAME)
UNPANE .............. STOP USING ANY PANE                                         PLNOTES MARKDOWN INTEGRATION
PANEOUTLINE ......... OUTLINE THE PANE                                    ---------------------------------------------
BLANK ............... XL XH YL YH - SET BLANK AREA                        ) GPLOT COMMMAND
UNBLANK ............. STOP USING ANY BLANK AREA                           ) ... ETC ... BUT DON'T USE DEVICE OR CLEAR
BLANKOUTLINE ........ OUTLINE THE BLANK AREA                                BLANK LINE ENDS
MOVE ................ X Y - MOVE TO POSITION                              MODEXEC,BUILD,PLNOTES,ARG=ANOTE -> ANOTE.HTML     
DRAW ................ X Y - DRAW TO POSITION
PATH ................ "C" OR "O" - DRAW A CLOSED / OPEN POLYLINE. COORDINATES FROM X,Y ARRAYS.
CIRCLE .............. X Y R - DRAW CIRCLE, CENTER X Y, RADIUS R.
ARC ................. X Y R A1 A2 - DRAW A CIRCULAR ARC, CENTER X Y, RADIUS R START ANGLE A1, END ANGLE A2.
RECT ................ X Y W H - DRAW A RECTANGLE, BOTTOM LEFT CORNER AT X Y, WIDTH W, HEIGHT H.
CRECT ............... X Y W H - DRAW A RECTANGLE, CENTER X Y, WIDTH W, HEIGHT H.
FONT ................ SET FONTNAME - FONT/SYMBOLS/MARKERS TO USE. SET: 1 2 3 S M FOR 3 ALPHABETS, SYMBOL, MARKER.
LISTFONT ............ LIST AVAILABLE FONTS
SYMHT ............... TEXT/SYMBOL/MARKER HEIGHT IN USER BOUNDS UNITS
SYMANG .............. TEXT DRAWING ANGLE WRT X AXIS (CCW DEGREES)
TEXT ................ "TEXT" - DRAW TEXT (WITH FORMAT CONTROL)
CTEXT ............... W "TEXT" - TEXT H CENTERED ON CUR XPOS,YPOS, WIDTH W USER UNITS (O FOR DEFAULT)
-------------------------------------------------------------------------------------------- RPN EVALUATOR ------------
ERANGE .............. BASE START STOP NELEM - SET RANGE OF VALUES IN X/0 STACK REGISTER. BASE 1 LINEAR, ELSE LOG BASE.
EVAL ................ RPN - EXECUTE A PROCEDURE / EVALUATE A FUNCTION USING AN RPN NOTATION, REF PROC REGS USING @1 ETC.
ITEVAL .............. START END STEP RPN - ITERATED EVALUATION OVER START TO END BY STEP, ITERATION IN I
PROC ................ N RPN - STORE RPN CODE STRING IN PROC REGISTER N. USE WITH @N FOR EVAL / ITEVAL.
LOADPROC ............ N NAME - LOAD A NAMED PROCEDURE FROM GPLPROC PROCEDURE LIBRARY FILE INTO PROC REGISTER N.
STRING .............. N "TEXT" - SET CONTENTS OF STRING REGISTER N
STO ................. N X - STORE X IN MEMORY REGISTER N
RCL ................. N - DISPLAY CONTENTS OF MEMORY REGISTER N
ZEROVAL ............. V - SET VALUE FOR DIVIDE-BY-ZERO TRAPS. DEFL. 1E-9. 0 TO QUIT EVAL ON DIVIDE-BY-ZERO.
BBSTART ............. START TO FIND BOUNDING BOX INSTEAD OF DRAWING ANYTHING WITH M AND D OPERATORS.
BBEND ............... EXIT BOUNDING BOX MODE AND RETURN TO DRAWING WITH M AND D OPERATORS.
BBSET ............... SET THE BOUNDS TO MATCH THE EVALUATOR BOUNDING BOX FOUND WITH BBSTART/BBEND.
-------------------------------------------------------------------------------------------- LSYSTEM EVALUATOR ---------
LSYSTEM ............. NRULES NITER ANGLE - GENERATE AND DRAW AN L-SYSTEM. AXIOM IN STRING 1, RULES IN STRINGS 2-9.
========================================================================================================================

========================================================================================================================
EVAL RPN EVALUATOR TOKENS : PROCEDURE TO EVALUATE GIVEN AS STRING: TOKEN,TOKEN,...,TOKEN - NO ABBREVIATIONS ALLOWED
========================================================================================================================
A: ARRAY OF NELEM VALUES TO WHICH OPERATOR IS APPLIED. C: SCALAR / CONSTANT VALUE (CONTENTS OF ARRAY ELEMENT A[1]).
-------------------------------------------------------------------------------------------- OPERANDS ------------------
<DIGITS> ............ (    -- C1 ) : SET TOS ARRAY TO A LITERAL CONSTANT.
X ................... (    -- A1 ) : SET TOS TO X RANGE ARRAY.
PI .................. (    -- C1 ) : SET TOS TO PI.
E ................... (    -- C1 ) : SET TOS TO E.
I ................... (    -- C1 ) : SET TOS TO ITERATION NUMBER FROM ITEVAL. 1 IF IN EVAL.
IDX ................. (    -- A1 ) : SET TOS TO THE ARRAY ELEMENT INDEX.
TWPI ................ (    -- C1 ) : SET TOS TO 2 PI.
PI/2 ................ (    -- C1 ) : SET TOS TO PI / 2.
-------------------------------------------------------------------------------------------- STACK MANIPULATION --------
SWAP ................ ( A1 A2 -- A2 A1 ) : SWAP OR EXCHANGE TOP 2 STACK ARRAYS.
DUP ................. ( A1 -- A1 A1 ) : DUPLICATE TOP OF STACK
POP ................. ( A1 -- ) : POP TOP OF STACK
SETX ................ ( A1 -- A1 ) : OVERWRITE RANGE / GRAPH X VALUES WITH STACK TOP VALUES: X = A1
SETY ................ ( A1 -- A1 ) : OVERWRITE GRAPH Y VALUES WITH STACK TOP VALUES: Y = A1
XLIN ................ ( C1 C2 C3 -- ) : X = C1 TO C2 IN C3 LINEAR STEPS
XLOG ................ ( C1 C2 C3 C4 -- ) : X = C1**C2 TO C1**C3 IN C4 STEPS
ELEM ................ ( A1 C2 -- A1 C2 ) : C2 = A1[C2]
I0IJ ................ ( C1 C2 -- C1' C2' ) : C1'=MOD(C1,C2), C2'=(C1/C2) : 1D TO 2D INDEX CONVERSION, 0 BASED.
I1IJ ................ ( C1 C2 -- C1' C2' ) : C1'=MOD(C1,C2)+1, C2'=(C1/C2)+1 : 1D TO 2D INDEX CONVERSION, 1 BASED.
-------------------------------------------------------------------------------------------- BASIC ARITHMETIC ----------
+ ................... ( A1 A2 -- A1 ) : ADD ARRAY ELEMENTS: A1 = A1 + A2
- ................... ( A1 A2 -- A1 ) : SUBTRACT ARRAY ELEMENTS: A1 = A1 - A2
R- .................. ( A1 A2 -- A1 ) : REVERSE SUBTRACT: A1 = A2 - A1
* ................... ( A1 A2 -- A1 ) : MULTIPLY ARRAY ELEMENTS: A1 = A1 * A2
** .................. ( A1 A2 -- A1 ) : EXPONENTIATION: A1 = A1 ** A2
/ ................... ( A1 A2 -- A1 ) : DIVIDE ARRAY ELEMENTS: A1 = A1 / A2
R/ .................. ( A1 A2 -- A1 ) : REVERSE DIVIDE: A1 = A2 / A1
RCP ................. ( A1 -- A1 ) : RECIPROCAL A1 = 1.0 / A1
CHS ................. ( A1 -- A1 ) : A1 = -A1
ABS ................. ( A1 -- A1 ) : A1 = ABS(A1)
-------------------------------------------------------------------------------------------- MATH FUNCTIONS -----------
SIN ................. ( A1 -- A1 ) : A1 = SIN(A1)
COS ................. ( A1 -- A1 ) : A1 = COS(A1)
TAN ................. ( A1 -- A1 ) : A1 = TAN(A1)                                      GPLPROC LIBRARY FORMAT
ASIN ................ ( A1 -- A1 ) : A1 = ARCSIN(A1)                                   ----------------------
ACOS ................ ( A1 -- A1 ) : A1 = ARCCOS(A1)                                   NAME1
ATAN ................ ( A1 -- A1 ) : A1 = ARCTAN(A1)                                   RPN FOR PROCEDURE NAME1
SINH ................ ( A1 -- A1 ) : A1 = SINH(A1), DOMAIN |X| < 742.36                NAME2
COSH ................ ( A1 -- A1 ) : A1 = COSH(A1), DOMAIN |X| < 742.36                RPN FOR PROCEDURE NAME2
TANH ................ ( A1 -- A1 ) : A1 = TANH(A1), DOMAIN |X| < 742.36                ... ETC. ...
SQRT ................ ( A1 -- A1 ) : A1 = SQRT(A1)
LOG ................. ( A1 -- A1 ) : BASE E LOGARITHM: A1 = LN(A1)
LG10 ................ ( A1 -- A1 ) : BASE 10 LOGARITHM: A1 = LOG10(A1)
LOG2 ................ ( A1 -- A1 ) : BASE 2 LOGARITHM: A1 = LOG2(A1)
EXP ................. ( A1 -- A1 ) : A1 =  E ** A1  DOMAIN: -675.81 TO 741.66
RAND ................ ( A1 -- A1 ) : UNIFORMLY DISTRIBUTED RANDOMS [0:A1): A1 = A1 * RANDOM
SEED ................ ( C1 -- ) : SET RANDOM NUMBER SEED TO A1[1]
-------------------------------------------------------------------------------------------- NUMBER RANGE RELATED -----
MIN ................. ( A1 A2 -- A1 ) : A1 = MIN(A1,A2)
MAX ................. ( A1 A2 -- A1 ) : A1 = MAX(A1,A2)
MOD ................. ( A1 A2 -- A1 ) : A1 = MOD(A1,A2) OR A1 = A1 % A2
SIGN ................ ( A1 -- A1 ) : A1 = (A1 < 0) ? -1 : 1
-------------------------------------------------------------------------------------------- CONDITIONALS -------------
ODD ................. ( A1 -- A1 ) : A1 = 1 WHERE INT(A1) IS ODD ELSE 0
GT .................. ( A1 A2 -- A1 A2 A3 ) : A3 = (A1 > A2) ? 1 : 0 ;
LT .................. ( A1 A2 -- A1 A2 A3 ) : A3 = (A1 < A2) ? 1 : 0 ;
LE .................. ( A1 A2 -- A1 A2 A3 ) : A3 = (A1 <= A2) ? 1 : 0 ;
GE .................. ( A1 A2 -- A1 A2 A3 ) : A3 = (A1 >= A2) ? 1 : 0 ;
EQ .................. ( A1 A2 -- A1 A2 A3 ) : A3 = (A1 == A2) ? 1 : 0 ;
NE .................. ( A1 A2 -- A1 A2 A3 ) : A3 = (A1 != A2) ? 1 : 0 ;
NOT ................. ( A1 -- A1 ) : A1 = (A1 == 0) ? 1 : 0 ;
SEL ................. ( A1 A2 A3 -- A1 A2 A3 ) : A3 = (A3 == 0) ? A1 : A2 ;
-------------------------------------------------------------------------------------------- NUMBER TYPE CONVERSIONS --
INT ................. ( A1 -- A1 ) : TRUNCATION A1 = INT(A1)
FRAC ................ ( A1 -- A1 ) : FRACTIONAL PART A1 = FRAC(A1)
FLR ................. ( A1 -- A1 ) : A1 = FLOOR(A1)
CEIL ................ ( A1 -- A1 ) : A1 = CEILING(A1)
-------------------------------------------------------------------------------------------- GRAPHICS -----------------
M ................... ( C1 C2 -- ) : MOVE TO (C1,C2)
D ................... ( C1 C2 -- ) : DRAW TO (C1,C2)
C ................... ( C1 C2 C3 -- ) : DRAW CIRCLE, CENTER C1,C2 RADIUS C3.
A ................... ( C1 C2 C3 C4 C5 -- ) : DRAW ARC, CENTER C1,C2 RADIUS C3, ANGLES C4 TO C5.
BOX ................. ( C1 C2 C3 C4 -- ) : DRAW RECTANGLE, BOTTOM LEFT C1,C2 WIDTH C3 HEIGHT C4.
HDSH ................ ( A1 -- A1 ) : DRAW RELATIVE (1,0) IF A1[I] > 0 ELSE MOVE
VDSH ................ ( A1 -- A1 ) : DRAW RELATIVE (0,1) IF A1[I] > 0 ELSE MOVE
PTHO ................ ( A1 A2 -- ) : DRAW POLYLINE X=A1,Y=A2, OPEN
PTHC ................ ( A1 A2 -- ) : DRAW POLYLINE X=A1,Y=A2, CLOSED
T ................... ( C1 -- ) : DRAW CONTENTS OF STRING REGISTER C1 AS TEXT.
TVF ................. ( C1 C2 -- C1 ) : DRAW C1 AS TEXT USING FORMAT STRING IN C2.
TVI ................. ( C1 C2 -- C1 ) : DRAW INT(C1) AS TEXT USING FORMAT STRING IN C2.
TS .................. ( C1 -- ) : DRAW SYMBOL WITH CODE NUMBER C1
TM .................. ( C1 -- ) : DRAW MARKER WITH CODE NUMBER C1
TC .................. ( C1 -- ) : DRAW CHARACTER WITH CODE C1 FROM CURRENT ALPHABET
TH .................. ( C1 -- ) : SET TEXT/MARKER/SYMBOL HEIGHT
TA .................. ( C1 -- ) : SET TEXT DRAWING ANGLE IN DEGREES CCW
TSC ................. ( -- ) : START TEXT CONTINUATION
TEC ................. ( -- ) : END TEXT CONTINUATION
TLEN ................ ( C1 -- C1 ) : GET THE LENGTH IN BOUNDS UNITS AT UNIT HEIGHT OF STRING IN REGISTER C1.
-------------------------------------------------------------------------------------------- SYSTEM -------------------
STO ................. ( A1 C2 -- A1 ) : STORE A1[1] (A.K.A. C1) IN REGISTER C2
RCL ................. ( C1 -- C1 ) : RECALL CONST VALUE FROM REGISTER INT(C1)
P ................... ( -- ) : PRINT TOS ARRAY FIRST AND LAST ELEMENTS
PE .................. ( A1 C2 C3 -- A1 ) : PRINT ELEMENTS C2 TO C3 OF A1 IN FREE FORMAT
PC .................. ( C1 -- C1 ) : PRINT C1 (TOS A[1]) IN FREE FORMAT.
DUMP ................ ( -- ) : PRINT ALL STACK LEVELS FIRST AND LAST ELEMENTS
========================================================================================================================

========================================================================================================================
L-SYSTEM EVALUATOR (LSYSTEM NRULES NITER ANGLE) - HOW TO WRITE AXIOMS AND RULES
========================================================================================================================
INSPIRATION: HTTP://PAULBOURKE.NET/FRACTALS/LSYS/
-------------------------------------------------------------------------------------------- SYMBOLS -------------------
A,B,C,D,E,F,M,X,Y ... SUBSTITUTION VARIABLES
F ................... DRAW 1 UNIT FORWARD AT CURRENT ACCUMULATED ANGLE
M ................... MOVE 1 UNIT FORWARD AT CURRENT ACCUMULATED ANGLE
+ ................... TURN LEFT BY DEFINITION ANGLE
- ................... TURN RIGHT BY DEFINITION ANGLE
[ ................... PUSH CURRENT POSITION AND ANGLE
] ................... POP TO LAST PUSHED POSITION AND ANGLE
-------------------------------------------------------------------------------------------- REGISTERS -----------------
STRING 1 ............ AXIOM
STRING 2 TO 9 ....... PRODUCTION RULES (FIRST NRULES REGISTERS IN THIS SET ARE USED) VARIABLE:RULE
SCALAR 1 ............ ORIGIN X COORDINATE
SCALAR 2 ............ ORIGIN Y COORDINATE
SCALAR 3 ............ INITIAL ANGLE IN DEGREES
========================================================================================================================

========================================================================================================================
DIMFILM TEXT STRING ESCAPE SEQUENCES: GENERALLY, *X BEGINS A MODE AND $X ENDS IT, ** AND $$ FOR LITERAL * AND $.
========================================================================================================================
*U .................. START UPPER CASE (DEFAULT STATE - $U NOT USEFUL)
*L .. $L ............ START / END LOWER CASE
*B .................. BACKSPACE
*1 *2 *3 ............ SELECT FONT SET (SEE FONT COMMAND)
*+ .. $+ ............ START / END SUPERSCRIPT (2 LEVELS MAX)
*- .. $- ............ START / END SUBSCRIPT (2 LEVELS MAX)
*N .................. RESET SUB / SUPER SCRIPT LEVEL TO ZERO
*O .. $O ............ SUB / SUPER SCRIPTS TO BE FULLY ABOVE OR BELOW PREVIOUS CHAR (FOR LIMITS OF SUMS / INTEGRALS)
*, NUM $, DEN $. .... FRACTION WITH NUMERATOR NUM AND DENOMINATOR DEN (*, NUM *. DEN $. IS IDENTICAL)
*:NN ................ OUTPUT SYMBOL WITH CODE NUMBER NN (00 TO 96)
*::NN ............... OUTPUT MARKER WITH CODE NUMBER NN (00 TO 96)
*VNN ................ OUTPUT CHARACTER IN CURRENT FONT SET WITH CODE NUMBER NN (00 TO 96)
========================================================================================================================
```

GPLOT demonstration scripts
---------------------------

There are a number of GPLOT scripts ("obey files") which demonstrate
various features of GPLOT.
