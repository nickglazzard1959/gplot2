
GPLOT - A Graph Plotting and Drawing Program based on DIMFILM
=============================================================

GPLOT is a graph plotting program, similar in intent to Gnuplot but very
different in detail (and less capable -- although it also does some things
Gnuplot does not). It is intended to run primarily
on CDC NOS 2.8.7, but it will also run on "Unix-like" systems, DEC VAX/VMS
(and probably OpenVMS) and (experimentally) CDC NOS/VE.

In addition to plotting graphs of
data (stored in files), the current version of GPLOT has some features
which can be useful for general drawing, especially of things such as
block diagrams. All the graphics in this document (and all other documents
in this project) were created with GPLOT, for example.

Here are a couple of examples of GPLOT/DIMFILM output.

---

![](gr27001.svg)

---

![](g1fm001.svg)

---


Introduction
------------

GPLOT is based on the DIMFILM library (Descriptive Instructions for MicroFILM)
which was written by Dr. John Gilbert at ULCC between 1972 (first released
in 1973) and the mid 1990s.
The version of DIMFILM used is the second major version, from around 1984.
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
systems thanks to **gfortran** (along with modern Fortran, which is **gfortran**'s
main focus, of course).

The DIMFILM library can also be cross-compiled and used on the Cray COS
operating system on a (simulated) CRAY X-MP. GPLOT itself is currently
not available for COS, although that should be straightforward to add in
a future "release".

GPLOT and DIMFILM are developed and maintained primarily on CDC NOS 2.8.7.
NOS has its own way of managing source code, at the heart of which is 
a program called MODIFY. As of V0.89, GPLOT and DIMFILM can also be built
on VAX/VMS and NOS/VE, although the latter port is "experimental" at present.

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
of Windows. If a good FORTRAN-77 compiler is available for Windows (perhaps
**gfortran**?), it should be fairly easy to get them working on Windows, but
that is an "exercise for the reader" ... 

Note that ports to other operating systems also rely on some of the tools
written for MODIFY/Git inter-operation. If you are interested in using GPLOT
on those systems, you need to install the tools as per 
[this section](#itiot) below.


Documentation
-------------

Documentation for GPLOT takes the form of a "traditional" manual in PDF
form that can be found [here](doc/pre-built/gplot.pdf) and a more "task
oriented" or "tutorial" form described [here](#tutdoc). 

Documentation for DIMFILM can be found [here](doc/pre-built/dimfilm-manual.pdf).

When using GPLOT after becoming familiar with it, the following "cheat sheets" might be all you need.

---

![](cht1001.svg)

---

![](cht2001.svg)

---


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
shell script to run **gfortran** on every `.f` file. 

`--omit a,b,c` will omit modules `a`, `b` and `c` from the
script file. This may be needed for modules that are non-standard
FORTRAN (e.g. include COMPASS assembler). These cannot be compiled
by **gfortran**.

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

The various components involved and their relationships are shown in this
diagram:

![](iocd001.svg)

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

<a id="itiot"></a>
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



Installing on NOS 1: Transferring all components of GPLOT and DIMFILM between Git and NOS
-----------------------------------------------------------------------------------------

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


Installing on NOS 2: Building all components of GPLOT and DIMFILM on NOS.
-------------------------------------------------------------------------

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
a list of terminals for the interactive session (so that the 
Full Screen Editor - FSE - "just works"). You may want to modify this part,
depending on which
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

Detailed information on building GPLOT and DIMFILM on "Unix-like" systems
can be found [here](ports/unix/README.md). These systems include macOS and
Linux (Debian Linux specifically has been tested).

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
[here](https://github.com/andrastantos/cray-sim).

It is possible to build the DIMFILM library (but not GPLOT, currently)
on COS and then write and run programs that use it on that operating
system.

Full details on how to do this can be found [here](ports/cos/README.md).

The Cray X-MP is very much an "attached processor". In this case, it
must be attached to a (simulated) CDC Cyber 170-865 running NOS 2.8.7.
See [this page](https://github.com/kej715/DtCyber/tree/main/NOS2.8.7) in
Kevin Jordan's DtCyber repository for information on setting all this up.


Building all components of GPLOT and DIMFILM on VAX/VMS.
--------------------------------------------------------

It is very straightforward to build DIMFILM and GPLOT on VAX/VMS (and
probably OpenVMS on Alpha, Itanium and x86_64 hardware too, but I have
not tested that). Detailed information on how to do that can be
found [here](ports/vms/README.md).


Building all components of GPLOT and DIMFILM on NOS/VE.
-------------------------------------------------------

Kevin Jordan has a branch in his DtCyber repository that implements the full
CDC Cyber 180 instruction set and otherwise does everything necessary to be able to 
run NOS/VE.

This capability is still under development and all use of NOS/VE should be
considered "experimental". The Cyber 180 architecture is very complex and
the emulation is not yet totally stable (although it is close, with failures
about once a week of continuous operation -- which is certainly usable, so long
as "work is saved" frequently).

NOS/VE is a sophisticated operating system which owes a lot to Multics and is
run in a "dual state" configuration with NOS. That is, one set of (simulated)
hardware "simultaneously" runs two very different instructions sets (Cyber 180
and Cyber 170) and two very different operating systems (NOS/VE and NOS). This
is quite a trick and makes this system of particular historical interest.

The current scheme to build and use DIMFILM and GPLOT on NOS/VE is
described [here](ports/nosve/README.md). This is subject to change, however,
as Cyber 180 and NOS/VE support in DtCyber matures.

<a id="tutdoc"></a>
GPLOT examples and tutorial
---------------------------

Along with a traditional manual,
the documentation for GPLOT -- how to use it and what it can do -- takes the form
of a "tutorial" with examples of almost every capability. 

This "tutorial" can be found [here](tutorial/tutorial.md). For most purposes,
please refer to it as the primary documentation for GPLOT.

It also serves as the "test suite" for GPLOT and DIMFILM as discussed below.


Verifying GPLOT/DIMFILM is working correctly
--------------------------------------------

The "test suite" for GPLOT (and, indirectly, DIMFILM) consists of the obey files that
create the figures for the "tutorial" document and this README. 
These aren't exhaustive tests, of course, but
if the full set of Figures can be correctly generated on a system for all four
supported devices, it is likely that (almost?) everything is working correctly.

To aid with verification, there are two obey files which generate all the figures
in EPS or SVG format. These are `OBALEPS` and `OBALSVG`, which both rely on `OBALTST`.

To run these on NOS, use:
```
ATTACH,GPLOT.
GPLOT,GET=YES,SAVE=YES,OBEY=OBALEPS.
GPLOT,GET=YES,SAVE=YES,OBEY=OBALSVG.
```
You may want to fetch the result files to some system on which they can be
viewed after each step, as the `OBALSVG` stage will overwrite the EPS files
generated by `OBALEPS` (the output files have the same names -- NOS file names
have no extensions and are too short to add anything meaningful to distinguish 
EPS from SVG).

These tests will take some time to run and consume a lot of resources, so
it is recommended to use:
```
SETTL,*.
SETJSL,*.
```
to prevent running into time and resource limits.

To fetch the output files from NOS to a "Unix-like" system, you can use NOSFTP
with a fetch list. This list is in a file `fetch-list` which can be recreated
with `get-images-list.sh` if `OBALTST` is modified (it scans that obey file for
output image information). For example, to get all SVG files generated by the above
on NOS into a directory called `temp2`:
```
$ mkdir temp2
$ cd temp2
$ nosftp tester nuc1
Password for NOS account: 
Contacting NOS FTP server on host: nuc1
230 USER LOGGED IN, PROCEED.
220 SERVICE READY FOR NEW USER.
Local cwd now: /Volumes/qemu-main/gitprojects/gplot2/temp2
NOS FTP> mget ../obey-files/fetch-list ascii svg
... getting: gr1001 ( 1 )
... retrieving file: gr1001
226 CLOSING DATA CONNECTION.
...
...
... getting: fin001 ( 73 )
... retrieving file: fin001
226 CLOSING DATA CONNECTION.
Got 73 of 73 files OK

NOS FTP> bye

Exiting normally.
221 SERVICE CLOSING CONTROL CONNECTION. LOGGED OUT.

$ ls
cht1001.svg	f02t001.svg	f05t001.svg	f11t001.svg	f17t001.svg	
g2fm001.svg	gh1b001.svg	gr12001.svg	gr18001.svg	gr21001.svg	
gr30001.svg	iocd001.svg	lsys001.svg cht2001.svg	f03s001.svg	
f06t001.svg	f12t001.svg	f18t001.svg	g2sp001.svg	gh2b001.svg	
gr13001.svg	gr1a001.svg	gr22001.svg	gr3001.svg	lsbr001.svg
f01m001.svg	f03t001.svg	f07t001.svg	f13t001.svg	fin001.svg	
g3sp001.svg	ghbb001.svg	gr14001.svg	gr1i001.svg	gr27001.svg	
gr4001.svg	lsgo001.svg f01s001.svg	f04s001.svg	f08t001.svg	
f14t001.svg	fosm001.svg	gd01001.svg	gr10001.svg	gr15001.svg	
gr1x001.svg	gr28001.svg	gr5001.svg	lsks001.svg f01t001.svg	
f04t001.svg	f09t001.svg	f15t001.svg	g1fm001.svg	gdea001.svg	
gr1001.svg	gr16001.svg	gr20001.svg	gr29001.svg	gr7001.svg	
lssm001.svg f02s001.svg	f05s001.svg	f10t001.svg	f16t001.svg	
g1sp001.svg	gemz001.svg	gr11001.svg	gr17001.svg	gr2001.svg	
gr2a001.svg	gr8001.svg	lssp001.svg
```

There is a script which will run the SVGView tool on all SVG
files in a directory to make the output easy to examine:
```
svgviewall.sh
```
This is installed when the other tools described above are
installed.

To verify correct operation on the two interactive devices,
use GPLOT interactively, choose the device (`GTERM` or `TEK4K`)
then use:
```
OBEY OBALTST
```
and inspect the output visually. The obey script pauses
after each output "frame" for the user to type `(return)`
(or `Q (return)` to stop the script). On "Unix-like"
systems, you may need to use `PREFIX` to specify where obey
files are to be found first.

To run the EPS and SVG format verification tests on
"Unix-like" systems, use:
```
gplot obey=obaleps
gplot obey=obalsvg
```
from the `obey-files` directory (the generated
files can be tidied up afterwards, as desired).

The output files will have `.svg` or `.eps` extensions in 
this case, of course.


Other software to use with GPLOT
--------------------------------

For historical accuracy -- if you want to experience what online
graphics was like in the 1970's and the first half of the 1980's --
using Rene Richarz's superb Tektronix 401x terminal emulator with
GPLOT is the best way to go. This gets very close to the real thing
and behaves very like the Tek 4006 I used in 1980-83. 
It can be found on Github [here](https://github.com/rricharz/Tek4010)

For less historical accuracy, but more utility, my 
[GTerm project](https://github.com/nickglazzard1959/gterm)
is a colour graphics terminal for use with GPLOT (and NOS APL).
It is idiosyncratic and has some issues, but it is quite useful
with GPLOT.

Because Telnet servers are unpopular on modern operating systems for
security reasons, it may be difficult to use GPLOT on "Unix-like"
systems with online graphics terminals. 
The GTerm project includes a minimal but sufficient Telnet
client (`ctelnet`) for use with, for example, a Tektronix 401x terminal
emulator, but it needs to talk to a Telnet server, and that is no
longer available on macOS, for one. Various Telnet servers can be
installed, of course, but they are far more complicated than needed
to just access GPLOT running on "Unix". One alternative is
[mini_telnetd](https://github.com/Troll338cz/mini_telnetd), available
on Github. Unfortunately, this will not build "out of the box" on
modern systems, but it isn't hard to fix that. It is very minimal,
but quite sufficient for using GPLOT online on "Unix".

To patch `mini_telnetd` after cloning its repository, use the
file: `tools/mini_telnetd.patch` as follows:
```
git apply mini_telnetd.patch 
```
then rebuild it with `make`. More information on
running `mini_telnetd` is found in the GPLOT PDF manual (it
is trivial, with no "installation" needed).
