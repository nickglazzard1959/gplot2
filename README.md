
GPLOT - A Graph Plotting Program based on DIMFILM
=================================================

GPLOT is a graph plotting program, similar in intent to Gnuplot but very
different in detail (and less capable). It is intended to run primarily
on CDC NOS 2.8.7, but it will also run on "Unix-like" systems.

It is based on the DIMFILM library (Descriptive Instructions for MicroFILM)
which was written by Dr. John Gilbert at ULCC between 1972 and the mid 1990s.
The version of DIMFILM used is the second major version, from around 1982.
The first version was CDC specific, while the second was much more easily 
ported and ran on IBM MVS, Cray COS and UNICOS and Convex machines.

I do not have the original fonts for DIMFILM and the source may be an
early beta version. At any rate, I did some work on it in 2008 to get
it to work using Hershey fonts, amongst other things. 

I am not aware of DIMFILM being preserved anywhere else. Although the code
does not contain any copyright notices, it is the work of John Gilbert.
I am reluctant to include it, but GPLOT is largely a "front end" to it, so
it isn't optional. Also, I fear DIMFILM might be entirely lost otherwise,
as I can find no trace of it elsewhere.

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
modgitedit.sh
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

The contents must be set so that FTP and submitting batch jobs to NOS with
`rjecli` in `rje-station` (using RBF - Remote Batch facility) will work.
The NOS account given via `NOSUSER`, `NOSPW` and `NOSHOST` must be the one
where the MODIFY PL lives, of course. `NOSHOST` will be the host name or
IP address of the physical machine running DtCyber.
