
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
systems thanks to gfortran (along with modern Fortran, which is its main
focus, of course).

GPLOT and DIMFILM are developed and maintained primarily on CDC NOS 2.8.7.
NOS has its own way of managing source code at the heart of which is 
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
concerned.

So the GPLOT project establishes a fair amount of infrastructure that
allows MODIFY and Git to inter-operate without too much pain.


MODIFY/Git Interoperability Infrastructure
------------------------------------------


