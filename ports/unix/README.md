Building all components of GPLOT and DIMFILM on "Unix".
=======================================================

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
to the NOS version. These systems are very different, of course, especially
where files are concerned, and various rules and heuristics are used to
try to hide these differences as much as possible, as explained below.

The build procedure is very similar to that on NOS.

## Build the utilities library from PLUTILS

```
cd ports/unix
./make-utils.sh
```
This pre-processes the source in `utils-library` to a newly created
source directory `utils-source`, then compiles the contents with
`gfortran`, creating the library `lib/utils.a`

In fact, only the source file `chars.f` is needed for GPLOT/DIMFILM.

## Build the graphics device support library from PLGRDEV

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

## Build the DIMFILM library

```
./make-dimfm.sh
```
This pre-processes the source in `dimfm-library` to a newly created
source directory `dimfm-source`, then compiles the contents with
`gfortran`, creating the library `lib/dimfm.a`

This library has identical functionality to the NOS version.
FORTRAN-77 (and maybe later versions of Fortran?) programs can
be compiled with `gfortran` and linked with it (and the other
libraries noted above) to produce graphical output.

## Build the Unix support library

```
./make-support.sh
```
This compiles the support library code (from `support.f` and
`support_c.c`) to create the library `lib/support.a`. Any programs
using DIMFILM will have to link with this library in addition to
the others mentioned above.

## Build the GPLOT program

```
./make-gplot.sh
```
This pre-processes the source in `gplot-library` to a newly created
source directory `gplot-source`, then compiles the contents with
`gfortran`, creating a relocateable object file which is then
linked with all the above libraries to create the `gplot` program.

Before GPLOT can be used, an install script must be run:
```
./install.sh
```
This copies the `gplot` binary and a script file, `ugplot` to 
`/usr/local/bin`. It also creates `/usr/local/share/dimfilm` and
copies the DIMFILM font file, found in `gplot-library/dadimfo.src`,
to it. After this:
```
./gplot
```
will start GPLOT, which should function identically to the NOS
version. (The `ugplot` script file needed to run GPLOT on "Unix"
prior to GPLOT V0.89 is no longer needed.)

Note that command line arguments are as described for NOS in the
GPLOT documentation, except that arguments are separated by spaces
rather than commas. The same `keyword=value` and `keyword` format
is retained, as used on NOS.

## Build the DIMFILM test programs

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

## Using EPS files on macOS

Unfortunately, recent versions of macOS can no longer view EPS
and other PostScript files just by opening them (double-clicking 
or whatever), for reasons best known to Apple. 

A script called `epsview.sh` is included in the `tools` directory
and installed along with the other tools by `install.sh` in that
location. This uses programs in the `ghostscript` package to
convert EPS to PDF and then open the PDF version. This displays
output in macOS `Preview`. It can also produce a `.png` image version 
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

The `epsview.sh` script also works on Linux systems, where it uses 
`xdg-open` to display the generated PDF file. The same options
are available there as for macOS.

There is also a "minimal as can be" SVG viewer program called `svgview`
which can quickly display a named SVG file:
```
svgview afile.svg
```
This program is based on PySDL2.
