Building all components of GPLOT and DIMFILM on NOS/VE
======================================================

There is a "ready to run" (RTR)  dual-state NOS and NOS/VE system constructed by Kevin Jordan.
This can be used with the "nosve" branch in his DtCyber fork.

The following information assumes you have built DtCyber and followed the instructions
to get a working NOS + NOS/VE dual-state system up and running.


Overall strategy
----------------

The NOS/VE system obtains the source for the GPLOT and DIMFILM components from MODIFY PLs
on the NOS "side" of the RTR. It is very easy to move files between the NOS and NOS/VE sides
of this system.

NOS/VE has a tool to convert MODIFY PLs to its own Source Code Utility (SCU) equivalent of
MODIFY. This can perform the same conditional compilation and "include" processing as
MODIFY does on NOS working on the automatically converted MODIFY PLs. If conditional compilation
sections for NOS/VE are included in the common source used on all systems, no other manual steps
are needed.

A small set of NOS/VE batch jobs can be used to get the MODIFY PLs from NOS, convert them
to SCU PLs, run SCU to create compilable code with appropriate conditional code
sections and "include expanded", compile that code and create object libraries or an
executable (sort of) for GPLOT itself.

It turns out NOS/VE does not have "pre-linked" executables as such. In fact, nor do more
familiar systems with dynamic linking. But in the case of those more familiar systems,
information on the libraries needed to run the program are inserted into an "executable file",
so there is a "file that is run".

On NOS/VE, the required libraries are not hidden in a file. Instead, a "program description"
is created in an object library and that specifies the required libraries. It is that
"program description" which is "run" to activate the program.


Configure the guest account in the NOS side of the RTR
------------------------------------------------------

The `guest` account on the RTR as shipped is configured to prevent the creation
of permanent files as large as those needed by GPLOT and DIMFILM. It is also helpful
to stop `guest` interactive sessions timing out (after about 10 minutes of inactivity).
The `guest` account is already configured to allow multiple concurrent logins, which is also very useful.

To make the desired changes, use `MODVAL` on the NOS console.
```
X.MODVAL.
K,<modval jsn>.
K.U,GUEST
K.MS=77B
K.CM=77B
K.CS=7
K.FS=7
K.DS=7
K.AW=CTIM
K.END
K.END
[
AB.
```

You can check which privileges the `guest` account has using the `awdecode` tool.
This lets you enter the `AW` octal value and outputs a list of enabled privileges.


Move the source to NOS on the dual-state RTR system
---------------------------------------------------

The first step is to copy the source for the MODIFY libraries on NOS to the RTR NOS
OS using the `guest` account (password `guest`). We will assume that the RTR is running
on DtCyber on `localhost`in what follows.

For background, refer to the NOS instructions in the main README document.

First, use `modgitedit.sh` or directly edit `.modgitproject` in the top directory
of the GPLOT source tree on the "Unix-like" Git host system (`localhost`). Make
sure `.modgitproject` looks like this:
```
export MODGITPROJECT="GPLOT and DIMFILM"
export NOSUSER=guest
export NOSPW=guest
export NOSHOST=localhost
export RJEHOME=~/rje-station
export RJESPOOL=${RJEHOME}/spool
```

Then use the `export-modify.sh` tool to put the MODIFY source for each DIMFILM/GPLOT
component on to NOS on the RTR. Note that only the first part of the usual 
`export-modify.sh` processing will work -- the part that creates the `SRxxxxx` files on
NOS. Because the RTR is not configured to support RBF, the second part, in which a remote
batch job is run to create the MODIFY PLs, cannot work. Instead, there will be a
`LOGIN IS IMPOSSIBLE` message when an attempt is made to submit the job. End the RBF
client with `control-C`.

Use these commands:
```
export-modify.sh utils -a
export-modify.sh grdev -a
export-modify.sh dimfm -a
export-modify.sh gplot -a
```

After this, the files `SRUTILS`, `SRGRDEV`, `SRDIMFM` and `SRGPLOT` should have been created
on the NOS OS on the RTR under the `guest` account.


Create the MODIFY program libraries on NOS on the dual-state RTR system
-----------------------------------------------------------------------

Go to the `ports/nosve` directory if not already there. Then:
```
./put-ba-files.sh
```

This will create the files `BAUTILS`, `BAGRDEV`, `BADIMFM` and `BAGPLOT` on the NOS side of the
RTR.

Log in to the NOS side of the RTR as `guest`. Then run those transferred files as batch
jobs on NOS. This will create the MODIFY program libraries
`PLUTILS`, `PLGRDEV`, `PLDIMFM` and `PLGPLOT` from the previously transferred MODIFY
format source code in `SRUTILS`, `SRGRDEV`, `SRDIMFM` and `SRGPLOT`.
```
get,bautils.
submit,bautils,no.
get,bagrdev.
submit,bagrdev,no.
get,badimfm.
submit,badimfm,no.
get,bagplot.
submit,bagplot,no.
```

These jobs will produce no output listings, but you can observe their progress on the
NOS console. You should see `MODIFICATION COMPLETE` and no errors in each case. Note that
these jobs delete `SRUTILS`, `SRGRDEV`, `SRDIMFM` and `SRGPLOT` when they are run.


Building on NOS/VE
------------------

There are two ways to do this -- using batch jobs submitted from NOS to run on NOS/VE or
interactively on NOS/VE directly. In both cases, the first step is to move those batch
jobs to the NOS side of the RTR. This can be done by:
```
./put-vj-files.sh
```

This creates the files `vjutils`, `vjgrdev`, `vjdimfm`, `vjvesup` and `vjgplot` on NOS,
as well as some other "`vj`" files which can be used to help verify GPLOT is
working correctly, as explained below.


### Using batch jobs from NOS

It is easy to send batch jobs from NOS to run in NOS/VE on the RTR. The following
will submit the `vj` batch jobs to the input batch job queue on NOS/VE:
```
submit,vjutils,dc=in,st=nve.
submit,vjgrdev,dc=in,st=nve.
submit,vjdimfm,dc=in,st=nve.
submit,vjvesup,dc=in,st=nve.
submit,vjgplot,dc=in,st=nve.
```

The output from these jobs will go to the NOS system line printer, although you
can use `dc=to` in each `submit` to put the output on your `wait queue` instead.

This should leave the "object library" `gplot` in the `guest` account of NOS/VE.
Then, GPLOT can be run by `gplot.gplot` after `setwc $user` in the `guest` account,
or `gplot` (the library) can be added to the command list (using `crecle gplot`), after
which the simple command `gplot` can be used to, er, run GPLOT.


### Using NOS/VE interactively

You can login to NOS/VE interactively using WEBTERM or another suitable terminal.

If you connect to the RTR host, you will initially be "talking" to the NOS side
of the RTR. You can directly login to NOS/VE from the first `FAMILY` prompt, as
follows:
```
,guest,guest,veiaf
```

You can then build DIMFILM and GPLOT using these commands:
```
setwc $user

chala user=guest password=guest

getf vjutils
subj f=vjutils odi=log_utils ujn=jobutils

getf vjgrdev
subj f=vjgrdev odi=log_grdev ujn=jobgrdev

getf vjdimfm
subj f=vjdimfm odi=log_dimfm ujn=jobdimfm

getf vjvesup
subj f=vjvesup odi=log_vesup ujn=jobvesup

getf vjgplot
subj f=vjgplot odi=log_gplot ujn=jobgplot
```

You can track the progress of batch jobs using:
```
disjs jn=jobutils [do=all]
```
etc.

If desired, you can stop a batch job using:
```
terj jn=jobutils
```
but you shouldn't need to do this. 

NOTE: if you are running`gplot` and then build `gplot` with `vjgplot`,
it will "hang" with "cycle wait" status shown in `disjs`, because the final step cannot overwrite
the `gplot` library if it is open (because `gplot` is running). In this case, you should try to
find the other login or batch job that is running `gplot` and exit it, after which the build
will complete normally. Note that having `gplot` (the library) set in the command list *will* open
that library and prevent builds from completing too! I think you could use higher "cycle" numbers
to allow concurrent builds and execution, but that is probably a needless complication here.

You should let a batch job finish before starting the next one, of course, because later jobs
use the products of earlier jobs.

After these jobs are finished, you can run GPLOT as described above for the "batch job from NOS"
approach.


Verifying GPLOT/DIMFILM is working correctly
--------------------------------------------

As explained in the main project `README.md`, the "test suite" for GPLOT (and, indirectly, DIMFILM) 
consists of the obey files that create the figures for the "tutorial" document.

To get these on to NOS/VE is a two step process.

The first step is to move them from the Git repo on a "Unix-like" machine to
NOS. This step is explained in the main project `README.md` [here](../README.md#verfwork).
You can use `./send-files-nosve.sh` instead of `send-files.sh`, but they are effectively identical
(the NOS/VE variant sends the files as ASCII rather than Display Code, 
but the contents are upper case only anyway).

Once they are on the NOS part of the dual-state RTR, they can be transferred to the NOS/VE part
by running a batch job: `vjobget`. This can be submitted from NOS or from a NOS/VE interactive
session in the same way that GPLOT build jobs can be run in these two different ways.

Once the obey files are on NOS/VE (in a `$user.obey-files` catalog), you can run the tests using:
```
/ gplot.gplot (or gplot if a crecl gplot has been issued)
? prefix $user.obey-files
? device <desired device>
? obey obaltst
? exit
/
```
You can omit the `dev` command and use `ob obalsvg` or `ob obaleps` to generate
SVG or EPS files if desired.

If SVG files are output, the batch job: `vjsvput` can be used to move the SVG files
from NOS/VE to NOS. They can then be further transferredfrom NOS to a "Unix-like" system using
the shell script `./
