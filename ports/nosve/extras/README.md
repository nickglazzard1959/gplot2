Scripts that may be useful when working on NOS/VE
=================================================

Here are a few SCL scripts and libraries that might be useful.

The `prolog` script is run whenever
the user logs in or runs a batch job. This `prolog`
sets the working catalog to `$USER` and, if the login
is interactive, asks the user for the terminal type and sets
terminal parameters accordingly. It also makes some
extra commands available as described below.

There is also a terminal definition for the NCDXT terminal,
as originally defined by Gerard van der Grinten for NOS.

## Installing the scripts

First, transfer the files to the NOS side of the NOS-NOS/VE dual-state
RTR.
```
./put-extras.sh
```

Then, log in to NOS/VE interactively. Move the files
from NOS to NOS/VE and update libraries as follows:
```
setwc $user
chala user=guest password=guest
getf prolog
getf ncdxt_source ncdtnve
deft ncdxt_source
crev ignore kind=status
getf updlib
incf updlib
```

Log out of NOS/VE then log in to it again. The `prolog` script should run and give you
the option to choose a terminal type. You should be able to choose `ncdxt` at this point.

The `prolog` script also sets the command list to include a NOS/VE supplied set of
additional commands (`$system.osf$site_command_library`) 
and a small library of commands created during GPLOT development,
`fileslib`.

The `fileslib` library provides:

- `l` : a command which lists the last 24 commands entered with an index number that
  can be used with the NOS/VE additional command `modpc` to recall and edit a previous
  command. NOTE: `modpc` sometimes fails to work with certain commands, such as `edif`.
  It is unclear why this is.
- `r` : a command which searches back through previous commands for one beginning with
  a supplied string, then recalls that command for possible editing and "redo". This
  mimics the NOS `R` command. Usage:
  ```
  r,'string'
  r,'dis' for example
  ```
- `dir` : a command which lists files whose names contain a specified string. The
  output is a bit like `ls -l` on Unix (but not much like it). Usage:
  ```
  dir 'string'
  dir '_svg' for example.
  ```
  The output is in alphabetical file name order.
- `dirt` : as per `dir`, but the output is sorted in ascending modification time
  order, so the last modified file is listed last in the output.
- `files` : this generates a file of SCL commands which can be executed to apply
  commands to a set of files. NOS/VE SCL does not have wild card syntax for file names,
  and this is an attempt to provide some wild card-like functionality. Usage:
  ```
  files 'template' 'pattern' outputFile
  files 'copf f=* o=^' '_svg' copscl for example.
  ```
  Any asterisk in the template is replaced by a file name that contains the
  specified pattern. Any carat is replaced by the matching file name with the
  specified pattern removed. This scheme may be reasonably flexible without
  being too expensive to execute. The output file contents can be executed
  using the `incf` (`include_file`) command. Many extensions would be possible,
  but performance might be an issue. The output file for the above example is:
  ```
  copf f=GR10001_SVG o=GR10001
  copf f=GR1001_SVG o=GR1001
  ...
  copf f=GR8001_SVG o=GR8001
  ```

The `fileslib` library also contains two "commands" (effectively functions used
internally in `fileslib`) which are potentially more generally useful:
`file_list` returns an array of strings of matching file names given a
pattern string and `replace_string` replaces specified substrings in a string
with new strings.
