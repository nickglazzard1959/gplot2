Scripts that may be useful when working on NOS/VE
=================================================

Here are a few SCL scripts that might be useful.

- `prolog` : This is a script that will appear as
  `prolog` in `$USER` on NOS/VE. This is run whenever
  the user logs in or runs a batch job. This `prolog`
  sets the working catalog to `$USER` and, if the login
  is interactive, inquires the terminal type and sets
  terminal parameters accordingly. It also makes the
  `l` and `r` scripts (below) available as commands.
  It also adds `$system.osf$site_command_library` to
  the command list which provides many useful commands.
- `l` : This lists the last 25 commands entered with
  a number that can be used with the `modify_previous_command`
  (`modpc`) cpmmand to recall and edit a previously entered
  command.
- `r` : This mimics the `R` command on NOS, allowing a
  previous command to be recalled by giving some number of
  characters it starts with. It can then be edited and
  repeated. One difference from `R` on NOS is that you must
  put the partial command to search for in single quotes.
  For example: `r,'dis'`.

There is also a terminal definition for the NCDXT terminal,
as originally defined by Gerard van der Grinten for NOS.

## Installing the scripts

First, transfer the files to the NOS side of the NOS-NOS/VE dual-state
RTR.
```
./put-extras.sh
```

Then, log in to NOS/VE interactively. Move the files
from NOS to NOS/VE as follows:
```
setwc $user
chala user=guest password=guest
getf l
getf r
getf prolog
getf ncdxt_source ncdtnve
deft ncdxt_source
```

Log out of NOS/VE then log in to it again. The `prolog` script should run and give you
the option to choose a terminal type. You should be able to choose `ncdxt` at this point.

