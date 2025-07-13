#!/usr/bin/env python3
"""
This creates a NOS batch job that creates or updates a MODIFY PL so that it
corresponds to the files (which have certain extensions) in a specified
directory.

This can include the source code to insert/update in the OPL in the batch job
stream, or it can create a file on NOS which must be transferred there by FTP
before the batch job is run.

The environment variables NOSUSER and NOSPW must have been set (to specify the
NOS account to use) before this is run.

Source file extensions currently processed are:
 .f    - FORTRAN source code
 .cmn  - FORTRAN included file. Marked as COMMON in MODIFY source output.
 .src  - Other source using UPPER CASE only.
 .asc  - Other source with lower case marked as ASCII (up to 72 lower case chars = 144 6/12 chars).

By default, only files Git indicates have been modified since the
last Git commit will be included in the MODIFY source. In this case, a MODIFY
OPL should already exist on the NOS system, and only a subset of its modules
will be updated (or added).

If --committed is used, files changed in the last commit will be selected instead of files
modifed since the last commit.

If --all is used, the output will create a new MODIFY PL from the source. That OPL
should not already exist on the NOS system. All the source files (with the above
extensions) in the source directory will be used as MODIFY input.

If --noprocess is used, no source code transformations are applied.
This should be used to keep MODIFY code unchanged, for example if CCL
procedures are being stored.

If --ftpmode is used, the NOS batch job will be generated based on the MODIFY
source (genearted her) having been transferred by FTP to the NOS system before
the batch job is run. This is HIGHLY RECOMMENDED, as it allows lines longer than
80 characters (needed for 6/12 coded lines longer than 36 characters) to be
preserved.

Without --ftpmode, the MODIFY source is included in the batch job and lines will
be truncated at 80 characters. 
"""

import os, argparse, time, sys, fnmatch
import subprocess

def all_files( root, patterns='*', single_level=False, yield_folders=False ):
    """
    Iterator for files in directories: for fred in all_files(folder,...) ...
    """
    patterns = patterns.split( ';' )
    for path, subdirs, files in os.walk( root ):
        if yield_folders:
            files.extend( subdirs )
        files.sort()
        for name in files:
            for pattern in patterns:
                if fnmatch.fnmatch( name, pattern ):
                    yield os.path.join( path, name )
                    break
        if single_level:
            break

def create_folder_if_not_exists( path ):
    """
    Create a folder if it does not exist.
    """
    if not os.access( path, os.F_OK ):
        try:
            os.mkdir( path )
            return True
        except:
            print('Cannot create folder %s' % path)
            return False
    else:
        return True

def to_612( line ):
    """
    Convert a line of ASCII to 6/12 Display Code.
    This is only applied to modules marked as ASCII and undoes the
    conversion to ASCII performed on those by modsplit.py.
    """
    specialmap = { ':':'@D', '@':'@A', '^':'@B', "`":'@G', '{':'^0',
                   '|':'^1', '}':'^2', '~':'^3' }
    retline = ''
    for achar in line:
        if achar.isprintable():
            if achar in specialmap:
                retline += specialmap[achar]
            else:
                if achar.islower():
                    retline += '^'
                retline += achar.upper()

    retline += '\n'
    return retline

def main():
    """
    Mainline.
    """
    parser = argparse.ArgumentParser()
    parser.add_argument("indir", help="Input directory, 1 file/module to go into MODIFY PL.")
    parser.add_argument("outjob", help="Output job, submit to CR to create MODIFY PL.")
    parser.add_argument("plname", help="Name of MODIFY PL to create (or update).")
    parser.add_argument("-n","--noprocess", help="Do no conversions, assume already MODIFY source.", action="store_true")
    parser.add_argument("-f","--ftpmode", help="The source has been FTPd to this file already (source not in batch job).")
    parser.add_argument("-a","--all", help="Create a new MODIFY PL rather than inserting changed modules into an existing one.",
                        action="store_true")
    parser.add_argument("-c","--committed", help="Find modified modules from last Git commit rather than current modified files.",
                        action="store_true")

    args = parser.parse_args()

    try:
        username = os.getenv('NOSUSER')
    except:
        print('The NOSUSER environment variable must be defined.')
        sys.exit(1)

    try:
        password = os.getenv('NOSPW')
    except:
        print('The NOSPW environment variable must be defined.')
        sys.exit(1)

    ftpmode = ( args.ftpmode is not None )

    if not os.path.isdir(args.indir):
        print('This:',args.indir,'is not a directory.')
        sys.exit(1)

    # If updating an existing OPL, find which files have been modified from Git.
    # Include only files from the "source library directory" being processed.
    if not args.all:
        if args.committed:
            cmd = ['git', 'diff-tree', '-r', '--no-commit-id', '--name-only', 'HEAD']
        else:
            cmd = ['git', 'ls-files', '-m']
        try:
            retstring = subprocess.check_output(cmd, universal_newlines=True)
        except Exception as e:
            if args.committed:
                print('git diff-tree -r --no-commit-id --name-only HEAD failed.')
            else:
                print('git ls-files -m failed.')
            print('Reason:', e)
            sys.exit(1)
        retlist = retstring.splitlines()
        module_list = []
        for changed_module in retlist:
            if changed_module.startswith(args.indir):
                module_list.append(changed_module)

    # Create the output directory as needed.
    outdir,fname = os.path.split(args.outjob)
    if not create_folder_if_not_exists(outdir):
        print('Cannot create output folder:',outdir,' Giving up.')
        sys.exit(9)        

    # Create the output batch job file.
    try:
        fout = open(args.outjob, 'w')
    except Exception as e:
        print('Cannot create output job file. Reason:',e)
        sys.exit(2)

    ftp_mod_job = """MODMJOB.
USER,{0},{1}.
SETTL,*.
SETJSL,*.
GET,{2}.
REWIND,{2}.
GET,{3}.
REWIND,{3}.
FSE,FN={3},OP=D.D;Q
REWIND,{3}.
MODIFY,P={2},N=NEWPL,L=0,S=0,C=0,F,Z./*CREATE {3}
PURGE,{2}.
REPLACE,NEWPL={2}.
PURGE,{3}.
CATLIST.
"""
    ftp_create_job = """MODCJOB.
USER,{0},{1}.
SETTL,*.
SETJSL,*.
GET,{3}.
REWIND,{3}.
FSE,FN={3},OP=D.D;Q
REWIND,{3}.
MODIFY,P=0,N={2},L=0,S=0,C=0,F,Z./*CREATE {3}
REPLACE,{2}.
PURGE,{3}.
CATLIST.
"""
    batch_mod_job = """MODMJOB.
USER,{0},{1}.
SETTL,*.
SETJSL,*.
GET,{2}.
REWIND,{2}.
COPYEI,INPUT,SRCIN.
REWIND,SRCIN.
MODIFY,P={2},N=NEWPL,L=0,S=0,C=0,F,Z./*CREATE SRCIN
PURGE,{2}.
REPLACE,NEWPL={2}.
CATLIST.
~eor
"""
    
    batch_create_job = """MODCJOB.
USER,{0},{1}.
SETTL,*.
SETJSL,*.
COPYEI,INPUT,SRCIN.
REWIND,SRCIN.
MODIFY,P=0,N={2},L=0,S=0,C=0,F,Z./*CREATE SRCIN
REPLACE,{2}.
CATLIST.
~eor
"""
     
    if ftpmode:
        path,fname = os.path.split(args.ftpmode)
        nosftpname,ext = os.path.splitext(fname)
        print('NOS PL source file name should be:', nosftpname.upper())
        if not args.all:
            fout.write(ftp_mod_job.format(username.upper(),password.upper(),args.plname,nosftpname.upper()))
        else:
            fout.write(ftp_create_job.format(username.upper(),password.upper(),args.plname,nosftpname.upper()))
        
        eor_string = '(EOR)\n'
        eof_string = '(EOF)\n'

    else:
        if not args.all:
            fout.write(batch_mod_job.format(username.upper(),password.upper(),args.plname))
        else:
            fout.write(batch_create_job.format(username.upper(),password.upper(),args.plname))
            
        eor_string = '~eor\n'
        eof_string = '~eof\n'

    # If ftpmode, close the batch job file and open the source output file.
    # Otherwise, the batch job file contains the source code "inline".
    if ftpmode:
        fout.close()
        try:
            fout = open(os.path.join(outdir,nosftpname)+'.plsrc', 'w')
        except Exception as e:
            print('Cannot create output PL source file. Reason:',e)
            sys.exit(2)
        fout.write('DELETE ME\n')

    # Get a list of source file names to process. 
    n_modules = 0
    n_lines = 0
    filenames = []
    for filename in all_files(args.indir, patterns='*.cmn;*.f;*.src;*.asc', single_level=True):
        if not args.all:
            if filename in module_list:
                filenames.append(filename)
        else:
            filenames.append(filename)

    # Perhaps there is nothing to be done (e.g. because nothing has been modified).
    # If not, exit with a special condition code, so the caller can stop now.
    if len(filenames) == 0:
        print('No modules need to be processed.')
        sys.exit(213)

    # Loop, processing each selected input source file. Read a whole file at a time.
    n_filenames = len(filenames)
    for filename in filenames:
        try:
            fin = open(filename, 'r')
            text = fin.readlines()
            fin.close()

            path,fname = os.path.split(filename)
            stem,ext = os.path.splitext(fname)

            if args.noprocess:
                # Do no conversions on the source. Assume it is valid MODIFY source already.
                # This should be used with source from modsplit.py processed *with* the --noprocess option.
                # This should be used for CCL procedure code.                
                line1 = text[0]
                if (len(line1) > 0) and (line1[0] == ' '):
                    print('Expected a module name on first line of file. Got space. Giving up.')
                    sys.exit(5)
                fout.writelines(text)
            
            else:
                # Converting to MODIFY module format. Add module name.
                # This should be used for Fortran (and other) source from modsplit.py processed *without*
                # the --noprocess option.                    
                # Add type line if COMMON (.cmn) or ASCII (.asc).
                # Convert INCLUDE to *CALL if not ASCII.
                # Convert C pre-processor #if / #ifdef / #ifndef to MODIFY equivalents, if not ASCII.
                # Convert ASCII characters to 6/12 Display Code if ASCII.
                fout.write(stem.upper()+'\n')
                if ext == '.cmn':
                    fout.write('COMMON\n')
                elif ext == '.asc':
                    fout.write('ASCII\n')
                    
                if ext == '.asc':
                    for line in text:
                        line612 = to_612(line)
                        fout.write(line612)
                else:
                    for line in text:
                        is_comment = (line[0] == 'C') or (line[0] == 'c')
                        if (not is_comment) and ('INCLUDE ' in line):
                            words = line.split()
                            if len(words) == 2:
                                fout.write('*CALL '+ words[1].upper()[1:-5] + '\n')
                            else:
                                print('WARNING: INCLUDE has unexpected format. Dropping.')
                        elif line[0] == '#':
                            if line[:4].lower() == '#if ':
                                ifparms = line[4:].strip().split()
                                if len(ifparms) == 3:
                                    if ifparms[1] == '==':
                                        fout.write('*IF EQ,'+ifparms[0].strip().upper()+','+ifparms[2].strip().upper()+'\n')
                                    elif ifparms[1] == '!=':
                                        fout.write('*IF NE,'+ifparms[0].strip().upper()+','+ifparms[2].strip().upper()+'\n')
                                    else:
                                        print('WARNING: Malformed #if (bad operator), dropped.')
                                else:
                                    print('WARNING: Malformed #if (bad parameter count), dropped.')
                            elif line[:7].lower() == '#ifdef ':
                                ifparms = line[7:].strip().split()
                                if len(ifparms) == 1:
                                    fout.write('*IF DEF,'+ifparms[0].strip().upper()+'\n')
                                else:
                                    print('WARNING: Malformed #ifdef (bad parameter count), dropped.')
                            elif line[:8].lower() == '#ifndef ':
                                ifparms = line[8:].strip().split()
                                if len(ifparms) == 1:
                                    fout.write('*IF UNDEF,'+ifparms[0].strip().upper()+'\n')
                                else:
                                    print('WARNING: Malformed #ifndef (bad parameter count), dropped.')
                            elif line[:5].lower() == '#else':
                                fout.write('*ELSE\n')
                            elif line[:6].lower() == '#endif':
                                fout.write('*ENDIF\n')
                            else:
                                print('WARNING: Unhandled pre-processor line detected. Dropped.')
                        else:
                            fout.write(line)                

            # Information and statistics.
            n_modules += 1
            if n_modules == n_filenames:
                fout.write(eof_string)
            else:
                fout.write(eor_string)
            print('Module: {:7s}  Lines: {:5d}'.format(stem.upper(),len(text)))
            n_lines += len(text)

        # Oops.
        except Exception as e:
            print('Failed to insert data from file:',filename,' Reason:',e)
            sys.exit(3)

    # Close the output file (FTP source file or batch job file).
    fout.close()
    action_info = 'Updated:' if not args.all else 'Added:'
    print(action_info,n_modules,'modules with:',n_lines,'lines.')
    sys.exit(0)

if __name__ == '__main__':
    main()
