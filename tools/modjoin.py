#!/usr/bin/env python3
"""
Combine files created by:
python ../unix/modsplit.py -d cursrc -u -n -c modify_source_output
into a new MODIFY program library by creating a batch job.
"""

import os, argparse, time, sys, fnmatch

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

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("indir", help="Input directory, 1 file/module to go into MODIFY PL.")
    parser.add_argument("outjob", help="Output job, submit to CR to create MODIFY PL.")
    parser.add_argument("plname", help="Name of MODIFY PL to create.")
    parser.add_argument("-u","--user", help="Name of user for the job.")
    parser.add_argument("-p","--passw", help="Password of user for the job.")
    parser.add_argument("-n","--noinclude", help="Convert INCLUDE to *CALL, convert to MODIFY source.", action="store_true")
    parser.add_argument("-f","--ftpmode", help="The source has been FTPd to this file already (no in batch source).")

    args = parser.parse_args()

    if args.user is None:
        username = 'NICK'
    else:
        username = args.user
        
    if args.passw is None:
        password = 'DLRS2'
    else:
        password = args.passw

    ftpmode = ( args.ftpmode is not None )

    if not os.path.isdir(args.indir):
        print('This:',args.indir,'is not a directory.')
        sys.exit(1)

    # Create the output directory as needed.
    outdir,fname = os.path.split(args.outjob)
    if not create_folder_if_not_exists(outdir):
        print('Cannot create output folder:',outdir,' Giving up.')
        sys.exit(9)        

    try:
        fout = open(args.outjob, 'w')
    except Exception as e:
        print('Cannot create output job file. Reason:',e)
        sys.exit(2)

        
    if ftpmode:
        path,fname = os.path.split(args.ftpmode)
        nosftpname,ext = os.path.splitext(fname)
        print('NOS PL source file name should be:', nosftpname.upper())
        fout.write("""MODCJOB.
USER,{0},{1}.
SETTL,*.
SETJSL,*.
GET,{3}.
REWIND,{3}.
FSE,FN={3},OP=D.D;Q
REWIND,{3}.
MODIFY,P=0,N={2},L=0,S=0,C=0,F,Z./*CREATE {3}
REPLACE,{2}.
CATLIST.
""".format(username.upper(),password.upper(),args.plname,nosftpname.upper()))
        eor_string = '(EOR)\n'
        eof_string = '(EOF)\n'

    else:
        fout.write("""MODCJOB.
USER,{0},{1}.
SETTL,*.
SETJSL,*.
COPYEI,INPUT,SRCIN.
REWIND,SRCIN.
MODIFY,P=0,N={2},L=0,S=0,C=0,F,Z./*CREATE SRCIN
REPLACE,{2}.
CATLIST.
~eor
""".format(username.upper(),password.upper(),args.plname))
        eor_string = '~eor\n'
        eof_string = '~eof\n'

    # If ftpmode, close the batch job file and open the source output file.
    if ftpmode:
        fout.close()
        try:
            fout = open(os.path.join(outdir,nosftpname)+'.plsrc', 'w')
        except Exception as e:
            print('Cannot create output PL source file. Reason:',e)
            sys.exit(2)
        fout.write('DELETE ME\n')
            
    n_modules = 0
    n_lines = 0
    filenames = []
    for filename in all_files(args.indir, patterns='*.cmn;*.f;*.src', single_level=True):
        filenames.append(filename)
    n_filenames = len(filenames)
    for filename in filenames:
        try:
            fin = open(filename, 'r')
            text = fin.readlines()
            fin.close()
            if args.noinclude:
                path,fname = os.path.split(filename)
                stem,ext = os.path.splitext(fname)
                fout.write(stem.upper()+'\n')
                if ext == '.cmn':
                    fout.write('COMMON\n')
                for line in text:
                    if 'INCLUDE ' in line:
                        words = line.split()
                        if len(words) == 2:
                            fout.write('*CALL '+ words[1].upper()[1:-5] + '\n')
                    else:
                        fout.write(line)
            else:
                line1 = text[0]
                if (len(line1) > 0) and (line1[0] == ' '):
                    print('Expected a module name on first line of file. Got space. Giving up.')
                    sys.exit(5)
                fout.writelines(text)
            n_modules += 1
            if n_modules == n_filenames:
                fout.write(eof_string)
            else:
                fout.write(eor_string)
            print('Module: {:7s}  Lines: {:5d}'.format(stem.upper(),len(text)))
            n_lines += len(text)
        except Exception as e:
            print('Failed to insert data from file:',filename,' Reason:',e)
            sys.exit(3)

    fout.close()
    print('Added:',n_modules,'modules with:',n_lines,'lines.')
    sys.exit(0)
