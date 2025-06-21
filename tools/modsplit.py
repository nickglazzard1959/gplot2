#!/usr/bin/env python3
"""
Split the SOURCE output of MODIFY into individual files.
Determine the file type.
Save either MODIFY SOURCE or Unix compilable code.
Optionally generate a build script for Unix.
"""

import os, argparse, time, sys

def read_module(fin, modsrc):
    """
    Read a single module from fin. Return a quadruplet:
    (modname, extension, numlines, text)
    Return ('','.src',0,[]) on error or EOF.
    """
    fortran_clues = ['SUBROUTINE','FUNCTION','PROGRAM','BLOCK DATA']
    modname = ''
    extension = '.src'
    numlines = 0
    text = []
    for line in fin:
        if (len(line) > 0) and ((line[0] == '~') or (line[0] == '}')):
            return (modname, extension, numlines, text)
        elif (len(line) >= 7) and (line[:7] == '--EOR--'):
            return (modname, extension, numlines, text)
        elif (len(line) >= 7) and (line[:7] == '--EOF--'):
            return (modname, extension, numlines, text)
        else:
            sline = line.rstrip()
            if numlines == 0:
                modname = sline.lower()
                if modsrc:
                    text.append(sline)
            else:
                if extension == '.src':
                    upsline = sline.upper()
                    if upsline[:6] == '      ':
                        for clue in fortran_clues:
                            if clue in upsline:
                                extension = '.f'
                if (numlines == 1) or (numlines == 2):
                    if sline.lower() == 'common':
                        extension = '.cmn'
                        if modsrc:
                            text.append(sline)
                    elif sline.lower() == 'ascii':
                        if modsrc:
                            text.append(sline)                        
                    else:
                        text.append(sline)
                else:
                    text.append(sline)
        numlines += 1

    # Should never get here, but just in case.
    return (modname, extension, numlines, text)

def write_module(modinfo, upcase, outdir, omits, modsrc):
    """
    Write out a module as a file. 
    If upcase, make all code UPPER CASE.
    Convert *call xxx to INCLUDE 'xxx'
    Prepend outdir to the output filename.
    """
    name = modinfo[0]
    extension = modinfo[1]
    text = modinfo[3]
    path = os.path.join(outdir,name) + extension
    fout = open(path,'w')
    if upcase:
        incstr = 'INCLUDE '
    else:
        incstr = 'include '
    for line in text:
        if (line[:5].lower() == '*call') and (not modsrc):
            incname = line[6:].lower() + '.cmn'
            fout.write('      '+incstr+"'"+incname+"'"+'\n')
        else:
            if upcase:
                fout.write(line.upper()+'\n')
            else:
                fout.write(line+'\n')
    fout.close()

def write_script(sources, script, outdir, library, omits):
    """
    Write a compile script to file name  script in outdir.
    If library is not '', make a static library.
    """
    path = os.path.join(outdir,script)
    fout = open(path,"w")
    fout.write('#!/bin/sh\n')
    for module in sources:
        if not module in omits:
            fout.write('gfortran -c -std=legacy '+ module + '.f\n')
    if library != '':
        numsources = len(sources)
        fout.write('libtool -static -o '+library+'.a ')
        cursource = 1
        for module in sources:
            if not module in omits:
                if cursource == numsources:
                    fout.write(module + '.o\n')
                else:
                    fout.write(module + '.o \\\n')
            cursource += 1
    fout.close()

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
    epilog_text = """

The input to this program must be MODIFY SOURCE output. This file
will contain multiple records. Copying with SCOPY can preserve the
record structure for text files transferred with FTP.

The following batch job will create a suitable output file. That file 
must be transferred by FTP (using "nosftp.py" is easiest).

BADIMFM.
USER,NICK,DLRS2.
*
* GET SOURCE FROM A MODIFY LIBRARY PLDIMFM.
* THE RESULT IS SSDIMFM.
* THAT CAN BE FTP-D WITH RECORD STRUCTURE
* PRESERVED.
*
SETTL,*.
SETJSL,*. 
GET,PLDIMFM.
MODIFY,P=PLDIMFM,N=0,L=0,C=0,S=SRDIMFM,F.
REWIND,SRDIMFM.
SCOPY,SRDIMFM,SSDIMFM.
REPLACE,SSDIMFM.

The modjoin.py program will take the MODIFY SOURCE type of output
from this program and create a batch job that will recreate a
MODIFY program library.

"""
    parser = argparse.ArgumentParser(epilog=epilog_text, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("infile", help="Input file from MODIFY source output.")
    parser.add_argument("-d","--outdir", help="Set the output directory name.")
    parser.add_argument("-u","--upper", help="Convert to UPPER CASE.", action="store_true")
    parser.add_argument("-s","--script", help="Write compile script to this file.")
    parser.add_argument("-l","--library", help="Make a static library with the specified name.")
    parser.add_argument("--omit", help="Comma separated list of modules to not process.")
    parser.add_argument("-n","--noinclude", help="Do not convert *CALL to INCLUDE, save as MODIFY source.", action="store_true")
    parser.add_argument("-c","--cards", help="Source is from card punch, skip first line.", action="store_true")

    args = parser.parse_args()
    start_time = time.time()

    if args.outdir != None:
        outdir = args.outdir
    else:
        outdir = 'outmodules'

    if args.library != None:
        library = args.library
    else:
        library = ''

    omits = []
    if args.omit != None:
        omits = args.omit.split(',')
        print('Omitting modules:',omits)

    # Create the output directory as needed.
    if not create_folder_if_not_exists(outdir):
        print('Cannot create output folder:',outdir,' Giving up.')
        sys.exit(9)

    # Open the input MODIFY source file.
    try:
        fin = open(args.infile)
    except:
        print('Cannot open MODIFY source input file:',args.infile)
        sys.exit(1)

    # If source from a card punch, skip the first line.
    if args.cards:
        junk = fin.readline()

    # Accumulate data to write a compile script.
    sources = []

    # Process one module at a time.
    n_modules = 0
    while True:
        modinfo = read_module(fin, args.noinclude)

        # If the name is empty, we have finished.
        if modinfo[0] == '':
            elapsed_time = time.time() - start_time
            elapsed_mins = int(elapsed_time) / 60
            elapsed_secs = elapsed_time - ( 60 * elapsed_mins )
            print('INFO: Processing took:',elapsed_mins,'minutes',int(10.0*elapsed_secs)/10.0,'seconds.')
            print('INFO: Number of modules processed:',n_modules)
            print('Done.')
            break;

        # Otherwise, write the module as a file with appropriate changes.
        else:
            mtype = 'source '
            if modinfo[1] == '.cmn':
                mtype = 'common '
            elif modinfo[1] == '.f':
                mtype = 'fortran'
                sources.append(modinfo[0])
            status = ' '
            if modinfo[0] in omits:
                status = '(OMITTED)'
            else:
                status = ' '
            print('Module: {:7s}  Type: {:7s}  Lines: {:5d} {:7s}'.format(modinfo[0],mtype,modinfo[2],status))
            #print 'Module:',modinfo[0],'Type:',mtype,'Lines:',modinfo[2] 
            write_module(modinfo, args.upper, outdir, omits, args.noinclude)
            n_modules += 1

    # Optionally, write a compile script.
    if args.script != None:
        write_script(sources, args.script, outdir, library, omits)

    # Close the input module.
    fin.close()
    sys.exit(0)
