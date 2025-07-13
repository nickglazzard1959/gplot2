#!/usr/bin/env python3
"""
Trim trailing comment lines from .f and .cmn files.
These can cause trouble when compiling because they can make the
compiler think another program unit is coming (subroutine, etc.)
and then it just gets EOF. This may be considered a fatal error.
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

def main():
    """
    Mainline.
    """
    parser = argparse.ArgumentParser()
    parser.add_argument("indir", help="Input directory. All .f and .cmn files here will be processed.")
    parser.add_argument("outdir", help="Output directory. Processed files will be written here.")

    args = parser.parse_args()

    if not os.path.isdir(args.indir):
        print('This:',args.indir,'is not a directory.')
        sys.exit(1)

    if not create_folder_if_not_exists(args.outdir):
        print('Cannot create output folder:',args.outdir,' Giving up.')
        sys.exit(1)

    n_files = 0
    m_files = 0
    e_files = 0
    for filename in all_files(args.indir, patterns='*.cmn;*.f', single_level=True):
        try:
            fin = open(filename)
            lines = fin.readlines()
            fin.close()
        except Exception as e:
            print('Failed to read:', filename, 'Reason:', e)
            sys.exit(1)
            
        path,fname = os.path.split(filename)
        n_lines = m_lines = len(lines)
        for i_line in range(n_lines-1,-1,-1):
            if (lines[i_line][0] != 'c') and (lines[i_line][0] != 'C'):
                break
            m_lines -= 1
            
        print('File: {:12s}  Lines: {:>5d}  Trimmed: {:>3d}'.format(fname, len(lines), n_lines-m_lines))
        
        if m_lines == 0:
            print('... WARNING: all lines trimmed, file EMPTY!')
            e_files += 1
        
        outfilename = os.path.join(args.outdir, fname)
        try:
            fout = open(outfilename, 'w')
            fout.writelines(lines[:m_lines])
            fout.close()
        except Exception as e:
            print('Failed to write:', outfilename, 'Reason:', e)
            sys.exit(1)

        n_files += 1
        if (n_lines - m_lines) > 0:
            m_files += 1
        
    print('Files processed:', n_files)
    print('Files modified:', m_files)
    print('Files emptied:', e_files)
    sys.exit(0)

if __name__ == '__main__':
    main()
    
