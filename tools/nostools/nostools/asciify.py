#!/usr/bin/env python3
"""
Convert Display Code lines which *probably* contain lower case strings
in 6-12 encoding to ASCII lower case characters.
This is only intended to work for FORTRAN-77 source files and is not
intended to be reversible.
It is used to make code extracted from MODIFY libraries suitable for
compilation on Unix-like systems.
"""

import os, argparse, time, sys, json

def to_ascii( line ):
    """
    Convert a line of 6/12 Display Code to ASCII.
    This is only done with modules marked as ASCII (extension .asc).
    """
    topmap = { '0':'{', '1':'|', '2':'}', '3':'~' }
    atmap = { 'A':'@', 'B':'^', 'G':"`", 'D':':' }
    retline = ''
    len_line = len(line)
    i = 0
    while i < len_line:
        j = i + 1
        achar = line[i]
        
        if achar == '^':
            if j < len_line:
                nextchar = line[j]
                if nextchar.isprintable():
                    if nextchar.isalpha():
                        retline += nextchar.lower()
                    else:
                        if nextchar in topmap:
                            retline += topmap[nextchar]
                        else:
                            retline += '*'  # Should not happen.
                i = j

        elif achar == '@':
            if j < len_line:
                nextchar = line[j]
                if nextchar.isprintable():                
                    if nextchar in atmap:
                        retline += atmap[nextchar]
                    else:
                        retline += '*'  # Should not happen.
                i = j
            
        else:
            retline += achar

        i += 1

    return retline

def main():
    """
    Mainline.
    """
    parser = argparse.ArgumentParser()
    parser.add_argument("infile", help="Input file with 6/12 character constants.")
    parser.add_argument("outfile", help="Output file with ASCII character constants.")
    parser.add_argument("-m", "--mapfile", help="JSON file with dictionary defining string mappings.")
    parser.add_argument("-d", "--displaycode", help="Preserve as Display Code.", action="store_true")

    args = parser.parse_args()

    if args.mapfile is None:
        map_dict = None
    else:
        try:
            jfin = open(args.mapfile)
            map_dict = json.load(jfin)
            jfin.close()
            print(map_dict)
        except Exception as e:
            print('ERROR: Cannot open JSON string map file:',args.mapfile)
            print('... Reason:', e)
            sys.exit(1)

    # Open the input FORTRAN source file.
    try:
        fin = open(args.infile)
    except:
        print('Cannot open FORTRAN source input file:',args.infile)
        sys.exit(1)

    # Create the output FORTRAN source file.
    try:
        fout = open(args.outfile, 'w')
    except:
        print('Cannot create FORTRAN source output file:',args.outfile)
        sys.exit(1)

    # If a line contains a ' take it to be a string constant.
    # This will NOT find strings in continued lines spanning 3 or more lines.
    for line in fin:
        if map_dict is not None:
            for old_string in map_dict.keys():
                if old_string in line:
                    new_string = map_dict[old_string]
                    line = line.replace(old_string, new_string)
        if ("'" in line) and (not args.displaycode):
            outline = to_ascii(line)
        else:
            outline = line
        fout.write(outline)

    # Clean up.
    fout.close()
    fin.close()
    sys.exit(0)
    
if __name__ == '__main__':
    main()
