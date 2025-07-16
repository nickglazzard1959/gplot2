#!/usr/bin/env python3
"""
Convert Display Code lines which *probably* contain lower case strings
in 6-12 encoding to ASCII lower case characters.
This is only intended to work for FORTRAN-77 source files and is not
intended to be reversible.
It is used to make code extracted from MODIFY libraries suitable for
compilation on Unix-like systems.

It can do general string substitution. This is useful to convert
some non-standard code to something certain compilers will accept.

It can do enough "pre-processing" for conditional compilation using
CPP compatible statements #ifdef, #ifndef, #if a == b, #if a != b,
#else, #endif. These are those to which modsplit translates MODIFY
conditional compilation directives. This means the FORTRAN compiler
does not have to be able to run CPP (or an equivalent) which is
important for kFTC. #if a is also supported, which is True if a is
not 0.

Conditionals can be nested and indented, which is not really needed
but may become useful and is easy to do.
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
    parser.add_argument("-v", "--verbose", help="Output some processing information.", action="store_true")
    parser.add_argument("-c", "--conditional", help="Conditional compile using definitions in this JSON file.")

    args = parser.parse_args()

    # Read any string mapping dictionary.
    if args.mapfile is None:
        map_dict = None
    else:
        try:
            jfin = open(args.mapfile)
            map_dict = json.load(jfin)
            jfin.close()
            if args.verbose:
                print('String mapping dictionary')
                print(map_dict)
        except Exception as e:
            print('ERROR: Cannot open JSON string map file:',args.mapfile)
            print('... Reason:', e)
            sys.exit(1)

    # Read any conditional compilation symbols dictionary.
    if args.conditional is None:
        docond = False
    else:
        docond = True
        try:
            jfin = open(args.conditional)
            cond_dict = json.load(jfin)
            jfin.close()
            if args.verbose:
                print('Conditional compilation symbols dictionary')
                print(cond_dict)
        except Exception as e:
            print('ERROR: Cannot open JSON symbol definition file:',args.conditional)
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

    # Step over input source lines.
    condout = True
    incondsect = False
    condstack = []
    i_line = 0
    o_line = 0
    for line in fin:
        i_line += 1

        # Do optional general string mapping.
        if map_dict is not None:
            for old_string in map_dict.keys():
                if old_string in line:
                    new_string = map_dict[old_string]
                    line = line.replace(old_string, new_string)

        # Optionally map 6/12 Display Code string constants to lower/upper case ASCII.
        # If a line contains a ' take it to be a string constant.
        # This will NOT find strings in continued lines spanning 3 or more lines.
        if ("'" in line) and (not args.displaycode):
            outline = to_ascii(line)
        else:
            outline = line

        # Conditional compilation output logic.
        if docond:
            stripped_outline = outline.strip()
            if (len(stripped_outline) > 1) and (stripped_outline[0] == '#'):
                line = stripped_outline
                
                if line[:4].lower() == '#if ':
                    condstack.append((incondsect,condout))
                    ifparms = line[4:].strip().split()
                    if len(ifparms) == 3:
                        symbol = ifparms[0]
                        relation = ifparms[1]
                        testvalue = ifparms[2]
                        if symbol not in cond_dict.keys():
                            print('ERROR: symbol',symbol,'in #if is not defined. Line:',i_line)
                            sys.exit(1)
                        else:
                            value = cond_dict[symbol]
                            if relation == '==':
                                condout = (value == testvalue)
                            elif relation == '!=':
                                condout = (value != testvalue)
                            else:
                                print('ERROR: Malformed #if (bad relational operator). Line:',i_line)
                                sys.exit(1)
                    elif len(ifparms) == 1:
                        symbol = ifparms[0]
                        if symbol not in cond_dict.keys():
                            print('ERROR: symbol',symbol,'in #if is not defined. Line:',i_line)
                            sys.exit(1)
                        else:
                            value = cond_dict[symbol]
                            condout = (value != '0')
                    else:
                        print('ERROR: Malformed #if (bad parameter count). Line:',i_line)
                        sys.exit(1)
                    incondsect = True
                        
                elif line[:7].lower() == '#ifdef ':
                    condstack.append((incondsect,condout))
                    ifparms = line[7:].strip().split()
                    if len(ifparms) == 1:
                        symbol = ifparms[0]
                        condout = (symbol in cond_dict.keys())
                    else:
                        print('ERROR: Malformed #ifdef (bad parameter count). Line:',i_line)
                        sys.exit(1)
                    incondsect = True
                        
                elif line[:8].lower() == '#ifndef ':
                    condstack.append((incondsect,condout))
                    ifparms = line[8:].strip().split()
                    if len(ifparms) == 1:
                        symbol = ifparms[0]
                        condout = (symbol not in cond_dict.keys())
                    else:
                        print('ERROR: Malformed #ifndef (bad parameter count). Line:',i_line)
                        sys.exit(1)
                    incondsect = True
                        
                elif line[:5].lower() == '#else':
                    if not incondsect:
                        print('ERROR: #else outside conditional section. Line:',i_line)
                        sys.exit(1)                        
                    condout = not condout
                    
                elif line[:6].lower() == '#endif':
                    if not incondsect:
                        print('ERROR: #endif outside conditional section. Line:',i_line)
                        sys.exit(1)
                    if len(condstack) == 0:
                        print('ERROR: Unmatched #endif. Line:',i_line)
                        sys.exit(1)
                    incondsect, condout = condstack.pop()
                    
                else:
                    print('WARNING: Unhandled # pre-processor line detected. Line:',i_line)
                    sys.exit(1)

            else:
                if( ( not incondsect ) or (incondsect and condout) ):
                    fout.write(outline)
                    o_line += 1

        # No conditional compilation in effect.
        else:
            fout.write(outline)
            o_line += 1

    # Clean up.
    fout.close()
    fin.close()
    if args.verbose:
        print('Input lines:',i_line,' Output lines:',o_line)
    sys.exit(0)
    
if __name__ == '__main__':
    main()
