#!/usr/bin/env python
"""
nosftp.py - Simple FTP client with some special features useful for NOS.
"""

import os
import argparse
import sys
import getpass
from ftplib import FTP
from enum import Enum
import readline as rl
import atexit
import time

def valid_nos_name(name):
    """
    See if name looks valid for NOS. If so, return True, else False.
    """
    if len(name) > 7:
        return False
    if not name[0].isalpha():
        return False
    for i in range(1,len(name)):
        if not (name[i].isalpha() or name[i].isdigit()):
            return False
    return True
    
def quit_ftp(ftp):
    """
    Close FTP connection.
    """
    try:
        print(ftp.quit())
        return True
    except Exception as e:
        print('Quit failed.')
        print('... reason:', e)
        return False

def login_to_ftp(ftp, username, password):
    """
    Login to FTP server.
    """
    try:
        print(ftp.login(user=username,passwd=password))
        print(ftp.getwelcome())
        return True
    except Exception as e:
        print('Login failed.')
        print('... reason:', e)
        quit_ftp(ftp)
        return False

def dir_list(ftp):
    """
    Get a short directory listing.
    """
    try:
        filelist = sorted(ftp.nlst())
        nfiles = nleft = len(filelist)
        for i in range(nfiles):
            print('{0: <7} '.format(filelist[i]), end='')
            if ((i+1)%10) == 0:
                print('\n', end='')
                nleft -= 10
        if nleft > 0:
            print('\n', end='')
        print(nfiles, ' files.')
        return True
    except Exception as e:
        print('Directory retrieval failed.')
        print('... reason:', e)
        return False

def file_info(ftp, nosfilename):
    """
    Get detailed information on a single file.
    """
    try:
        upnosfilename = nosfilename.upper()
        cmd = f'FN={upnosfilename}'
        ftp.dir(cmd)
        return True
    except Exception as e:
        print('Directory retrieval (file info) failed.')
        print('... reason:', e)
        return False  

def guess_if_file_is_binary(filename):
    """
    Make a good guess as to whether a file is binary or ASCII.
    """
    textchars = bytearray({7,8,9,10,12,13,27} | set(range(0x20, 0x100)) - {0x7f})
    is_binary_string = lambda bytes: bool(bytes.translate(None, textchars))
    try:
        with open(filename, 'rb') as  fin:
            return is_binary_string(fin.read(1024))
    except Exception as e:
        print('WARNING: Cannot open file:', filename)
        return False

def send_file(ftp, filename, nosoutfilename, cs_string, is_binary=None, indirect_binary=None):
    """
    Send a single file.
    """
    # See if it is binary.
    if is_binary is None:
        is_binary = guess_if_file_is_binary(filename)
    print('Sending file: {0} ({1}) ...'.format(filename, 'binary' if is_binary else 'text'))

    # Try STORing the file as nosoutfilename. Deal appropriately according to binary or not.
    # Binary files are stored as DA (direct access) unless indirect_binary is True.
    # Coded (non-binary) files are always stored IA (indirect access).
    # For coded files, the data is interpreted according to character set cs_string (DIS or ASCII).
    try:
        with open(filename, 'rb') as fin:
            print('... storing file as:', nosoutfilename)
            if is_binary:
                if (indirect_binary is None) or (not indirect_binary):
                    cmd = 'STOR {0},DA'.format(nosoutfilename)
                else:
                    cmd = 'STOR {0},IA'.format(nosoutfilename)
                print(ftp.storbinary(cmd, fin))
            else:
                cmd = 'STOR {0},IA,CS={1}'.format(nosoutfilename, cs_string)
                print(ftp.storlines(cmd, fin))
            return True
    except Exception as e:
        print('send_file({0}): STOR failed.'.format(filename))
        print('... reason:', e)
        return False

def get_file(ftp, nosfilename, outfilename, nostype):
    """
    Get a single file.
    """
    # Try RETRing the file nosfilename. Deal appropriately according to binary or not.
    try:
        with open(outfilename, 'wb') as fout:

            def callback_ascii(linedata):
                linedata += '\n'
                fout.write(linedata.encode('utf-8'))

            def callback_binary(bytedata):
                fout.write(bytedata)
                
            print('... retrieving file:', nosfilename)
            if nostype == 'binary':
                cmd = 'RETR {0}'.format(nosfilename)
                print(ftp.retrbinary(cmd, callback_binary))
            else:
                if nostype == 'ascii':
                    cmd = 'RETR {0},CS=ASCII'.format(nosfilename)
                else:
                    cmd = 'RETR {0},CS=DIS'.format(nosfilename)
                print(ftp.retrlines(cmd, callback_ascii))
            return True
    except Exception as e:
        print('get_file({0}): RETR failed.'.format(nosfilename))
        print('... reason:', e)
        return False

def delete_file(ftp, nosfilename):
    """
    Delete a single file.
    """
    try:
        print(ftp.delete(nosfilename))
        return True
    except Exception as e:
        print('delete_file({0}): ftp.delete() failed.'.format(nosfilename))
        print('... reason:', e)
        return False        

def main():
    """
    Mainline.
    """
    # Set up minimal readline history functionality.
    history_file = os.path.join(os.path.expanduser("~"), ".nos_ftp_history")
    try:
        rl.read_history_file(history_file)
        #rl.clear_history()
    except FileNotFoundError:
        pass
    atexit.register(rl.write_history_file, history_file)

    # Get and parse command line arguments.
    parser = argparse.ArgumentParser()
    parser.add_argument("user", help="Name of user on NOS host.")
    parser.add_argument("hostname", help="Name of NOS host for TCP/IP.")
    parser.add_argument("-d", "--debug", help="Debug level, 0, 1 or 2.")
    parser.add_argument("-p", "--password", help="Plain text password (prompt if omitted.)")
    parser.add_argument("-e", "--execute", help="Single command to execute, then exit.")
    args = parser.parse_args()

    if args.debug is None:
        deblevel = 0
    else:
        try:
            deblevel = max(0, min(2, int(args.debug) ) )
        except Exception as e:
            print('--debug option value invalid.')
            quit_ftp(ftp)
            sys.exit(66)
        
    # Get the NOS account password "secretly", if not provided as an argument.
    password = args.password
    if password is None:
        try:
            password = getpass.getpass('Password for NOS account: ')
        except Exception as e:
            print('Failed to get NOS account password. Reason:', e)
            sys.exit(1)

    if (len(password) < 4) or (len(password) > 7):
        print('Invalid password length')
        sys.exit(2)

    if not valid_nos_name(password):
        print('Invalid password characters')
        sys.exit(3)

    # Open FTP connection.
    print('Contacting NOS FTP server on host:', args.hostname)
    ntries = 40
    for i in range(ntries):
        try:
            ftp = FTP(args.hostname)
            break
        except Exception as e:
            if i == (ntries - 1):
                print('Cannot open FTP connection to host', args.hostname)
                print('... reason:', e)
                sys.exit(4)
            else:
                print('... ',i+1,' knock, knock ...')
                time.sleep(2)
    ftp.set_debuglevel(deblevel)

    # Login to host FTP server.
    if not login_to_ftp(ftp, args.user, password):
        sys.exit(5)

    # Define commands.
    class cmds(Enum):
        NONE = 0
        QUIT = 1
        GET = 2
        PUT = 3
        DIR = 4
        INFO = 5
        LPWD = 6
        LCD = 7
        LS = 8
        DEL = 9
        BPUT = 10
        
    commands = {'quit': (0, 0, '', cmds.QUIT, 'Exit NOS FTP.'),
                'exit': (0, 0, '', cmds.QUIT, 'Exit NOS FTP.'),
                'bye':  (0, 0, '', cmds.QUIT, 'Exit NOS FTP.'),
                'get':  (3, 3, 'nosname localname ascii|display|binary', cmds.GET, 'Get a file from the NOS FTP server.'),
                'put':  (2, 3, 'localname nosname [display]', cmds.PUT, 'Send a file to the NOS FTP server.'),
                'dir':  (0, 0, '', cmds.DIR, 'Compact list of all permanent files.'),
                'info': (1, 1, 'nosname', cmds.INFO, 'List full information on a single permanent file.'),
                'lpwd': (0, 0, '', cmds.LPWD, 'Print local working directory name.'),
                'lcd':  (1, 1, 'localdir', cmds.LCD, 'Change local working directory.'),
                'ls':   (0, 1, '', cmds.LS, '[string] List local working directory, [names containing string].'),
                'del':  (1, 1, 'nosname', cmds.DEL, 'Delete a permanent file.'),
                'bput': (2, 3, 'localname nosname [direct]', cmds.BPUT, 'Send a binary file to the server, indirect by default.')}

    def parse_cmd( cmdline, commands ):
        """
        Recognize commands in cmdline. Return cmdline split into
        space separated words and command enum: (cmd_enum, args_list)
        """
        cmdline = cmdline.lower().strip()
        if len(cmdline) == 0:
            return (cmds.NONE, [])
        else:
            cmdwords = cmdline.split()
            cmd = cmdwords[0]
            if cmd == 'help':
                sorted_keys = sorted(commands.keys())
                for key in sorted_keys:
                    print(key, ':', commands[key][2], ':', commands[key][4])
                return (cmds.NONE, [])
            if cmd not in commands:
                print('Unknown command.')
                return (cmds.NONE, [])
            nargs = len(cmdwords) - 1
            if nargs < commands[cmd][0]:
                print('Missing arguments:', cmd, commands[cmd][2])
                return (cmds.NONE, [])
            elif nargs > commands[cmd][1]:
                print('Excess arguments:', cmd, commands[cmd][2])
                return (cmds.NONE, [])
            else:
                return (commands[cmd][3], cmdwords[1:])

    # Get the current working directory.
    cwd = os.getcwd()
    print('Local cwd now:', cwd)

    # Main  command parsing loop
    while True:
        if args.execute is None:
            try:
                cmdline = input('\nNOS FTP> ')
            except Exception as e:
                print('\nFailed to read command input. Quitting.')
                quit_ftp(ftp)
                sys.exit(6)

        else:
            cmdline = args.execute    

        cmdcode, argslist = parse_cmd(cmdline, commands)
        if cmdcode != cmds.NONE:

            if cmdcode == cmds.QUIT:
                print('\nExiting normally.')
                quit_ftp(ftp)
                sys.exit(0)

            elif cmdcode == cmds.GET:
                nostypes_list = ['ascii','display','binary']
                if not argslist[2] in nostypes_list:
                    print('File type must be one of:', nostypes_list)
                else:
                    if not valid_nos_name(argslist[0]):
                        print('NOS file name is invalid.')
                    else:
                        if not get_file(ftp, argslist[0], argslist[1], argslist[2]):
                            print('Failed.')

            elif cmdcode == cmds.PUT:
                if not valid_nos_name(argslist[1]):
                    print('NOS file name is invalid.')
                else:
                    if (len(argslist) == 3) and (argslist[2] == 'display'):
                        cs_string = 'DIS'
                    else:
                        cs_string = 'ASCII'
                    if not send_file(ftp, argslist[0], argslist[1], cs_string):
                        print('Failed')

            elif cmdcode == cmds.DIR:
                if not dir_list(ftp):
                    print('Failed')
                    
            elif cmdcode == cmds.INFO:
                if not valid_nos_name(argslist[0]):
                    print('NOS file name is invalid.')
                else:
                    if not file_info(ftp, argslist[0]):
                        print('Failed')

            elif cmdcode == cmds.LPWD:
                print('Local directory:', cwd)

            elif cmdcode == cmds.LCD:
                newdir = ''
                if argslist[0][0] == '~':
                    newdir = os.path.expanduser(argslist[0])
                elif argslist[0][0] == '.':
                    newdir = os.path.join(cwd, argslist[0])
                else:
                    newdir = argslist[0]
                newdir = os.path.normpath(newdir)
                if not os.path.isdir(newdir):
                    print('Not a directory:', newdir)
                    print('Local cwd unchanged at:', cwd)
                else:
                    try:
                        os.chdir(newdir)
                    except Exception as e:
                        print('Cannot change cwd to:', newdir)
                        print('... reason:', e)
                    cwd = os.getcwd()
                    print('Local cwd now:', cwd)

            elif cmdcode == cmds.LS:
                try:
                    filelist = sorted(os.listdir())
                    if len(argslist) == 1:
                        newfilelist = []
                        for filename in filelist:
                            if filename.lower().find(argslist[0]) != -1:
                                newfilelist.append(filename)
                        if len(newfilelist) == 0:
                            print('No matching files found.')
                            continue
                        filelist = newfilelist
                    nfiles = len(filelist)
                    lenline = 0
                    for i in range(nfiles):
                        lenname = len(filelist[i])
                        if (lenline + lenname + 2) > 80:
                            print('\n', end='')
                            lenline = 0
                        print(filelist[i]+'  ', end='')
                        lenline += (lenname + 2)
                    if lenline != 0:
                        print(' ')
                    print(nfiles, ' files.')
                except Exception as e:
                    print('Local directory retrieval failed.')
                    print('... reason:', e)

            elif cmdcode == cmds.DEL:
                if not valid_nos_name(argslist[0]):
                    print('NOS file name is invalid.')
                else:
                    if not delete_file(ftp, argslist[0]):
                        print('Failed.')

            elif cmdcode == cmds.BPUT:
                if not valid_nos_name(argslist[1]):
                    print('NOS file name is invalid.')
                else:
                    as_indirect = (len(argslist) < 3) or (argslist[2] != 'direct')
                    if not send_file(ftp, argslist[0], argslist[1], 'none',
                                     is_binary=True, indirect_binary=as_indirect):
                        print('Failed')                        

        if args.execute is not None:
            quit_ftp(ftp)
            sys.exit(0)

if __name__ == '__main__':
    main()
    
