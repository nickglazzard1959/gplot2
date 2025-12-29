#!/usr/bin/env python
"""
vmsftp.py - Simple FTP client with some special features useful for VMS.
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
import json

# We need to treat the VAX/VMS V4.7 FTP server with great care.
# It only seems to work if full paths are specified for directory operations (create/change/etc.)
# It also needs VMS format directory specifications.
# Prepare to keep track of the user's home directory and the full path.
g_vms47 = False
g_root47 = ''
g_path47 = ''

def valid_vms_name(name):
    """
    See if name looks valid for VMS ODS 2. If so, return True, else False.
    This isn't really very careful, but it might catch some mistakes.
    Some legal names are also rejected.
    """
    specials = ['.', '[', ']', '-', ';', '*', '_', '$']
    if len(name) > 79:
        return False
    stem, ext = os.path.splitext(name)
    if len(stem) > 39:
        return False
    if len(ext) > 40:
        return False
    if not (name[0].isalpha() or (name[0] == '[') or (name[0] == '*')):
        return False
    for i in range(1,len(name)):
        if not (name[i].isalpha() or name[i].isdigit() or (name[i] in specials)):
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
        # If trying to connect to VMS 4.7, do not use passive mode.
        if g_vms47:
            ftp.set_pasv(False)
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
        nfiles = len(filelist)
        for i in range(nfiles):
            print('{0} '.format(filelist[i]))
        print(nfiles, ' files.')
        return True
    except Exception as e:
        print('Directory retrieval failed.')
        print('... reason:', e)
        return False

def file_info(ftp, vmsfilename):
    """
    Get detailed information on a single file.
    """
    try:
        upvmsfilename = vmsfilename.upper()
        cmd = f'FN={upvmsfilename}'
        ftp.dir(cmd)
        return True
    except Exception as e:
        print('Directory retrieval (file info) failed.')
        print('... reason:', e)
        return False  

def send_file(ftp, filename, vmsoutfilename, is_binary=False):
    """
    Send a single file.
    """
    print('Sending file: {0} ({1}) ...'.format(filename, 'binary' if is_binary else 'text'))

    # Try STORing the file as vmsoutfilename. Deal appropriately according to binary or not.
    try:
        with open(filename, 'rb') as fin:
            cmd = 'STOR {0}'.format(vmsoutfilename)
            print('... storing file as:', vmsoutfilename)
            if is_binary:
                print(ftp.storbinary(cmd, fin))
            else:
                print(ftp.storlines(cmd, fin))
            return True
    except Exception as e:
        print('send_file({0}): STOR failed.'.format(filename))
        print('... reason:', e)
        return False

def get_file(ftp, vmsfilename, outfilename, is_binary=False):
    """
    Get a single file.
    """
    # Try RETRing the file vmsfilename. Deal appropriately according to binary or not.
    try:
        with open(outfilename, 'wb') as fout:

            def callback_ascii(linedata):
                linedata += '\n'
                fout.write(linedata.encode('utf-8'))

            def callback_binary(bytedata):
                fout.write(bytedata)
                
            print('... retrieving file:', vmsfilename, 'binary' if is_binary else 'text')
            cmd = 'RETR {0}'.format(vmsfilename)
            if is_binary:
                print(ftp.retrbinary(cmd, callback_binary))
            else:
                print(ftp.retrlines(cmd, callback_ascii))
            return True
    except Exception as e:
        print('get_file({0}): RETR failed.'.format(vmsfilename))
        print('... reason:', e)
        return False

def delete_file(ftp, vmsfilename):
    """
    Delete a single file.
    """
    try:
        print(ftp.delete(vmsfilename))
        return True
    except Exception as e:
        print('delete_file({0}): ftp.delete() failed.'.format(vmsfilename))
        print('... reason:', e)
        return False

def delete_directory(ftp, dirname):
    """
    Delete a directory.
    """
    try:
        print(ftp.rmd(dirname))
        return True
    except Exception as e:
        print('delete_directory({0}): ftp.rmd() failed.'.format(dirname))
        print('... reason:', e)
        return False

def change_remote_directory(ftp, dirname):
    """
    Change the remote working directory to dirname.
    Note that this is only intended to "go down" a single directory.
    Specifying a multi-element path is not intended to work and will not.
    VMS 4.7 needs to be handled specially. Using ".." as a dirname will
    "go up" one level. 
    """
    global g_path47
    
    if g_vms47:
        if dirname == '..':
            if g_path47 == g_root47:
                print('Cannot go up, already at user root.')
                dirname = g_root47
            else:
                dotindex = g_path47.rfind('.')
                if dotindex >= 0:
                    dirname = g_path47[:dotindex]+']'
                else:
                    dirname = g_root47
        else:
            dirname = g_path47[:-1]+'.'+dirname+']'
        print('change_remote_directory() VMS 4.7, dirname=',dirname)
    try:
        print(ftp.cwd(dirname))
        g_path47 = dirname
        return True
    except Exception as e:
        print('change_remote_directory({0}): ftp.cwd() failed.'.format(dirname))
        print('... reason:', e)
        return False

def create_remote_directory(ftp, dirname):
    """
    Create remote directory called dirname.
    VMS 4.7 needs to be handled specially.
    """
    if g_vms47:
        dirname = g_path47[:-1]+'.'+dirname+']'
        print('create_remote_directory() VMS 4.7, dirname=',dirname)        
    try:
        print(ftp.mkd(dirname))
        return True
    except Exception as e:
        print('create_remote_directory({0}): ftp.mkd() failed.'.format(dirname))
        print('... reason:', e)
        return False

def show_directory(ftp):
    """
    Show remote directory name.
    """
    try:
        print(ftp.pwd())
        return True
    except Exception as e:
        print('show_directory({0}): ftp.pwd() failed.'.format(dirname))
        print('... reason:', e)
        return False

def main():
    """
    Mainline.
    """
    global g_vms47, g_root47, g_path47
    
    # Set up minimal readline history functionality.
    history_file = os.path.join(os.path.expanduser("~"), ".vms_ftp_history")
    try:
        rl.read_history_file(history_file)
        #rl.clear_history()
    except FileNotFoundError:
        pass
    except Exception as e:
        print('Error reading history file:', history_file)
        print('... reason:', e)
    
    atexit.register(rl.write_history_file, history_file)

    # Get and parse command line arguments.
    parser = argparse.ArgumentParser()
    parser.add_argument("user", help="Name of user on VMS host.")
    parser.add_argument("hostname", help="Name of VMS host for TCP/IP.")
    parser.add_argument("-d", "--debug", help="Debug level, 0, 1 or 2.")
    parser.add_argument("-p", "--password", help="Plain text password (prompt if omitted.)")
    parser.add_argument("-e", "--execute", help="Single command to execute, then exit.")
    parser.add_argument("-b", "--binary", help="Set initial transfer type to BINARY (def: TEXT).", action='store_true')
    parser.add_argument("-q", "--quiterror", help="Quit if an error occurs.", action='store_true')
    parser.add_argument("-m", "--mapfile", help="JSON file with dictionary defining file extension mappings.")
    parser.add_argument("-r", "--root47", help="Full path of user directory to work around VMS 4.7 FTP issues.")
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

    as_binary = args.binary

    # Read any extension mapping dictionary.
    if args.mapfile is None:
        map_dict = None
    else:
        try:
            jfin = open(args.mapfile)
            map_dict = json.load(jfin)
            jfin.close()
            print('File extension mapping dictionary')
            print(map_dict)
        except Exception as e:
            print('ERROR: Cannot open JSON file extension map file:',args.mapfile)
            print('... Reason:', e)
            sys.exit(1)
            
    # Get the VMS account password "secretly", if not provided as an argument.
    password = args.password
    if password is None:
        try:
            password = getpass.getpass('Password for VMS account: ')
        except Exception as e:
            print('Failed to get VMS account password. Reason:', e)
            sys.exit(1)

    if not valid_vms_name(password):
        print('Invalid password characters')
        sys.exit(3)

    # Get the VMS 4.7 user directory "root", if any. If present, try to make things work
    # with the VMS 4.7 FTP server. This seems to work if given absolute paths for directories ...
    g_vms47 = False
    g_root47 = ''
    g_path47 = ''
    if (args.root47 is not None) and (args.root47 != 'none'):
        g_vms47 = True
        g_root47 = args.root47
        g_path47 = args.root47
        print('Using VAX/VMS V4.7 FTP server compatibility mode.')

    # Open FTP connection.
    print('Contacting VMS FTP server on host:', args.hostname)
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
        MPUT = 11
        MGET = 12
        CD = 13
        CRED = 14
        BIN = 15
        TEXT = 16
        DELD = 17
        PWD = 18
        
    commands = {'quit': (0, 0, '', cmds.QUIT, 'Exit VMS FTP.'),
                'exit': (0, 0, '', cmds.QUIT, 'Exit VMS FTP.'),
                'bye':  (0, 0, '', cmds.QUIT, 'Exit VMS FTP.'),
                'get':  (1, 2, 'vmsname [localname]', cmds.GET, 'Get a file from the VMS FTP server.'),
                'put':  (1, 2, 'localname [vmsname]', cmds.PUT, 'Send a file to the VMS FTP server.'),
                'dir':  (0, 0, '', cmds.DIR, 'List of all files.'),
                'info': (1, 1, 'vmsname', cmds.INFO, 'List full information on a single file.'),
                'lpwd': (0, 0, '', cmds.LPWD, 'Print local working directory name.'),
                'lcd':  (1, 1, 'localdir', cmds.LCD, 'Change local working directory.'),
                'ls':   (0, 1, '', cmds.LS, '[string] List local working directory, [names containing string].'),
                'del':  (1, 1, 'vmsname', cmds.DEL, 'Delete a file.'),
                'mput': (1, 1, 'listname', cmds.MPUT, 'Send a list of files to the server.'),
                'mget': (1, 1, 'listname', cmds.MGET, 'Get a list of files from the server.'),
                'cd':   (1, 1, 'dirname', cmds.CD, 'Change to remote sub-directory. Use .. to go up.'),
                'cred': (1, 1, 'dirname', cmds.CRED, 'Create remote sub-directory.'),
                'bin':  (0, 0, '', cmds.BIN, 'Change transfer mode to BINARY.'),
                'text': (0, 0, '', cmds.TEXT, 'Change transfer mode to TEXT.'),
                'deld': (1, 1, 'dirname', cmds.DELD, 'Delete a remote sub-directory.'),
                'pwd':  (0, 0, '', cmds.PWD, 'Show current remote working directory.')}

    def parse_cmd( cmdline, commands ):
        """
        Recognize commands in cmdline. Return cmdline split into
        space separated words and command enum: (cmd_enum, args_list)
        """
        cmdline = cmdline.strip()
        if len(cmdline) == 0:
            return (cmds.NONE, [])
        else:
            cmdwords = cmdline.split()
            cmd = cmdwords[0].lower()
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

    def quit_on_error():
        """
        Exit on error, if desired.
        """
        print('Exiting on error as requested.')
        quit_ftp(ftp)
        sys.exit(33)

    # Get the current working directory.
    cwd = os.getcwd()
    print('Local cwd now:', cwd)

    # Main  command parsing loop
    while True:
        if args.execute is None:
            try:
                cmdline = input('\nVMS FTP> ')
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
                if not valid_vms_name(argslist[0]):
                    print('VMS file name is invalid.')
                    if args.quiterror:
                        quit_on_error()
                else:
                    if len(argslist) == 2:
                        localfile = argslist[1]
                    else:
                        localfile = argslist[0]
                    if not get_file(ftp, argslist[0], localfile, as_binary):
                        print('Failed.')
                        if args.quiterror:
                            quit_on_error()

            elif cmdcode == cmds.MGET:
                try:
                    with open(argslist[0], 'r') as fin:
                        inlist = fin.readlines()
                except Exception as e:
                    print('Failed to read files list from:',argslist[0])
                    if args.execute is not None:
                        quit_ftp(ftp)
                        sys.exit(0)
                    else:
                        if args.quiterror:
                            quit_on_error()

                ngot = nfiles = 0
                for inname in inlist:
                    inname = inname.strip()
                    if len(inname) > 0:
                        if inname[0] != '#':
                            inname = inname.lower()
                            nfiles += 1
                            print('... getting:', inname, '(', nfiles, ')')
                            if not valid_vms_name(inname):
                                print('... ... VMS file name is invalid. Skipping ...')
                                if args.quiterror:
                                    quit_on_error()
                            else:
                                outname = inname + '.' + argslist[2]
                                if not get_file(ftp, inname, outname, as_binary):
                                    print('... ... Failed to send file. Skipping ...')
                                else:
                                    ngot += 1

                print('Got', ngot, 'of', nfiles, 'files OK')                    

            elif cmdcode == cmds.PUT:
                if len(argslist) == 2:
                    remotefile = argslist[1]
                else:
                    remotefile = argslist[0]
                if map_dict is not None:
                    remstem, remext = os.path.splitext(remotefile)
                    if remext in map_dict.keys():
                        remotefile = remstem + map_dict[remext]
                if not valid_vms_name(remotefile):
                    print('VMS file name is invalid.')
                    if args.quiterror:
                        quit_on_error()
                else:
                    if not send_file(ftp, argslist[0], remotefile, as_binary):
                        print('Failed')
                        if args.quiterror:
                            quit_on_error()

            elif cmdcode == cmds.MPUT:
                try:
                    with open(argslist[0], 'r') as fin:
                        inlist = fin.readlines()
                except Exception as e:
                    print('Failed to read files list from:',argslist[0])
                    if args.execute is not None:
                        quit_ftp(ftp)
                        sys.exit(0)
                    else:
                        if args.quiterror:
                            quit_on_error()

                nsent = nfiles = 0
                for inname in inlist:
                    inname = inname.strip()
                    if len(inname) > 0:
                        if inname[0] != '#':
                            inname = remotefile = inname.lower()
                            nfiles += 1
                            if map_dict is not None:
                                remstem, remext = os.path.splitext(inname)
                                if remext in map_dict.keys():
                                    remotefile = remstem + map_dict[remext]
                                print('... putting:', inname, 'as', remotefile,'(', nfiles, ')')
                            else:
                                print('... putting:', inname, '(', nfiles, ')')
                            if not valid_vms_name(inname):
                                print('... ... VMS file name is invalid. Skipping ...')
                                if args.quiterror:
                                    quit_on_error()
                            else:
                                if not send_file(ftp, inname, remotefile, as_binary):
                                    print('... ... Failed to send file. Skipping ...')
                                    if args.quiterror:
                                        quit_on_error()                                 
                                else:
                                    nsent += 1

                print('Sent', nsent, 'of', nfiles, 'files OK')

            elif cmdcode == cmds.DIR:
                if not dir_list(ftp):
                    print('Failed')
                    if args.quiterror:
                        quit_on_error()
                    
            elif cmdcode == cmds.INFO:
                if not valid_vms_name(argslist[0]):
                    print('VMS file name is invalid.')
                    if args.quiterror:
                        quit_on_error()
                else:
                    if not file_info(ftp, argslist[0]):
                        print('Failed')
                        if args.quiterror:
                            quit_on_error()

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
                    if args.quiterror:
                        quit_on_error()
                else:
                    try:
                        os.chdir(newdir)
                    except Exception as e:
                        print('Cannot change cwd to:', newdir)
                        print('... reason:', e)
                        if args.quiterror:
                            quit_on_error()
                    cwd = os.getcwd()
                    print('Local cwd now:', cwd)

            elif cmdcode == cmds.LS:
                try:
                    filelist = sorted(os.listdir())
                    if len(argslist) == 1:
                        newfilelist = []
                        for filename in filelist:
                            if filename.lower().find(argslist[0].lower()) != -1:
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
                    if args.quiterror:
                        quit_on_error()

            elif cmdcode == cmds.DEL:
                if not valid_vms_name(argslist[0]):
                    print('VMS file name is invalid.')
                    if args.quiterror:
                        quit_on_error()
                else:
                    if not delete_file(ftp, argslist[0]):
                        print('Failed.')
                        if args.quiterror:
                            quit_on_error()

            elif cmdcode == cmds.CD:
                if not change_remote_directory(ftp, argslist[0]):
                    print('Failed.')
                    if args.quiterror:
                        quit_on_error()
                        
            elif cmdcode == cmds.CRED:
                if not create_remote_directory(ftp, argslist[0]):
                    print('Failed.')
                    if args.quiterror:
                        quit_on_error()
                        
            elif cmdcode == cmds.BIN:
                as_binary = True
                print('Binary transfer mode selected.')
                        
            elif cmdcode == cmds.TEXT:
                as_binary = False
                print('Text transfer mode selected.')

            elif cmdcode == cmds.DELD:
                if not valid_vms_name(argslist[0]):
                    print('VMS directory name is invalid.')
                    if args.quiterror:
                        quit_on_error()
                else:
                    if not delete_directory(ftp, argslist[0]+'.dir;1'):
                        print('Failed.')                
                        if args.quiterror:
                            quit_on_error()
                            
            elif cmdcode == cmds.PWD:
                if not show_directory(ftp):
                    print('Failed.')
                    if args.quiterror:
                        quit_on_error()
                        
        if args.execute is not None:
            quit_ftp(ftp)
            sys.exit(0)

if __name__ == '__main__':
    main()
    
