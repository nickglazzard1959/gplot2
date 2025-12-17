#!/usr/bin/env python
"""
awdecode.py - Decode NOS MODVAL AW bits.
"""

def main():
    while True:
        octaw = int(input('Enter AW octal value (0:exit, -1:list): '),8)
        if octaw == 0:
            return
        if octaw & 1:       #0
            print('CPWC - User can change batch and interactive passwords')
        if octaw & (1<<1):  #1
            print('CTPC - User can use the access subsystem commands')
        if octaw & (1<<2):  #2
            print('CLPF - User can create direct access permanent files')
        if octaw & (1<<3):  #3
            print('CSPF - User can create indirect access permanent files')
        if octaw & (1<<4):  #4
            print('CSOJ - User can have system origin capability')
        if octaw & (1<<5):  #5
            print('CASF - User can access system file with COMMON,SYSTEM.')
        if octaw & (1<<6):  #6
            print('CAND - User can request non-allocatable devices')
        if octaw & (1<<7):  #7
            print('CCNR - User need not enter charge or project number')
        if octaw & (1<<8):  #8
            print('CSRP - User can request non-allocatable devices')
        if octaw & (1<<9):  #9
            print('CSTP - User has special TAF privileges')
        if octaw & (1<<10): #10
            print('CTIM - User is not logged off because of timeout')
        if octaw & (1<<11): #11
            print('CUCP - User can access system control point facility')
        if octaw & (1<<12): #12
            print('CSAP - User has special accounting privileges')
        if octaw & (1<<13): #13
            print('CBIO - User has batch i/o subsystem privileges')
        if octaw & (1<<14): #14
            print('CPRT - User can preserve extended memory')
        if octaw & (1<<15): #15
            print('CPLK - User can transfer permanent files between hosts')
        if octaw & (1<<16): #16
            print('CQLK - User can transfer queued files between hosts')
        if octaw & (1<<17): #17
            print('CUST - User can specify LID for job or ROUTE')
        if octaw & (1<<18): #18
            print('CNVE - User can access NOS/VE')
        if octaw & (1<<19): #19
            print('CMNT - User has maintenance privileges')
        if octaw & (1<<20): #20
            print('CNOP - User can control NPUs')
        if octaw & (1<<21): #21
            print('CSAF - User can specify an alternative family on secondary USER commands')
        if octaw & (1<<22): #22
            print('CNRD - User can specify non-default charge and project numbers')
        if octaw & (1<<23): #23
            print('COPR - User can specify a password without randomization')
        if octaw & (1<<24): #24
            print('CLTD - User can specify preferred file residence on SAVE, DEFINE and CHANGE')
        if octaw & (1<<25): #25
            print('COPI - User need not enter personal ID on interactive login')
        if octaw & (1<<26): #26
            print('CACA - Allow concurrent interactive access (multiple simultaneous logins)')
        if octaw & (1<<27): #27
            print('CPAM - User can read system status information from non-SYOT jobs')
        if octaw & (1<<28): #28
            print('CSAU - User can specify alternate user name on secondary USER commands')
        if octaw & (1<<29): #29
            print('CRAU - User can specify alternate user name in USER commands routed with input disposition')
        if octaw & (1<<30): #30
            print('CRAF - User can specify alternate family in USER commands routed with input disposition')

if __name__ == '__main__':
    main()
