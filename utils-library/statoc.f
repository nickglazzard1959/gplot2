      INTEGER FUNCTION STATOC( AIN, COUT )
C-------------------------------------------------------(STRING)--------
C CONVERT AN ASCII CODE STRING TO CHARACTERS (6/12 FORMAT).
C RETURN THE NUMBER OF CHARACTERS IN COUT (0 TO KCMAX)
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER AIN(KSTDIM)
      CHARACTER*(KCMAX) COUT
C----
      INTEGER I, NOUT, IASCII
      BOOLEAN B612DC, ESCODE, CHCODE
      CHARACTER*1 ESCHAR
C
      CHARACTER*1 DSPCHR(0:63)
      BOOLEAN A2D612B(0:127)
C
C---- 6 BIT DISPLAY CODE CODES TO DISPLAY CODE CHARACTERS.
      DATA DSPCHR/':','A','B','C','D','E','F','G','H','I','J','K','L',
     +'M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','0','1',
     +'2','3','4','5','6','7','8','9','+','-','*','/','(',')','$','=',
     +' ',',','.','#','[',']','%','"','_','!','&','''','?','<','>','@',
     +'\','^',';'/
C
C---- ASCII CODES TO 6/12 DISPLAY CODE CODES
      DATA A2D612B/O"7640",O"7641",O"7642",O"7643",O"7644",O"7645",
     +O"7646",O"7647",O"7650",O"7651",O"7652",O"7653",O"7654",O"7655",
     +O"7656",O"7657",O"7660",O"7661",O"7662",O"7663",O"7664",O"7665",
     +O"7666",O"7667",O"7670",O"7671",O"7672",O"7673",O"7674",O"7675",
     +O"7676",O"7677",O"55",O"66",O"64",O"60",O"53",O"63",O"67",O"70",
     +O"51",O"52",O"47",O"45",O"56",O"46",O"57",O"50",O"33",O"34",O"35",
     +O"36",O"37",O"40",O"41",O"42",O"43",O"44",O"7404",O"77",O"72",
     +O"54",O"73",O"71",O"7401",O"1",O"2",O"3",O"4",O"5",O"6",O"7",
     +O"10",O"11",O"12",O"13",O"14",O"15",O"16",O"17",O"20",O"21",O"22",
     +O"23",O"24",O"25",O"26",O"27",O"30",O"31",O"32",O"61",O"75",O"62",
     +O"7402",O"65",O"7407",O"7601",O"7602",O"7603",O"7604",O"7605",
     +O"7606",O"7607",O"7610",O"7611",O"7612",O"7613",O"7614",O"7615",
     +O"7616",O"7617",O"7620",O"7621",O"7622",O"7623",O"7624",O"7625",
     +O"7626",O"7627",O"7630",O"7631",O"7632",O"7633",O"7634",O"7635",
     +O"7636",O"7637"/
C
C---- LOOP OVER THE POSSIBLE INPUT ASCII STRING CODES.
      NOUT = 0
      DO 1 I=1,KSTMAX
C
C---- GET THE ASCII CODE. IF KSTEND, WE ARE DONE.
         IASCII = AIN(I)
         IF( IASCII .EQ. KSTEND )GOTO 2
C
C---- GET THE DISPLAY CODE CODE CORRESPONDING TO THE ASCII CODE. THIS
C---- MAY BE 6 BIT OR 12 BIT, WHERE THE TOP 6 BITS ARE DISPLAY CODE
C---- FOR AN ESCAPE CHARACTER AND THE BOTTOM 6 BITS ANOTHER DISPLAY
C---- CODE CODE.
         B612DC = A2D612B(IASCII)
         ESCODE = AND( B612DC, O"7700" )
         CHCODE = AND( B612DC, O"77" )
C---- CONVERT DISPLAY CODE CODE TO DISPLAY CODE CHARACTER(S).
         IF( ESCODE .NE. 0 )THEN
            IF( ESCODE .EQ. O"7400" )THEN
               ESCHAR = '@'
            ELSE
               ESCHAR = '^'
            ENDIF
            NOUT = NOUT + 1
            COUT(NOUT:NOUT) = ESCHAR
         ENDIF
         NOUT = NOUT + 1
         COUT(NOUT:NOUT) = DSPCHR(CHCODE)
 1    CONTINUE
 2    CONTINUE
C---- RETURN THE NUMBER OF CHARACTERS PLACED IN COUT.
      STATOC = NOUT
      RETURN
      END
