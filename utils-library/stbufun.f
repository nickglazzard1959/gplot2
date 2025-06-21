      INTEGER FUNCTION STBUFOP(FNO)
C-----------------------------------------------------(STRING)----------
C OPEN A STRING BUFFER FILE ON LUN FNO.
C THESE BUFFERS ALLOW LARGE NUMBERS OF STRINGS TO BE HELD FOR DIRECT
C ACCESS WITHOUT USING TOO MUCH MEMORY.
C RETURN 0 IF OK.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER FNO
C----
      INTEGER IOS
C
      OPEN(UNIT=FNO,STATUS='SCRATCH',ACCESS='DIRECT',
     +     FORM='UNFORMATTED',RECL=KSTDIM,ERR=1,IOSTAT=IOS)
      STBUFOP = 0
      RETURN
 1    CONTINUE
      STBUFOP = IOS
      RETURN
      END
C
      INTEGER FUNCTION STBUFWR(AIN,FNO,RECNO,MAXREC)
C-----------------------------------------------------(STRING)----------
C WRITE AIN TO STRING BUFFER FNO AS LINE NUMBER (RECORD) RECNO.
C RETURN THE HIGH WATER MARK OF LINES WRITTEN IN MAXREC.
C IF RECNO .LT. 0, ADD A LINE TO THE END OF THE BUFFER, BUMP MAXREC.
C MAXREC SHOULD BE INITIALIZED TO ZERO IF USED IN THIS WAY.
C RETURN 0 IF OK.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER AIN(KSTDIM), FNO, RECNO, MAXREC
C----
      INTEGER IOS
C
      IF( RECNO .LE. 0 )THEN
         MAXREC = MAXREC + 1
         WRITE(UNIT=FNO,IOSTAT=IOS,ERR=1,REC=MAXREC)AIN
      ELSE
         WRITE(UNIT=FNO,IOSTAT=IOS,ERR=1,REC=RECNO)AIN
         MAXREC = MAX(MAXREC,RECNO)
      ENDIF
      STBUFWR = 0
      RETURN
 1    CONTINUE
      STBUFWR = IOS
      RETURN
      END
C
      INTEGER FUNCTION STBUFRD(FNO,RECNO,MAXREC,AOUT)
C-----------------------------------------------------(STRING)----------
C READ LINE RECNO FROM STRING BUFFER FNO. THE NUMBER OF LINES IN THE
C BUFFER MUST BE SUPPLIED AS MAXREC. RETURN THE STRING IN AOUT.
C RETURN 0 IF OK, -1 IF RECNO INVALID, IO STATUS OTHERWISE.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER FNO, RECNO, AOUT(KSTDIM), MAXREC
C----
      INTEGER IOS
C
      IF( RECNO .GT. MAXREC )THEN
         STBUFRD = -1
         RETURN
      ENDIF
      READ(UNIT=FNO,IOSTAT=IOS,ERR=1,REC=RECNO)AOUT
      STBUFRD = 0
      RETURN
 1    CONTINUE
      STBUFRD = IOS
      RETURN
      END
C
      INTEGER FUNCTION STBUFEN(FNO)
C--------------------------------------------------------(STRING)-------
C FINISH USING THE STRING BUFFER FNO.
C RETURN 0 IF OK.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER FNO
C----
      INTEGER IOS
      CLOSE(UNIT=FNO,IOSTAT=IOS,ERR=1,STATUS='DELETE')
      STBUFEN = 0
      RETURN
 1    CONTINUE
      STBUFEN = IOS
      RETURN
      END
