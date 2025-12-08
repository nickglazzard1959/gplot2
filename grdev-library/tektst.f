      SUBROUTINE A12GO( UNIT )
C-------------------------------------------------
C PREPARE TO SEND "8-IN-12" DATA (E.G. ASCII) TO
C LOGICAL UNIT LUN. LUN SHOULD NORMALLY HAVE BEEN
C OPENED TO A TERMINAL USING CALL CONNEC(LUN).
C-------------------------------------------------
      INTEGER UNIT
      INCLUDE 'a12cmn.cmn'
      INTEGER I
      A12UNT = UNIT
C--- SET ALL BUFFER 60 BIT WORDS (OR BYTES) TO 0.
      DO 1 I=1,NA12WD
         BUF(I) = 0
 1    CONTINUE
#ifndef PORTF77      
C--- TO START "8-IN-12" THE FIRST 12 BITS OF THE OUTPUT
C--- MUST BE 0007.
      BUF(1) = SHIFT(O"0007",48)
#endif
      IWP = 1
      ICP = 1
      RETURN
      END
C
      SUBROUTINE A12OUT( VAL8 )
C--------------------------------------------------
C SEND AN 8 BIT VALUE, VAL8, TO THE "8-IN-12" OUTPUT
C STREAM.
C--------------------------------------------------
      INCLUDE 'a12cmn.cmn'
#ifdef UNIX
      INTEGER VAL8
C--- JUST INSERT IN TO THE BUFFER (WHICH IS BYTE SIZED).
C--- NOTE: INT(V,N) IS GFORTRAN SPECIFIC. N IS A BYTES IN RESULT TYPE.
      BUF(IWP) = INT(IAND(VAL8,255),1)
      IWP = IWP + 1
C--- FLUSH THE BUFFER TO OUTPUT WHEN ALL BYTES USED.
      IF( IWP .GT. NA12WD )THEN
         CALL A12FLS
      ENDIF
#endif
#ifdef VMS
      BYTE VAL8
C--- JUST INSERT IN TO THE BUFFER (WHICH IS BYTE SIZED).
      BUF(IWP) = VAL8
      IWP = IWP + 1
C--- FLUSH THE BUFFER TO OUTPUT WHEN ALL BYTES USED.
      IF( IWP .GT. NA12WD )THEN
         CALL A12FLS
      ENDIF
#endif
#ifdef NOSVE
      BOOLEAN VAL8
C--- NOS/VE FORTRAN DOES NOT HAVE A BYTE DATA TYPE.
C--- TRY USING BUFFER OUT ON NOS/VE TOO.
C--- IN THIS CASE, WORDS ARE 64 BIT AND NO SPECIAL
C--- BIT PATTERNS ARE NEEDED IN THE DATA (?).
C--- PACK BYTES INTO WORDS HERE.
      BUF(IWP) = OR(BUF(IWP),SHIFT(VAL8,(8-ICP)*8))
      ICP = ICP + 1
      IF( ICP .GT. 8 )THEN
         ICP = 1
         IWP = IWP + 1
C--- FLUSH THE BUFFER TO OUTPUT WHEN ALL WORDS USED.
         IF( IWP .GT. NA12WD )THEN
            CALL A12FLS
         ENDIF
      ENDIF
#endif
#ifndef PORTF77
      BOOLEAN VAL8
      BOOLEAN VAL12
C--- THE 12 BITS MUST BEGIN WITH 0100 IN TOP 4 BITS
C--- INSERT 12 BITS IN RIGHT CHUNK OF 60 BIT WORD.
C--- ICP COUNTS 12 BIT CHUNKS IN WORD, IWP WORDS.
      VAL12 = OR(O"4000",VAL8)
      BUF(IWP) = OR(BUF(IWP),SHIFT(VAL12,(4-ICP)*12))
      ICP = ICP + 1
      IF( ICP .GT. 4 )THEN
         ICP = 0
         IWP = IWP + 1
C--- FLUSH THE BUFFER TO OUTPUT WHEN ALL WORDS USED.
         IF( IWP .GT. NA12WD )THEN
            CALL A12FLS
         ENDIF
      ENDIF
#endif
      RETURN
      END
C
#ifdef UNIX
      SUBROUTINE A12OUTB( VAL8 )
C--------------------------------------------------
C SEND AN 8 BIT VALUE, VAL8, TO THE "8-IN-12" OUTPUT
C STREAM.
C--------------------------------------------------
      BYTE VAL8
      INCLUDE 'a12cmn.cmn'
C--- JUST INSERT IN TO THE BUFFER (WHICH IS BYTE SIZED).
      BUF(IWP) = VAL8
      IWP = IWP + 1
C--- FLUSH THE BUFFER TO OUTPUT WHEN ALL BYTES USED.
      IF( IWP .GT. NA12WD )THEN
         CALL A12FLS
      ENDIF
      RETURN
      END
C
#endif
      SUBROUTINE A12SEQ( VAL8S, NVAL8 )
C---------------------------------------------------
C SEND A SEQUENCE OF NVAL8 8 BIT VALUES, VAL8S.
C---------------------------------------------------
      INTEGER NVAL8
      BOOLEAN VAL8S(NVAL8)
C
      DO 1 I=1,NVAL8
#ifdef VMS
         CALL A12OUT(VAL8S(I))
#else
         CALL A12OUT(INT(VAL8S(I)))
#endif
  1   CONTINUE
      RETURN
      END
C
      SUBROUTINE A12FLS
C---------------------------------------------------
C FLUSH THE "8-IN-12" OUTPUT STREAM.
C---------------------------------------------------
      INCLUDE 'a12cmn.cmn'
C--- IF THERE IS ANY DATA IN THE BUFFER, SEND IT.
#ifdef UNIX
      IF(IWP.GT.1)THEN
         ITOP = MIN(IWP,NA12WD)
         CALL SNDBYT(BUF,ITOP)
      ENDIF
C--- RE-INITIALIZE THE BUFFER.
      IWP = 1
      DO 1 I=1,NA12WD
         BUF(I) = 0
  1   CONTINUE      
#endif
#ifdef VMS
      IF(IWP.GT.1)THEN
         ITOP = MIN(IWP,NA12WD)
         CALL SNDBYT(BUF,ITOP)
      ENDIF
C--- RE-INITIALIZE THE BUFFER.
      IWP = 1
      DO 1 I=1,NA12WD
         BUF(I) = 0
  1   CONTINUE      
#endif
#ifdef NOSVE
      IF((IWP.EQ.1 .AND. ICP.GT.1).OR.IWP.GT.1)THEN
         ITOP = MIN(IWP,NA12WD)
         BUFFER OUT (A12UNT,1) (BUF(1),BUF(ITOP))
         IF( UNIT(A12UNT) .GE. 0 )THEN
            PRINT *,'BUFFER OUT FAILED.'
         ENDIF
      ENDIF
C--- RE-INITIALIZE THE BUFFER.
      ICP = 1
      IWP = 1
      DO 1 I=1,NA12WD
         BUF(I) = 0
  1   CONTINUE
#endif      
#ifndef PORTF77
      IF((IWP.EQ.1 .AND. ICP.GT.1).OR.IWP.GT.1)THEN
         ITOP = MIN(IWP,NA12WD)
         BUFFER OUT (A12UNT,1) (BUF(1),BUF(ITOP))
      ENDIF
C--- RE-INITIALIZE THE BUFFER.
      ICP = 1
      IWP = 1
      DO 1 I=1,NA12WD
         BUF(I) = 0
  1   CONTINUE
      BUF(1) = SHIFT(O"0007",48)
#endif      
      RETURN
      END
C
      SUBROUTINE GTONUM( NUM, DIGITS )
C---------------------------------------------------
C SEND INTEGER NUM WITH DIGITS CHARACTERS FILLED
C WITH LEADING ZEROS.
C---------------------------------------------------
      INTEGER NUM, DIGITS
      INCLUDE 'gtcmn.cmn'
      INTEGER POWDIV, CNUM, DIGIT
      IF( NUM .LT. 0)NUM = 0
      POWDIV = 1
      DO 1 I=1,DIGITS-1
         POWDIV = POWDIV * 10
  1   CONTINUE
      CNUM = NUM
      DO 2 I=1,DIGITS
         DIGIT = CNUM / POWDIV
         CALL A12OUT(DIGIT+48)
         CNUM = CNUM - DIGIT * POWDIV
         POWDIV = POWDIV / 10
  2   CONTINUE
      RETURN
      END
C
      SUBROUTINE GTINI( LUN )
C---------------------------------------------------
C INITIALIZE GTERM GRAPHICS.
C---------------------------------------------------
      INTEGER LUN
      INCLUDE 'gtcmn.cmn'
C      PRINT 999
C 999  FORMAT(1X,'GTINI')
      GTLUN = LUN
      CALL A12GO( GTLUN )
      RETURN
      END
C
      SUBROUTINE GTCLR
C---------------------------------------------------
C GTERM CLEAR COMMAND.
C---------------------------------------------------
      INCLUDE 'gtcmn.cmn'
      BOOLEAN SCLR(4)
      DATA SCLR /27,91,48,122/
C      PRINT 999
C 999  FORMAT(1X,'GTCLR')
      CALL A12SEQ( SCLR, 4 )
      CALL A12FLS
      RETURN
      END
C
      SUBROUTINE GTCOL( R, G, B )
C---------------------------------------------------
C GTERM SET COLOUR COMMAND.
C R, G, B IN RANGE 0 TO 1
C---------------------------------------------------
      REAL R, G, B
      INCLUDE 'gtcmn.cmn'
      REAL LR, LG, LB
      INTEGER IR, IG, IB
      BOOLEAN SCOL(3)
      DATA SCOL /27,91,49/
C      PRINT 999,R,G,B
C 999  FORMAT(1X,'RGB=',F12.6,F12.6,F12.6)
      CALL A12SEQ( SCOL, 3 )
      LR = MAX( 0.0, MIN( R, 1.0 ) )
      LG = MAX( 0.0, MIN( G, 1.0 ) )
      LB = MAX( 0.0, MIN( B, 1.0 ) )
      IR = NINT( 999.0 * LR )
      IG = NINT( 999.0 * LG )
      IB = NINT( 999.0 * LB )
      CALL GTONUM( IR, 3 )
      CALL GTONUM( IG, 3 )
      CALL GTONUM( IB, 3 )
      CALL A12OUT( 122 )
      RETURN
      END
C
      SUBROUTINE GTFILL
C---------------------------------------------------
C GTERM FILL WINDOW WITH CURRENT COLOUR COMMAND.
C---------------------------------------------------
      INCLUDE 'gtcmn.cmn'
      BOOLEAN SFIL(4)
      DATA SFIL /27,91,50,122/
C      PRINT 999
C 999  FORMAT(1X,'GTFILL')
      CALL A12SEQ( SFIL, 4 )
      RETURN
      END
C
      SUBROUTINE GTPEN( X, Y, IPEN )
C---------------------------------------------------
C MOVE (IPEN=0) OR DRAW (IPEN=1) TO (X,Y)
C---------------------------------------------------
      REAL X, Y
      INCLUDE 'gtcmn.cmn'
      REAL CX, CY
      INTEGER IX, IY
      BOOLEAN SMOV(3)
      REAL UNSCALE
      PARAMETER( UNSCALE=9999.0/7500.0 )
      DATA SMOV /27,91,51/
      SMOV(3) = 51
      IF(IPEN.EQ.1)SMOV(3) = 52
      CALL A12SEQ( SMOV, 3 )
      CX = MAX(0.0,MIN(X,9999.0))
      CY = MAX(0.0,MIN(Y*UNSCALE,9999.0))
      IX = NINT(CX)
      IY = NINT(CY)
C      PRINT 999,IX,IY,IPEN
C 999  FORMAT(1X,'IX,IY=',I6,',',I6,',','IPEN=',I3)
      CALL GTONUM(IX,4)
      CALL GTONUM(IY,4)
      CALL A12OUT(122)
      RETURN
      END
C
      SUBROUTINE GTMOVE( X, Y )
C---------------------------------------------------
C MOVE TO (X,Y)
C---------------------------------------------------
      REAL X, Y
      INCLUDE 'gtcmn.cmn'
      CALL GTPEN(X,Y,0)
      RETURN
      END
C
      SUBROUTINE GTDRAW( X, Y )
C---------------------------------------------------
C DRAW TO (X,Y)
C---------------------------------------------------
      REAL X, Y
      INCLUDE 'gtcmn.cmn'
      CALL GTPEN(X,Y,1)
      RETURN
      END
C
      SUBROUTINE GTFLUSH
C---------------------------------------------------
C GTERM FLUSH COMMANDS (DRAW THEM NOW).
C---------------------------------------------------
      INCLUDE 'gtcmn.cmn'
      BOOLEAN SFIL(4)
      DATA SFIL /27,91,53,122/
      CALL A12SEQ( SFIL, 4 )
      CALL A12FLS
      RETURN
      END
C
      SUBROUTINE GTWIDTH( WIDTH )
C---------------------------------------------------
C GTERM SET LINE WIDTH (0:10)
C---------------------------------------------------
      REAL WIDTH
      INCLUDE 'gtcmn.cmn'
      BOOLEAN SWID(3)
      DATA SWID /27,91,54/
      CALL A12SEQ( SWID, 3 )
      IW = NINT(99.0*MAX(0.0,MIN(10.0,WIDTH)))
C      PRINT 999,IW
C 999  FORMAT(1X,'IW=',I6)
      CALL GTONUM( IW, 3 )
      CALL A12OUT(122)
      RETURN
      END
C
      SUBROUTINE GTWAIT
C----------------------------------------------------
C PROMPT AND WAIT FOR RETURN KEY.
C FOR "UNIX-LIKE" SYSTEMS, NEEDS TO READ FROM STDIN,
C WHICH IS ALWAYS ASSOCIATED WITH LUN 5.
C----------------------------------------------------
      INCLUDE 'gtcmn.cmn'
      CHARACTER*1 C
      INTEGER NPROMPT
#ifdef NOSVE
      PARAMETER( NPROMPT=8 )
      BOOLEAN PROMPT(NPROMPT)
      DATA PROMPT /13,10,32,71,111,63,62,7/
#else
      PARAMETER( NPROMPT=5 )
      BOOLEAN PROMPT(NPROMPT)
      DATA PROMPT /71,111,63,62,7/
#endif
      CALL GTFLUSH
      CALL A12SEQ( PROMPT, NPROMPT )
      CALL A12FLS
 8    CONTINUE
#ifndef PORTF77
      READ(7,101,END=9,ERR=9)C
#endif
#ifdef UNIX
      READ(5,101,END=9,ERR=9)C
#endif
#ifdef NOSVE
      READ(*,101,END=9,ERR=9)C
#endif
#ifdef VMS
      READ(*,101,END=9,ERR=9)C
#endif
 101  FORMAT(A1)
      IF( C(1:1) .EQ. 'N' )GOTO 8
      IF( C(1:1) .EQ. 'Q' )THEN
#ifdef NOSVE
         PRINT *,'EXIT REQUESTED'
#endif
         STOP 'EXIT REQUESTED'
      ENDIF
 9    CONTINUE
#ifndef PORTF77
C--- ON NOS, AN EMPTY LINE WILL BE TREATED AS EOF. IF INTERACTIVE
C--- JOB, TRAP THAT AND TREAT IT AS 'Y' (CONTINUE).
      IF( (EOF(7) .NE. 0) .AND. (IGETOT() .EQ. 3) )GOTO 1
      STOP 'UNEXPECTED EOF.'
 1    CONTINUE
#endif
      CALL GTCLR
      RETURN
      END
C
      SUBROUTINE TEKINI( LUN )
C---------------------------------------------------
C PREPARE TO OUTPUT TEKTRONIX 4010 STYLE GRAPHICS TO
C LOGICAL UNIT LUN. THIS SHOULD BE ASSOCIATED WITH THE
C TERMINAL (BY A PREVIOUS CALL CONNEC(LUN) ON NOS).
C---------------------------------------------------
      INTEGER LUN
      INCLUDE 'tekcmn.cmn'
      TKLUN = LUN
C--- PREPARE TO REMEMBER THE LAST PEN POSITION.
      LASTXL = -1
      LASTYL = -1
      LASTXH = -1
      LASTYH = -1
C--- TRACK GRAPHICS/TEXT MODE AND IF ANYTHING DRAWN.
      INGRAF = 0
      DRAWN = 0
C--- SET UP FOR "8-IN-12" OUTPUT TO LUN.
      CALL A12GO( LUN )
C--- SWITCH TO TEXT MODE WITH A SPECIFIC CHAR SIZE.
      CALL TEKTEXT
      RETURN
      END
C
      SUBROUTINE TEKDRAW( IX, IY, IPEN )
C----------------------------------------------------
C MOVE THE "PEN" TO (IX,IY) [0:1023] WITH THE PEN UP
C (IPEN=0) OR DOWN (IPEN=1).
C----------------------------------------------------
      INCLUDE 'tekcmn.cmn'
C--- DO NOTHING IF NOT IN GRAPHICS MODE.
      IF( INGRAF .EQ. 0 )RETURN
C--- IF MOVE, SEND ASCII GS.
      IF( IPEN .EQ. 0 )THEN
         CALL A12OUT( 29 )
      ELSE
         DRAWN = DRAWN + 1
      ENDIF
C--- SPLIT COORDS IN TO UPPER AND LOWER 5 BITS.
      IXH = AND(SHIFT(IX,-5),O"37")
      IXL = AND(IX,O"37")
      IYH = AND(SHIFT(IY,-5),O"37")
      IYL = AND(IY,O"37")
C--- SEND HIGH Y IF THAT HAS CHANGED.
      IF( IYH .NE. LASTYH )THEN
         CALL A12OUT( IYH+32 )
         LASTYH = IYH
      ENDIF
C--- SEND LOW Y IF THAT OR HIGH X HAS CHANGED.
      IF( (IYL .NE. LASTYL).OR.(IXH .NE. LASTXH) )THEN
         CALL A12OUT( IYL+96 )
         LASTYL = IYL
      ENDIF
C--- SEND HIGH X IF THAT HAS CHANGED.
      IF( IXH .NE. LASTXH )THEN
         CALL A12OUT( IXH+32 )
         LASTXH = IXH
      ENDIF
C--- ALWAYS SEND LOW X.
      CALL A12OUT( IXL+64 )
      RETURN
      END
C
      SUBROUTINE TEKGRAF
C----------------------------------------------------
C ENTER GRAPHICS MODE.
C----------------------------------------------------
      INCLUDE 'tekcmn.cmn'
      CALL A12OUT(29)
      INGRAF = 1
      DRAWN = 0
      RETURN
      END
C
      SUBROUTINE TEKTEXT
C----------------------------------------------------
C ENTER TEXT MODE.
C----------------------------------------------------
      INCLUDE 'tekcmn.cmn'
      CALL A12OUT(27)
      CALL A12OUT(57)      
      CALL A12OUT(31)
      INGRAF = 0
      DRAWN = 0
      RETURN
      END
C
      SUBROUTINE TEKCLR
C----------------------------------------------------
C CLEAR THE SCREEN.
C----------------------------------------------------
      INCLUDE 'tekcmn.cmn'
      CALL A12OUT(27)
      CALL A12OUT(12)
      CALL A12FLS
      DRAWN = 0
      RETURN
      END
C
      SUBROUTINE TEKWAIT
C----------------------------------------------------
C PROMPT AND WAIT FOR RETURN KEY.
C FOR "UNIX-LIKE" SYSTEMS, NEEDS TO READ FROM STDIN,
C WHICH IS ALWAYS ASSOCIATED WITH LUN 5.
C----------------------------------------------------
      INCLUDE 'tekcmn.cmn'
      CHARACTER*1 C
      INTEGER NPROMPT
#ifdef NOSVE
      PARAMETER( NPROMPT=8 )
      BOOLEAN PROMPT(NPROMPT)
      DATA PROMPT /13,10,32,71,111,63,62,7/
#else
      PARAMETER( NPROMPT=5 )
      BOOLEAN PROMPT(NPROMPT)
      DATA PROMPT /71,111,63,62,7/
#endif
      IF( INGRAF .EQ. 0 )CALL TEKGRAF
      CALL TEKDRAW( 3, 747, 0 )
      CALL TEKTEXT
      CALL A12SEQ( PROMPT, NPROMPT )
      CALL A12FLS
 8    CONTINUE
#ifndef PORTF77
      READ(7,101,END=9,ERR=9)C
#endif
#ifdef UNIX
      READ(5,101,END=9,ERR=9)C
#endif
#ifdef NOSVE
      READ(*,101,END=9,ERR=9)C
#endif
#ifdef VMS
      READ(*,101,END=9,ERR=9)C
#endif
 101  FORMAT(A1)
      IF( C(1:1) .EQ. 'N' )GOTO 8
      IF( C(1:1) .EQ. 'Q' )THEN
#ifdef NOSVE
         PRINT *,'EXIT REQUESTED'
#endif
         STOP 'EXIT REQUESTED'
      ENDIF
 9    CONTINUE
#ifndef PORTF77
C--- ON NOS, AN EMPTY LINE WILL BE TREATED AS EOF. IF INTERACTIVE
C--- JOB, TRAP THAT AND TREAT IT AS 'Y' (CONTINUE).
      IF( (EOF(7) .NE. 0) .AND. (IGETOT() .EQ. 3) )GOTO 1
      STOP 'UNEXPECTED EOF.'
 1    CONTINUE
#endif      
      CALL TEKCLR
      RETURN
      END
