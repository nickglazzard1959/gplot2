      SUBROUTINE DFX202(NCHAR,L1,L2)
C    PLOT SINGLE SINGLE CHARACTER NCHAR FROM CURRENT ALPHABET
C    AT CURRENT POSITION
C    NOTE - L1,L2 PASSED SO ROUTINE CAN HANDLE ALL ALPHABETS
C------------------------------------------------------
C
C            CHARACTER DATA
C            THIS IS MODIFIED AND EXTENDED HERSHEY SPECIFICATION
C
C            COORDINATE VALUES WITH ABSOLUTE VALUE 100 OR GREATER
C            ARE SPECIAL FLAGS -
C
C            -100,0     --  PEN UP TO NEXT COORDINATE PAIR
C            -101,Z     --  PEN UP, POINT AT NEXT COORDINATE PAIR,
C                           PEN UP - Z/100 (>0) SIZE OF POINT IN
C                           GRID UNITS, Z=0 DEVICE SMALLEST POINT
C            -102,Y     --  SUBSEQUENT Y-VALUES OFFSET BY Y/100 UNITS
C            -103,X     --  SUBSEQUENT X-VALUES OFFSET BY X/100 UNITS
C            -104,Z     --  Z=0 TERMINATES X AND Y OFFSETS
C                           Z=+1 TERMINTAES X OFFSETS (EQUIV -103,0)
C                           Z=-1 TERMINATES Y OFFSETS (EQUIV -102,0)
C            -105,N     --  FOLLOWING COORDINATES RELATIVE TO
C                           PROPORTIONAL CHARACTER
C                           I.E. ACTUAL LEFT/RIGHT EXTENT, ACTUAL
C                           HEIGHT MAX/MIN
C                           N ANY DUO SUM OF +1 HEIGHT MAX
C                                            -1 HEIGHT MIN
C                                            +3 RIGHT EXTENT
C                                            -3 LEFT EXTENT
C
C                                    -2    +1     +4
C                                      -----------
C                                      :    :    :
C                                      :    :    :
C                                    -3:----0----:+3
C                                      :    :    :
C                                      :    :    :
C                                      -----------
C                                    -4    -1     +2
C
C            -106,N     --  FOLLOWING COORDINATES REVERT TO ABSOLUTE
C                           (MUST TERMINATE RELATIVE SEQUENCES -
C                            EXCEPT ACCENTS - IN THIS WAY)
C
C
C             ACCENTS ARE DEFINED FROM BYTE1 TO BYTEN:
C             BYTE1,BYTE2 ALWAYS A SPECIAL PAIR
C
C             BYTE1 = 0  -- COORDINATES ABSOLUTE (ABOUT 0,0) - BUT
C                           NO MEANS OF DEALING WITH HEIGHTS IN
C                           INDEX ENTRY
C                   =+1  -- COORDINATES RELATIVE TO ACTUAL HEIGHT MAX
C                   =-1  -- COORDINATES RELATIVE TO ACTUAL HEIGHT MIN
C
C             BYTE2 = 0  -- BYTES 3-N HOLD COMMON DATA
C                   > 0  -- NUMBER OF LOWER CASE ONLY DATA COORDINATE
C                           PAIRS
C                           I.E. IF LOWER CASE DO BYTE3-BYTE(2+BYTE2+2)
C                                IF UPPER CASE DO BYTE(2+BYTE2+3)-BYTEN
C
C   BYTE2>0: BYTE1:BYTE2:<LOWER CASE>:<UPPER CASE>
C   BYTE2=0: BYTE1:BYTE2:<UPPER/LOWER CASE - COMMON - DATA>
C
C            N.B. ACCENTS ARE PERFORMED CENTRED ON PREVIOUS CHARACTER
C
C                 INDEX ENTRY FOR ACCENT HAS SPECIAL SIGNIFICANCE:
C                 HEIGHT MAX/MIN  NON-ZERO - BOTH RELATIVE TO CHARACTER
C                                    IF ABS 100 OR MORE, OFFSET BY 100,
C                                    ABSOLUTE IF <100,
C                                 BOTH ZERO - WITHIN CHARCTER SPACE
C------------------------------------------------------
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc01.cmn'
      INCLUDE 'dfxc01s.cmn'
C    MAXBET IS NUMBER OF SIMULTANEOUSLY IN-CORE ALPHABETS
C    DIMFILM CURRENTLY SUPPORTS MAXBET UP TO A MAXIMUM OF 3
      PARAMETER (MAXBET=3)
      INCLUDE 'dfxc20.cmn'
      INCLUDE 'dfxc20s.cmn'
      INCLUDE 'dfxc22.cmn'
      LOGICAL ON,PT
      INTEGER XYDATA(2,DATBUF)
      EQUIVALENCE (ICDATA,XYDATA)
      INTEGER LXM(9),RXM(9),LYM(9),UYM(9)
      SAVE LXM, RXM, LYM, UYM
      DATA LXM/1,1,1,6*0/
      DATA RXM/6*0,1,1,1/
      DATA LYM/1,0,0,1,0,0,1,0,0/
      DATA UYM/0,0,1,0,0,1,0,0,1/
      ON = .FALSE.
C    START CHARACTER WITH BEAM OFF
      XOFF = 0.0
      YOFF = 0.0
      PT = .FALSE.
      IF (.NOT.NMONSP) NCOX = -LXTENT(NCHAR,NBET)
      IF (BACK) THEN
                     CXS = CX
                     CX = CX - RXT - NCOX*CXF
                     RXTS = RXT
      ENDIF
      NCOXA = NCOX
      NCOYA = NCOY
      COXA = FLOAT(NCOXA)
      COYA = FLOAT(NCOYA)
      IF (LENGTH) GO TO 999
   99 L1L2 = L2 - L1 + 1
      NN = MIN(L1L2,2*DATBUF)
      CALL DFXM20(NBET,L1,L1+NN-1)
C    GO UNPACK BUFFER OF CHARACTER DATA
      NI = NN/2
   10 DO 1 I=1,NI
      IX = XYDATA(1,I)
      IY = XYDATA(2,I)
      IF (ABS(IX).GE.100) GO TO 2
C    JUMP IF CONTROL FUNCTION
      Y = (COYA - FLOAT(IY)+YOFF)*CYF
      X = (COXA + FLOAT(IX)+XOFF)*CXF
      IF (NITAL) X = X + NUITAL*Y
      X = CX + X
      Y = CY + Y
      IF (PT) GO TO 4
      IF (.NOT.ON) CALL DFX110(X,Y)
      IF (ON) CALL DFX106(X,Y)
      ON = .TRUE.
C    BEAM OFF AFFECTS 1 COORDINATE PAIR ONLY
      GO TO 1
C    POINT HERE
    4 PT = .FALSE.
      IF (IZ.EQ.0) GO TO 5
C    DRAW OWN POINT
      Z = FLOAT(IZ)/100.
      DX = Z*CXF
      DY = Z*CYF
      CALL DFX110(X+DX,Y)
      CALL DFX106(X,Y-DY)
      CALL DFX106(X-DX,Y)
      CALL DFX106(X,Y+DY)
      CALL DFX106(X+DX,Y)
      GO TO 1
C   DEVICE MINIMUM POINT
    5 NS = NPTTYP
      NPTTYP = 1
      CALL DFX129(X,Y)
      NPTTYP = NS
      GO TO 1
C    CONTROL FUNCTIONS HERE
    2 IF (IX.NE.-100) GO TO 3
C    BEAM OFF IS SPECIAL CASE
      ON = .FALSE.
      GO TO 1
    3 KDO = -IX - 100
      GO TO (101,102,103,104,105),KDO
C    POINT AT NEXT COORDINATE
  101 PT = .TRUE.
      ON = .FALSE.
      IZ = IY
      GO TO 1
C    Y-OFFSETS
  102 YOFF = FLOAT(IY)/100.
      GO TO 1
C    X-OFFSETS
  103 XOFF = FLOAT(IX)/100.
      GO TO 1
C    TERMINATE OFFSETS
  104 IF (IY.EQ.0.OR.IY.EQ.1) XOFF = 0.0
      IF (IY.EQ.0.OR.IY.EQ.2) YOFF = 0.0
      GO TO 1
C    SET RELATIVE COORDS (ONLY FOR ACCENTS, WHEN PREVIOUS
C                         CHARACTER USED)
  105 IY = IY + 5
      COXA = LXTENT(NCHAR,NBET)*LXM(IY) + RXTENT(NCHAR,NBET)*RXM(IY)
      IF ((LXM(IY)+RXM(IY)).NE.0) COXA = COXA - CXOFF
      COYA = LYTENT(NCHAR,NBET)*LYM(IY) + UYTENT(NCHAR,NBET)*UYM(IY)
      GO TO 1
    1 CONTINUE
C    NOW SEE IF WHOLE CHARACTER DONE
      IF (NN.EQ.L1L2) GO TO 999
      L1 = L1 + NN
C    GO DO NEXT BUFFER
      GO TO 99
  999 IF (.NOT.NMONSP) THEN
                       IX = RXTENT(NCHAR,NBET)
                       ELSE
                       IX = MHWRRU(NBET)
      ENDIF
C    RIGHT EXTENT = LEFT IS ZERO WIDTH CHARACTER I.E. NON-SPACING
C    (MAKES GENERAL SENSE ONLY IN MONO, IF PROPORTIONAL WOULD
C     REQUIRE TO BE FOLLOWED BY SAME WIDTH CHAR FOR SENSE)
      IF (IXTENT.EQ.0) THEN
                         RXT = 0.0
                         GO TO 1000
      ENDIF
      RXT = .5*FLOAT(IX+NCOXA)*CXF + CSEP
      CX = CX + FLOAT(NCOXA+IX)*CXF + CSEP
 1000 IF (BACK) THEN
                     BACK = .FALSE.
                     CX = MAX(CX,CXS)
                     RXT = MAX(RXT,RXTS)
      ENDIF
 9999 RETURN
C
C    ACCENT ENTRY
C
      ENTRY DFX203(NCHAR,NCHARA,L1,L2)
      IXTENT = RXTENT(NCHARA,NBET) - LXTENT(NCHARA,NBET)
      IF (LENGTH) THEN
C    GET RELATIVE/ABSOLUTE FLAG AND RETURN IN L2
                   CALL DFXM20(NBET,L1,L1+1)
                   L2 = XYDATA(1,1)
                   GO TO 9999
      ENDIF
      BACK = .TRUE.
C    DUMMY BACKSPACE TO CENTRE ACCENT
      CXS = CX
      CX = CX - RXT
      IF (.NOT.NMONSP) THEN
                       IX = RXTENT(NCHAR,NBET)
                       ELSE
                       IX = MHWRRU(NBET)
      ENDIF
      CXOFF = .5*FLOAT(IX-NCOX)
      RXTS = RXT
      ON = .FALSE.
C    START CHARACTER WITH BEAM OFF
      XOFF = 0.0
      YOFF = 0.0
      PT = .FALSE.
      L1L2 = L2 - L1 + 1
      NN = MIN(L1L2,2*DATBUF)
      CALL DFXM20(NBET,L1,L1+NN-1)
C    GO UNPACK BUFFER OF CHARACTER DATA
      NI = NN/2
      IX = XYDATA(1,1)
      IY = XYDATA(2,1)
C    IY = 0 COMMON ACCENT DATA
      IF (IY.GT.0) THEN
C    IY IS NUMBER OF LOWER CASE ONLY DATA PAIRS
               IF (NCHAR.LE.64) THEN
C                  UPPER CASE
                           L1 = L1 + 2*IY
               ELSE
C                  LOWER CASE
                           L2 = L2 - 2*IY
               ENDIF
               L1L2 = L2 - L1 + 1
               NN = MIN(L1L2,2*DATBUF)
               CALL DFXM20(NBET,L1,L1+NN-1)
C             GO UNPACK BUFFER OF CHARACTER DATA
               NI = NN/2
      ENDIF
      IF (IX.NE.0) THEN
C    ADJUST IF RELATIVE COORDINATES
                 XYDATA(1,1) = -105
                 XYDATA(2,1) = IX
                 NCOXA = 0.0
      ELSE
C    DUMMY (OFF) IF ABSOLUTE (MUST ALL BE ABSOLUTE)
                 XYDATA(1,1) = -100
                 XYDATA(2,1) = 0
                 NCOXA = NCOX
                 IF (.NOT.NMONSP) NCOXA = -LXTENT(NCHAR,NBET)
                 CX = CX - NCOXA*CXF - CXOFF
      ENDIF
      NCOYA = NCOY
      COXA = FLOAT(NCOXA)
      COYA = FLOAT(NCOYA)
      IF (.NOT.NMONSP) THEN
                 RXTS = MAX(RXTS,FLOAT(RXTENT(NCHARA,NBET))*CXF+CSEP)
                 CXS = MAX(CXS,CX+RXTS)
      ENDIF
      GO TO 10
      END
