      SUBROUTINE DFX000(IF,X,Y,Z,N)
      INTEGER IF,N(*)
      REAL X(*),Y(*),Z(*)
C
C
C*****************************************************************
C
C    DFX000 IS DIMFILM INTERFACE ROUTINE TO ALL DEVICE DRIVERS,
C    COMMUNICATION WITH WHICH IS VIA DFXM05.
C
C    IF IS USED TO SELECT THE REQUIRED GRAPHIC FUNCTION.
C
C    WHERE A WORK STATION DESIGNATION IS REQUIRED THIS IS SUPPLIED
C    IN NWS OF /DFXCBD/; ZERO INDICATES ALL OPEN/ACTIVE WORK
C    STATIONS AS RELEVANT.
C
C    I = INPUT FUNCTION ONLY
C    O = OUTPUT FUNCTION ONLY
C    IO = COMMON INPUT/OUTPUT FUNCTION
C    * = NOT YET IMPLEMENTED
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C    NEGATIVE VALUES ARE WORK STATION FUNCTIONS
C
C-----------------------------------------------------------------
C
C
C    -2000 AND LOWER VALUES ARE DEVICE SPECIFIC AND WILL
C          RELAY PARAMETERS TO DEVICE NWS UNPROCESSED
C
C IO -6    FLUSH BUFFERS ON ALL ACTIVE AND IMMEDIATE (ASAP) WS
C
C I  -5    ACTIVATE WORK STATION NWS
C
C IO -4    SET NEW WORK STATION TRANSFORMATION FOR NWS
C
C IO -3    CLEAR FRAME ON ACTIVE WORK STATION FOR NWS
C
C IO -2    CLOSE WORK STATION NWS
C
C IO -1    OPEN WORK STATION NWS
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C    1-10 ARE 2-D GRAPHIC FUNCTIONS WITH INPUT (X,Y) COORDINATES
C    IN WORLD COORDINATES TRANSFORMED TO NORMALIZED DEVICE
C    COORDINATES BEFORE PASSING TO ACTIVE WORK STATIONS
C
C-----------------------------------------------------------------
C
C
C IO  1    OFF-MOVE TO (X,Y) - NON-ZERO N PLOTS POINT
C                            MAY CONVERT TO 2 ON OUTPUT
C
C I   2    OFF-MOVE TO (X,Y) TO BE ACCUMULATED
C  O       OFF-MOVE TO (X,Y) (MAY BE ACCUMULATED BY DEVICE DRIVER
C                             IF NO CURSOR OR OTHER IMMEDIATE
C                             REQUIREMENT)
C                    MAY ORIGINATE FROM INPUT 1 FUNCTION
C
C I   3    ON-MOVE (+ACCUMULATED OFF-MOVE) TO (X,Y)
C
C IO  4    ON-MOVE (NO ACCUMULATED OFF-MOVE) TO (X,Y)
C                  (ON INPUT USED ONLY WHERE GUARANTEE OF
C                   NO OTHER INTERVENING INTERACTION)
C
C *   5    POLYLINE (X,Y),J=1,N (INITIAL RESET PATTERN)
C
C *   6    CONTINUED POLYLINE  (X,Y),J=1,N
C
C *   7    POLYMARKER
C
C *   8    TEXT
C
C *   9    FILL AREA
C
C IO 10    PIXEL ARRAY
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C    100-      ATTRIBUTE SPECIFICATIONS
C
C-----------------------------------------------------------------
C
C *O 100       FORCE POLYLINE BUNDLE TRANSFER TO WORK STATION
C                    (PLSET IS SET TRUE WHEN ANY POLYLINE
C                     ATTRIBUTE IS SET)
C
C IO 101       SET LOOK UP TABLE ENTRIES R(J),G(J),B(J) FROM
C              DIMFILM GLOBAL LUT IN RGBLUT(1/2/3,J)
C              J=N(1),N(2) FOR WORK STATIONS NWS
C              (NWS ZERO INDICATES ALL OPEN WORK STATIONS)
C
C IO 102       SET LOOK UP TABLE ENTRIES R(J),G(J),B(J) FROM
C              ARGUMENTS X(J),Y(J),Z(J) RESPECTIVELY
C              J=N(1),N(2) FOR WORK STATIONS NWS
C              (NWS ZERO INDICATES ALL OPEN WORK STATIONS)
C
C              THERE IS PRESENTLY NO COMMAND FOR IMMEDIATE
C              TRANSFER OF COLOUR TO DEVICE.  PENDING TRANSFER
C              WILL BE INDICATED BY DEVCOL(NWS) SET TRUE, DRIVER
C              SHOULD SET FALSE AFTER TRANSFER.
C              NOTE - DEVICE DRIVER MUST CHECK DEVCOL PRIOR TO
C                     ANY RELEVANT PLOT FUNCTION.
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C    200-      WORK STATION SPECIFIC FUNCTIONS
C
C-----------------------------------------------------------------------
C
C I  200       N = 0 RETURN XLAST,YLAST IN X,Y
C              N = 1 RESTORE XLAST,YLAST FROM X,Y AND SET OFF = .TRUE.
C
C I  201       OUTLINE WORK STATION MAXIMUM DISPLAY (NWS.GE.0)
C
C I  202       OUTLINE WORK STATION VIEW PORT       (NWS.GE.0)
C
C I  203       OUTLINE WORK STATION WINDOW          (NWS.GE.0)
C
C IO 204       N = 0 OFF-MOVE TO (X,Y) IN DEVICE COORDINATES
C              N = 1 ON-MOVE TO (X,Y) IN DEVICE COORDINATES
C              (NWS MUST BE SPECIFIED AND NON-ZERO)
C
C*****************************************************************
C
C
      INCLUDE 'params.cmn'
      INCLUDE 'dfxcp0.cmn'
      INCLUDE 'dfxcba.cmn'
      INCLUDE 'dfxcbb.cmn'
      INCLUDE 'dfxcbd.cmn'
      INCLUDE 'dfxcbe.cmn'
      INCLUDE 'dfxcaa.cmn'
      INCLUDE 'dfxcac.cmn'
      INCLUDE 'dfxcacs.cmn'
      INCLUDE 'dfxcad.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      INCLUDE 'dfxc24.cmn'
C
      EXTERNAL DFXB00
C    FORCE LOAD OF BLOCK DATA FOR FIRST INITIALISATIONS
C
      LOGICAL OFF,IN1,IN2
      INTEGER IFLAST
      REAL XLAST,YLAST,XCUT(4),YCUT(4)
      SAVE OFF,IFLAST,XLAST,YLAST,ZDUMMY,NDUMMY
      LOGICAL INSIDE
      DATA OFF/.FALSE./
      DATA XLAST,YLAST/0.0,0.0/
      DATA IFLAST/0/
C    PRESET DUMMY ARGUMENTS
      DATA ZDUMMY,NDUMMY/0.0,0/
      INSIDE(ZZ,Z1,Z2) = (ZZ.LE.Z2).AND.(ZZ.GE.Z1)
      IF (GKSOS.EQ.0) GO TO 10000
C
C
      IFUNC = IF
      IF (IFUNC.LE.0) GO TO 1000
      IF (IFUNC.GT.10) GO TO 2000
C
      IF (NCACWS.GT.0) GO TO 3000
C
C     ELSE ONLY SAVE ACCUMULATED OFF-MOVE/ON-MOVE IN XLAST/YLAST
C
      IF (.NOT.ACTFN) GO TO 230
C
C    GIVE ACTIVE FUNCTION WHEN NO ACTIVE WS ONLY ONCE
C    IN ANY SEQUENCE OF INACTIVE WS
  232 ACTFN = .FALSE.
      IF (ICHECK.GT.0) WRITE(ERRREC,231) ROUTIN
      CALL DFX130(0)
  231 FORMAT(1H0,'**DIMFILM WARNING** PLOTTING INSTRUCTION ISSUED WHEN N
     1O WORK STATION ACTIVE'/1H ,21X,'USER ACCESSED ROUTINE WAS - ',
     2 A6/1H ,21X,'DIAGNOSTIC WILL NOT BE REISSUED AGAIN PRIOR TO NEXT A
     3CTIVATION OF ANY WORK STATION')
C
  230 IF (IFUNC.GT.4) GO TO 9999
      IFUNC = 2
C
C
 3000 ACTFN = .TRUE.
      GO TO (1,2,3,4,5,6,7,8,9,10),IFUNC
C
C
C-----------------------------------------------------------------
C    1 -  OFF-MOVE TO (X,Y)    N NON-ZERO IS POINT
C-----------------------------------------------------------------
C
    1 OFF = .TRUE.
      XLAST = XOWCND + X(1)*XSWCND
      YLAST = YOWCND + Y(1)*YSWCND
      IF1 = IFUNC
      IF (N(1).LE.0) IF1 = 2
      DO 220 I=1,NCACWS
      NWS = LACWS(I)
  220 CALL DFXM05(IF1,XLAST,YLAST,ZDUMMY,N)
      GO TO 9999
C
C
C-----------------------------------------------------------------
C    2 - OFF-MOVE TO (X,Y) TO BE ACCUMULATED
C-----------------------------------------------------------------
C
    2 OFF = .TRUE.
      XLAST = XOWCND + X(1)*XSWCND
      YLAST = YOWCND + Y(1)*YSWCND
      GO TO 9999
C
C
C-----------------------------------------------------------------
C    3 - ON-MOVE (+ ACCUMULATED OFF-MOVE) TO (X,Y)
C-----------------------------------------------------------------
C
    3 IF (.NOT.OFF) GO TO 222
      DO 221 I=1,NCACWS
      NWS = LACWS(I)
  221 CALL DFXM05(2,XLAST,YLAST,ZDUMMY,NDUMMY)
  222 OFF = .FALSE.
C
C
C-----------------------------------------------------------------
C    4 - ON-MOVE TO (X,Y)
C-----------------------------------------------------------------
C
    4 IF (.NOT.PLSET.AND.IFLAST.LE.4) GO TO 224
      DO 223 I=1,NCACWS
      NWS =LACWS(I)
  223 CALL DFXM05(100,ZDUMMY,ZDUMMY,ZDUMMY,NDUMMY)
      PLSET = .FALSE.
  224 XNEW = XOWCND + X(1)*XSWCND
      YNEW = YOWCND + Y(1)*YSWCND
      DO 200 I=1,NCACWS
      NWS = LACWS(I)
      IF (WSCAT(NWS).GT.3) GO TO 200
C    OUTPUT CLASSES (0-3) ONLY
      I2 = 4
      IF (GKSOS.EQ.4) GO TO 210
C    SEGMENTS ARE NOT CLIPPED UNTIL DISPLAYED
      XW1 = CWSWIN(1,NWS)
      XW2 = CWSWIN(2,NWS)
      YW1 = CWSWIN(3,NWS)
      YW2 = CWSWIN(4,NWS)
      IF (.NOT.VPCLIP) GO TO 201
      XW1 = AMAX1(XW1,NDCVP(1,NOTRP))
      XW2 = AMIN1(XW2,NDCVP(2,NOTRP))
      YW1 = AMAX1(YW1,NDCVP(3,NOTRP))
      YW2 = AMIN1(YW2,NDCVP(4,NOTRP))
C
C    NOW CLIP AS NECESSARY
C
  201 IN1 = INSIDE(XLAST,XW1,XW2).AND.INSIDE(YLAST,YW1,YW2)
      IN2 = INSIDE(XNEW,XW1,XW2).AND.INSIDE(YNEW,YW1,YW2)
      IF (IN1.AND.IN2) GO TO 202
      NCUT = 0
      XMIN = AMIN1(XLAST,XNEW)
      XMAX = AMAX1(XLAST,XNEW)
      YMIN = AMIN1(YLAST,YNEW)
      YMAX = AMAX1(YLAST,YNEW)
      IF (XLAST.EQ.XNEW) GO TO 203
      IF (YLAST.EQ.YNEW) GO TO 204
C
C    HERE FOR ANGLED LINES NOT TOTALLY IN
C
      G = (YLAST-YNEW)/(XLAST-XNEW)
      C = YLAST - G*XLAST
      IF (INSIDE(XW1,XMIN,XMAX)) THEN
             NCUT = NCUT + 1
             XCUT(NCUT) = XW1
             YCUT(NCUT) = G*XW1 + C
             IF (.NOT.INSIDE(YCUT(NCUT),YW1,YW2)) NCUT = NCUT - 1
      ELSE IF (INSIDE(XW2,XMIN,XMAX)) THEN
             NCUT = NCUT + 1
             XCUT(NCUT) = XW2
             YCUT(NCUT) = G*XW2 + C
             IF (.NOT.INSIDE(YCUT(NCUT),YW1,YW2)) NCUT = NCUT - 1
      ELSE IF (INSIDE(YW1,YMIN,YMAX)) THEN
             NCUT = NCUT + 1
             YCUT(NCUT) = YW1
             XCUT(NCUT) = (YW1 - C)/G
             IF (.NOT.INSIDE(XCUT(NCUT),XW1,XW2)) NCUT = NCUT - 1
      ELSE IF (INSIDE(YW2,YMIN,YMAX)) THEN
             NCUT = NCUT + 1
             YCUT(NCUT) = YW2
             XCUT(NCUT) = (YW2 - C)/G
             IF (.NOT.INSIDE(XCUT(NCUT),XW1,XW2)) NCUT = NCUT - 1
      END IF
      IF (NCUT.EQ.0) GO TO 200
C
C    NOW ORDER CUTS
C
      N1 = 1
      N2 = 2
      IF (NCUT.EQ.1) GO TO 207
      DO 206 J=2,NCUT
      IF (XCUT(J).GT.XCUT(N2)) N2 = J
      IF (XCUT(J).LT.XCUT(N1)) N1 = J
  206 CONTINUE
      IF (XLAST.LE.XNEW) GO TO 207
      J = N1
      N1 = N2
      N2 = J
  207 I2 = 2
      IF (IN1) I2 = 4
      CALL DFXM05(I2,XCUT(N1),YCUT(N1),ZDUMMY,NDUMMY)
      IF (NCUT.EQ.1) GO TO 202
      I2 = 4
      IF (IN1) I2 = 2
      CALL DFXM05(I2,XCUT(N2),YCUT(N2),ZDUMMY,NDUMMY)
  202 I2 = 2
      IF (IN2) I2 = 4
  210 CALL DFXM05(I2,XNEW,YNEW,ZDUMMY,NDUMMY)
      GO TO 200
C
C    INTERSECTIONS PARALLEL Y-AXIS
C
  203 IF (INSIDE(YW1,YMIN,YMAX)) THEN
               NCUT = NCUT + 1
               XCUT(NCUT) = XLAST
               YCUT(NCUT) = YW1
      ELSE IF (INSIDE(YW2,YMIN,YMAX)) THEN
               NCUT = NCUT + 1
               XCUT(NCUT) = XLAST
               YCUT(NCUT) = YW2
      END IF
      IF (NCUT.EQ.0) GO TO 200
      IF (YLAST.LE.YNEW) THEN
               N1 = 1
               N2 = NCUT
      ELSE
               N1 = NCUT
               N2 = 1
      ENDIF
      GO TO 207
C
C    INTERSECTIONS PARALLEL X-AXIS
C
  204 IF (INSIDE(XW1,XMIN,XMAX)) THEN
               NCUT = NCUT + 1
               YCUT(NCUT) = YLAST
               XCUT(NCUT) = XW1
      ELSE IF (INSIDE(XW2,XMIN,XMAX)) THEN
               NCUT = NCUT + 1
               YCUT(NCUT) = YLAST
               XCUT(NCUT) = XW2
      ENDIF
      IF (NCUT.EQ.0) GO TO 200
      IF (XLAST.LE.XNEW) THEN
               N1 = 1
               N2 = NCUT
      ELSE
               N1 = NCUT
               N2 = 1
      ENDIF
      GO TO 207
C
  200 CONTINUE
C
      XLAST = XNEW
      YLAST = YNEW
C
C
C-----------------------------------------------------------------------
C    RETURN VIA HERE
C-----------------------------------------------------------------------
C
 9999 IFLAST = IFUNC
      RETURN
C
C
C-----------------------------------------------------------------------
C    GENERAL PURPOSE FUNCTION TRANSFER TO ACTIVE WS
C-----------------------------------------------------------------------
 9000 IF (NCACWS.LE.0) GO TO 9999
      IF (NWS.NE.0) THEN
C   IF NOMINATED WS MUST BE ACTIVE
          IF (WSAC(NWS)) GO TO 9002
      ELSE
          DO 9001 I=1,NCACWS
          NWS = LACWS(I)
 9001     CALL DFXM05(IFUNC,X,Y,Z,N)
      ENDIF
      GO TO 9999
C
C-----------------------------------------------------------------------
C    GENERAL PURPOSE FUNCTION TRANSFER TO WS
C-----------------------------------------------------------------------
 9002 CALL DFXM05(IFUNC,X,Y,Z,N)
      GO TO 9999
C
C-----------------------------------------------------------------------
C    GENERAL PURPOSE FUNCTION TRANSFER TO OPEN WS
C-----------------------------------------------------------------------
 9003 IF (NCOPWS.LE.0) GO TO 9999
      IF (NWS.NE.0) THEN
C   IF NOMINATED WS MUST BE OPEN
          IF (WSOP(NWS)) GO TO 9002
      ELSE
          DO 9004 I=1,NCOPWS
          NWS = LOPWS(I)
 9004     CALL DFXM05(IFUNC,X,Y,Z,N)
      ENDIF
      GO TO 9999
C
C
C
C
C-----------------------------------------------------------------
C    5 - POLYLINE (X,Y),J=1,N   (WITH INITIAL RESET PATTERN)
C-----------------------------------------------------------------
C
    5 GO TO 9999
C
C
C-----------------------------------------------------------------
C    6 - CONTINUED POLYLINE (X,Y),J=1,N
C-----------------------------------------------------------------
C
    6 GO TO 9999
C
C
C-----------------------------------------------------------------
C    7 - POLYMARKER
C-----------------------------------------------------------------
C
    7 GO TO 9999
C
C
C-----------------------------------------------------------------
C    8 - TEXT
C-----------------------------------------------------------------
C
    8 GO TO 9999
C
C
C-----------------------------------------------------------------
C    9 - FILL AREA
C-----------------------------------------------------------------
C
    9 GO TO 9999
C
C
C-----------------------------------------------------------------
C    10 - PIXEL ARRAY
C-----------------------------------------------------------------
C
   10 CONTINUE
C
C    IF SET UP ENTRY AND CELL LINE MUST SET PARAMETER BLOCK
C
      IF ( (NPAR.EQ.1) .OR. ( (NPAR.GT.0) .AND.
     1       ( (IFLAST.NE.10).OR.RASTWN) ) ) CALL DFX171
C
C    NOTE:
C           CELL ARRAYS WILL REFERENCE DFX170.
C           THE FIRST CELL SCAN REFERENCES DFX171 AND THIS
C           REFERENCES DFX170.
C           DFX170 WILL SET RASTWN TRUE TO ENSURE WINDOW/BLANKING
C           SET UP IN DRIVER.
C
C    RETURN IF RASTER ERROR FOR CELL LINE SCAN
      IF ((NPAR.GT.0).AND.(RSTERR.NE.0)) GO TO 9999
C
      DO 225 I=1,NCACWS
      NWS = LACWS(I)
C    IF FIRST SET-UP CELL ARRAY ENTRY (NPAR = 0, OR FIRST LINE)
      IF ((NPAR.LE.0).OR.(IFLAST.NE.10).OR.RASTWN) THEN
C    COMPUTE WS CLIPPING/BLANKING IN DC
C    (NOTE - SEGMENTS NOT CLIPPED UNTIL DISPLAYED)
           XW1 = CWSWIN(1,NWS)
           XW2 = CWSWIN(2,NWS)
           YW1 = CWSWIN(3,NWS)
           YW2 = CWSWIN(4,NWS)
           IF (VPCLIP) THEN
                 XW1 = AMAX1(XW1,NDCVP(1,NOTRP))
                 XW2 = AMIN1(XW2,NDCVP(2,NOTRP))
                 YW1 = AMAX1(YW1,NDCVP(3,NOTRP))
                 YW2 = AMIN1(YW2,NDCVP(4,NOTRP))
            ENDIF
            IF (LPAR(1)) THEN
                 XW1 = AMAX1(XW1,RPAR(1))
                 XW2 = AMIN1(XW2,RPAR(2))
                 YW1 = AMAX1(YW1,RPAR(3))
                 YW2 = AMIN1(YW2,RPAR(4))
            ENDIF
            NDCL(1) = XW1
            NDCL(2) = XW2
            NDCL(3) = YW1
            NDCL(4) = YW2
      ENDIF
      IF (LPAR(2)) THEN
            NDBL(1) = RPAR(5)
            NDBL(2) = RPAR(6)
            NDBL(3) = RPAR(7)
            NDBL(4) = RPAR(8)
      ENDIF
C    TESTS FOR VALIDITY OF AREAS RE CLIPPING/BLANKING DONE IN
C    DRIVER TO AVOID RESOLUTION PROBLEMS
      CALL DFXM05(IFUNC,X,Y,Z,N)
      DEVCOL(NWS) = .TRUE.
  225 CONTINUE
C
C FORCE OFF TO LAST VECTOR POSITION ON RESUMPTION OF VECTORS
      OFF = .TRUE.
      GO TO 9999
C
C
C=================================================================
C    NEGATIVE FUNCTION VALUES - WORK STATION FUNCTIONS
C                               (NON-PLOTTING)
C=================================================================
C
C
 1000 CONTINUE
C
C-----------------------------------------------------------------
C     0 - CLEAR LAST OP FLAG TO ZERO - USED WHEN DIRECT DEVICE ENTRY
C-----------------------------------------------------------------
C
      IF (IFUNC.EQ.0) GO TO 9999
      IF (IFUNC.LE.-2000) GO TO 2999
      IPF = -IFUNC
      GO TO (1001,1002,1003,1004,1005,1006),IPF
C
C-----------------------------------------------------------------
C    -1 - OPEN WORK STATION NWS
C-----------------------------------------------------------------
C
 1001 CALL DFXM05(IFUNC,ZDUMMY,ZDUMMY,ZDUMMY,NDUMMY)
      GO TO 9999
C
C-----------------------------------------------------------------
C    -2 - CLOSE WORK STATION NWS  (ALL IF NWS = 0)
C-----------------------------------------------------------------
C
 1002 IF (NWS.GT.0) GO TO 1200
C    SAVES ONLY MOST RECENT GKS ERROR IN CLOSE DEVICE
C    THIS IS PART OF EMERGENCY CLOSE FACILITY AND SHOULD NOT
C    OTHERWISE BE USED
      IWS = NWS
      IF (NCOPWS.LE.0) GO TO 9999
      IERR = 0
      DO 1201 I=1,NCOPWS
      NWS = LOPWS(I)
      CALL DFXM05(IFUNC,ZDUMMY,ZDUMMY,ZDUMMY,NDUMMY)
 1201 IF (GKSERR.NE.0) IERR = GKSERR
      NCOPWS = 0
      GKSERR = IERR
      GKSOS = 1
      GO TO 9999
 1200 CALL DFXM05(IFUNC,ZDUMMY,ZDUMMY,ZDUMMY,NDUMMY)
      GO TO 9999
C
C-----------------------------------------------------------------
C    -3 - CLEAR FRAME ON ALL ACTIVE WORK STATIONS
C-----------------------------------------------------------------
C
 1003 IF (NWS.GT.0) THEN
          IF (WSAC(NWS)) CALL DFXM05(IFUNC,ZDUMMY,ZDUMMY,ZDUMMY,NDUMMY)
          WSDSE(NWS) = .TRUE.
      ELSE
          DO 300 I=1,NCACWS
          NWS = LACWS(I)
          CALL DFXM05(IFUNC,ZDUMMY,ZDUMMY,ZDUMMY,NDUMMY)
C    FLAG EMPTY DISPLAY SURFACE
  300     WSDSE(NWS) = .TRUE.
      ENDIF
C    RESET CELL MAPPED AREA POINTER
      RASTLN = 0
      GO TO 9999
C
C-----------------------------------------------------------------
C    -4 - SET NEW WORK STATION TRANSFORMATION ON WS NWS
C-----------------------------------------------------------------
C
 1004 CALL DFXM05(IFUNC,ZDUMMY,ZDUMMY,ZDUMMY,NDUMMY)
      GO TO 9999
C
C-----------------------------------------------------------------------
C    -5 - ACTIVATE WORK STATION NWS I.E. SEND POSITION/COLOUR
C-----------------------------------------------------------------------
C
 1005 DEVCOL(NWS) = .TRUE.
      CALL DFXM05(1,XLAST,YLAST,ZDUMMY,0)
      GO TO 9999
C
C-----------------------------------------------------------------------
C    -6 - FLUSH BUFFER FOR EACH ACTIVE AND IMMEDIATE (ASAP) WS
C-----------------------------------------------------------------------
C
 1006 IF (NCACWS.LE.0) GO TO 9999
      DO 301 I=1,NCACWS
      NWS = LACWS(I)
      IF (WSDM(NWS).NE.0) GO TO 301
      CALL DFXM05(IFUNC,ZDUMMY,ZDUMMY,ZDUMMY,NDUMMY)
  301 CONTINUE
      GO TO 9999
C
C=================================================================
C     ADDITIONAL GRAPHIC FUNCTIONS FOR WORK STATIONS
C     FUNCTION CODES IN EXCESS OF 10
C=================================================================
C
 2000 IF (IFUNC.GE.200) GO TO 2500
C    HERE FOR IFUNC 100 - 199
      IF ((IFUNC.LT.101).OR.(IFUNC.GT.102)) GO TO 9999
C
C-----------------------------------------------------------------------
C   101 - SEND LUT ENTRY (OR BLOCK) FROM GLOBAL SET TO OPEN WS
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
C   102 - SEND LUT ENTRY (OR BLOCK) FROM ARGUMENTS X,Y,Z TO OPEN WS
C-----------------------------------------------------------------------
C
C    HANDLED BY GENERAL PURPOSE OPEN WS FUNCTION TRANSFER
      GO TO 9003
 2500 IF (IFUNC.EQ.200) GO TO 2200
C    FUNCTION CODES IN EXCESS OF 200
C
C    CHECK ACTIVE WS
C
      IF (NCACWS.GT.0) GO TO 2501
      IF (ACTFN) GO TO 232
      GO TO 9999
 2501 IF1 = IFUNC - 200
      ACTFN = .TRUE.
      GO TO (2201,2201,2201,2204),IF1
C
C
C-----------------------------------------------------------------------
C    200 - SAVE/RESTORE XLAST,YLAST ACCORDING TO N
C-----------------------------------------------------------------------
C
C
 2200 IF (N(1).EQ.1) GO TO 2502
C    N = 0 RETURN XLAST,YLAST IN X,Y
      X(1) = XLAST
      Y(1) = YLAST
      GO TO 9999
C    N = 1 RESTORE XLAST,YLAST FROM X,Y AND SET OFF=.TRUE.
 2502 XLAST = X(1)
      YLAST = Y(1)
      OFF = .TRUE.
      GO TO 9999
C
C
C-----------------------------------------------------------------------
C    201 - OUTLINE WORK STATION LIMITS
C    202 - OUTLINE WORK STATION VIEW PORT
C    203 - OUTLINE WORK STATION WINDOW
C-----------------------------------------------------------------------
C
C
 2201 NNWS = NWS
      DO 2202 I=1,NCACWS
      NWS = LACWS(I)
      IF ((NNWS.GT.0).AND.(NWS.NE.NNWS)) GO TO 2202
                             IF1 = 204
                             IF2 = 204
                             N1 = 0
                             N2 = 1
      IF (IFUNC.EQ.201) THEN
                             X1 = 0.0
                             X2 = WSMDCS(1,NWS)
                             Y1 = 0.0
                             Y2 = WSMDCS(2,NWS)
      ELSE IF (IFUNC.EQ.202) THEN
                             X1 = CWSVP(1,NWS)
                             X2 = CWSVP(2,NWS)
                             Y1 = CWSVP(3,NWS)
                             Y2 = CWSVP(4,NWS)
      ELSE IF (IFUNC.EQ.203) THEN
                             X1 = CWSWIN(1,NWS)
                             X2 = CWSWIN(2,NWS)
                             Y1 = CWSWIN(3,NWS)
                             Y2 = CWSWIN(4,NWS)
                             IF1 = 2
                             IF2 = 4
                             N2 = 0
      ENDIF
      CALL DFXM05(IF1,X1,Y1,ZDUMMY,N1)
      CALL DFXM05(IF2,X2,Y1,ZDUMMY,N2)
      CALL DFXM05(IF2,X2,Y2,ZDUMMY,N2)
      CALL DFXM05(IF2,X1,Y2,ZDUMMY,N2)
      CALL DFXM05(IF2,X1,Y1,ZDUMMY,N2)
 2202 CONTINUE
      OFF = .TRUE.
      GO TO 9999
C
C
C-----------------------------------------------------------------------
C    204 - OFF/ON MOVE IN DEVICE COORDINATES FOR NWS ACCORDING TO N
C-----------------------------------------------------------------------
C
C
 2204 IF (NWS.LE.0) GO TO 2203
      IF (.NOT.WSAC(NWS)) GO TO 2203
C    PROCEED ONLY FOR ACTIVE WORK STATION (PRESENTLY NO DIAGNOSTIC)
      CALL DFXM05(204,X,Y,Z,N)
 2203 CONTINUE
      OFF = .TRUE.
      GO TO 9999
C    ******HERE IF GKS NOT OPEN AT TIME OF REFERENCE TO DFX000******
10000 GKSERR = 8
C    RETURN SHOULD BE VIA GKS ERROR HANDLER ROUTINE
      GO TO 9999
C
C
C-----------------------------------------------------------------------
C   -2000 AND LOWER - DIRECT COMMUNICATION WITH SINGLE DEVICE NWS
C-----------------------------------------------------------------------
C
C    NOTE - NO CHECK IS MADE ON DEVICE STATUS; THIS IS RESPONSIBILITY
C           OF CALLING PROGRAM
C
 2999 IF (NWS.GT.0) CALL DFXM05(IF,X,Y,Z,N)
      GO TO 9999
      END
