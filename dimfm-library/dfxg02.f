      SUBROUTINE DFXG02(ID,CONID,WSEXT)
C    OPEN WS
      INTEGER ID,CONID
      EXTERNAL WSEXT
      INCLUDE 'params.cmn'
      INCLUDE 'dfxcaa.cmn'
      INCLUDE 'dfxcac.cmn'
      INCLUDE 'dfxcacs.cmn'
      INCLUDE 'dfxcad.cmn'
      INCLUDE 'dfxcba.cmn'
      INCLUDE 'dfxcbd.cmn'
      INCLUDE 'dfxcd0.cmn'
      INCLUDE 'dfxcd0s.cmn'
      NWS = ID
      NCON = CONID
      GKSERR = 8
      IF (GKSOS.EQ.0) GO TO 99
      GKSERR = 20
      IF (NWS.LE.0.OR.NWS.GT.MSOPWS) GO TO 99
      GKSERR = 24
      IF (WSOP(NWS)) GO TO 99
C    INVALID CONNECTION ID MUST BE DETERMINED BY DRIVER
C    (AS SOME BATCH DEVICES WILL IGNORE AND SET TO 0 AS SET INTERNALLY)
      GKSERR = 0
      CALL DFXM06(WSEXT,NWS)
C    ASSIGN DRIVER EXTERNAL TO WS ID
      CALL DFX000(-1,ZDUMMY,ZDUMMY,ZDUMMY,NDUMMY)
      IF (GKSERR.NE.0) GO TO 99
      NCOPWS = NCOPWS + 1
      WSOP(NWS) = .TRUE.
C    DISPLAY SURFACE EMPTY
      WSDSE(NWS) = .TRUE.
      J = 0
      DO 2 I=1,MSOPWS
      IF (.NOT.WSOP(I)) GO TO 2
      J = J + 1
      LOPWS(J) = I
    2 CONTINUE
      WSCON(NWS) = NCON
      IF (GKSOS.EQ.1) GKSOS = 2
   99 RETURN
      END
C
C----------------------------------------------
C
