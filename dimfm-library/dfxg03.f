      SUBROUTINE DFXG03(IWS)
C    CLOSE WORK STATION
      INCLUDE 'params.cmn'
      INCLUDE 'dfxcaa.cmn'
      INCLUDE 'dfxcac.cmn'
      INCLUDE 'dfxcacs.cmn'
      INCLUDE 'dfxcad.cmn'
      INCLUDE 'dfxcba.cmn'
      INCLUDE 'dfxcbd.cmn'
      GKSERR = 7
      IF (GKSOS.LE.1) GO TO 99
      GKSERR = 20
      IF (IWS.LE.0.OR.IWS.GT.MSOPWS) GO TO 99
      NWS = IWS
      GKSERR = 25
      IF (.NOT.WSOP(NWS)) GO TO 99
      GKSERR = 29
      IF (WSAC(NWS)) GO TO 99
C**** GKSERR = 147
      GKSERR = 0
      CALL DFX000(-2,ZDUMMY,ZDUMMY,ZDUMMY,NDUMMY)
      NCOPWS = NCOPWS - 1
      WSOP(NWS) = .FALSE.
      IF (NCOPWS.EQ.0) GO TO 3
      J = 0
      DO 1 I=1,MSOPWS
      IF (.NOT.WSOP(I)) GO TO 1
      J = J + 1
      LOPWS(J) = I
    1 CONTINUE
    3 IF (NCOPWS.EQ.0) GKSOS = 1
   99 RETURN
      END
C
C----------------------------------------------
C
