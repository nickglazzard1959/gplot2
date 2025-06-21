      SUBROUTINE DFXG05(IWS)
C    DEACTIVATE WORK STATION
      INCLUDE 'params.cmn'
      INCLUDE 'dfxcaa.cmn'
      INCLUDE 'dfxcac.cmn'
      INCLUDE 'dfxcacs.cmn'
      INCLUDE 'dfxcad.cmn'
      INCLUDE 'dfxcba.cmn'
      INCLUDE 'dfxcbd.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      GKSERR = 3
      IF (GKSOS.NE.3) GO TO 99
      GKSERR = 20
      IF (IWS.LE.0.OR.IWS.GT.MSOPWS) GO TO 99
      NWS = IWS
      GKSERR = 30
      IF (.NOT.WSAC(NWS)) GO TO 99
      GKSERR = 33
      IF (WSCAT(NWS).EQ.5) GO TO 99
      GKSERR = 35
      IF (WSCAT(NWS).EQ.4) GO TO 99
      GKSERR = 0
      WSAC(NWS) = .FALSE.
C****PLUS OTHER DEACTIVATION ACTIONS*****
      NCACWS = NCACWS - 1
      IMM = .FALSE.
      IF (NCACWS.LE.0) GO TO 2
      J = 0
      DO 1 I=1,NCACWS
      IF (.NOT.WSAC(I)) GO TO 1
      IF (WSDM(I).EQ.0) IMM = .TRUE.
      J = J + 1
      LACWS(J) = I
    1 CONTINUE
    2 IF (NCACWS.EQ.0) GKSOS = 2
   99 RETURN
      END
C
C----------------------------------------------
C
