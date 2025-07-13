      SUBROUTINE DFXG04(ID)
C    ACTIVATE WS
      INTEGER ID
      INCLUDE 'params.cmn'
      INCLUDE 'dfxcaa.cmn'
      INCLUDE 'dfxcbd.cmn'
      INCLUDE 'dfxcac.cmn'
      INCLUDE 'dfxcacs.cmn'
      INCLUDE 'dfxcad.cmn'
      INCLUDE 'dfxcba.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      GKSERR = 6
      IF (GKSOS.NE.2.AND.GKSOS.NE.3) GO TO 99
      NWS = ID
      GKSERR = 20
      IF (NWS.LE.0.OR.NWS.GT.MSOPWS) GO TO 99
      GKSERR = 25
      IF (.NOT.WSOP(NWS)) GO TO 99
      GKSERR = 29
      IF (WSAC(NWS)) GO TO 99
      GKSERR = 33
      IF (WSCAT(NWS).EQ.5) GO TO 99
      GKSERR = 35
      IF (WSCAT(NWS).EQ.4) GO TO 99
      GKSERR = 0
      NCACWS = NCACWS + 1
      WSAC(NWS) = .TRUE.
      IF (WSDM(NWS).EQ.0) IMM = .TRUE.
      J = 0
      DO 2 I = 1,MSOPWS
      IF (.NOT.WSAC(I)) GO TO 2
      J = J + 1
      LACWS(J) = I
    2 CONTINUE
C    MUST SET POSITION AND COLOUR
      CALL DFX000(-5,DUM,DUM,DUM,IDUM)
      GKSOS = 3
      ACTFN = .TRUE.
   99 RETURN
      END
