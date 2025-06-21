      SUBROUTINE DFXG55(IWS,XMIN,XMAX,YMIN,YMAX)
      INCLUDE 'params.cmn'
      INCLUDE 'dfxcaa.cmn'
      INCLUDE 'dfxcac.cmn'
      INCLUDE 'dfxcacs.cmn'
      INCLUDE 'dfxcad.cmn'
      INCLUDE 'dfxcae.cmn'
      INCLUDE 'dfxcbd.cmn'
      INCLUDE 'dfxcd0.cmn'
      INCLUDE 'dfxcd0s.cmn'
      NWS = IWS
      GKSERR = 7
      IF (GKSOS.LE.1) GO TO 99
      GKSERR = 20
      IF (NWS.LE.0.OR.NWS.GT.MSOPWS) GO TO 99
      GKSERR = 25
      IF (WSOP(NWS)) GO TO 99
      GKSERR = 33
      IF (WSCAT(NWS).EQ.5) GO TO 99
      GKSERR = 36
      IF (WSCAT(NWS).EQ.0) GO TO 99
      GKSERR = 51
      IF (XMIN.GE.XMAX) GO TO 99
      IF (YMIN.GE.YMAX) GO TO 99
      GKSERR = 54
      IF (AMIN1(XMIN,YMIN).LT.0.0) GO TO 99
      IF (XMAX.GT.WSMDCS(1,NWS)) GO TO 99
      IF (YMAX.GT.WSMDCS(2,NWS)) GO TO 99
      GKSERR = 0
      RWSVP(1,NWS) = XMIN
      RWSVP(2,NWS) = XMAX
      RWSVP(3,NWS) = YMIN
      RWSVP(4,NWS) = YMAX
      IF (DMWSTR(NWS)) GO TO 1
      IF (WSDSE(NWS)) GO TO 1
      WSTRUP(NWS) = .TRUE.
      GO TO 99
    1 DO 2 I=1,4
    2 CWSVP(I,NWS) = RWSVP(I,NWS)
      WSTRUP(NWS) = .FALSE.
      CALL DFX000(-4,ZDUMMY,ZDUMMY,ZDUMMY,NDUMMY)
   99 RETURN
      END
C
C----------------------------------------------
C
