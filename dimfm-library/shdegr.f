      SUBROUTINE SHDEGR(X,Y,N,BAR,THETA,GAP)
      REAL X(N),Y(N)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'SHDEGR'
      IF (BAR.NE.0.0) IHIST = ISIGN(3,IHIST)
      IF (GAP.LE.0.0) THEN
              IF (ICHECK.GT.0) WRITE(ERRREC,11)ROUTIN,THETA,GAP
   11 FORMAT(1H0,'**DIMFILM WARNING**  ILLEGAL GAP SPECIFIED IN CALL OF
     1',A/1H ,21X,'CALL FOR HATCHING - ',2E16.8/
     21H ,21X,'HISTOGRAMS WILL NOT BE HATCHED')
      CALL DFX130(0)
      ENDIF
      CALL DFX315(THETA,GAP)
C-----------------------------------------------
C    EQUIVALENT TO HISTGR(X,Y,N,BAR)
      HIST = BAR
      IF (BAR.LT.-.5) HIST = -1.0
      CALL DFX336(X,Y,N)
      HIST = -10.0
C-----------------------------------------------
      IHIST = ISIGN(1,IHIST)
      IF (IMM) CALL DFX000(-6,DUMMY,DUMMY,DUMMY,NDUMMY)
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
