      SUBROUTINE STEPGR(X,Y,N)
      REAL X(N),Y(N)
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'STEPGR'
      BAR = -1.0
      IHIST = ISIGN(2,IHIST)
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
