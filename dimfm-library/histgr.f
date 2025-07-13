      SUBROUTINE HISTGR(X,Y,N,BAR)
      REAL X(N),Y(N)
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'HISTGR'
      HIST = BAR
      IF (BAR.LT.-.5) HIST = -1.0
      CALL DFX336(X,Y,N)
      HIST = -10.0
      IF (IMM) CALL DFX000(-6,DUMMY,DUMMY,DUMMY,NDUMMY)
      ROUTIN = STARS6
      RETURN
      END
