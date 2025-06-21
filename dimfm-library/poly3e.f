      SUBROUTINE POLY3E(X,Y,N1,MX,MY)
      REAL X(N1,MX),Y(N1,MY)
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'POLY3E'
      N = N1
      NDEG = 3
      CALL DFX326(X,Y,N)
      IF (IMM) CALL DFX000(-6,DUMMY,DUMMY,DUMMY,NDUMMY)
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
