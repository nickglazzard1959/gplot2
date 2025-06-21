      SUBROUTINE YTV1(Y,T,V)
      REAL Y,T,V
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'YTV1'
      CALL DFX160(4,Y,T,V,RRR,GGG,BBB,IERR)
      IF (IERR.LE.0) CALL DFX143(1,RRR,GGG,BBB)
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
