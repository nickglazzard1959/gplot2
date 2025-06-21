      SUBROUTINE HSV3(H,S,V)
      REAL H,S,V
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'HSV3'
      CALL DFX160(5,H,S,V,RRR,GGG,BBB,IERR)
      IF (IERR.LE.0) CALL DFX143(3,RRR,GGG,BBB)
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
