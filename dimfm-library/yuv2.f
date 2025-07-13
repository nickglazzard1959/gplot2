      SUBROUTINE YUV2(Y,U,V)
      REAL Y,U,V
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'YUV2'
      CALL DFX160(3,Y,U,V,RRR,GGG,BBB,IERR)
      IF (IERR.LE.0) CALL DFX143(2,RRR,GGG,BBB)
      ROUTIN = STARS6
      RETURN
      END
