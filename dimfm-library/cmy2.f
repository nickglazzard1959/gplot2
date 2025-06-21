      SUBROUTINE CMY2(C,M,Y)
      REAL C,M,Y
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'CMY2'
      CALL DFX160(1,C,M,Y,RRR,GGG,BBB,IERR)
      IF (IERR.LE.0) CALL DFX143(2,RRR,GGG,BBB)
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
