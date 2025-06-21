      SUBROUTINE YIQ1(Y,I,Q)
      REAL Y,I,Q
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'YIQ1'
      CALL DFX160(2,Y,I,Q,RRR,GGG,BBB,IERR)
      IF (IERR.LE.0) CALL DFX143(1,RRR,GGG,BBB)
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
