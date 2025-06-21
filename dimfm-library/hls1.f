      SUBROUTINE HLS1(H,L,S)
      REAL H,L,S
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'HLS1'
      CALL DFX160(6,H,L,S,RRR,GGG,BBB,IERR)
      IF (IERR.LE.0) CALL DFX143(1,RRR,GGG,BBB)
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
