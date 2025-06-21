      SUBROUTINE RGB2(R,G,B)
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'RGB2'
      CALL DFX160(0,R,G,B,RR,GG,BB,IERR)
      IF (IERR.LE.0) CALL DFX143(2,RR,GG,BB)
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
