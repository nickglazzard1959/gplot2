      SUBROUTINE HSV(H,S,V)
      REAL H,S,V
      INCLUDE 'dfxc04.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'HSV'
      CALL DFX160(5,H,S,V,RRR,GGG,BBB,IERR)
      IF (IERR.LE.0) THEN
                        DO 1 I=1,NRGB
    1                   CALL DFX143(I,RRR,GGG,BBB)
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
