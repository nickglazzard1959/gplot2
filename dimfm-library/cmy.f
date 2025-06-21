      SUBROUTINE CMY(C,M,Y)
      REAL C,M,Y
      INCLUDE 'dfxc04.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'CMY'
      CALL DFX160(1,C,M,Y,RRR,GGG,BBB,IERR)
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
