      SUBROUTINE YIQ(Y,I,Q)
      REAL Y,I,Q
      INCLUDE 'dfxc04.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'YIQ'
      CALL DFX160(2,Y,I,Q,RRR,GGG,BBB,IERR)
      IF (IERR.LE.0) THEN
                        DO 1 J=1,NRGB
    1                   CALL DFX143(J,RRR,GGG,BBB)
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
