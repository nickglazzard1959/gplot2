      SUBROUTINE YUV(Y,U,V)
      REAL Y,U,V
      INCLUDE 'dfxc04.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'YUV'
      CALL DFX160(3,Y,U,V,RRR,GGG,BBB,IERR)
      IF (IERR.LE.0) THEN
                        DO 1 J=1,NRGB
    1                   CALL DFX143(J,RRR,GGG,BBB)
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
