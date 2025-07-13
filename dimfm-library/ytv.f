      SUBROUTINE YTV(Y,T,V)
      REAL Y,T,V
      INCLUDE 'dfxc04.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'YTV'
      CALL DFX160(4,Y,T,V,RRR,GGG,BBB,IERR)
      IF (IERR.LE.0) THEN
                        DO 1 J=1,NRGB
    1                   CALL DFX143(J,RRR,GGG,BBB)
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
