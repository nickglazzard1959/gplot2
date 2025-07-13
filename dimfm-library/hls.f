      SUBROUTINE HLS(H,L,S)
      REAL H,L,S
      INCLUDE 'dfxc04.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'HLS'
      CALL DFX160(6,H,L,S,RRR,GGG,BBB,IERR)
      IF (IERR.LE.0) THEN
                        DO 1 I=1,NRGB
    1                   CALL DFX143(I,RRR,GGG,BBB)
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
