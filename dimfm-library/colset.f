      SUBROUTINE COLSET
      INCLUDE 'dfxc04.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'COLSET'
      IF (ICOLPT(IRGBN).NE.IRGBN) CALL DFX140(IRGBN)
      DO 1 I=1,NRGB
    1 ICOLPT(I) = I
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
