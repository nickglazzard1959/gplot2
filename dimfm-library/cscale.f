      SUBROUTINE CSCALE(SCALE)
      INCLUDE 'dfxc11.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'CSCALE'
      IF (SCALE.NE.0.0) CS = SCALE
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
