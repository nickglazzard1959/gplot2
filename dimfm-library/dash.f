      SUBROUTINE DASH
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'DASH'
      IDASH = 1
      NO1 = 11
      NO2 = 12
      TL = 2.0*XQ
      CALL DFX105
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
