      SUBROUTINE OWNDSH
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'OWNDSH'
      IDASH = 4
      NO1 = 1
      NO2 = N4
      TL = TL4*XQ
      CALL DFX105
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
