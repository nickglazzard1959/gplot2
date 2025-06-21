      SUBROUTINE DSHDOT
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'DSHDOT'
      IDASH = 3
      NO1 = 15
      NO2 = 18
      TL = 3.0*XQ
      CALL DFX105
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
