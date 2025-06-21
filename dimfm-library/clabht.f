      SUBROUTINE CLABHT(HTC,ANGLE)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc03.cmn'
      INCLUDE 'dfxc11.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'CLABHT'
      PHI = ANGLE*ANGCON(1,ANGRP(1))
      CCHT = HTC
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
