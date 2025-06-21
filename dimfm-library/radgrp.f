      SUBROUTINE RADGRP(IGROUP)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'RADGRP'
      CALL DFX112(IGROUP,2)
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
