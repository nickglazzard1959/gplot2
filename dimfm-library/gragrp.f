      SUBROUTINE GRAGRP(IGROUP)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'GRAGRP'
      CALL DFX112(IGROUP,3)
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
