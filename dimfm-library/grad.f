      SUBROUTINE GRAD
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'GRAD'
      J = 3
      DO 2 I=1,4
    2 ANGRP(I) = J
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
