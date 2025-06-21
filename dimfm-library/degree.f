      SUBROUTINE DEGREE
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'DEGREE'
      J = 1
      DO 2 I=1,4
    2 ANGRP(I) = J
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
