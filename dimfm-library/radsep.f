      SUBROUTINE RADSEP(ANG)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc03.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'RADSEP'
      A = ANG
      DEGPRT = A*ANGCON(1,ANGRP(2))
C    NEGATIVE IS AUTO CALCULATION
C    POSITIVE OR ZERO IS ACTUAL SPECIFIED SEPARATION
      ROUTIN = STARS6
      RETURN
      END
