      SUBROUTINE INTVAR(HFRAC,HSTEP)
      INCLUDE 'dfxc07.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'INTVAR'
      IF (HFRAC.GT.0..AND.HFRAC.LT.1.0) HTFRAC = HFRAC
      ZISTEP = HSTEP
      IVAR = .TRUE.
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
