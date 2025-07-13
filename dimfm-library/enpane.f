      SUBROUTINE ENPANE
      INCLUDE 'params.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      INCLUDE 'dfxcac.cmn'
      INCLUDE 'dfxcacs.cmn'
      INCLUDE 'dfxcbc.cmn'
      INCLUDE 'dfxcbcs.cmn'
      ROUTIN = 'ENPANE'
      PCLPWC(1,1) = WCBND1(1)
      PCLPWC(2,1) = WCBND1(2)
      PCLPWC(3,1) = WCBND1(3)
      PCLPWC(4,1) = WCBND1(4)
      PRECLP(1) = .FALSE.
      IF (NOTR.EQ.1) CALL DFX2AA
      ROUTIN = STARS6
      RETURN
      END
