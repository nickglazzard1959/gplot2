      SUBROUTINE MONTHY(SPOINT)
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'MONTHY'
      YTYPE = 4
      YMONTH = SPOINT
      IF (YMONTH.LT.1..OR.YMONTH.GE.13.) THEN
                       YMONTH = 1.
                       IF (ICHECK.GT.0) WRITE(ERRREC,4) SPOINT
      CALL DFX130(0)
    4 FORMAT(1H0,'**DIMFILM WARNING** INVALID ARGUMENT ',E10.2,
     1 ' IN CALL TO MONTHY'/1H ,21X,'DEFAULT OF 1.0 WILL BE USED')
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
