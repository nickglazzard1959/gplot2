      SUBROUTINE MONTHX(SPOINT)
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'MONTHX'
      XTYPE = 4
      XMONTH = SPOINT
      IF (XMONTH.LT.1..OR.XMONTH.GE.13.) THEN
                    XMONTH = 1.
                    IF (ICHECK.GT.0) WRITE(ERRREC,4) SPOINT
      CALL DFX130(0)
    4 FORMAT(1H0,'**DIMFILM WARNING** INVALID ARGUMENT ',E10.2,
     1 ' IN CALL TO MONTHX'/1H ,21X,'DEFAULT OF 1.0 WILL BE USED')
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
