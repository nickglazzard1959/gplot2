      SUBROUTINE YAXIS(ZL,ZR)
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'YAXIS'
      FIXY = .TRUE.
      YGL = ZL
      YGU = ZR
      IF (YGU.EQ.YGL) THEN
                  FIXY = .FALSE.
                  IF (ICHECK.GT.0) WRITE(ERRREC,4) ZL,ZR
      CALL DFX130(0)
    4 FORMAT(1H0,'**DIMFILM WARNING** INVALID EQUAL ARGUMENTS - ',
     1 1P2E10.3, ' IN CALL TO YAXIS'/
     2 1H ,21X,'AUTOY WILL BE PERFORMED')
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
