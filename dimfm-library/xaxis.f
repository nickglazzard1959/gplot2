      SUBROUTINE XAXIS(ZL,ZR)
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'XAXIS'
      FIXX = .TRUE.
      XGL = ZL
      XGR = ZR
      IF (XGR.EQ.XGL) THEN
               FIXX = .FALSE.
               IF (ICHECK.GT.0) WRITE(ERRREC,4) ZL,ZR
      CALL DFX130(0)
    4 FORMAT(1H0,'**DIMFILM WARNING** INVALID EQUAL ARGUMENTS - ',
     1 1P2E10.3, ' IN CALL TO XAXIS'/
     2 1H ,21X,'AUTOX WILL BE PERFORMED')
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
