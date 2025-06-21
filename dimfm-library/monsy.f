      SUBROUTINE MONSY(NUMMON)
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'MONSY'
      IF (NUMMON.LT.2) THEN
             MONY = 0
             IF (ICHECK.GT.0) WRITE(ERRREC,2) NUMMON
      CALL DFX130(0)
    2 FORMAT(1H0,'**DIMFILM WARNING** INVALID ARGUMENT ',I10,
     1 ' IN CALL TO MONSY'/1H ,21X,'AUTOY WILL BE PERFORMED')
      ELSE
             MONY = NUMMON
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
