      SUBROUTINE MONSX(NUMMON)
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'MONSX'
      IF (NUMMON.LT.2) THEN
             MONX = 0
             IF (ICHECK.GT.0) WRITE(ERRREC,2) NUMMON
      CALL DFX130(0)
    2 FORMAT(1H0,'**DIMFILM WARNING** INVALID ARGUMENT ',I10,
     1 ' IN CALL TO MONSX'/1H ,21X,'AUTOX WILL BE PERFORMED')
      ELSE
             MONX = NUMMON
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
