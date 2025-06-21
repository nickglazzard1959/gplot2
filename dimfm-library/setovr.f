      SUBROUTINE SETOVR(OVHT,OVY,UNY)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc01.cmn'
      INCLUDE 'dfxc01s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'SETOVR'
      IF ((OVHT.LE.0.0).OR.(OVY.LT.0.0).OR.(UNY.LT.0.0)) THEN
                  IF (ICHECK.GT.0) WRITE(ERRREC,100) OVHT,OVY,UNY,
     1                                             OVERH,OVERY,UNDERY
      CALL DFX130(0)
  100 FORMAT(1H0,'**DIMFILM WARNING**  SETOVR CALLED WITH INVALID VALUE(
     1S), CALLED PARAMETERS WERE ',1P3E16.8/1H ,21X,'CURRENT VALUES (',
     2 1P3E16.8,') RETAINED')
      ELSE
                  OVERH = OVHT
                  OVERY = OVY
                  UNDERY = UNY
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
