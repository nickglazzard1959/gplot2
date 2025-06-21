      SUBROUTINE SETUN(UNY)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc01.cmn'
      INCLUDE 'dfxc01s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'SETUN'
      IF (UNY.LT.0.0) THEN
                  IF (ICHECK.GT.0) WRITE(ERRREC,100) UNY,UNYSP
      CALL DFX130(0)
  100 FORMAT(1H0,'**DIMFILM WARNING**  SETUN CALLED WITH INVALID SEPARAT
     1ION ',1PE16.8/1H ,21X,'CURRENT VALUE ',1PE16.8,' RETAINED')
      ELSE
                  UNYSP = UNY
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
