      SUBROUTINE SETCEX(EX)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc01.cmn'
      INCLUDE 'dfxc01s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'SETCEX'
      IF (EX.LE.0.0) THEN
              IF (ICHECK.GT.0) WRITE(ERRREC,100) EX,CEXP
      CALL DFX130(0)
  100 FORMAT(1H0,'**DIMFILM WARNING**  SETCEX CALLED WITH INVALID EXPANS
     1ION FACTOR ',1PE16.8,' CURRENT VALUE ',E16.8,' RETAINED')
      ELSE
              CEXP = EX
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
