      SUBROUTINE XUNIT(X)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'XUNIT'
      IF (X.LE.0.0) THEN
                 IF (ICHECK.GT.0) WRITE(ERRREC,3) X
      CALL DFX130(0)
    3 FORMAT(1H0,'**DIMFILM WARNING**  XUNIT CALLED WITH ILLEGAL ARGUMEN
     1T ',1PE14.5,' CALL IGNORED')
      ELSE
                 CALL DFX135(X)
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
