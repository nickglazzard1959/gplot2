      SUBROUTINE DFX304
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      IF (ICHECK.GT.0) WRITE(ERRREC,3) ROUTIN
      CALL DFX130(0)
    3 FORMAT(1H0, '**DIMFILM WARNING**  CALL TO ',A,' WAS MADE WITH UNSE
     1T RANGE'/1H ,25X,'CALL IGNORED')
      RETURN
      END
C
C----------------------------------------------
C
