      SUBROUTINE CINTER(IS)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc11.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'CINTER'
      IF (IS.LE.0) THEN
              IF (ICHECK.GT.0) WRITE(ERRREC,2)IS
    2 FORMAT(1H0, 63H**DIMFILM WARNING**  CINTER CALLED WITH NON-POSITIV
     1E ARGUMENT -,I6/1H ,25X, 34HNO INTERPOLATION WILL BE PERFORMED)
      CALL DFX130(0)
              INTERP = .FALSE.
      ELSE
              NSTEP = IS
              INTERP = .TRUE.
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
