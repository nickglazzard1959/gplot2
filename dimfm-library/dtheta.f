      SUBROUTINE DTHETA(THETA)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc03.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'DTHETA'
      IF (THETA.GT.0.0) THEN
                ANGSTP = THETA*ANGCON(1,ANGRP(3))
      ELSE
                IF (ICHECK.GT.0) WRITE(ERRREC,2) THETA
      CALL DFX130(0)
    2 FORMAT(1H0,'**DIMFILM WARNING**  CALL TO DTHETA WITH ZERO OR NEGAT
     1IVE ARGUMENT ',1PE14.5,' IS IGNORED')
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
