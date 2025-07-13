      SUBROUTINE TLEVS(XLEV,I1)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      REAL XLEV(I1)
      INCLUDE 'dfxc03.cmn'
      ROUTIN = 'TLEVS'
      I = I1
      IF (I.LE.0.OR.I.GT.4) THEN
                      IF (ICHECK.GT.0) WRITE(ERRREC,3)I
    3 FORMAT(1H0,'**DIMFILM WARNING**  SECOND ARGUMENT IN CALL TO TLEVS
     1HAS ILLEGAL VALUE ',I6,' WHICH SHOULD BE IN RANGE 1 - 4'/
     2     1H ,21X,'CALL IGNORED')
      CALL DFX130(0)
      ELSE
                      DO 1 J=1,I
    1                 RTHLEV(J) = XLEV(J)*ANGCON(1,ANGRP(2))
                      NLEV = I
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
