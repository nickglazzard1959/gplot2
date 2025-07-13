      SUBROUTINE COLOR(N)
      INCLUDE 'dfxc04.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'COLOR'
      CALL DFX140(IRGBN)
      CALL DFX161(N,IERR)
      IF (IERR.EQ.0) THEN
                        DO 1 I=1,NRGB
    1                   NLUT(I) = N
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
