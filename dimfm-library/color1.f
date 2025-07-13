      SUBROUTINE COLOR1(N)
      INCLUDE 'dfxc04.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'COLOR1'
      IF (ICOLPT(IRGBN).EQ.1) CALL DFX140(IRGBN)
      CALL DFX161(N,IERR)
      IF (IERR.EQ.0) NLUT(1) = N
      ROUTIN = STARS6
      RETURN
      END
