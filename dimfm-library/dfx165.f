      SUBROUTINE DFX165(I,N)
      INCLUDE 'dfxc04.cmn'
      IF (ICOLPT(IRGBN).EQ.I) CALL DFX140(IRGBN)
      NLUT(I) = N
      RETURN
      END
C
C----------------------------------------------
C
