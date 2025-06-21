      SUBROUTINE DFX143(N,RR,GG,BB)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc04.cmn'
C    SELECT TRUE COLOUR MODE
      NLUT(N) = -1
C    NOW STORE COLOUR
      R0(N) = RR
      G0(N) = GG
      B0(N) = BB
      ZINT0(N) = DFX146(N)
C    NOW SET CURRENT COLOUR/INTENSITY TO BASE SET
      R(N) = R0(N)
      G(N) = G0(N)
      B(N) = B0(N)
      ZINT(N) = ZINT0(N)
      IF (ICOLPT(IRGBN).EQ.N) CALL DFX140(IRGBN)
      RETURN
      END
C
C----------------------------------------------
C
