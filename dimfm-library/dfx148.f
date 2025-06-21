      SUBROUTINE DFX148(I,Z)
C    COPY ICOLPT(I) TO 4 (I.E. NRGB) AND MODIFY INTENSITY TO Z
      INCLUDE 'dfxc04.cmn'
      J = ICOLPT(I)
      R0(NRGB) = R0(J)
      G0(NRGB) = G0(J)
      B0(NRGB) = B0(J)
      ZINT0(NRGB) = ZINT0(J)
      CALL DFX142(NRGB,Z)
      SFLIN(NRGB) = SFLIN(J)
      SFSPOT(NRGB) = SFSPOT(J)
      RETURN
      END
C
C----------------------------------------------
C
