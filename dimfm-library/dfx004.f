      REAL FUNCTION DFX004(TT)
C    GET RADIANS INTO RANGE 0,TWOPI
      INCLUDE 'dfxc03.cmn'
      T = AMOD(TT,TWOPI)
      IF (T.LT.0.) T = T + TWOPI
      DFX004 = T
      RETURN
      END
