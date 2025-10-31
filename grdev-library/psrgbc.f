      SUBROUTINE PSRGBC( R, G, B )
C --- ------------------------------------------------------------------
C --- SET THE COLOUR TO DRAW IN. NORMALISED (0 TO 1) RGB.
C --- ------------------------------------------------------------------
      IMPLICIT LOGICAL (A-Z)
      REAL R, G, B
C
      INCLUDE 'dfxpsn.cmn'
C
      RED = R
      GREEN = G
      BLUE = B
#ifdef NOSVE
      WRITE(LUN,100)R,G,B, CHAR(99)
 100  FORMAT(F12.6,1X,F12.6,1X,F12.6,1X,A)
#else
      WRITE(LUN,100)R,G,B
 100  FORMAT(F12.6,1X,F12.6,1X,F12.6,' ^C')
#endif
      END
