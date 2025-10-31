      SUBROUTINE PSWID( W )
C --- ------------------------------------------------------------------
C --- SET THE LINE WIDTH TO DRAW WITH. W IS IN POINTS.
C --- ------------------------------------------------------------------
      IMPLICIT LOGICAL (A-Z)
      REAL W
C
      INCLUDE 'dfxpsn.cmn'
C
      WIDTH = W
#ifdef NOSVE
      WRITE(LUN,100)W, CHAR(119)
 100  FORMAT(F12.6,1X,A)
#else
      WRITE(LUN,100)W
 100  FORMAT(F12.6,' ^W')
#endif
      END
