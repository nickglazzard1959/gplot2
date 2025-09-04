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
      WRITE(LUN,100)W
C
 100  FORMAT(F12.6,' ^W')
      END
