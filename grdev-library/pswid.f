      SUBROUTINE PSWID( W )
C --- ------------------------------------------------------------------
C
C --- SET THE LINE WIDTH TO DRAW WITH.
C
      INCLUDE 'dfxpsn.cmn'
C
      WRITE(LUN,100)W
C
 100  FORMAT(F12.6,' ^W')
      END
C
C --- ------------------------------------------------------------------
