      SUBROUTINE PSRGBC( R, G, B )
C --- ------------------------------------------------------------------
C
C --- SET THE COLOUR TO DRAW IN.
C
      INCLUDE 'dfxpsn.cmn'
C
      WRITE(LUN,100)R,G,B
C
 100  FORMAT(F12.6,1X,F12.6,1X,F12.6,' ^C')
      END
C
C --- ------------------------------------------------------------------
