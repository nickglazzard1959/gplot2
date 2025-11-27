      SUBROUTINE EPSNAM( EFNAME )
C --- ------------------------------------------------------------------
C
C --- SET THE OUTPUT FILENAME STEM TO EFNAME, RESET FRAME NUMBER.
C
      CHARACTER*(*) EFNAME
C
      INCLUDE 'dfxpsn.cmn'
      INCLUDE 'dfxpcn.cmn'
C
#ifdef UNIXNVE
      EPFNL = MIN(72,LEN(EFNAME))
#else
      EPFNL = MIN(4,LEN(EFNAME))
#endif
      EPFN(1:EPFNL) = EFNAME
      FRNO = 0
C
      END
C
C
C --- ------------------------------------------------------------------
