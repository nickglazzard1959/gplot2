      SUBROUTINE EPSNAM( EFNAME )
C --- ------------------------------------------------------------------
C
C --- SET THE OUTPUT FILENAME STEM TO EFNAME
C
      CHARACTER*(*) EFNAME
C
      INCLUDE 'dfxpsn.cmn'
      INCLUDE 'dfxpcn.cmn'
C
#ifdef UNIX
      EPFNL = MIN(72,LEN(EFNAME))
#else
      EPFNL = MIN(4,LEN(EFNAME))
#endif
      EPFN(1:EPFNL) = EFNAME
C
      END
C
C
C --- ------------------------------------------------------------------
