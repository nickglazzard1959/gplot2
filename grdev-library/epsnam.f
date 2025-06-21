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
      EPFNL = LEN(EFNAME)
      EPFN(1:EPFNL) = EFNAME
C
      END
C
C
C --- ------------------------------------------------------------------
