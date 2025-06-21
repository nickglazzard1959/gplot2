      SUBROUTINE EPSRUT( ERNAME )
C --- ------------------------------------------------------------------
C
C --- SET THE OUTPUT ROUTE TO ERNAME
C
      CHARACTER*(*) ERNAME
C
      INCLUDE 'dfxpsn.cmn'
      INCLUDE 'dfxpcn.cmn'
C
      EPRTL = LEN(ERNAME)
      EPRT(1:EPRTL) = ERNAME
C
      END
C
C
C --- ------------------------------------------------------------------
