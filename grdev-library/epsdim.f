      SUBROUTINE EPSDIM( UXS, UYS, UXOFF, UYOFF )
C --- ------------------------------------------------------------------
C
C --- SET THE EPS SIZE TO UXS X UYS INCHES OFFSET BY (UXOFF,UYOFF)
C --- INCHES
C
      INCLUDE 'dfxpsn.cmn'
C
      IF( UXS .GT. 0.1 .AND. UYS .GT. 0.1 )THEN
         XSIZE = UXS
         YSIZE = UYS
         XOFFST = UXOFF
         YOFFST = UYOFF
         DIMXXX = .TRUE.
      ENDIF
C
      END
C
C
C --- ------------------------------------------------------------------
