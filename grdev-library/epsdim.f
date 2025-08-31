      SUBROUTINE EPSDIM( UXS, UYS, UXOFF, UYOFF )
C --- ------------------------------------------------------------------
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
      RETURN
      END
C
      SUBROUTINE EPSDGT( UXS, UYS, UXOFF, UYOFF )
C --- ------------------------------------------------------------------
C --- RETURN THE EPS SIZE PREVIOUSLY SET WITH EPSDIM().
C
      INCLUDE 'dfxpsn.cmn'
C
      UXS = XSIZE
      UYS = YSIZE
      UXOFF = XOFFST
      UYOFF = YOFFST
C
      RETURN
      END
