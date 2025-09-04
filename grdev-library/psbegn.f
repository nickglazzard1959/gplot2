      SUBROUTINE PSBEGN
C --- ------------------------------------------------------------------
C
C --- THIS ROUTINE INITIALISES POSTSCRIPT OUTPUT.
C
      INCLUDE 'dfxpsn.cmn'
C
C --- SET THE PLOT SIZE AND OFFSET FROM ORIGIN (ALL IN INCHES).
C --- THE DEFAULT VALUES PRODUCE SOMETHING WHICH LOOKS SENSIBLE.
C
      IF( .NOT. DIMXXX )THEN
         XSIZE = 5.0
         YSIZE = 5.0
         XOFFST = 0.5
         YOFFST = 0.5
         DIMXXX = .TRUE.
      ENDIF
      RED = 0.0
      GREEN = 0.0
      BLUE = 0.0
      WIDTH = 1.5
C      PRINT 888
C 888  FORMAT(1X,'PSBEGN SET SIZE/OFFSET')
C
C --- CHOOSE A LUN TO OUTPUT ON.
C
      LUN = 15
      OPENED = .FALSE.
      CALL PSCLR
C
      END
C --- ------------------------------------------------------------------
