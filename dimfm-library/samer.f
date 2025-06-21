      SUBROUTINE SAMER
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'SAMER'
      IF (RGL.EQ.RGR) THEN
            CALL DFX304
      ELSE
            FIXR = .TRUE.
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
