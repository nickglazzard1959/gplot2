      SUBROUTINE SAMEX
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'SAMEX'
      IF (XGL.EQ.XGR) THEN
            CALL DFX304
      ELSE
            FIXX = .TRUE.
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
