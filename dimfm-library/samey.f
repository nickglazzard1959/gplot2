      SUBROUTINE SAMEY
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'SAMEY'
      IF (YGL.EQ.YGU) THEN
            CALL DFX304
      ELSE
            FIXY = .TRUE.
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
