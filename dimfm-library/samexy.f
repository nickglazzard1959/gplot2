      SUBROUTINE SAMEXY
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'SAMEXY'
      IF ((XGL.EQ.XGR).OR.(YGL.EQ.YGU)) THEN
            CALL DFX304
      ELSE
            FIXX = .TRUE.
            FIXY = .TRUE.
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
