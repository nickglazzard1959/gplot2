      SUBROUTINE SAMERT
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'SAMERT'
      IF ((RGL.EQ.RGR).OR.(THGL.EQ.THGR)) THEN
            CALL DFX304
      ELSE
            FIXR = .TRUE.
            FIXTH = .TRUE.
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
