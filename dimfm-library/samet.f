      SUBROUTINE SAMET
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'SAMET'
      IF (THGL.EQ.THGR) THEN
            CALL DFX304
      ELSE
            FIXTH = .TRUE.
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
