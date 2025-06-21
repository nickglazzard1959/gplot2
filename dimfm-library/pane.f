      SUBROUTINE PANE(XLEFT,XRIGHT,YLOW,YUP)
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'PANE'
      CALL DFX128(XLEFT,XRIGHT,YLOW,YUP)
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
