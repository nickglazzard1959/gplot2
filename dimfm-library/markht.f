      SUBROUTINE MARKHT(HEIGHT)
      INCLUDE 'dfxc01.cmn'
      INCLUDE 'dfxc01s.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'MARKHT'
      IF (HEIGHT.GT.0.0) THEN
                  HTMK = HEIGHT
      ELSE
                  IF (ICHECK.GT.0) WRITE(ERRREC,1) HEIGHT,HTMK
      CALL DFX130(0)
    1 FORMAT(1H0,'**DIMFILM WARNING**  MARKHT CALLED WITH ILLEGAL ARGUME
     1NT ',1PE14.5/1H ,21X,'HEIGHT USED WAS ',1PE14.5)
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
