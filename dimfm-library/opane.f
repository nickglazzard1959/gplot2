      SUBROUTINE OPANE
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      INCLUDE 'params.cmn'
      INCLUDE 'dfxcac.cmn'
      INCLUDE 'dfxcacs.cmn'
      INCLUDE 'dfxcbc.cmn'
      INCLUDE 'dfxcbcs.cmn'
      ROUTIN = 'OPANE'
      IF (PWIND) THEN
              IF (PRECLP(NOTR)) THEN
                       I = 2
              ELSE
                       IF (ICHECK.GT.0) WRITE(ERRREC,2)
    2 FORMAT(1H0,'**DIMFILM WARNING**  PANE OUTLINING CALLED WHILE PANE
     1UNSET - BY DEFAULT PANE EQUALS BOUNDS'/1H ,21X,'BOUNDS WILL BE OUT
     2LINED - DETECTED BY OPANE')
      CALL DFX130(0)
                       I = 1
              ENDIF
              CALL DFX502(I,0.0)
      IF (IMM) CALL DFX000(-6,DUMMY,DUMMY,DUMMY,NDUMMY)
      ELSE
              IF (ICHECK.GT.0) WRITE(ERRREC,8)
    8 FORMAT(1H0,'**DIMFILM WARNING**  PANE OUTLINING CALLED WHILE PRECL
     1IPPING INACTIVE'/1H ,21X,'CALL OF OPANE IGNORED')
      CALL DFX130(0)
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
