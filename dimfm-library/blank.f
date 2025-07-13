      SUBROUTINE BLANK(XLEFT,XRIGHT,YLOW,YUP)
      INCLUDE 'params.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      INCLUDE 'dfxcac.cmn'
      INCLUDE 'dfxcacs.cmn'
      INCLUDE 'dfxcbc.cmn'
      INCLUDE 'dfxcbcs.cmn'
      ROUTIN = 'BLANK'
      PBLKWC(1,1) = XLEFT
      PBLKWC(2,1) = XRIGHT
      PBLKWC(3,1) = YLOW
      PBLKWC(4,1) = YUP
      PREBLK(1) = .TRUE.
      IF ((XLEFT.GE.XRIGHT).OR.(YLOW.GE.YUP))
     1      CALL DFX001('BLANK',4)
      IF (NOTR.EQ.1) THEN
                     CALL DFX2AA
             IF ((XLEFT.LT.XB1).OR.(XRIGHT.GT.XB2).OR.
     1           (YLOW.LT.YB1).OR.(YUP.GT.YB2)) THEN
           IF (ICHECK.GT.0) THEN
           WRITE(ERRREC,6) XLEFT,XRIGHT,YLOW,YUP,XB1,XB2,YB1,YB2
      CALL DFX130(0)
    6 FORMAT(1H0,'**DIMFILM WARNING**  POSSIBLE ERROR IN CALL TO BLANK'/
     1  1H0,21X,'BLANKING DOES NOT LIE ENTIRELY WITHIN CURRENT BOUNDS, B
     2LANKING PARAMETERS WERE '/1H0,21X,1P4E14.5/1H0,21X,'CURRENT BOUNDS
     3 WERE ',1P4E14.5)
      ENDIF
      ENDIF
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
