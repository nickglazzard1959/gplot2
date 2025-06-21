      SUBROUTINE DFX128(XLEFT,XRIGHT,YLOW,YUP)
C
C    **INTERNAL PANE FUNCTION**
C
      INCLUDE 'params.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      INCLUDE 'dfxcac.cmn'
      INCLUDE 'dfxcacs.cmn'
      INCLUDE 'dfxcbc.cmn'
      INCLUDE 'dfxcbcs.cmn'
C
      IF (ROUTIN.EQ.'PANE') THEN
C    APPLIES TO NOTR = 1 IF DIRECT REFERENCE FROM PANE
                     NTRAN = 1
      ELSE
C    OTHERWISE IS APPLIED TO CURRENT TRANSFORM (E.G. FROM GRAPHING)
                     NTRAN = NOTR
      ENDIF
      PCLPWC(1,NTRAN) = XLEFT
      PCLPWC(2,NTRAN) = XRIGHT
      PCLPWC(3,NTRAN) = YLOW
      PCLPWC(4,NTRAN) = YUP
      PRECLP(NTRAN) = .TRUE.
      IF ((XLEFT.GE.XRIGHT).OR.(YLOW.GE.YUP))
     1   CALL DFX001('PANE',2)
      IF (NOTR.EQ.NTRAN) THEN
             CALL DFX2AA
             IF ((XLEFT.LT.XB1).OR.(XRIGHT.GT.XB2).OR.
     1           (YLOW.LT.YB1).OR.(YUP.GT.YB2))THEN
                 IF (ICHECK.GT.0) THEN
                 WRITE(ERRREC,4) XLEFT,XRIGHT,YLOW,YUP,XB1,XB2,YB1,YB2
      CALL DFX130(0)
    4 FORMAT(1H0,'**DIMFILM WARNING** POSSIBLE ERROR IN CALL TO PANE'/
     11H0,21X,'PANE DOES NOT LIE ENTIRELY WITHIN CURRENT BOUNDS, PANE PA
     2RAMETERS WERE '/1H0,1P4E14.5/1H0,21X,'CURRENT BOUNDS WERE ',
     3  1P4E14.5)
      ENDIF
      ENDIF
      ENDIF
      RETURN
      END
C
C----------------------------------------------
C
