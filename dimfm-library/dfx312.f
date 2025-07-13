      SUBROUTINE DFX312(X,Y,N1,NX,NY,FN1,FN2)
      REAL X(N1,NX),Y(N1,NY)
      INCLUDE 'dfxc06.cmn'
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc09.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      LOGICAL SYMX,SYMY,ERRX,ERRY
      REAL SAVE(4)
      CALL DFX300
      ISDASH = IDASH
      ICCS = ICHECK
      ICHECK = -2
      SYMX = NX.EQ.2
      SYMY = NY.EQ.2
      ERRX = NX.NE.1
      ERRY = NY.NE.1
      SAVE(1) = XTB1
      SAVE(2) = XTB2
      SAVE(3) = YTB1
      SAVE(4) = YTB2
      CALL DFX128(XW1,XW2,YW1,YW2)
      DT = 0.5*TC1*AMIN1(XTB2-XTB1,YTB2-YTB1)
      DO 1 I=1,N1
      XP = (FN1(X(I,1)) - XXGL)*SS1 + XW1
      YP = (FN2(Y(I,1)) - YYGL)*SS2 + YW1
      IF (.NOT.ERRX) GO TO 2
      XPLUS = ABS(X(I,2))
      XMINUS = XPLUS
      IF (SYMX) GO TO 3
      XMINUS = ABS(X(I,3))
    3 XMP = (FN1(X(I,1)-XMINUS) - XXGL)*SS1 + XW1
      XPP = (FN1(X(I,1)+XPLUS) - XXGL)*SS1 + XW1
      IDASH = 0
      CALL DFX110(XMP,YP-DT)
      CALL DFX106(XMP,YP+DT)
      IDASH = ISDASH
      IF (XMP.EQ.XPP) GO TO 2
      CALL DFX110(XMP,YP)
      CALL DFX106(XPP,YP)
      IDASH = 0
      CALL DFX110(XPP,YP-DT)
      CALL DFX106(XPP,YP+DT)
      IDASH = ISDASH
    2 IF (.NOT.ERRY) GO TO 1
      YPLUS = ABS(Y(I,2))
      YMINUS = YPLUS
      IF (SYMY) GO TO 4
      YMINUS = ABS(Y(I,3))
    4 YMP = (FN2(Y(I,1)-YMINUS) - YYGL)*SS2 + YW1
      YPP = (FN2(Y(I,1)+YPLUS) - YYGL)*SS2 + YW1
      IDASH = 0
      CALL DFX110(XP-DT,YMP)
      CALL DFX106(XP+DT,YMP)
      IDASH = ISDASH
      IF (YMP.EQ.YPP) GO TO 1
      CALL DFX110(XP,YMP)
      CALL DFX106(XP,YPP)
      IDASH = 0
      CALL DFX110(XP-DT,YPP)
      CALL DFX106(XP+DT,YPP)
      IDASH = ISDASH
    1 CONTINUE
      CALL DFX128(SAVE(1),SAVE(2),SAVE(3),SAVE(4))
      ICHECK = ICCS
      CALL DFX301
      RETURN
      END
