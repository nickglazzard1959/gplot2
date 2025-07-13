      SUBROUTINE DFX404(GRID,MX,MY,NCC,CVALS)
      REAL SAVE(4)
      REAL GRID(MX,MY),CVALS(NCC)
      INCLUDE 'dfxc11.cmn'
      INCLUDE 'dfxc15.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      INCLUDE 'dfxc06.cmn'
      INCLUDE 'dfxc03.cmn'
      NC = NCC
      XT = XTB2 - XTB1
      YT = YTB2 - YTB1
      IF (IQUAD.EQ.3) XT = .5*XT
      IF (IQUAD.EQ.4) YT = .5*YT
      CHT = .02*AMIN1(XT,YT)
      IF (IQUAD.EQ.2) CHT = .5*CHT
      NX = MX
      NY = MY
      IF (CCHT.LE.0.) GO TO 2
      CHT = CCHT
    2 CALL DFX215(PHI*ANGCON(2,ANGRP(1)))
      HT = CHT
      A1 = AMIN1(XT/FLOAT(NX-1),YT/FLOAT(NY-1))*DC1/(3.*CHT)
C    THIS IS USED TO ENSURE 3*CHARACTER HEIGHT MAY BE FITTED BETWEEN
C    ADJACENT CONTOUR LINES FOR ANNOTATION PURPOSES.
      DDX = (XTB2-XTB1)*DC2
      DDY = (YTB2 - YTB1)*DC2
      X1 = XTB1 + DDX
      Y1 = YTB1 + DDY
      X2 = XTB2 - DDX
      Y2 = YTB2 - DDY
      SAVE(1) = XTB1
      SAVE(2) = XTB2
      SAVE(3) = YTB1
      SAVE(4) = YTB2
      X3 = .5*(X1 + X2)
      Y3 = .5*(Y1 + Y2)
      GO TO (11,12,13,14),IQUAD
   12 X0T = X3
      Y0T = Y3
C    FORCE TRANSFORM
      CALL DFX137
      DX = (X3 - X1)/FLOAT(NX-1)
      DY = (Y3 - Y1)/FLOAT(NY-1)
      FX = DX
      FY = DY
      CALL DFX128(X3,X2,Y3,Y2)
      CALL DFX400(GRID,NX,NY,NC,CVALS)
      FX = -DX
      CALL DFX128(X1,X3,Y3,Y2)
      CALL DFX400(GRID,NX,NY,NC,CVALS)
      FY = -DY
      CALL DFX128(X1,X3,Y1,Y3)
      CALL DFX400(GRID,NX,NY,NC,CVALS)
      FX = DX
      CALL DFX128(X3,X2,Y1,Y3)
      CALL DFX400(GRID,NX,NY,NC,CVALS)
      GO TO 5
   13 X0T = X3
      Y0T = Y1
C    FORCE TRANSFORM
      CALL DFX137
      DX = (X3 - X1)/FLOAT(NX-1)
      DY = (Y2 - Y1)/FLOAT(NY-1)
      FX = DX
      FY = DY
      CALL DFX128(X3,X2,Y1,Y2)
      CALL DFX400(GRID,NX,NY,NC,CVALS)
      FX = -DX
      CALL DFX128(X1,X3,Y1,Y2)
      CALL DFX400(GRID,NX,NY,NC,CVALS)
      GO TO 5
   14 X0T = X1
      Y0T = Y3
C    FORCE TRANSFORM
      CALL DFX137
      DX = (X2 - X1)/FLOAT(NX-1)
      DY = (Y3 - Y1)/FLOAT(NY-1)
      FX = DX
      FY = DY
      CALL DFX128(X1,X2,Y3,Y2)
      CALL DFX400(GRID,NX,NY,NC,CVALS)
      FY = -DY
      CALL DFX128(X1,X2,Y1,Y3)
      CALL DFX400(GRID,NX,NY,NC,CVALS)
      GO TO 5
   11 DX = (X2 - X1)/FLOAT(NX-1)
      DY = (Y2 - Y1)/FLOAT(NY-1)
      FX = DX
      FY = DY
      X0T = X1
      Y0T = Y1
C    FORCE TRANSFORM
      CALL DFX137
      CALL DFX128(X1,X2,Y1,Y2)
      CALL DFX400(GRID,NX,NY,NC,CVALS)
    5 CALL DFX128(SAVE(1),SAVE(2),SAVE(3),SAVE(4))
      RETURN
      END
C
C----------------------------------------------
C
      LOGICAL FUNCTION DFX405(XVAL,YVAL)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc11.cmn'
      INCLUDE 'dfxc01.cmn'
      INCLUDE 'dfxc01s.cmn'
      INCLUDE 'dfxc15.cmn'
      LOGICAL TEMP
      XV = XVAL
      YV = YVAL
      TEMP = .FALSE.
      XZ = HT*DFX211(1,CHS,IDUMMY,IFMT)
      X4 = XV + XZ*CANGL
      X3 = HT*SANGL
      X1 = XV - X3
      X3 = X4 - X3
      XL = AMIN1(X1,XV,X3,X4) + X0T
      XU = AMAX1(X1,XV,X3,X4) + X0T
      IF (XL.LT.XTB1.OR.XU.GT.XTB2) GO TO 1
      Y3 = YV + XZ*SANGL
      Y4 = HT*CANGL
      Y2 = YV + Y4
      Y4 = Y3 + Y4
      YL = AMIN1(YV,Y2,Y3,Y4) + Y0T
      YU = AMAX1(YV,Y2,Y3,Y4) + Y0T
      IF (YL.LT.YTB1.OR.YU.GT.YTB2) GO TO 1
      TEMP = .TRUE.
    1 DFX405 = TEMP
      RETURN
      END
