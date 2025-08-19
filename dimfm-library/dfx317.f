      SUBROUTINE DFX317(I1,T,Z)
C NG: DEALS WITH MONTH VALUES, I THINK.
      INCLUDE 'dfxc06.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      LOGICAL LAB,LEFT
      LOGICAL GRID
C      PRINT *,'DFX317'
      CALL DFX300
      GRID = .FALSE.
      I = I1
      LAB = .FALSE.
      IF (I.LT.20) GO TO 61
      LAB = .TRUE.
      LEFT = .TRUE.
      I = I - 20
      IF (I.LT.10) GO TO 60
      LEFT = .FALSE.
      I = I - 10
      GO TO 60
   61 IF (I.LT.10) GO TO 60
      GRID = .TRUE.
      I = I - 10
   60 XDD = XTB2 - XTB1
      YDD = YTB2 - YTB1
      A = DC1*XDD
      B = DC1*YDD
      GO TO (1,2),I
    1 ZD = A
      ZS = XTB1 + DC2*(XTB2-XTB1)
      ZMONTH = XMONTH
      Y1 = Z
      MONZ = MONX
      IF (MONZ.LT.0) MONZ = -MONZ
      TIK1 = B
      GO TO 3
    2 ZD = B
      ZS = YTB1 + DC2*(YTB2-YTB1)
      ZMONTH = YMONTH
      MONZ = MONY
      IF (MONZ.LT.0) MONZ = -MONZ
      X1 = Z
      TIK1 = A
    3 S = AMIN1((XTB2-XTB1),(YTB2-YTB1))
      TIK = SIGN(TC1*S,T)
      IF (GRID) TIK = TIK1
      GAP = ZMONTH - AINT(ZMONTH)
      IF (GAP.LE.0.0) MONZ = MONZ - 1
      MJ = 1
      IF (MONZ.GT.12) MJ = 2
      IF (MONZ.GT.24) MJ = 4
      IF (MONZ.GT.36) MJ = 6
      IF (MONZ.GT.60) MJ = 12
      ZJ = FLOAT(MJ)
      ZINT = ZJ*ZD/FLOAT(MONZ)
      IF (LAB) GO TO 62
C    NOTE..TICKING MONTH AXIS REQUIRES NO TICK AT EXTREMA
      M = MONZ - 1
      GO TO (11,12),I
   11 X1 = ZS
      DO 14 II=1,M,MJ
      X1 = X1 + ZINT
      CALL DFX110(X1,Y1)
      CALL DFX106(X1,Y1+TIK)
   14 CONTINUE
      GO TO 100
   12 Y1 = ZS
      DO 16 II=1,M,MJ
      Y1 = Y1 + ZINT
      CALL DFX110(X1,Y1)
      CALL DFX106(X1+TIK,Y1)
   16 CONTINUE
      GO TO 100
   62 IF (.NOT.AXLAB) GO TO 63
      GO TO (64,65),I
   64 Y0T = Z - YTB1 - DC2*YDD
      IF (.NOT.LEFT) Y0T = Z - YTB2 + DC2*YDD
      GO TO 63
   65 X0T = Z - XTB1 - DC2*XDD
      IF (.NOT.LEFT) X0T = Z - XTB2 + DC2*XDD
   63 CALL DFX137
C    FORCE TRANSFORM
      I = I + 20
      IF (.NOT.LEFT) I = I + 10
      GAP = AINT(ZMONTH)
      CALL DFX318(I,GAP,ZINT,ZS,MJ,MONZ)
  100 CALL DFX301
      RETURN
      END
