      SUBROUTINE DFX123(THETA1,GAP)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc03.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      IF (GAP.LE.0.0) GO TO 99
      THETA = THETA1
      IF (ANGRP(1).NE.1) THETA = THETA1*ANGCON(1,ANGRP(1))*CON1
C     GET THETA INTO DEGREES
      ICSAVE = ICHECK
      ICHECK = -1
      CALL DFX300
      IF (ABS(THETA).GT.90.0) GO TO 1
    7 IF (ABS(THETA).EQ.90.0) GO TO 2
      IF (THETA.EQ.0.0) GO TO 3
      TH = THETA*ANGCON(1,ANGRP(1))
      XD = XTB2 - XTB1
      YD = YTB2 - YTB1
      DX = GAP/SIN(ABS(TH))
      DDX = YD/TAN(ABS(TH))
      X1 = XTB1 - DDX
      X2 = XTB2
      X3 = XTB1
      IF (THETA.LT.0.0) GO TO 4
      Y1 = YTB1
      Y3 = YTB2
    5 X1 = X1 + DX
      X3 = X3 + DX
      IF (X1.GE.X2) GO TO 6
      CALL DFX110(X1,Y1)
      CALL DFX106(X3,Y3)
      GO TO 5
    4 Y1 = YTB2
      Y3 = YTB1
      GO TO 5
    1 T = AMOD(THETA,360.0)
      IF (T.LT.0.0) T = T + 360.0
      IF (T.GT.90.0) T = T - 180.0
      IF (T.GT.90.0) T = T - 180.0
C    REPEATED STATEMENT NECESSARY TO GET RESULT IN QUADRANTS 1 OR 4
      THETA = T
      GO TO 7
    2 X1 = XTB1
      X3 = XTB1
      X2 = XTB2
      Y1 = YTB1
      Y3 = YTB2
      DX = GAP
      GO TO 5
    3 X1 = XTB1
      X3 = XTB2
      Y1 = YTB1
      Y3 = YTB1
      Y2 = YTB2
    8 Y1 = Y1 + GAP
      Y3 = Y3 + GAP
      IF (Y1.GE.Y2) GO TO 6
      CALL DFX110(X1,Y1)
      CALL DFX106(X3,Y3)
      GO TO 8
    6 CALL DFX301
      ICHECK = ICSAVE
   99 CONTINUE
      RETURN
      END
C
C----------------------------------------------
C
