      SUBROUTINE DFX321(DX,DY,A,I,SSH,CS)
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      REAL X(8),Y(8)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc06.cmn'
      REAL PART(7)
      INCLUDE 'dfxc03.cmn'
      SAVE X, Y, PART
      DATA PART/0.,0.,-1.,-1.,0.,0.,-1./
      DATA X/-1.,0.,-1.,-2.,-1.,-2.,-1.,0./
      DATA Y/-1.,-.5,0.,-.5,0.,-.5,-1.,-.5/
C    I=1 FOR BEGIN RADIUS, I=2 FOR END RADIUS
C    IN CASE OF ERROR RETURN 0.0
      DX = 0.0
      DY = 0.0
      CA = ABS(COS(A))
      SA = ABS(SIN(A))
      TIK = AMIN1(XTB2-XTB1,YTB2-YTB1)*TC1
      S = 0.0
      DO 1 II=1,5
      J = II
      IF (ABS(A-S).LE.DEGPRT) GO TO 2
      IF (A.GT.S.AND.A.LT.(S+PIBY2-DEGPRT)) GO TO 3
    1 S = S + PIBY2
C    HERE FOR NORMAL
    3 IF (J.GT.4) RETURN
      II = J
      J = II + (I-1)*2
      DX = PART(J)*2.*CS
      DX = DX + SA*TIK*(2.*PART(J)+1.)
      J = II + 3 - (I-1)*2
      DY = PART(J)*SSH
      DY = DY + CA*TIK*(2.*PART(J)+1.)
      RETURN
    2 IF (J.EQ.5) J = 1
      IF (J.EQ.1.OR.J.EQ.3) GO TO 4
C    HERE FOR 90/270
      DX = SSH*.5/ABS(TAN(A))
      DX = DX + TIK
      IF (I.NE.J/2) DX = -DX
      GO TO 5
    4 DY = CS*ABS(TAN(A))
      DY = DY + TIK
      K = I + J
      IF (K.EQ.2.OR.K.EQ.5) DY = -DY
    5 II = 4*(I-1) + J
      DX = X(II)*CS + DX
      DY = Y(II)*SSH + DY
      RETURN
      END
