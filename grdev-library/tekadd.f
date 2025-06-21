      SUBROUTINE TEKPEN( X, Y, IPEN )
C---------------------------------------------------
C MOVE (IPEN=0) OR DRAW (IPEN=1) TO (X,Y)
C---------------------------------------------------
      REAL X, Y
      INTEGER IPEN
      INCLUDE 'tekcmn.cmn'
      REAL CX, CY
      INTEGER IX, IY
      CX = MAX(0.0,MIN(X,1.0))
      CY = MAX(0.0,MIN(Y,1.0))
      IX = NINT(1023.0*CX)
      IY = NINT(767.0*CY)
      CALL TEKDRAW( IX, IY, IPEN )
      RETURN
      END
