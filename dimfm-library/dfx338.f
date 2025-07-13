      SUBROUTINE DFX338
      INCLUDE 'dfxc06.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      DDX = DC2*(XTB2-XTB1)
      DDY = DC2*(YTB2-YTB1)
      X1 = XTB1 + DDX
      X2 = XTB2 - DDX
      Y1 = YTB1 + DDY
      Y2 = YTB2 - DDY
      CALL DFX300
C    SET COLOUR/INTENSITY
      CALL DFX147(3)
      CALL DFX110(X1,Y1)
      CALL DFX106(X2,Y1)
      CALL DFX106(X2,Y2)
      CALL DFX106(X1,Y2)
      CALL DFX106(X1,Y1)
      CALL DFX301
      RETURN
      END
