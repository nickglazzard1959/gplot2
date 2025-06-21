      SUBROUTINE DFX101(X,Y)
      LOGICAL CUTS, CUT
      SAVE CUTS,DX1,X1,Y1
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      DATA CUTS/.FALSE./
      A = X - XPOS
      B = Y - YPOS
      DX = A*A + B*B
      IF (DX.EQ.0.0) RETURN
      IF (.NOT.CUTS) GO TO 1
      IF (DX.GE.DX1) RETURN
    1 CUTS = .TRUE.
      X1 = X
      Y1 = Y
      DX1 = DX
      RETURN
      ENTRY DFX102(CUT,X,Y)
      CUT = CUTS
      CUTS = .FALSE.
      IF (.NOT.CUT) RETURN
      X = X1
      Y = Y1
      RETURN
      END
C
C----------------------------------------------
C
      LOGICAL FUNCTION DFX103(Z,Z1,Z2)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      DFX103 = (Z.GE.Z1).AND.(Z.LE.Z2)
      RETURN
      END
C
C----------------------------------------------
C
