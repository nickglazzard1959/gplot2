      SUBROUTINE DFX150(NDEG,X,Y,COEFF,ISING)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      REAL COEFF(6),X(6),Y(6),A(36)
C    NDEG CAN HAVE VALUES 3 OR 5 ONLY IN PRESENT FORM OF GRAPH ROUTINE
      N = NDEG + 1
      DO 1 I=1,N
      A(I) = 1.0
      Z = X(I)
      ZZ = X(I)
      K = 0
      DO 2 J=1,NDEG
      K = K + N
      A(I+K) = Z
    2 Z = Z*ZZ
    1 CONTINUE
      DO 14 I=1,N
   14 COEFF(I) = Y(I)
C    FORWARD SOLUTION
      TOL = 0.0
      ISING = 0
      JJ = -N
      DO 3 J=1,N
      JY = J + 1
      JJ = JJ + N + 1
      BIGA = 0.0
      IT = JJ - J
      DO 4 I=J,N
C    SEARCH FOR MAXIMUM COEFFICIENT IN COLUMN
      IJ = IT + I
      IF (ABS(BIGA).GE.ABS(A(IJ))) GO TO 4
      BIGA = A(IJ)
      IMAX = I
    4 CONTINUE
C    TEST FOR PIVOT LESS THAN TOLERANCE (SINGULAR MATRIX)
      IF (ABS(BIGA).GT.TOL) GO TO 7
      ISING = 1
      IF (ICHECK.GT.0) WRITE(ERRREC,13)
      CALL DFX130(0)
   13 FORMAT(1H0, 71H**DIMFILM WARNING**  SINGULARITY OCCURRED DURING AT
     1TEMPT TO INTERPOLATE/1H ,21X, 18HCALL TO POLY ENDED)
      RETURN
C    INTERCHANGE ROWS IF NECESSARY
    7 I1 = J + N*(J - 2)
      IT = IMAX - J
      DO 8 K=J,N
      I1 = I1 + N
      I2 = I1 + IT
      SAVE = A(I1)
      A(I1) = A(I2)
      A(I2) = SAVE
C    DIVIDE EQUATION BY LEADING COEFFICIENT
    8 A(I1) = A(I1)/BIGA
      SAVE = COEFF(IMAX)
      COEFF(IMAX) = COEFF(J)
      COEFF(J) = SAVE/BIGA
C    ELIMINATE NEXT VARIABLE
      IF (J.EQ.N) GO TO 10
      IQS = N*(J - 1)
      DO 3 IX = JY,N
      IXJ = IQS + IX
      IT = J - IX
      DO 11 JX = JY,N
      IXJX = N*(JX - 1) + IX
      JJX = IXJX + IT
   11 A(IXJX) = A(IXJX) - (A(IXJ)*A(JJX))
    3 COEFF(IX) = COEFF(IX) - (COEFF(J)*A(IXJ))
C    BACK SOLUTION
   10 NY = N - 1
      IT = N*N
      DO 12 J=1,NY
      IA = IT - J
      IB = N - J
      IC = N
      DO 12 K=1,J
      COEFF(IB) = COEFF(IB) - A(IA)*COEFF(IC)
      IA = IA - N
   12 IC = IC - 1
      RETURN
      END
