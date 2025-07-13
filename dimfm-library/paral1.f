      SUBROUTINE PARAL1(X1,Y1,X2,Y2,X3,Y3)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      REAL X(4),Y(4)
      LOGICAL OUT
      ROUTIN = 'PARAL1'
      ICSAVE = ICHECK
      IF (ICHECK.EQ.2) ICHECK = -2
      X(1) = X1
      Y(1) = Y1
      X(2) = X2
      Y(2) = Y2
      X(3) = X3
      Y(3) = Y3
      X(4) = X1 + X3 - X2
      Y(4) = Y1 + Y3 - Y2
      CALL DFX503(.TRUE.,X,Y,4,OUT)
      ICHECK = ICSAVE
      IF (.NOT.OUT) GO TO 99
      IF (ICHECK.EQ.2) WRITE(ERRREC,20)X1,Y1,X2,Y2,X3,Y3
      CALL DFX130(1)
   20 FORMAT(1H0,'**DIMFILM WARNING**  OUT OF BOUNDS DETECTED DURING CAL
     1L OF PARAL1, CALLED WITH -'/1H ,21X,6(E16.8,2X))
   99 CONTINUE
      IF (IMM) CALL DFX000(-6,DUMMY,DUMMY,DUMMY,NDUMMY)
      ROUTIN = STARS6
      RETURN
      END
