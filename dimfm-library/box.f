      SUBROUTINE BOX(X1,X2,Y1,Y2)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      REAL X(4), Y(4)
      INCLUDE 'dfxc03.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      LOGICAL OUT
      ROUTIN = 'BOX'
      X(1) = X1
      X(2) = X2
      X(3) = X2
      X(4) = X1
      Y(1) = Y1
      Y(2) = Y1
      Y(3) = Y2
      Y(4) = Y2
      CALL DFX503(.TRUE.,X,Y,4,OUT)
      IF (.NOT.OUT) GO TO 99
      IF (ICHECK.NE.2) GO TO 99
      WRITE(ERRREC,20) ROUTIN,X(1),X(2),Y(1),Y(3)
      CALL DFX130(1)
   20 FORMAT(1H0,'**DIMFILM WARNING**  OUT OF BOUNDS DETECTED DURING CAL
     1L OF ',A,', CALLED WITH -'/1H ,21X,4(E16.8,2X))
   99 CONTINUE
      IF (IMM) CALL DFX000(-6,DUMMY,DUMMY,DUMMY,NDUMMY)
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
