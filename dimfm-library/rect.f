      SUBROUTINE RECT(X0,Y0,ANGLE,SIDE1,SIDE2)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      REAL X(4), Y(4)
      INCLUDE 'dfxc03.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      LOGICAL OUT
      ROUTIN = 'RECT'
      X(1) = X0
      Y(1) = Y0
      ALPHA1 = ANGLE*ANGCON(1,ANGRP(4))
      C = COS(ALPHA1)
      S = SIN(ALPHA1)
      X(2) = SIDE1*C + X(1)
      Y(2) = SIDE1*S + Y(1)
      X(3) = X(2) - SIDE2*S
      Y(3) = Y(2) + SIDE2*C
      X(4) = X(1) - SIDE2*S
      Y(4) = Y(1) + SIDE2*C
      CALL DFX503(.TRUE.,X,Y,4,OUT)
      IF (.NOT.OUT) GO TO 99
      IF (ICHECK.NE.2) GO TO 99
      WRITE(ERRREC,20) ROUTIN,X0,Y0,ANGLE,SIDE1,SIDE2
      CALL DFX130(1)
   20 FORMAT(1H0,'**DIMFILM WARNING**  OUT OF BOUNDS DETECTED DURING CAL
     1L OF ',A,', CALLED WITH -'/1H ,21X,5(E16.8,2X))
   99 CONTINUE
      IF (IMM) CALL DFX000(-6,DUMMY,DUMMY,DUMMY,NDUMMY)
      ROUTIN = STARS6
      RETURN
      END
