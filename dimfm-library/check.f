      SUBROUTINE CHECK(I)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'CHECK'
      I1 = I
      IF (IABS(I).LE.3) GO TO 2
      I1 = ISIGN(3,I)
      IF (ICHECK.GT.0) WRITE(ERRREC,1) I, I1
      CALL DFX130(0)
    1 FORMAT(1H0,'**DIMFILM WARNING**  ILLEGAL PARAMETER IN CALL TO CHEC
     1K'/1H ,21X,'PARAMETER WAS ',I6,' DEFAULT OF ',I6,' USED')
    2 ICHECK = I1
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
