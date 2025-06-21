      SUBROUTINE INTERP(STEP)
      INTEGER STEP
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'INTERP'
      IXSTEP = STEP
      IF (IXSTEP.LT.2) GO TO 10
    1 CONTINUE
      ROUTIN = STARS6
      RETURN
   10 IF (ICHECK.GT.0) WRITE(ERRREC,11) STEP
      CALL DFX130(0)
   11 FORMAT(1H0,'**DIMFILM WARNING**  INTERPOLATION STEP MUST BE GREATE
     1R THAN 1, CALL WAS MADE WITH ILLEGAL VALUE - ',I10/1H ,21X,
     2    'DEFAULT OF 5 WILL BE USED')
      IXSTEP = 5
      GO TO 1
      END
C
C----------------------------------------------
C
