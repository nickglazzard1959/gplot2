      SUBROUTINE RRANGE(R1,R2)
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc03.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'RRANGE'
      IF (R1.LT.0.0.OR.R2.LE.R1) THEN
               FIXR = .FALSE.
               IF (ICHECK.GT.0) WRITE(ERRREC,3)R1,R2
    3 FORMAT(1H0,'**DIMFILM WARNING**  INVALID ARGUMENTS IN CALL TO RRAN
     1GE - ',1P2E16.8/1H ,21X,'BOTH SHOULD BE NON-NEGATIVE WITH THE SECO
     2ND GREATER THAN THE FIRST'/1H ,21X,'AUTOR WILL BE PERFORMED')
      CALL DFX130(0)
      ELSE
               FIXR = .TRUE.
               RGL = R1
               RGR = R2
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
