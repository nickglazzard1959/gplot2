      SUBROUTINE TRANGE(R1,R2)
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc03.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'TRANGE'
      THG1 = R1*ANGCON(1,ANGRP(2))
      THG2 = R2*ANGCON(1,ANGRP(2))
      IF (R2.LE.R1) THEN
                 FIXTH = .FALSE.
                 IF (ICHECK.GT.0) WRITE(ERRREC,4)R1,R2
    4 FORMAT(1H0,'**DIMFILM WARNING**  INVALID ARGUMENTS IN CALL TO TRAN
     1GE - ',1P2E16.8/1H ,21X,'THE SECOND SHOULD BE GREATER THAN THE FIR
     2ST'/1H ,21X,'AUTOT WILL BE PERFORMED')
      CALL DFX130(0)
      ELSE
                 IF ((THG2-THG1).LT.TWOPI) THEN
                            THGL = DFX004(THG1)
                            THGR = DFX004(THG2)
                 ELSE
                            THGL = 0.0
                            THGR = TWOPI
                 ENDIF
                 FIXTH = .TRUE.
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
