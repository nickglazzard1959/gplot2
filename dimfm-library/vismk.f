      SUBROUTINE VISMK(I)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'VISMK'
      IF (I.LT.1.OR.I.GT.3) THEN
                  IF (ICHECK.GE.1) WRITE(ERRREC,11) I,MKVIS
   11 FORMAT(1H0,'**DIMFILM WARNING** VISMK CALLED WITH INVALID PARAMETE
     1R ',I3,' CURRENT VALUE ',I1,' RETAINED')
      CALL DFX130(0)
      ELSE
                  MKVIS = I
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
