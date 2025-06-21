      SUBROUTINE ALTPEN(I)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc01.cmn'
      INCLUDE 'dfxc01s.cmn'
      INCLUDE 'dfxc04.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      INCLUDE 'dfxc14.cmn'
      ROUTIN = 'ALTPEN'
      IF ((I.LE.0).OR.(I.GT.NRGB-1)) GO TO 1
      IALT = I
   99 CONTINUE
      ROUTIN = STARS6
      RETURN
    1 IF (ICHECK.GT.0) WRITE(ERRREC,10) I,NRGB-1,IALT
   10 FORMAT(1H0,'**DIMFILM WARNING** ALTPEN ATTEMPTED TO SET PEN ',I3,
     1 ', OUTSIDE RANGE 1 TO ',I1,' - CURRENT VALUE ',I1,' UNALTERED')
      CALL DFX130(0)
      GO TO 99
      END
C
C----------------------------------------------
C
