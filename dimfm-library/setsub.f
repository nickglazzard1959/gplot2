      SUBROUTINE SETSUB(SHT,SY)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc01.cmn'
      INCLUDE 'dfxc01s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'SETSUB'
      IF (SHT.LE.0.0) THEN
                    IF (ICHECK.GT.0) WRITE(ERRREC,100) SHT,SY,SUBH,SUBY
      CALL DFX130(0)
  100 FORMAT(1H0,'**DIMFILM WARNING**  SETSUB CALLED WITH INVALID HEIGHT
     1, CALLED PARAMETERS WERE ',1P2E16.8/1H ,21X,'CURRENT VALUES (',
     2 1P2E16.8,') RETAINED')
      ELSE
                    SUBH = SHT
                    SUBY = SY
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
