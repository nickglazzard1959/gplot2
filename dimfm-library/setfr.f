      SUBROUTINE SETFR(FHT,FOFF,FGAP)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc01.cmn'
      INCLUDE 'dfxc01s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'SETFR'
      IF ((FHT.LE.0.0).OR.(FGAP.LE.0.0)) THEN
                 IF (ICHECK.GT.0) WRITE(ERRREC,100) FHT,FOFF,
     1                                         FGAP,FRACHT,FRACM,FRACGP
      CALL DFX130(0)
  100 FORMAT(1H0,'**DIMFILM WARNING**  SETFR CALLED WITH INVALID HEIGHT/
     1GAP, CALLED PARAMETERS WERE ',1P3E16.8/1H ,21X,'CURRENT VALUES (',
     2 1P3E16.8,') RETAINED')
      ELSE
                 FRACHT = FHT
                 FRACM = FOFF
                 FRACGP = FGAP
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
