      SUBROUTINE DFX161(N,IERR)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc04.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      INCLUDE 'dfxc17.cmn'
      IF (N.LT.0.OR.N.GE.LUTSIZ) GO TO 1
      IERR = 0
    2 RETURN
    1 IF (ICHECK.GT.0) WRITE(ERRREC,10)ROUTIN,N
      CALL DFX130(0)
   10 FORMAT(1H0,'**DIMFILM WARNING**  ROUTINE ',A,' REFERENCED WI
     1TH INVALID LUT POINTER ',I3,'; CALL IGNORED')
      IERR = 1
      GO TO 2
      END
C
C----------------------------------------------
C
