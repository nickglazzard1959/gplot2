      SUBROUTINE DFX340(X,Y,N,NCHAR)
      REAL X(N),Y(N)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      INCLUDE 'dfxc09.cmn'
      OK = .FALSE.
      IF (NCHAR.LT.1.OR.NCHAR.GT.48.OR.N.LT.2) GO TO 1
      OK = .TRUE.
C    FLAG POINTS ONLY
      NDEG = -1
      INTSAV = INTCH
      INTCH = 0
      INSAVE = INTEND
      INTEND = 1
      ICHSAV = IICHAR
      IICHAR = NCHAR
      CALL DFX336(X,Y,N)
      NDEG = 0
      INTEND = INSAVE
      IICHAR = ICHSAV
      INTCH = INTSAV
    3 CONTINUE
      RETURN
    1 IF (ICHECK.GT.0) WRITE(ERRREC,2)ROUTIN,N,NCHAR
    2 FORMAT(1H0,'**DIMFILM WARNING**  ',A,' CALLED WITH ILLEGAL ARGUMEN
     1T, NUMBER OF POINTS (',I3,') MUST EXCEED 1 AND'/1H ,21X,'CHARACTER
     2 (',I3,') MUST BE IN RANGE 1 - 48'/1H ,21X,'CALL IGNORED')
      CALL DFX130(0)
      GO TO 3
      END
C
C----------------------------------------------
C
