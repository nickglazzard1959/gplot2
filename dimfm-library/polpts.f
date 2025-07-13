      SUBROUTINE POLPTS(R,THETA,N,NCHAR)
      REAL R(N),THETA(N)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'POLPTS'
      IF (NCHAR.LT.1.OR.NCHAR.GT.48) THEN
              IF (ICHECK.GT.0) WRITE(ERRREC,21) NCHAR
   21 FORMAT(1H0,'**DIMFILM WARNING** POLPTS CALLED WITH INVALID FOURTH
     1PARAMETER ',I4,' SHOULD BE IN RANGE 1 - 48'/1H ,21X,'CALL IGNORED'
     2/)
      CALL DFX130(0)
      ELSE
C   FLAG POINTS
              NDEG = -1
              INTSAV = INTCH
              INTCH = 0
              INSAVE = INTEND
              INTEND = 1
              ICHSAV = IICHAR
              IICHAR = NCHAR
              CALL DFX339(R,THETA,N)
              NDEG = 0
              INTEND = INSAVE
              IICHAR = ICHSAV
              INTCH = INTSAV
      IF (IMM) CALL DFX000(-6,DUMMY,DUMMY,DUMMY,NDUMMY)
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
