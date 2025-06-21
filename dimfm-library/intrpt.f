      SUBROUTINE INTRPT(N,NCHAR)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'INTRPT'
      IF (N.LT.1.OR.NCHAR.LT.1.OR.NCHAR.GT.48) THEN
                  IF (ICHECK.GT.0) THEN
                              WRITE(ERRREC,2) N, NCHAR
      CALL DFX130(0)
    2 FORMAT(1H0, 58H**DIMFILM WARNING**  INTRPT CALLED WITH ILLEGAL ARG
     1UMENTS ,2I10/1H ,21X, 17HTHIS CALL IGNORED)
                  ENDIF
      ELSE
                  INTEND = N
                  IICHAR = NCHAR
                  INTCH = 0
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
