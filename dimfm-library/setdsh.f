      SUBROUTINE SETDSH(X,N)
      REAL X(N)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'SETDSH'
      N3 = N
      IF (N3.LT.1) GO TO 6
      IF (N3.GT.10) GO TO 2
    3 N4 = N3
      IF (N.GT.10) N4 = 10
      TL4 = 0.0
      DO 1 I=1,N4
      TL4 = TL4 + ABS(X(I))
    1 XLINE(I) = X(I)*XQ
      IF (IDASH.EQ.4) THEN
C    IF OWNDSH IN OPERATION - RESET TO BEGINNING OF PATTERN
                   NO1 = 1
                   NO2 = N4
                   TL = TL4*XQ
                   CALL DFX105
      ENDIF
   10 CONTINUE
      ROUTIN = STARS6
      RETURN
    6 IF (ICHECK.GT.0) WRITE(ERRREC,4) N3
      CALL DFX130(0)
    4 FORMAT(1H0,'**DIMFILM WARNING**  SETDSH CALLED WITH ILLEGAL SECOND
     1 ARGUMENT ',I4/1H ,21X,'CALL IGNORED')
      GO TO 10
    2 IF (ICHECK.GT.0) WRITE(ERRREC,5) N3
      CALL DFX130(0)
    5 FORMAT(1H0,'**DIMFILM WARNING**  SETDSH CALLED WITH ILLEGAL SECOND
     1 ARGUMENT ',I4/1H ,21X,'DEFAULT VALUE OF 10 WAS USED, LINE PATTERN
     2 WILL BE TRUNCATED')
      N3 = 10
      GO TO 3
      END
C
C----------------------------------------------
C
