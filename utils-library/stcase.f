      INTEGER FUNCTION STUPRCS(AIN, AOUT)
C---------------------------------------------------------(STRING)------
C CONVERT ALL CHARACTERS IS STRING AIN TO UPPER CASE IN AOUT.
C RETURN 0 IF OK.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER AIN(KSTDIM), AOUT(KSTDIM)
C----
      INTEGER STLEN
C
      INTEGER I, L, K, C
C
      STUPRCS = 0
      L = STLEN(AIN)
      K = 0
      DO 1 I=1,L
         C = AIN(I)
         IF( (C .GT. 96) .AND. (C .LT. 123) )C = C - 32
         AOUT(I) = C
         K = K + 1
 1    CONTINUE
      K = K + 1
      AOUT(K) = KSTEND
      RETURN
      END
C
      INTEGER FUNCTION STLWRCS(AIN, AOUT)
C---------------------------------------------------------(STRING)------
C CONVERT ALL CHARACTERS IS STRING AIN TO LOWER CASE IN AOUT.
C RETURN 0 IF OK.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER AIN(KSTDIM), AOUT(KSTDIM)
C----
      INTEGER STLEN
C
      INTEGER I, L, K, C
C
      STLWRCS = 0
      L = STLEN(AIN)
      K = 0
      DO 1 I=1,L
         C = AIN(I)
         IF( (C .GT. 64) .AND. (C .LT. 91) )C = C + 32
         AOUT(I) = C
         K = K + 1
 1    CONTINUE
      K = K + 1
      AOUT(K) = KSTEND
      RETURN
      END
C
      INTEGER FUNCTION STCASER(AIN, AOUT, DOFIRST)
C---------------------------------------------------------(STRING)------
C MAKE THE FIRST NON-BLANK CHARACTER IN AIN AFTER EVERY PERIOD UPPER
C CASE. ALSO THE FIRST CHARACTER OF THE LINE IF DOFIRST IS NOT 0.
C RETURN 1 IF THE LAST NON-BLANK IN THE OUTPUT IS A PERIOD, ELSE 0.
C EXCLAMATION MARKS AND QUESTION MARKS ARE TREATED THE SAME AS PERIODS.
C LOWER CASE I CHARACTERS WITH SPACES EACH SIDE ARE MADE UPPER CASE.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER AIN(KSTDIM), AOUT(KSTDIM), DOFIRST
C----
      INTEGER STLNBC, STNNBC, STREPLN, STCTOA
C
      INTEGER ATMP(KSTDIM), ALWRI(KSTDIM), AUPRI(KSTDIM)
      INTEGER ILNBC, IFNBC, I, C, K, N, ILC
      LOGICAL FAFP
C
      FAFP = .FALSE.
      STCASER = 0
C---- FIND FIRST AND LAST NON-BLANK CHARACTERS.
      IFNBC = STNNBC(AIN, 1)
      IF( IFNBC .EQ. 0 )RETURN
      ILNBC = STLNBC(AIN, 1)
C---- IF LAST NON-BLANK IS A PERIOD, RETURN 1. EVENTUALLY.
      ILC = AIN(ILNBC)
      IF( (ILC .EQ. 46) .OR. (ILC .EQ. 33) .OR. (ILC .EQ. 63) )
     +   STCASER = 1
C---- FILL THE OUTPUT UP TO ILNBC-1 WITH SPACES.
      K = 0
      IF( IFNBC .GT. 1 )THEN
         DO 2 I=1,IFNBC-1
         ATMP(I) = 32
         K = K + 1
 2       CONTINUE
      ENDIF
C---- PROCESS INPUT CHARACTERS FROM FIRST TO LAST NON-BLANK.
      DO 1 I=IFNBC, ILNBC
         C = AIN(I)
C---- MAKE THE CHARACTER LOWER CASE IF IT IS UPPER CASE.
         IF( (C .GT. 64) .AND. (C .LT. 91) )C = C + 32
C---- IF IT IS THE FIRST NON-BLANK AND DOFIRST IS 1, MAKE IT UPPER CASE.
         IF( (I .EQ. IFNBC) .AND. (DOFIRST .NE. 0) )THEN
            IF( (C .GT. 96) .AND. (C .LT. 123) )C = C - 32
         ENDIF
C---- IF IT IS THE FIRST NON-BLANK AFTER A PERIOD, MAKE IT UPPER CASE.
         IF( (C .NE. 32) .AND. (C .NE. 9) .AND. FAFP )THEN
            IF( (C .GT. 96) .AND. (C .LT. 123) )C = C - 32
            FAFP = .FALSE.
         ENDIF
C---- SET THE OUTPUT CHARACTER AND CHECK IF IT IS A PERIOD.
         ATMP(I) = C
         K = K + 1
         IF( .NOT. FAFP )FAFP = ((C .EQ. 46) .OR. (C .EQ. 33) .OR.
     +                                            (C .EQ. 63))
 1    CONTINUE
C---- TERMINATE THE STRING.
      K = K + 1
      ATMP(K) = KSTEND
C---- CAPITALIZE " I ".
      N = STCTOA( ' ^I ', ALWRI )
      N = STCTOA( ' I ', AUPRI )
      N = STREPLN(ATMP, ALWRI, AUPRI, 99, AOUT)
      RETURN
      END
