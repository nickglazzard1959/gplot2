      INTEGER FUNCTION STSLICE(AIN,ISTART,IEND,AOUT)
C---------------------------------------------------------(STRING)------
C OBTAIN SLICE AIN(ISTART:IEND) INCLUSIVE OF IEND.
C RETURN THE NUMBER OF CHARACTERS IN THE SLICE.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER AIN(KSTDIM), ISTART, IEND, AOUT(KSTDIM)
C----
      INTEGER STLEN
C
      INTEGER L, I, K
C
      STSLICE = 0
      L = STLEN(AIN)
      IF( ISTART .LT. 1 .OR. ISTART .GT. L )GOTO 9
      IEND = MIN(IEND, L)
      K = 0
      DO 1 I=ISTART,IEND
         K = K + 1
         AOUT(K) = AIN(I)
 1    CONTINUE
      STSLICE = K
      K = K + 1
      AOUT(K) = KSTEND
 9    CONTINUE
      RETURN
      END
C
      INTEGER FUNCTION STSLICX(AIN,ISTART,IEND,AOUT)
C---------------------------------------------------------(STRING)------
C OBTAIN SLICE AIN(ISTART:IEND) EXCLUSIVE OF IEND.
C RETURN THE NUMBER OF CHARACTERS IN THE SLICE.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER AIN(KSTDIM), ISTART, IEND, AOUT(KSTDIM)
C----
      INTEGER STSLICE
C
      STSLICX = STSLICE(AIN,ISTART,IEND-1,AOUT)
      RETURN
      END
C
      INTEGER FUNCTION STSTRIP(AIN,AOUT)
C---------------------------------------------------------(STRING)------
C REMOVE ANY LEADING AND TRAILING BLANKS FROM AIN, RESULT TO AOUT.
C RETURN THE NUMBER OF CHARACTERS IN THE STRIPPED RESULT.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER AIN(KSTDIM), AOUT(KSTDIM)
C----
      INTEGER STSLICE, STLEN
C
      INTEGER L, I, IGO, IEND
C
      STSTRIP = 0
      L = STLEN(AIN)
      DO 1 I=1,L
         IF( AIN(I) .NE. 32 )THEN
            IGO = I
            GOTO 2
         ENDIF
 1    CONTINUE
      AOUT(1) = KSTEND
      RETURN
 2    CONTINUE
      DO 3 I=L,1,-1
         IF( AIN(I) .NE. 32 )THEN
            IEND = I
            GOTO 4
         ENDIF
 3    CONTINUE
 4    CONTINUE
      STSTRIP = STSLICE(AIN,IGO,IEND,AOUT)
      RETURN
      END
