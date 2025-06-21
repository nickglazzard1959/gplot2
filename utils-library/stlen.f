      INTEGER FUNCTION STLEN( AIN )
C-----------------------------------------------------(STRING)----------
C RETURN THE LENGTH OF AN ASCII CODE STRING, AIN.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER AIN(KSTDIM)
C----
      INTEGER I, LEN
C
      LEN = 0
      DO 1 I=1,KSTMAX
         IF( AIN(I) .EQ. KSTEND )GOTO 2
         LEN = LEN + 1
 1    CONTINUE
 2    CONTINUE
      STLEN = LEN
      RETURN
      END
