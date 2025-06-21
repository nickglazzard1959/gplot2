      INTEGER FUNCTION STLNBC(S,B)
C-------------------------------------------------------(STRING)-------
C FIND LAST NON BLANK CHARACTER IN S FROM END BACK TO B.
C BLANK IS SPACE OR TAB.
C RETURN 0 IF NOT FOUND. RETURN INDEX OF LAST BLANK OTHERWISE.
C----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER S(KSTDIM), B
C----
      INTEGER STLEN
C
      INTEGER J, I, L, BB
C
      STLNBC = 0
      L = STLEN(S)
      BB = MAX( B, 1 )
      DO 1 I = L, BB, -1
         J = S(I)
         IF (J .NE. 32 .AND. J .NE. 9) THEN
            STLNBC = I
            GOTO 2
         ENDIF
1     CONTINUE
2     RETURN
      END
C
      INTEGER FUNCTION STNNBC(S,B)
C-------------------------------------------------------(STRING)-------
C FIND NEXT NON BLANK CHARACTER IN S STARTING AT INDEX B.
C RETURN 0 IF NO BLANK FOUND. RETURN INDEX OF WHERE FOUND OTHERWISE.
C----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER S(KSTDIM), B
C----
      INTEGER STLEN
C
      INTEGER I, J, BB, L
C
      STNNBC = 0
      L = STLEN(S)
      BB = MAX( B, 1 )
      DO 1 I = BB, L
         J = S(I)
         IF ( J .NE. 32 .AND. J .NE. 9) THEN
            STNNBC = I
            GOTO 2
         ENDIF
1     CONTINUE
2     RETURN
      END
C
      INTEGER FUNCTION STNOCC(S,C,B)
C-------------------------------------------------------(STRING)-------
C FIND NEXT MEMBER OF CHARACTER CLASS (ANY CHARACTER IN C).
C SEARCH S FROM B TO END FOR ANY CHARACTER IN C.
C RETURN 0 IF NON FOUND, OR INDEX WHERE FOUND OTHERWISE.
C----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER S(KSTDIM), C(KSTDIM)
      INTEGER B
C----
      INTEGER STLEN
C
      INTEGER I, J, BB, SC, L, LC
C
      STNOCC = 0
      L = STLEN(S)
      LC = STLEN(C)
      BB = MAX( B, 1 )
      DO 1 I = BB, L
         SC = S(I)
         DO 3 J = 1, LC
            IF ( C(J) .EQ. SC )THEN
               STNOCC = I
               GOTO 2
            ENDIF
3        CONTINUE
1     CONTINUE
2     RETURN
      END
C
      INTEGER FUNCTION STLOCC(S,C,B)
C-------------------------------------------------------(STRING)-------
C FIND LOCATION OF LAST MEMBER OF CHARACTER CLASS (ANY CHARACTER IN C).
C SEARCH S FROM END TO B FOR ANY CHARACTER IN C.
C RETURN 0 IF NOT FOUND, OR INDEX WHERE FOUND OTHERWISE.
C----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER S(KSTDIM), C(KSTDIM)
      INTEGER B
C----
      INTEGER STLEN
C
      INTEGER I, J, BB, SC, L, LC
C
      STLOCC = 0
      L = STLEN(S)
      LC = STLEN(C)
      BB = MAX( B, 1 )
      DO 1 I = L, BB, -1
         SC = S(I)
         DO 3 J = 1, LC
            IF( C(J) .EQ. SC )THEN
               STLOCC = I
               GOTO 2
            ENDIF
3        CONTINUE
1     CONTINUE
2     RETURN
      END
C
      INTEGER FUNCTION STNBC(S,B)
C-------------------------------------------------------(STRING)-------
C FIND LOCATION OF NEXT BLANK CHARACTER.
C SEARCH S FROM B TO END FOR A BLANK.
C RETURN 0 IF NOT FOUND, INDEX OF WHERE FOUND OTHERWISE.
C----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER S(KSTDIM)
      INTEGER B
C----
      INTEGER STLEN
      INTEGER I, J, BB, L
C
      STNBC = 0
      L = STLEN(S)
      BB = MAX( B, 1 )
      DO 1 I = BB, L
         J = S(I)
         IF( J .EQ. 32 .OR. J .EQ. 9 )THEN
            STNBC = I
            GOTO 2
         ENDIF
1     CONTINUE
2     RETURN
      END
C
      INTEGER FUNCTION STLBC(S,B)
C--------------------------------------------------------(STRING)------
C FIND LOCATION OF LAST BLANK CHARACTER IN S.
C SEARCH S FROM END BACK KTO B FOR A BLANK.
C RETURN 0 IF NOT FOUND, INDEX WHERE FOUND OTHERWISE.
C----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER S(KSTDIM)
      INTEGER B
C----
      INTEGER STLEN
      INTEGER I, J, BB, L
C
      STLBC = 0
      L = STLEN(S)
      BB = MAX( B, 1 )
      DO 1 I = L, BB, -1
         J = S(I)
         IF( J .EQ. 32 .OR. J .EQ. 9 )THEN
            STLBC = I
            GOTO 2
         ENDIF
1     CONTINUE
2     RETURN
      END
C
      INTEGER FUNCTION STFILL(AFILL, AOUT)
C--------------------------------------------------------(STRING)-------
C FILL AOUT BY REPEATING THE CONTENTS OF AFILL.
C RETURN 0 IF OK.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER AFILL(KSTDIM), AOUT(KSTDIM)
C----
      INTEGER STLEN
C
      INTEGER I, J, L
C
      STFILL = 0
      L = STLEN(AFILL)
      DO 1 I=1,KSTMAX
         J = MOD(I,L) + 1
         AOUT(I) = AFILL(J)
 1    CONTINUE
      AOUT(KSTDIM) = KSTEND
      RETURN
      END
