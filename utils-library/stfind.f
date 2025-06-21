      INTEGER FUNCTION STFIND(AIN,AFIND,ISTART)
C-------------------------------------------------------(STRING)--------
C LOOK FOR AFIND IN AIN STARTING AT CHARACTER ISTART.
C RETURN THE INDEX OF THE CHARACTER WHERE AFIND IS FOUND OR 0 IF IT IS
C NOT FOUND.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER AIN(KSTDIM), AFIND(KSTDIM), ISTART
C----
      INTEGER STLEN, STATOC
C
      INTEGER LAIN, LAFIND, MAXEND, I, J, K
      CHARACTER*160 CS
C
C---- SEE IF A MATCH IS POSSIBLE BASED ON LENGTHS.
      STFIND = 0
      LAIN = STLEN(AIN)
      LAFIND = STLEN(AFIND)
      MAXEND = LAIN - LAFIND + 1
      IF( (MAXEND - ISTART) .LT. 0 )THEN
         STFIND = 0
         RETURN
      ENDIF
C
C---- BRUTE FORCE MATCH
      DO 1 I=ISTART,MAXEND
         DO 2 J=1,LAFIND
            K = I + J - 1
            IF( AIN(K) .NE. AFIND(J) )GOTO 1
 2       CONTINUE
C
C---- MATCH AT POSITION I IN AIN.
         STFIND = I
         RETURN
 1    CONTINUE
C
C---- NO MATCH
      STFIND = 0
      RETURN
      END
C
      INTEGER FUNCTION STFINDR(AIN,AFIND)
C-------------------------------------------------------(STRING)--------
C LOOK FOR AFIND IN AIN BACKWARDS STARTING AT THE END OF AIN.
C RETURN 0 IF NOT FOUND, OR THE CHARACTER INDEX AT WHICH IT WAS FOUND.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER AIN(KSTDIM), AFIND(KSTDIM)
C----
      INTEGER STLEN
C
      INTEGER LAIN, LAFIND, MAXEND, I, J, K
C
      LAIN = STLEN(AIN)
      LAFIND = STLEN(AFIND)
      MAXEND = LAIN - LAFIND + 1
      IF( MAXEND .LE. 0 )THEN
         STFINDR = 0
         RETURN
      ENDIF
C
C---- BRUTE FORCE MATCH STARTING FROM MAXEND BACK TO 1
      DO 1 I=MAXEND,1,-1
         DO 2 J=1,LAFIND
            K = I + J - 1
            IF( AFIND(J) .NE. AIN(K) )GOTO 1
 2       CONTINUE
C
C---- MATCH STARTING AT I
         STFINDR = I
         RETURN
 1    CONTINUE
C
C---- NO MATCH
      STFINDR = 0
      RETURN
      END
C
      LOGICAL FUNCTION STATBEG(AIN,AFIND)
C-------------------------------------------------------(STRING)--------
C IF AFIND IS AT THE BEGINNING OF AIN, RETURN TRUE, ELSE FALSE.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER AIN(KSTDIM), AFIND(KSTDIM)
C----
      INTEGER STFIND
C
      STATBEG = STFIND(AIN,AFIND) .EQ. 1
      RETURN
      END
C
      LOGICAL FUNCTION STATEND(AIN,AFIND)
C-------------------------------------------------------(STRING)--------
C IF AFIND IS A T THE END OF AIN, RETURN TRUE, ELSE FALSE.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER AIN(KSTDIM), AFIND(KSTDIM)
C----
      INTEGER STLEN, STFINDR
C
      INTEGER L, LF
C
      L = STLEN(AIN)
      LF = STLEN(AFIND)
      STATEND = STFINDR(AIN,AFIND) .EQ. (L-LF+1)
      RETURN
      END
