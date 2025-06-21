      INTEGER FUNCTION STCOPY(AIN,AOUT)
C--------------------------------------------------------(STRING)-------
C COPY AIN TO AOUT.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER AIN(KSTDIM), AOUT(KSTDIM)
C----
      INTEGER I, K
C
C---- COPY UP TO EOS. COUNT CHARACTERS COPIED, EXCLUDING EOS.
      K = 0
      DO 1 I=1,KSTMAX
         AOUT(I) = AIN(I)
         IF(AIN(I) .EQ. KSTEND)GOTO 2
         K = K + 1
 1    CONTINUE
      AOUT(KSTDIM) = KSTEND
 2    CONTINUE
      STCOPY = K
      RETURN
      END
C
      INTEGER FUNCTION STREPL1(AIN,AOLD,ANEW,AOUT)
C--------------------------------------------------------(STRING)-------
C REPLACE THE FIRST OCCURRENCE OF AOLD IN AIN WITH ANEW.
C RETURN THE INDEX OF THE CHARACTER WHERE AOLD WAS FOUND OR 0 IF NOT
C FOUND (AND THEREFORE NO REPLACEMENT MADE).
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER AIN(KSTDIM), AOLD(KSTDIM), ANEW(KSTDIM), AOUT(KSTDIM)
C----
      INTEGER STFIND, STLEN
C
      INTEGER IFOUND, IN, IOUT, LAOLD, LANEW, IFEND, RETSTAT
C
C---- FIND THE LENGTHS OF ALL THE INPUT STRINGS.
      LAOLD = STLEN(AOLD)
      LANEW = STLEN(ANEW)
C
C---- FIND THE STRING AOLD IN AIN. IF NOT FOUND, COPY AOUT = AIN.
      IFOUND = STFIND(AIN,AOLD,1)
      RETSTAT = IFOUND
      IF( IFOUND .EQ. 0 )IFOUND = KSTDIM
      IFEND = IFOUND + LAOLD
C
C---- ITERATE OVER THE POSSIBLE OUTPUT CHARACTERS.
      DO 1 IOUT=1,KSTMAX1
C---- -- REGION BEFORE AOLD FOUND.
         IF( IOUT .LT. IFOUND )THEN
            AOUT(IOUT) = AIN(IOUT)
            IF( AIN(IOUT) .EQ. KSTEND )GOTO 2
         ELSE
            IN = IOUT - IFOUND + 1
C---- ----- REGION WHERE REPLACING AOLD WITH ANEW.
            IF( IN .LE. LANEW )THEN
               AOUT(IOUT) = ANEW(IN)
C---- ----- REGION AFTER REPLACEMENT.
            ELSE
               AOUT(IOUT) = AIN(IFEND)
               IF( AOUT(IOUT) .EQ. KSTEND )GOTO 2
               IFEND = IFEND + 1
               IF( IFEND .GT. KSTMAX1 )GOTO 2
            ENDIF
         ENDIF
 1    CONTINUE
      AOUT(KSTDIM) = KSTEND
 2    CONTINUE
      STREPL1 = RETSTAT
      RETURN
      END
C
      INTEGER FUNCTION STREPLN(AIN,AOLD,ANEW,N,AOUT)
C----------------------------------------------------------(STRING)-----
C REPLACE THE FIRST N OCCURRENCES OF AOLD IN AIN WITH ANEW.
C RETURN THE NUMBER OF REPLACEMENTS MADE.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER AIN(KSTDIM), AOUT(KSTDIM), N, AOLD(KSTDIM), ANEW(KSTDIM)
C----
      INTEGER STCOPY, STREPL1
C
      INTEGER I, ATEMP(KSTDIM), ISTAT
C
C---- COPY AIN TO ATEMP
      ISTAT = STCOPY(AIN,ATEMP)
C
C---- REPLACE THE OCCURRENCE OF AOLD IN ATEMP WITH ANEW.
      DO 1 I=1,N
         IF( STREPL1(ATEMP,AOLD,ANEW,AOUT) .EQ. 0 )THEN
            STREPLN = I - 1
            RETURN
         ELSE
            ISTAT = STCOPY(AOUT,ATEMP)
         ENDIF
 1    CONTINUE
      STREPLN = N
      RETURN
      END
C
      INTEGER FUNCTION STADD(AIN,ANEW,AOUT)
C--------------------------------------------------------(STRING)-------
C ADD (CONCATENATE) ANEW ON THE END OF AIN.
C RETURN THE NUMBER OF CHARACTERS IN AOUT.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER AIN(KSTDIM), ANEW(KSTDIM), AOUT(KSTDIM)
C----
      INTEGER STLEN
C
      INTEGER LAIN, LANEW, I, J, K
C
      LAIN = STLEN(AIN)
      LANEW = STLEN(ANEW)
C
      K = 0
      J = 1
      DO 1 I=1,KSTMAX1
         IF( I .LE. LAIN )THEN
            AOUT(I) = AIN(I)
            K = K + 1
         ELSE
            IF( (ANEW(J) .EQ. KSTEND) .OR. (J .GT. LANEW) )GOTO 2
            AOUT(I) = ANEW(J)
            J = J + 1
            K = K + 1
         ENDIF
 1    CONTINUE
 2    CONTINUE
      AOUT(K+1) = KSTEND
      STADD = K
      RETURN
      END
C
      INTEGER FUNCTION STADDIP(AINOUT,ANEW)
C--------------------------------------------------------(STRING)-------
C ADD (CONCATENATE) ANEW ON THE END OF AIN, IN PLACE.
C RETURN THE NUMBER OF CHARACTERS IN AINOUT.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER AINOUT(KSTDIM), ANEW(KSTDIM)
C----
      INTEGER STLEN
C
      INTEGER LAIN, LANEW, I, J, K
C
      LAIN = STLEN(AINOUT)
      LANEW = STLEN(ANEW)
C
      K = 0
      J = 1
      DO 1 I=1,KSTMAX1
         IF( I .LE. LAIN )THEN
            K = K + 1
         ELSE
            IF( (ANEW(J) .EQ. KSTEND) .OR. (J .GT. LANEW) )GOTO 2
            AINOUT(I) = ANEW(J)
            J = J + 1
            K = K + 1
         ENDIF
 1    CONTINUE
 2    CONTINUE
      AINOUT(K+1) = KSTEND
      STADDIP = K
      RETURN
      END
