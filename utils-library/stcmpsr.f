      INTEGER FUNCTION STCOMPR( A1, A2 )
C-----------------------------------------------------(STRING)----------
C COMPARE STRINGS A1 AND A2. RETURN 0 IF A1.EQ.A2, -1 IF A1.LT.A2 AND
C +1 IF A1.GT.A2.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER A1(KSTDIM), A2(KSTDIM)
C----
      INTEGER I
C
      I = 1
 1    CONTINUE
         IF( A1(I) .NE. A2(I) ) GOTO 2
         IF( (A1(I) .EQ. KSTEND) .OR. (I .EQ. KSTMAX1) )THEN
            STCOMPR = 0
            RETURN
         ENDIF
         I = I + 1
         GOTO 1
 2    CONTINUE
C---- STRINGS DIFFER.
C---- IF A1 IS AT EOS, IT IS 'LESS', LIKEWISE A2.
      IF( A1(I) .EQ. KSTEND )THEN
         STCOMPR = -1
      ELSE IF( A2(I) .EQ. KSTEND )THEN
         STCOMPR = 1
      ELSE
C---- COMPARE ON CHARACTER CODE AT I.
         IF( A1(I) .LT. A2(I) )THEN
            STCOMPR = -1
         ELSE
            STCOMPR = 1
         ENDIF
      ENDIF
      RETURN
      END
C
      SUBROUTINE STSORTV( AARRAY, M, INDEXS, SHTDIM, MKINDEX, N )
C---------------------------------------------------------(STRING)------
C SORT LINE NUMBERS OF M STRINGS (INDEXS) IN AN ARRAY (AARRAY) SO THAT
C THEY CAN BE ACCESSED IN AARRAY IN ASCENDING ORDER.
C THE STRINGS IN AARRAY MAY BE SHORT TO SAVE MEMORY. IN THAT CASE,
C THEIR LENGTH IS SHTDIM.
C THE INDEXS ARRAY IS INITIALIZED HERE IF MKINDEX IS TRUE, OTHERWISE
C INDEXS MUST ALREADY CONTAIN VALID INDICES FOR AARRAY, BUT IT NEED
C ONLY CONTAIN N INDICES (A SUBSET OF AARRAY THAT IS TO BE SORTED).
C IF INDEXS IS INITIALIZED HERE, M = N IS REQUIRED.
C THE ALGORITHM IS SHELL SORT.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER M, N, SHTDIM
      INTEGER AARRAY(SHTDIM,M), INDEXS(N)
      LOGICAL MKINDEX
C----
      INTEGER STCOMPR
C
      INTEGER I, J, JG, GAP, ITEMP
C
C---- CREATE AN INDEX ARRAY. INDEXES ARE EXCHANGED, NOT STRINGS.
      IF( MKINDEX )THEN
         DO 1 I=1, N
            INDEXS(I) = I
 1       CONTINUE
      ENDIF
C
C---- FOR( GAP=N/2; GAP > 0; GAP = GAP/2 )
      GAP = N / 2
 2    CONTINUE
         IF( GAP .LE. 0 )GOTO 3
C
C----    FOR( I=GAP+1; I <= N; I = I + 1 )
         I = GAP + 1
 4       CONTINUE
            IF( I .GT. N )GOTO 5
C
C----       FOR( J=I-GAP; J > 0; J -= GAP )
            J = I - GAP
 6          CONTINUE
            IF( J .LE. 0 )GOTO 7
               JG = J + GAP
C
C---- COMPARE AARRAY(J) AND AARRAY(JG)
C---- SWAP IF AARRAY(JG) > AARRAY(J).
               IF( STCOMPR(AARRAY(1,INDEXS(J)),
     +                     AARRAY(1,INDEXS(JG))) .LE. 0 )GOTO 7
               ITEMP = INDEXS(J)
               INDEXS(J) = INDEXS(JG)
               INDEXS(JG) = ITEMP
               J = J - GAP
               GOTO 6
 7          CONTINUE
            I = I + 1
            GOTO 4
 5       CONTINUE
         GAP = GAP / 2
         GOTO 2
 3    CONTINUE
      RETURN
      END
C
      SUBROUTINE STSORTI( AARRAY, N, INDEXS )
C---------------------------------------------------(STRING)------------
C SORT LINE NUMBERS OF N STRINGS (INDEXS) IN AN ARRAY (AARRAY) SO THAT
C THEY CAN BE ACCESSED IN AARRAY IN ASCENDING ORDER.
C THE INDEXS ARRAY IS INITIALIZED HERE AND THE SORTED VERSION RETURNED.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      INTEGER N
      INTEGER AARRAY(KSTDIM,N), INDEXS(N)
C----
      CALL STSORTV( AARRAY, N, INDEXS, KSTDIM, .TRUE., N )
      RETURN
      END
