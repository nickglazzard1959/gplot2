      SUBROUTINE DFX003(TEXT)
      CHARACTER*(*) TEXT
      L = LEN(TEXT)
      DO 1 I=L,1,-1
      IF (TEXT(I:I).NE.' ') GO TO 2
    1 CONTINUE
      GO TO 99
    2 NA = I
C    NOW OUTPUT STRING TO JOB-LOG/DAYFILE ETC
      CALL DFXMS0(NA,TEXT)
   99 RETURN
      END
C
C----------------------------------------------
C
