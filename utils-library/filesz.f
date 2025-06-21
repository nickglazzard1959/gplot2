      INTEGER FUNCTION FILESZ( NAME, WORDS )
C---------------------------------------------------(NOS I/F)----------
C RETURN THE SIZE OF FILE CALLED NAME. IF WORDS .TRUE. RETURN IN WORDS
C ELSE IN SRUS (SECTORS OF 64 WORDS ON DISK). RETURN 0 IF FILE NOT
C FOUND.
C----------------------------------------------------------------------
      CHARACTER*(*) NAME
      LOGICAL WORDS
C----
      BOOLEAN BNAME, GETCARG, M
      INTEGER I
      INTEGER FILESZI
C
      INTEGER ISCALE
      ISCALE = 1
      IF( WORDS )ISCALE = 64
      BNAME = GETCARG( NAME, 1 )
      FILESZ = ISCALE * FILESZI( BNAME )
      RETURN
      END
