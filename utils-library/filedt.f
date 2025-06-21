      INTEGER FUNCTION FILEDT( NAME, DTC )
C--------------------------------------------------(NOS I/F)-----------
C RETURN THE 2 CHARACTER DEVICE TYPE CODE FOR THE DEVICE ON WHICH
C FILE CALLED NAME IS STORED. THE TYPE IS RETURNED IN DTC.
C RETURN 0 ON ERROR, ELSE NON-ZERO.
C----------------------------------------------------------------------
      CHARACTER*(*) NAME
      CHARACTER*2 DTC
C----
      BOOLEAN BNAME, GETCARG
      INTEGER FILEDTI
C
      BNAME = GETCARG( NAME, 1 )
      FILEDT = FILEDTI( BNAME, DTC )
      RETURN
      END
