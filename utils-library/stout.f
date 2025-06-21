      SUBROUTINE STOUT(LUN,TITLE,VALUE)
C------------------------------------------------------(STRING)---------
C WRITE A STRING VALUE TO LUN PREFIXED BY A TITLE (CHARACTER CONSTANT)
C MAINLY FOR DEBUGGING PURPOSES.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'stringh.cmn'
      CHARACTER*(*) TITLE
      INTEGER VALUE(KSTDIM), LUN
C----
      CHARACTER*160 CS
      INTEGER N, M
C
      INTEGER STATOC, LNBC
C
      N = STATOC(VALUE,CS)
      M = LNBC(TITLE)
      IF( LNBC(TITLE) .GT. 0 )THEN
         WRITE(LUN,1)TITLE(1:M),CS(1:N)
      ELSE
         WRITE(LUN,2)CS(1:N)
      ENDIF
 1    FORMAT(A,A)
 2    FORMAT(A)
      RETURN
      END
