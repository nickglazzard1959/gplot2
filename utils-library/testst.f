      PROGRAM TESTST(OUTPUT,TAPE6=OUTPUT)
C-----------------------------------------------------------------------
C TEST ST (ASCII STRING) FUNCTIONS.
C-----------------------------------------------------------------------
      IMPLICIT CHARACTER*1 (A-Z)
      INCLUDE 'utdecls.cmn'
      INTEGER ASTRING(KSTDIM), NAC, NCC, LENA, STATUS
      CHARACTER*(KCMAX) CSTRING
C
      NAC = STCTOA( 'THIS IS A STRING', ASTRING )
      WRITE(6,*)'NAC = ',NAC
C
      LENA = STLEN(ASTRING)
      WRITE(6,*)'LENA = ',LENA
C
      NCC = STATOC( ASTRING, CSTRING )
      WRITE(6,*)'NCC = ',NCC
C
      WRITE(6,*)'CSTRING = ',CSTRING(1:NCC)
C
      NAC = STCTOA( 'T^H^I^S ^I^S ^A ^S^T^R^I^N^G.', ASTRING )
      WRITE(6,*)'NAC = ',NAC
C
      LENA = STLEN(ASTRING)
      WRITE(6,*)'LENA = ',LENA
C
      NCC = STATOC( ASTRING, CSTRING )
      WRITE(6,*)'NCC = ',NCC
C
      WRITE(6,*)'CSTRING = ',CSTRING(1:NCC)
C
      STATUS = STOPEN(2, 'EGIN', 'OLD')
      WRITE(6,*)'STOPEN EGIN STATUS =',STATUS
      IF( STATUS .NE. 0 )THEN
         WRITE(6,*)'OPEN OF EGIN FAILED.'
         STOP
      ENDIF
      STATUS = STOPEN(3, 'EGOUT', 'NEW')
      WRITE(6,*)'STOPEN EGOUT STATUS =',STATUS
      IF( STATUS .NE. 0 )THEN
         WRITE(6,*)'CREATE OF EGOUT FAILED.'
         STOP
      ENDIF
C
 21   CONTINUE
      STATUS = STREAD(2, ASTRING, .TRUE.)
      WRITE(6,*)'NCHARS=',STATUS
      IF((STATUS .EQ. KSTEOF) .OR. (STATUS .EQ. KSTERR))GOTO 20
      STATUS = STWRITE(ASTRING, 3)
      IF(STATUS .EQ. KSTERR)GOTO 20
      GOTO 21
 20   CONTINUE
      WRITE(6,*)'FINAL COPY STATUS =',STATUS
C
      STATUS = STCLOSE(2)
      WRITE(6,*)'STCLOSE 2 STATUS =',STATUS
      IF( STATUS .NE. 0 )THEN
         WRITE(6,*)'CLOSE OF 2   FAILED.'
         STOP
      ENDIF
      STATUS = STCLOSE(3)
      WRITE(6,*)'STCLOSE 3 STATUS =',STATUS
      IF( STATUS .NE. 0 )THEN
         WRITE(6,*)'CLOSE OF 3   FAILED.'
         STOP
      ENDIF
C
      STOP
      END
