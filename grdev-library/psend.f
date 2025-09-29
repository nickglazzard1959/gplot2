      SUBROUTINE PSEND
C --- ------------------------------------------------------------------
C
C --- THIS ROUTINE TERMINATES POSTSCRIPT OUTPUT.
C
      IMPLICIT LOGICAL (A-Z)
      INCLUDE 'dfxpsn.cmn'
      INCLUDE 'dfxpcn.cmn'
#ifndef PORTF77
      INTEGER LNBC
      INTEGER IPFERR
#endif
C
      IF( EMPTYF ) THEN
         CLOSE( UNIT=LUN, STATUS='DELETE' )
C         PRINT 888
C 888     FORMAT(1X,'PSEND ASKED FOR EMPTY FILE TO BE DELETED.')
      ELSE
         CLOSE( UNIT=LUN, STATUS='KEEP' )
#ifndef PORTF77
         IF( AUTOSAV )THEN
            CALL PF('REPLACE',
     +           FNO(1:LNBC(FNO,1,1)),FNO(1:LNBC(FNO,1,1)),
     +           'NA','IGNR','RC',IPFERR)
            IF( IPFERR .NE. 0 )THEN
               WRITE(6,2101)IPFERR
 2101          FORMAT(1X,'EPS WARNING - PFERROR ',I4,' OCCURRED.')
            ENDIF               
         ENDIF
#endif         
      END IF
      OPENED = .FALSE.
C
      END
C --- ------------------------------------------------------------------
