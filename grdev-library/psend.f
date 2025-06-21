      SUBROUTINE PSEND
C --- ------------------------------------------------------------------
C
C --- THIS ROUTINE TERMINATES POSTSCRIPT OUTPUT.
C
      INCLUDE 'dfxpsn.cmn'
C
      IF( EMPTYF ) THEN
         CLOSE( UNIT=LUN, STATUS='DELETE' )
C         PRINT 888
C 888     FORMAT(1X,'PSEND ASKED FOR EMPTY FILE TO BE DELETED.')
      ELSE
         CLOSE( UNIT=LUN, STATUS='KEEP' )
      END IF
      OPENED = .FALSE.
C
      END
C --- ------------------------------------------------------------------
