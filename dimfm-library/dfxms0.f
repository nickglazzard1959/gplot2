      SUBROUTINE DFXMS0( NA, TEXT )
C --- -----------------------------------------------------------------
C
C --- WRITE AN ERROR MESSAGE TO A SUITABLE CHANNEL.
C --- NA IS THE LENGTH OF THE CHARACTER STRING TO BE OUTPUT AND TEXT
C --- THE ACTUAL STRING.
C
      CHARACTER TEXT*(*)
C
      INTEGER LUN
C
      LUN = 7
      WRITE( LUN, '(1X,A,A)' ) 'DIMFILM ERROR: ', TEXT(1:NA)
C
      RETURN
      END
