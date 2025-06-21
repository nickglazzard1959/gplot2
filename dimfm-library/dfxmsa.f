      SUBROUTINE DFXMSA( IERR )
C --- -----------------------------------------------------------------
C
C --- THIS ROUTINE STORES THE STATUS CODE IERR IN SYSERR AND
C --- CONVERTS IT TO HEX, STORING THE RESULT IN SYSCOD.
C
      INCLUDE 'dfxcbd.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
C
      SYSERR = IERR
      WRITE(SYSCOD,10) IERR
C
      RETURN
   10 FORMAT(Z8)
      END
C --- -----------------------------------------------------------------
