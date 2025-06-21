      SUBROUTINE RNUM(RNUMB,FMT)
      INCLUDE 'dfxc01.cmn'
      INCLUDE 'dfxc01s.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      INCLUDE 'dfxc04.cmn'
      INCLUDE 'dfxc14.cmn'
      CHARACTER*(*) FMT
      REAL RNUMB
      INTEGER INUMB
      ROUTIN = 'RNUM'
      K = 1
      CALL DFX204(K,RNUMB,INUMB,FMT)
      IF (IMM) CALL DFX000(-6,DUMMY,DUMMY,DUMMY,NDUMMY)
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
