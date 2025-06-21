      SUBROUTINE POLPY5(R,THETA,N)
      REAL R(N),THETA(N)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'POLPY5'
      NDEG = 5
      CALL DFX331(R,THETA,N)
      IF (IMM) CALL DFX000(-6,DUMMY,DUMMY,DUMMY,NDUMMY)
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
