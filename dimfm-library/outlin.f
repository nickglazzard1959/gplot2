      SUBROUTINE OUTLIN
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      INCLUDE 'dfxcbd.cmn'
      INCLUDE 'dfxcd0.cmn'
      INCLUDE 'dfxcd0s.cmn'
      ROUTIN = 'OUTLIN'
      CALL DFX147(3)
C    ALL WORK STATIONS
      NWS = 0
      CALL DFX000(201,ZDUMMY,ZDUMMY,ZDUMMY,NDUMMY)
      CALL DFX147(1)
      IF (IMM) CALL DFX000(-6,DUMMY,DUMMY,DUMMY,NDUMMY)
      ROUTIN = STARS6
      RETURN
      END
