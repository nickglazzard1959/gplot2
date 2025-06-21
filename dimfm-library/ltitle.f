      SUBROUTINE LTITLE(TEXT)
      INCLUDE 'dfxc01.cmn'
      INCLUDE 'dfxc01s.cmn'
      INCLUDE 'dfxc06.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      CHARACTER*(*) TEXT
      ROUTIN = 'LTITLE'
      Y = YTB1 + DC3*(YTB2 - YTB1)
      CALL DFX328(Y,TEXT)
      IF (IMM) CALL DFX000(-6,DUMMY,DUMMY,DUMMY,NDUMMY)
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
