      SUBROUTINE LXALAB(ITEXT)
      CHARACTER*(*) ITEXT
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'LXALAB'
      CALL DFX330(-3,ITEXT)
      IF (IMM) CALL DFX000(-6,DUMMY,DUMMY,DUMMY,NDUMMY)
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
