      SUBROUTINE LXLAB(ITEXT)
      CHARACTER*(*) ITEXT
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'LXLAB'
      CALL DFX310(ITEXT,1,.TRUE.)
      IF (IMM) CALL DFX000(-6,DUMMY,DUMMY,DUMMY,NDUMMY)
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
