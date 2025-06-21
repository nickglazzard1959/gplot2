      SUBROUTINE RYLAB(ITEXT)
      CHARACTER*(*) ITEXT
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'RYLAB'
      CALL DFX310(ITEXT,2,.FALSE.)
      IF (IMM) CALL DFX000(-6,DUMMY,DUMMY,DUMMY,NDUMMY)
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
