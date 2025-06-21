      SUBROUTINE LYTIK
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'LYTIK'
      CALL DFX311(1,0.0)
      IF (IMM) CALL DFX000(-6,DUMMY,DUMMY,DUMMY,NDUMMY)
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
