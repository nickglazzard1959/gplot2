      SUBROUTINE RESTXT
C    USER ACCESSIBLE ROUTINE TO RESTORE/RESET ALL NON-POSITIONAL
C    TEXT SETTINGS TO INITIAL STATE
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'RESTXT'
      CALL DFX206
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
