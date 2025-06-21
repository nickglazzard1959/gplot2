      SUBROUTINE DIMEND
C    CLOSE ALL OPEN/ACTIVE WORK STATIONS AND CLOSE DIMFILM/GKS
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'DIMEND'
      CALL DFX121
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
