      REAL FUNCTION SINUM(INUMB,FMT)
      CHARACTER*(*) FMT
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'SINUM'
      SINUM = DFX211(2,RDUMMY,INUMB,FMT)
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
