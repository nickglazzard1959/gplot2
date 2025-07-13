      SUBROUTINE LINSF(SF)
      INCLUDE 'dfxc04.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'LINSF'
      SSF = SF
      DO 1 I=1,NRGB
      CALL DFX149(I,SSF)
C    ENSURE DIAGNOSTIC ONLY ONCE
    1 SSF = SFLIN(1)
      ROUTIN = STARS6
      RETURN
      END
