      SUBROUTINE INTSF(Z)
      INCLUDE 'dfxc04.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'INTSF'
      ZZ = Z
      DO 1 I=1,NRGB
      CALL DFX144(I,ZZ)
C    ENSURE DIAGNOSTIC CAN ONLY BE ISSUED ONCE
    1 ZZ = ZRGB(1)
      ROUTIN = STARS6
      RETURN
      END
