      SUBROUTINE POSN(X,Y)
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'POSN'
      CALL DFX132(X,Y)
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
