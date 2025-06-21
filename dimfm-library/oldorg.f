      SUBROUTINE OLDORG
      INCLUDE 'params.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc13.cmn'
      INCLUDE 'dfxc13s.cmn'
      INCLUDE 'dfxcac.cmn'
      INCLUDE 'dfxcacs.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'OLDORG'
      XTORG(1) = 0.0
      YTORG(1) = 0.0
      IF (NOTR.EQ.1) CALL DFX4AA
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
