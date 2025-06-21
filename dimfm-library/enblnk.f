      SUBROUTINE ENBLNK
      INCLUDE 'params.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxcac.cmn'
      INCLUDE 'dfxcacs.cmn'
      INCLUDE 'dfxcbc.cmn'
      INCLUDE 'dfxcbcs.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'ENBLNK'
      PREBLK(1) = .FALSE.
      IF (NOTR.EQ.1) BLNKS = .FALSE.
      IF (NOTR.EQ.1) BLNK = .FALSE.
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
