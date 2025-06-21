      SUBROUTINE NEWORG(X1,Y1)
C    THIS FORMS A NEW ORIGIN AND CLEARS TEMPORARY ORIGIN
C    REFERENCES TO OLDORG WILL REVERT TO THIS ORIGIN AND TMPORG
C    WILL BE RELATIVE TO THIS - APPLICABLE TO WORLD COORD SET 1 ONLY
      INCLUDE 'params.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc01.cmn'
      INCLUDE 'dfxc01s.cmn'
      INCLUDE 'dfxc03.cmn'
      INCLUDE 'dfxc13.cmn'
      INCLUDE 'dfxc13s.cmn'
      INCLUDE 'dfxcac.cmn'
      INCLUDE 'dfxcacs.cmn'
      INCLUDE 'dfxcbc.cmn'
      INCLUDE 'dfxcbcs.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'NEWORG'
      XNORG(1) = X1
      YNORG(1) = Y1
      WCTRN(1) = .TRUE.
      XTORG(1) = 0.0
      YTORG(1) = 0.0
      AXANG(1) = 0.0
      IF (X1.EQ.0..AND.Y1.EQ.0.) WCTRN(1) = .FALSE.
C    NOTE IF OTHER TRANSFORMS ALLOWED MODIFY WCTRN TO TAKE ACCOUNT
      IF (NOTR.EQ.1) CALL DFX4AA
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
