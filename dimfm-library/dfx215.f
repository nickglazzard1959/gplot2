      SUBROUTINE DFX215(THETA)
C
C    **INTERNAL SYMANG EQUIVALENT FUNCTION**
C
      INCLUDE 'dfxc01.cmn'
      INCLUDE 'dfxc01s.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc03.cmn'
C    ANGLS IS PERMANENTLY SAVED ANGLE - ANGL IS FOR CURRENT TRANSFORM
      ANGLS = THETA*ANGCON(1,ANGRP(1))
      ANGL = ANGLS
      IF (.NOT.HS) ANGL = ANGL + ALPHA
      CANGL = COS(ANGL)
      SANGL = SIN(ANGL)
      RETURN
      END
C
C----------------------------------------------
C
