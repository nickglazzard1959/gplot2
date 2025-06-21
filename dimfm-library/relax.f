      SUBROUTINE RELAX
C    ANGLES ALWAYS HELD INTERNALLY RELATIVE TO FRAME AND THIS VALUE
C    RETURNED VIA VAL FUNCTION
      INCLUDE 'dfxc01.cmn'
      INCLUDE 'dfxc01s.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'RELAX'
      IF (HS) THEN
              HS = .FALSE.
              ANGL = ANGL + ALPHA
              CANGL = COS(ANGL)
              SANGL = SIN(ANGL)
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
