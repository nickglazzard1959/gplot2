      SUBROUTINE DFX325(S,J,DIFF,T,NIL)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc06.cmn'
      INCLUDE 'dfxc10.cmn'
      INCLUDE 'dfxc10s.cmn'
      INCLUDE 'dfxc04.cmn'
      LOGICAL NIL
      CALL DFX147(3)
      T = 1.
      IF (J.GT.0) GO TO 1
      S = S + DIFF
      RETURN
    1 S = S + DL(J)
      J = J + 1
      IF (J.EQ.10) J = 1
      IF (J.EQ.1) RETURN
      T = TC2
      CALL DFX147(NRGB)
      IF (NIL) T = 0.0
      RETURN
      END
