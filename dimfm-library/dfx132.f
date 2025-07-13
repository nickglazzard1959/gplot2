      SUBROUTINE DFX132(X,Y)
C
C    **INTERNAL POSN FUNCTION**
C
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      X = XPOS
      Y = YPOS
      IF (.NOT.WCTR) GO TO 1
      XT = XPOS - X0T
      YT = YPOS - Y0T
      X = XT*CALPHA + YT*SALPHA
      Y = YT*CALPHA - XT*SALPHA
    1 CONTINUE
      RETURN
      END
