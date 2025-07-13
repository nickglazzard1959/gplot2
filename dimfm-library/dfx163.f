      SUBROUTINE DFX163(N,H1,L,S,J,CENTRE)
      LOGICAL CENTRE
      REAL L
      INCLUDE 'dfxc17.cmn'
C    START AT HUE H1, DO CIRCLE AT S,L WITH J VALUES IN LUT AT N
C    CENTRE .TRUE. DO CENTRAL GREY FIRST
C    **LUTSIZ NEVER EXCEEDED**
      H = H1
      DH = 360./FLOAT(J)
      IF (CENTRE) THEN
         SS = S
         S = 0.0
         IF (N.LT.LUTSIZ)
     1    CALL DFX160(6,H,L,S,RGBLUT(1,N),RGBLUT(2,N),RGBLUT(3,N),IERR)
         S = SS
         N = N + 1
      ENDIF
      DO 1 I=1,J
      IF (N.LT.LUTSIZ)
     1    CALL DFX160(6,H,L,S,RGBLUT(1,N),RGBLUT(2,N),RGBLUT(3,N),IERR)
      N = N + 1
    1 H = H + DH
      RETURN
      END
