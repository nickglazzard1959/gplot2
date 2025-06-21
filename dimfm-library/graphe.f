      SUBROUTINE GRAPHE(X,Y,N1,MX,MY)
      REAL X(N1,MX),Y(N1,MY)
      EXTERNAL DFX303
      INTRINSIC ALOG10
      INCLUDE 'dfxc09.cmn'
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      CHARACTER*6 C
      ROUTIN = 'GRAPHE'
      N = N1
      NX = MX
      NY = MY
      CALL DFX336(X,Y,N)
      IF (.NOT.OK) GO TO 10
      M = NX
      C = 'THIRD'
      IF (NX.LT.1.OR.NX.GT.3) GO TO 11
      C = 'FOURTH'
      M = NY
      IF (NY.LT.1.OR.NY.GT.3) GO TO 11
      IX = 1
      IY = 1
      IF (XTYPE.EQ.2) IX = 2
      IF (YTYPE.EQ.2) IY = 2
      GO TO (1,2),IX
    1 GO TO (3,4),IY
    3 CALL DFX312(X,Y,N,NX,NY,DFX303,DFX303)
      GO TO 10
    4 CALL DFX312(X,Y,N,NX,NY,DFX303,ALOG10)
      GO TO 10
    2 GO TO (5,6),IY
    5 CALL DFX312(X,Y,N,NX,NY,ALOG10,DFX303)
      GO TO 10
    6 CALL DFX312(X,Y,N,NX,NY,ALOG10,ALOG10)
   10 CONTINUE
      IF (IMM) CALL DFX000(-6,DUMMY,DUMMY,DUMMY,NDUMMY)
      ROUTIN = STARS6
      RETURN
   11 IF (ICHECK.GT.0) WRITE(ERRREC,12)C,M
   12 FORMAT(1H0,'**DIMFILM WARNING**  GRAPHE DETECTS ILLEGAL ',A6,
     1' ARGUMENT ',I3,' OUT OF RANGE (1,3)'/1H ,21X,'NO ERROR BARS WILL
     2APPEAR')
      CALL DFX130(0)
      GO TO 10
      END
C
C----------------------------------------------
C
