      REAL FUNCTION DFX211(K,RNUMB,INUMB,FMT)
C
C    **INTERNAL SINUM/SRNUM FUNCTION**
C
      CHARACTER*(*) FMT
      INCLUDE 'dfxc01.cmn'
      INCLUDE 'dfxc01s.cmn'
      INCLUDE 'dfxc14.cmn'
      STR150 = ' '
      IF (K.EQ.1) THEN
                 WRITE(STR150,FMT) RNUMB
      ELSE IF (K.EQ.2) THEN
                 WRITE(STR150,FMT) INUMB
      ENDIF
      DO 1 I=150,1,-1
      IF (STR150(I:I).NE.' ') GO TO 2
    1 CONTINUE
      I = 0
    2 Z = 0.0
      IF (I.GT.0) Z = DFX212(STR150(1:I))
      DFX211 = Z
      RETURN
      END
