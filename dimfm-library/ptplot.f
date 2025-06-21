      SUBROUTINE PTPLOT(X,Y,N,NCHAR)
      REAL X(N),Y(N)
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'PTPLOT'
      CALL DFX340(X,Y,N,NCHAR)
      IF (IMM) CALL DFX000(-6,DUMMY,DUMMY,DUMMY,NDUMMY)
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
