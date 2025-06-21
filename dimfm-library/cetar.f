      SUBROUTINE CETAR(PX,QX,QY,PY,DX,DY,R,G,B)
      INTEGER DX,DY
      REAL R(DX,DY),G(DX,DY),B(DX,DY)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      INCLUDE 'dfxcp0.cmn'
      INCLUDE 'dfxcd0.cmn'
      INCLUDE 'dfxcd0s.cmn'
      ROUTIN = 'CETAR'
      IF ((DX.LE.0).OR.(DY.LE.0)) GO TO 10
      CALL DFX170(PX,PY,QX,QY,IERR)
      IF (IERR.NE.0) GO TO 99
      IPAR(1) = DX
      IPAR(2) = DY
      IPAR(3) = 0
      IPAR(4) = 0
      IPAR(5) = DX
      IPAR(6) = DY
C    FLAG AS CELL ARRAY
      NPAR = 0
C    FLAG AS COLOUR TRIPLET VALUES
      LPAR(3) = .FALSE.
C    INPUT RASTER IS COLOUR
      LPAR(4) = .TRUE.
      CALL DFX000(10,R,G,B,NDUMMY)
      IF (IMM) CALL DFX000(-6,DUMMY,DUMMY,DUMMY,NDUMMY)
   99 CONTINUE
      ROUTIN = STARS6
      RETURN
   10 IF (ICHECK.GT.0) WRITE(ERRREC,11) DX,DY
      CALL DFX130(0)
   11 FORMAT(1H0,'**DIMFILM WARNING** CETAR REFERENCED WITH INVALID ARRA
     1Y DIMENSIONS (',I6,',',I6,')'/
     2 1H0,21X,'CALL IGNORED')
      GO TO 99
      END
C
C----------------------------------------------
C
