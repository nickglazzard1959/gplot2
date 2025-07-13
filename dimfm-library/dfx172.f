      SUBROUTINE DFX172(PX,QX,QY,PY,R,G,B)
C
C          **EXTERNAL LIBRARY FUNCTION**
C    **EQUIVALENT TO CETAR FOR SINGLE RECTANGLE**
C
      REAL R,G,B
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc12.cmn'
      INCLUDE 'dfxcp0.cmn'
      INCLUDE 'dfxcd0.cmn'
      INCLUDE 'dfxcd0s.cmn'
      CALL DFX170(PX,PY,QX,QY,IERR)
      IF (IERR.NE.0) GO TO 99
      IPAR(1) = 1
      IPAR(2) = 1
      IPAR(3) = 0
      IPAR(4) = 0
      IPAR(5) = 1
      IPAR(6) = 1
C    FLAG AS CELL ARRAY
      NPAR = 0
C    FLAG AS COLOUR TRIPLET VALUES
      LPAR(3) = .FALSE.
C    INPUT RASTER IS COLOUR
      LPAR(4) = .TRUE.
      CALL DFX000(10,R,G,B,NDUMMY)
      IF (IMM) CALL DFX000(-6,DUMMY,DUMMY,DUMMY,NDUMMY)
   99 CONTINUE
      RETURN
      END
