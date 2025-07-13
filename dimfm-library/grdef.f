      SUBROUTINE GRDEF(ITITLE,IXLAB,IYLAB,IXY)
      CHARACTER*(*) ITITLE,IXLAB,IYLAB
C    IXY CONTROLS AXES
C    IXY = 0 NEITHER AXIS APPEARS
C    IXY = 1 X-AXIS AND TICKING ONLY
C    IXY = 2 Y-AXIS AND TICKING ONLY
C    ANY OTHER VALUE OF IXY RESULTS IN BOTH AXES AND TICKING OF EACH
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc06.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'GRDEF'
C----------------------------------
C    EQUIVALENT TO LTITLE(ITITLE)
      Y = YTB1 + DC3*(YTB2 - YTB1)
      CALL DFX328(Y,ITITLE)
C----------------------------------
C    EQUIVALENT TO LXLAB(IXLAB)
      CALL DFX310(IXLAB,1,.TRUE.)
C----------------------------------
C    EQUIVALENT TO LYLAB(IYLAB)
      CALL DFX310(IYLAB,2,.TRUE.)
C----------------------------------
C    EQUIVALENT TO GRFRAM
      CALL DFX338
C----------------------------------
C    EQUIVALENT TO LYVAL
      CALL DFX311(21,0.0)
C----------------------------------
C    EQUIVALENT TO LXVAL
      CALL DFX311(23,0.0)
C----------------------------------
C    EQUIVALENT TO LYTIK
      CALL DFX311(1,0.0)
C----------------------------------
C    EQUIVALENT TO RYTIK
      CALL DFX311(2,0.0)
C----------------------------------
C    EQUIVALENT TO LXTIK
      CALL DFX311(3,0.0)
C----------------------------------
C    EQUIVALENT TO UXTIK
      CALL DFX311(4,0.0)
      IF (IXY.EQ.0) GO TO 99
      IF (IXY.EQ.2) GO TO 1
C----------------------------------
C    EQUIVALENT TO DRAWXA
      CALL DFX335('X')
C----------------------------------
C    EQUIVALENT TO LXATIK
      CALL DFX329(-3)
C----------------------------------
C    EQUIVALENT TO UXATIK
      CALL DFX329(-4)
C----------------------------------
      IF (IXY.EQ.1) GO TO 99
C----------------------------------
C    EQUIVALENT TO DRAWYA
    1 CALL DFX335('Y')
C----------------------------------
C    EQUIVALENT TO LYATIK
      CALL DFX329(-1)
C----------------------------------
C    EQUIVALENT TO RYATIK
      CALL DFX329(-2)
C----------------------------------
   99 CONTINUE
      IF (IMM) CALL DFX000(-6,DUMMY,DUMMY,DUMMY,NDUMMY)
      ROUTIN = STARS6
      RETURN
      END
