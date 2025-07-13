      SUBROUTINE DFX333(ON,X,Y)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      INCLUDE 'dfxc06.cmn'
      CHARACTER*1 C
      LOGICAL ON
      LOGICAL SWCTR
      AZL = XGL
      AZR = XGR
      Z = X
      IF (XTYPE.NE.2) GO TO 1
      C = 'X'
      IF (AZL.LE.0..OR.AZR.LE.0..OR.Z.LE.0.) GO TO 3
      AZL = ALOG10(AZL)
      AZR = ALOG10(AZR)
      Z = ALOG10(Z)
    1 DX = XTB2 - XTB1
      XA = XTB1 + (DC2 + DC1*(Z-AZL)/(AZR-AZL))*DX
      AZL = YGL
      AZR = YGU
      Z = Y
      IF (YTYPE.NE.2) GO TO 2
      C = 'Y'
      IF (AZL.LE.0..OR.AZR.LE.0..OR.Z.LE.0.) GO TO 3
      AZL = ALOG10(AZL)
      AZR = ALOG10(AZR)
      Z = ALOG10(Z)
    2 DY = YTB2 - YTB1
      YA = YTB1 + (DC2 + DC1*(Z-AZL)/(AZR-AZL))*DY
      CCALPH = CALPHA
      SSALPH = SALPHA
      SX0T = X0T
      SY0T = Y0T
      SWCTR = WCTR
      X0T = 0.0
      Y0T = 0.0
      SALPHA = 0.0
      CALPHA = 1.0
      WCTR = .TRUE.
C    SINGLE PLOT INSTRUCTION ONLY
C      (NOT IN DFX300/DFX301 SEQUENCE SO CANNOT USE DFX137 TO FORCE)
      IF (.NOT.ON) CALL DFX111(XA,YA)
      IF (ON) CALL DFX124(XA,YA)
      CALPHA = CCALPH
      SALPHA = SSALPH
      X0T = SX0T
      Y0T = SY0T
      WCTR = SWCTR
   99 CONTINUE
      RETURN
    3 IF (ICHECK.GT.0) WRITE(ERRREC,4)C,Z,AZL,AZR,ROUTIN
    4 FORMAT(1H0,'**DIMFILM WARNING**  ATTEMPT TO PLOT TO GRAPH COORDINA
     1TE WITH LOGARITHMIC ',A1,'-AXIS'/1H ,21X,'WHEN EITHER POINT (',
     2   E16.8,') OR AXIS RANGE (',E16.8,',',E16.8,') IS NON-POSITIVE'/
     3   1H ,21X,'CALL OF ',A,' IGNORED')
      CALL DFX130(0)
      GO TO 99
      END
