      SUBROUTINE GRTOAC(XG,YG,XA,YA)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc06.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      CHARACTER*1 C
      ROUTIN = 'GRTOAC'
      DX = XTB2 - XTB1
      DY = YTB2 - YTB1
      AZL = XGL
      AZR = XGR
      Z = XG
      IF (XTYPE.NE.2) GO TO 1
      C = 'X'
      IF (AZL.LE.0..OR.AZR.LE.0..OR.Z.LE.0.) GO TO 3
      AZL = ALOG10(AZL)
      AZR = ALOG10(AZR)
      Z = ALOG10(Z)
    1 XA = XTB1 + (DC2 + DC1*(Z-AZL)/(AZR-AZL))*DX
      AZL = YGL
      AZR = YGU
      Z = YG
      IF (YTYPE.NE.2) GO TO 2
      C = 'Y'
      IF (AZL.LE.0..OR.AZR.LE.0..OR.Z.LE.0.) GO TO 3
      AZL = ALOG10(AZL)
      AZR = ALOG10(AZR)
      Z = ALOG10(Z)
    2 YA = YTB1 + (DC2 + DC1*(Z-AZL)/(AZR-AZL))*DY
   99 CONTINUE
      ROUTIN = STARS6
      RETURN
    3 IF (ICHECK.GT.0) WRITE(ERRREC,4)C,Z,AZL,AZR
    4 FORMAT(1H0, 73H**DIMFILM WARNING**  ATTEMPT TO OBTAIN GRAPH COORDI
     1NATE WITH LOGARITHMIC ,A1,  5H-AXIS/ 1H ,21X, 19HWHEN EITHER POINT
     2 (,   E16.8, 17H) OR AXIS RANGE (,E16.8,  1H,,E16.8, 17H) IS NON-P
     3OSITIVE/     1H ,21X, 12HCALL IGNORED)
      CALL DFX130(0)
      GO TO 99
      END
C
C----------------------------------------------
C
