      SUBROUTINE DFX337(TOTAL,OUTER,XTITHT,VALHT,FRCVAL,AVAIL,
     1                  XYLBHT,FACTOR,FRCLAB,SPACE,ENDVAL)
      INCLUDE 'dfxc06.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      P1 = TOTAL
      P2 = OUTER
      P3 = XTITHT
      P4 = VALHT
      P5 = FRCVAL
      P6 = AVAIL
      P7 = XYLBHT
      P8 = FACTOR
      P9 = FRCLAB
      P10 = SPACE
      P11 = ENDVAL
      I = 1
      IF (P1.LE.0..OR.P1.GE.1.) GO TO 1
      A1 = P1
      A2 = .5*(1.0 - A1)
      I = 2
      IF (P2.LE.0..OR.P2.GE.A2) GO TO 1
      A3 = P2
      I = 3
      IF (P3.LE.0..OR.P3.GE.(A2-A3)) GO TO 1
      A5 = P3
      A4 = A3 + A5
      I = 4
      IF (P4.LE.0..OR.P4.GE.(A2-A4)) GO TO 1
      A6 = P4
      I = 5
      IF (P5.LE.0..OR.P5.GT.1.) GO TO 1
      A7 = P5
      I = 6
      IF (P6.LE.0..OR.P6.GE.A2.OR.P6.LE.(A4+A6)) GO TO 1
      A8 = P6
      A9 = A8 - A4
      I = 7
      IF (P7.LE.0..OR.P7.GE.A5) GO TO 1
      A10 = P7
      I = 8
      IF (P8.LE..5.OR.P8.GE.(A1+A2)) GO TO 1
      A11 = P8
      I = 9
      IF (P9.LE.0..OR.P9.GT.(2.*A11-1.)) GO TO 1
      A12 = P9
      I = 10
      IF (P10.LE.0..OR.P10.GE.(A8-A6-A10-A4)) GO TO 1
      A13 = A4 + P10
      I = 11
      IF (P11.LE.0..OR.P11.GT..5*A1) GO TO 1
      A14 = P11
      DC1 = A1
      DC2 = A2
      DC3 = A3
      DC4 = A4
      DC5 = A5
      DC6 = A6
      DC7 = A7
      DC8 = A8
      DC9 = A9
      DC10 = A10
      DC11 = A11
      DC12 = A12
      DC13 = A13
      DC14 = A14
   99 CONTINUE
      RETURN
    1 IF (ICHECK.GT.0) WRITE(ERRREC,2)ROUTIN,I
    2 FORMAT(1H0,'**DIMFILM WARNING**  ',A,' DETECTS INVALID OR INCONSIS
     1TENT ',I2,'TH ARGUMENT'/1H ,21X,'NO CHANGE IN GRAPH LAYOUT WILL OC
     2CUR')
      CALL DFX130(0)
      GO TO 99
      END
C
C----------------------------------------------
C
