      SUBROUTINE LOGLIM(ZMIN,ZMAX)
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      CHARACTER*17 C
      SAVE C
      DATA C/'FIRSTSECONDBOTHS '/
      ROUTIN = 'LOGLIM'
      N1 = 1
      N2 = 5
      N3 = 17
      IF (ZMIN.LT.0.0) GO TO 1
      N1 = 0
      LOGMIN = ZMIN
    1 IF (ZMAX.LT.0.0) GO TO 2
      LOGMAX = ZMAX
      IF (N1.NE.0) GO TO 3
   99 CONTINUE
      ROUTIN = STARS6
      RETURN
    2 IF (N1.EQ.0) GO TO 5
      N1 = 12
      N2 = 15
      N3 = 16
      GO TO 3
    5 N1 = 6
      N2 = 11
    3 IF (ICHECK.GT.0) WRITE(ERRREC,4)C(N1:N2),C(N3:),C(N1:N2),C(N3:)
    4 FORMAT(1H0,'**DIMFILM WARNING**  LOGLIM DETECTS ',A,' PARAMETER',
     1A,'NEGATIVE, CHANGE TO ',A,' PARAMETER',A,'WILL BE IGNORED')
      CALL DFX130(0)
      GO TO 99
      END
