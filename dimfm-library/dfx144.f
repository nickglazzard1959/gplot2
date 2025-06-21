      SUBROUTINE DFX144(N,Z)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc04.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ZZ = AMIN1(AMAX1(Z,0.0),1.0)
      IF (ZZ.NE.Z) GO TO 2
    1 ZRGB(N) = ZZ
      IF (ICOLPT(IRGBN).EQ.N) CALL DFX140(IRGBN)
      RETURN
    2 IF (ICHECK.GT.0) WRITE(ERRREC,10) ROUTIN,Z,ZZ
      CALL DFX130(0)
   10 FORMAT(1H0,'**DIMFILM WARNING**  ',A,' CALLED WITH INVALID FACTOR
     1',1PE16.8,' A VALUE OF ',F5.2,' HAS BEEN SUBSTITUTED')
      GO TO 1
      END
C
C----------------------------------------------
C
