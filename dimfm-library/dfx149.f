      SUBROUTINE DFX149(N,SF)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc04.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      SSF = SF
      IF (SSF.LT.0.0) GO TO 1
    2 IF (ROUTIN(1:3).EQ.'LIN') SFLIN(N) = SSF
      IF (ROUTIN(1:4).EQ.'SPOT') SFSPOT(N) = SSF
      IF (ICOLPT(IRGBN).EQ.N) CALL DFX140(IRGBN)
      RETURN
    1 IF (ICHECK.GT.0) WRITE(ERRREC,10) ROUTIN,SSF
      CALL DFX130(0)
   10 FORMAT(1H0,'**DIMFILM WARNING**  ROUTINE ',A,' WAS REFERENCED WITH
     1 NEGATIVE SCALE FACTOR ',1PE16.8,' DEFAULT OF 1.0 WILL BE USED')
      SSF = 1.0
      GO TO 2
      END
C
C----------------------------------------------
C
