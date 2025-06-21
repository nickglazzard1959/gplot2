      SUBROUTINE SETLUT(N,V1,V2,V3)
C
C    +++++ALL CODE MUST BE REPLICATED IN DFX166+++++
C
      INTEGER NPT(2)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc04.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      INCLUDE 'dfxc17.cmn'
      INCLUDE 'dfxcbd.cmn'
      ROUTIN = 'SETLUT'
      IF (N.LT.0.OR.N.GE.LUTSIZ) THEN
                   IF (ICHECK.GT.0) WRITE(ERRREC,10)ROUTIN,N,LUTSIZ
      CALL DFX130(0)
   10 FORMAT(1H0,'**DIMFILM WARNING**  ROUTINE ',A,' REFERENCED WI
     1TH INVALID LUT POINTER ',I3,'RANGE SHOULD BE 0-',I3/
     2 1H0,21X,'CALL IGNORED')
      ELSE
                   CALL DFX160(NCMODE,V1,V2,V3,RRR,GGG,BBB,IERR)
                   IF (IERR.LE.0) THEN
                              RGBLUT(1,N) = RRR
                              RGBLUT(2,N) = GGG
                              RGBLUT(3,N) = BBB
                              NWS = 0
                              NPT(1) = N
                              NPT(2) = N
                              CALL DFX000(101,ZDUMMY,ZDUMMY,ZDUMMY,NPT)
                   ENDIF
      ENDIF
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
