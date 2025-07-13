      SUBROUTINE DFX166(N,V1,V2,V3)
C
C++++++++++++++CODE MUST REPLICATE THAT IN USER ACCESSIBLE SETLUT+++++++
C
      INTEGER NPT(2)
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc04.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      INCLUDE 'dfxc17.cmn'
      INCLUDE 'dfxcbd.cmn'
C   SUPPRESS DIAGNOSTICS
      ICHSAV = ICHECK
      ICHECK = 0
      CALL DFX160(NCMODE,V1,V2,V3,RRR,GGG,BBB,IERR)
      ICHECK = ICHSAV
      IF (IERR.LE.0) THEN
                  RGBLUT(1,N) = RRR
                  RGBLUT(2,N) = GGG
                  RGBLUT(3,N) = BBB
                  NWS = 0
                  NPT(1) = N
                  NPT(2) = N
                  CALL DFX000(101,ZDUMMY,ZDUMMY,ZDUMMY,NPT)
      ENDIF
      RETURN
      END
