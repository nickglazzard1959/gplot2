      SUBROUTINE POLDEF(ITITLE,IRLAB)
      CHARACTER*(*) ITITLE,IRLAB
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc06.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'POLDEF'
C----------------------------------
C    EQUIVALENT TO LTITLE(ITITLE)
      Y = YTB1 + DC3*(YTB2 - YTB1)
      CALL DFX328(Y,ITITLE)
C----------------------------------
C    EQUIVALENT TO LXLAB(IRLAB)
      CALL DFX310(IRLAB,1,.TRUE.)
C----------------------------------
C    EQUIVALENT TO POLFR
      CALL DFX324(0)
C----------------------------------
C    EQUIVALENT TO RVAL
      CALL DFX324(2)
C----------------------------------
C    EQUIVALENT TO RTIK
      CALL DFX324(4)
C----------------------------------
C    EQUIVALENT TO TVAL
C     CALL DFX322(.FALSE.) BUT DFX322 TAKES NO ARGUMENTS.
      CALL DFX322
C----------------------------------
      IF (IMM) CALL DFX000(-6,DUMMY,DUMMY,DUMMY,NDUMMY)
      ROUTIN = STARS6
      RETURN
      END
