      SUBROUTINE DFX002
      INCLUDE 'dfxcbd.cmn'
      INCLUDE 'dfxcd0.cmn'
      INCLUDE 'dfxcd0s.cmn'
C    ABORT/REPRIEVE ENTRY POINT
C    EMERGENCY CLOSE ALL W/S AND ABORT
C    PLOT IS TERMINATED
      NWS = 0
      CALL DFX000(-2,ZDUMMY,ZDUMMY,ZDUMMY,NDUMMY)
C    EXECUTION IS TERMINATED
      CALL DFXM00(2)
      END
C
C----------------------------------------------
C
