      SUBROUTINE GRCON(TOTAL,OUTER,XTITHT,VALHT,FRCVAL,AVAIL,
     1                  XYLBHT,FACTOR,FRCLAB,SPACE,ENDVAL)
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      ROUTIN = 'GRCON'
      CALL DFX337(TOTAL,OUTER,XTITHT,VALHT,FRCVAL,AVAIL,
     1                  XYLBHT,FACTOR,FRCLAB,SPACE,ENDVAL)
      ROUTIN = STARS6
      RETURN
      END
C
C----------------------------------------------
C
