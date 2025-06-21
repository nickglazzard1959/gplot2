      SUBROUTINE DFX133(ALPHA2)
C
C    **INTERNAL ROTATE FUNCTION**
C
      INCLUDE 'params.cmn'
      INCLUDE 'dfxc01.cmn'
      INCLUDE 'dfxc01s.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc03.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      INCLUDE 'dfxc13.cmn'
      INCLUDE 'dfxc13s.cmn'
      INCLUDE 'dfxcac.cmn'
      INCLUDE 'dfxcacs.cmn'
      INCLUDE 'dfxcbc.cmn'
      INCLUDE 'dfxcbcs.cmn'
C
      IF (ROUTIN.EQ.'ROTATE') THEN
C    APPLIES TO NOTR = 1 IF DIRECT REFERENCE FROM PANE
                     NTRAN = 1
      ELSE
C    OTHERWISE IS APPLIED TO CURRENT TRANSFORM (E.G. FROM GRAPHING)
                     NTRAN = NOTR
      ENDIF
      AXANG(NTRAN) = ALPHA2*ANGCON(1,ANGRP(1))
C    STORE AXIS ROTATION
      WCTRN(NTRAN) = .TRUE.
      IF (NOTR.EQ.NTRAN) CALL DFX4AA
      RETURN
      END
C
C----------------------------------------------
C
