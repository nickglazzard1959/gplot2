      SUBROUTINE DFX134(X1,Y1)
C
C    **INTERNAL TMPORG FUNCTION**
C
      INCLUDE 'params.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc13.cmn'
      INCLUDE 'dfxc13s.cmn'
      INCLUDE 'dfxcac.cmn'
      INCLUDE 'dfxcacs.cmn'
      INCLUDE 'dfxcbc.cmn'
      INCLUDE 'dfxcbcs.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
C
      IF (ROUTIN.EQ.'TMPORG') THEN
C    APPLIES TO NOTR = 1 IF DIRECT REFERENCE FROM PANE
                     NTRAN = 1
      ELSE
C    OTHERWISE IS APPLIED TO CURRENT TRANSFORM (E.G. FROM GRAPHING)
                     NTRAN = NOTR
      ENDIF
      XTORG(NTRAN) = XTORG(NTRAN) + X1
      YTORG(NTRAN) = YTORG(NTRAN) + Y1
      WCTRN(NTRAN) = .TRUE.
      IF (NOTR.EQ.NTRAN) CALL DFX4AA
      RETURN
      END
C
C----------------------------------------------
C
