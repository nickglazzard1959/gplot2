      SUBROUTINE GETNOSV( VERSION )
C-----------------------------------------------------(NOS I/F)---------
C RETURN A VERSION STRING DESCRIBING THE VERSION OF NOS BEING USED.
C-----------------------------------------------------------------------
      CHARACTER*20 VERSION
C----
      BOOLEAN RESULT(2)
C
      CALL GETVERI(RESULT)
      WRITE(VERSION,100)RESULT(1), RESULT(2)
 100  FORMAT(A10,A8)
      RETURN
      END
          IDENT  GETVERI
          ENTRY  GETVERI
*
* SUBROUTINE GETVERI( BOOLEAN RESULT(2) )
*-----------------------------------------------------(NOS I/F)---------
* RETURN OPERATING SYSTEM VERSION INFORMATION.
* RESULT(1) = NOS X.Y.Z
* RESULT(2) = LVL/LVL.
*-----------------------------------------------------------------------
*
 GETVERI  BSS    1           ENTRY.
          SB4    A1          B4 = ADDR(ARGLIST)
          VERSION SETUP      MACRO.
          SA2    INFO        X2 = INFO(0)
          BX6    X2          X6 = INFO(0)
          SA5    B4          X5 = ADDR(ARG1)
          SA6    X5          RESULT(0) = INFO(0)
          SA2    INFO+1      X2 = INFO(1)
          BX6    X2          X6 = INFO(1)
          SA5    B4          X5 = ADDR(ARG1)
          SA6    X5+1        RESULT(1) = INFO(1)
          EQ     GETVERI     RETURN
 SETUP    VFD    12/11B,12/0,12/0,24/INFO
 INFO     BSS    3
          END
