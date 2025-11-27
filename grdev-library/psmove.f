      SUBROUTINE PSMOVE( X, Y, ON )
C --- ------------------------------------------------------------------
C --- MOVE (ON=.FALSE.) OR DRAW (ON=.TRUE.) FROM (XPOS,YPOS) TO (X,Y),
C --- X,Y ARE IN INCHES. ALL OUTPUT IS IN POINTS (72 PTS/INCH).
C --- NOTE: XPOS,YPOS IS KEPT IN POINTS.
C --- ------------------------------------------------------------------
      IMPLICIT LOGICAL (A-Z)
      REAL INPS
      PARAMETER( INPS = 72.0 )
C
      REAL X, Y
      LOGICAL ON
C
      REAL XPS, YPS
C
      INCLUDE 'dfxpsn.cmn'
C
C --- NG- OCT 2008: CDC EMULATION ONLY.
C --- DUE TO AN ISSUE THAT ARISES SOMEWHERE IN SUBROUTINE DFX214,
C --- X AND/OR Y CAN BE INDEFINITE AT THIS POINT.
C --- NG- NOV 2008: SEVERAL THINGS WERE FOUND TO BE UNINITIALIZED.
C --- FIXES HAVE BEEN MADE. BUT BEST TO ZERO FILL CORE IN CASE
C --- THERE ARE OTHERS LURKING.
C
C      IF( LEGVAR(X) .NE. 0 .OR. LEGVAR(Y) .NE. 0 )THEN
C         RETURN
C      ENDIF
C
C --- SCALE X AND Y INTO POSTSCRIPT UNITS.
C
C      PRINT 999,X,XSIZE,XOFFST,Y,YSIZE,YOFFST
C 999  FORMAT(1X,'X=',F12.6,' XS=',F12.6,' XO=',F12.6,
C     1         ' Y=',F12.6,' YS=',F12.6,' YO=',F12.6)
C
      XPS = (X + XOFFST) * INPS
      YPS = (Y + YOFFST) * INPS
C
C --- BRANCH ACCORDING TO THE BEAM BRIGHTNESS.
C
      IF( ON ) THEN
#ifdef NOSVE
         WRITE(LUN, 100) XPS, YPS, XPOS, YPOS, CHAR(108)
#else
         WRITE(LUN, 100) XPS, YPS, XPOS, YPOS
#endif
         EMPTYF = .FALSE.
      ELSE
#ifdef NOSVE
         WRITE(LUN, 200) XPS, YPS, CHAR(109)
#else
         WRITE(LUN, 200) XPS, YPS
#endif
      END IF
C
      XPOS = XPS
      YPOS = YPS
C
C --- UPDATE THE LIMITS OF THE PLOT.
C
      IF( ON ) THEN
         IF( XPS .LT. XMIN ) XMIN = XPS
         IF( XPS .GT. XMAX ) XMAX = XPS
         IF( YPS .LT. YMIN ) YMIN = YPS
         IF( YPS .GT. YMAX ) YMAX = YPS
      END IF
C
#ifdef NOSVE
 100  FORMAT(F12.6, 1X, F12.6, 1X, F12.6, 1X, F12.6, 1X, A)
 200  FORMAT(F12.6, 1X, F12.6, 1X, A)
#else
 100  FORMAT(F12.6, 1X, F12.6, 1X, F12.6, 1X, F12.6, ' ^L')
 200  FORMAT(F12.6, 1X, F12.6, ' ^M')
#endif
      END
C
      SUBROUTINE PSBORD
C----------------------------------------------------------------------
C DRAW A BLACK, 1 POINT WIDE,  BORDER AROUND THE EDGE OF THE DEFINED
C CANVAS. RESTORE THE STATE ON ENTRY BEFORE RETURNING.
C----------------------------------------------------------------------
      IMPLICIT LOGICAL (A-Z)
      REAL PSIN
      PARAMETER( PSIN = 1.0/72.0 )
C
      INCLUDE 'dfxpsn.cmn'
C
      REAL SRGB(3), SWIDTH, X, Y
      SRGB(1) = RED
      SRGB(2) = GREEN
      SRGB(3) = BLUE
      SWIDTH = WIDTH
      CALL PSRGBC(0.0,0.0,0.0)
      CALL PSWID(1.0)
      X = XOFFST + PSIN
      Y = YOFFST + PSIN
      CALL PSMOVE(X,Y,.FALSE.)
      X = XOFFST + XSIZE - PSIN
      CALL PSMOVE(X,Y,.TRUE.)
      Y = YOFFST + YSIZE - PSIN
      CALL PSMOVE(X,Y,.TRUE.)
      X = XOFFST + PSIN
      CALL PSMOVE(X,Y,.TRUE.)
      Y = YOFFST + PSIN
      CALL PSMOVE(X,Y,.TRUE.)
      CALL PSWID(SWIDTH)
      CALL PSRGBC(SRGB(1),SRGB(2),SRGB(3))
      RETURN
      END
