      PROGRAM DIMTEST(OUTPUT=/132,TAPE6=OUTPUT)
C
C--- TRY OUT A FEW DIMFILM FUNCTIONS.
C
      REAL X(100), Y(100), GRID(5,5), CVALS(9)
      REAL PERC(6), Y2(100), Y3(100), X2(100)
      REAL XE(8,3), YE(8,3)
      CHARACTER*3 CAPT(6)
C
C--- GENERATE SOME DATA
C--- COSINE.
C
      DO 1 I=1,100
         X(I) = FLOAT(I) / 50.0 * 3.14159
         Y(I) = COS(X(I))
         Y2(I) = 0.5 * Y(I) + 0.5
         Y3(I) = 10.0 ** ( 3.0 * Y(I) )
 1    CONTINUE
C
C--- SINE WITH ASYMMETRIC RANDOM ERROR BARS ON BOTH AXES.
C
      DO 11 I=1,8
         XE(I,1) = FLOAT(I) / 4.0 * 3.14159
         XE(I,2) = RANF() * 0.2
         XE(I,3) = RANF() * 0.2
         YE(I,1) = SIN(XE(I,1))
         YE(I,2) = RANF() * 0.2
         YE(I,3) = RANF() * 0.2
 11   CONTINUE  
C
C--- GRID OF VALUES TO BE CONTOURED.
C
      DO 2 I=1,9
         CVALS(I) = 0.1 * I
 2       CONTINUE
      DO 3 J=1,5
         A = J - 2.5
         IF( A .LT. 0.0 )A = -A
         A = A / 2.5
         DO 4 I=1,5
            B = I - 2.5
            IF( B .LT. 0.0 )B = -B
            B = B / 2.5
            GRID(I,J) = A * B
 4          CONTINUE
 3       CONTINUE
C
C--- PIE CHART DATA
C
      PERC(1) = 10.0
      PERC(2) = 20.0
      PERC(3) = 40.0
      PERC(4) = 10.0
      PERC(5) =  5.0
      PERC(6) = 15.0
      CAPT(1) = 'A='
      CAPT(2) = 'B='
      CAPT(3) = 'C='
      CAPT(4) = 'D1='
      CAPT(5) = 'D2='
      CAPT(6) = 'E='   
C
C--- OPEN THE OUTPUT DEVICE
C
      CALL DIMSET
C      CALL EPSDIM( 2.0, 2.0, 1.0, 1.0 )
      CALL EPSNAM( 'NW' )
      CALL DEPCOL
C
C--- SELECT MODIFIED GRAPH LAYOUT
C--- AND SET THE BOUNDS.
C
      PRINT *,'GRCONA'
      CALL GRCONA
      PRINT *,'BOUNDS'
      CALL BOUNDS( 0.0, 1.0, 0.0, 1.0 )
C
C*** FRAME 1
C
C--- DO SOME BASIC PLOTTING
C
      PRINT *,'RGB'
      CALL RGB( 0.0, 1.0, 0.0 )
      PRINT *,'LINSF'
      CALL LINSF( 8.0 )
      PRINT *,'OFF2'
      CALL OFF2( 0.1, 0.1 )
      PRINT *,'ON2'
      CALL ON2( 0.9, 0.9 )
      PRINT *,'OFF2'
      CALL OFF2( 0.1, 0.8 )
      PRINT *,'RGB'
      CALL RGB( 1.0, 0.0, 0.0 )
      PRINT *,'LINSF'
      CALL LINSF( 1.0 )
C
C--- DRAW SOME FANCY TEXT
C
C      CALL LDABET( 1, 46 )
C      CALL SYMHT( 0.04 )
C     CALL ABET( 2 )
      PRINT *,'SYMTXT'
      CALL SYMTXT( 'ABCDEF*LGHIJKLMNOPQ*LRSTUVWXYZ 0123456789' )
C     CALL SETCEX( 2.0 )
      PRINT *,'OFF2'
      CALL OFF2( 0.5, 0.6 )
      PRINT *,'LINSF'
      CALL LINSF( 0.5 )
C      PRINT *,'SYMTXT'
C      CALL SYMTXT('z = *,X*+2$+ + 1*.*,1*.2Y$.$.')
C
C--- DRAW A SIMPLE GRAPH WITH LINEAR AXES
C
      PRINT *,'AUTOXY'
      CALL AUTOXY
      PRINT *,'PTPLOT'
      CALL PTPLOT( X, Y, 100, 3 )
      PRINT *,'GRDEF'
      CALL GRDEF( 'T*LEST PLOT*U', 'X', 'COS(X)', 3 )
      PRINT *,'FRAME'
      CALL FRAME
      PRINT *,'*** FIRST FRAME DONE ***'
C***
C      GOTO 2121
C
C*** FRAME 2
C
C--- PLOT SOME CONTOURS
C
      IF( .TRUE. )THEN
      CALL CINTER( 10 )
      CALL CLABHT( 0.015, 0.0 )
      CALL RGB( 1.0, 0.0, 0.0 )
      CALL CONTR( GRID, 5, 5, 9, CVALS )
      CALL XAXIS( -1.5, 2.5 )
      CALL YAXIS( -1.5, 2.5 )
      CALL RGB( 0.1, 0.1, 0.1 )
      CALL GRDEF( 'C*LONTOURS*U', 'X', 'Y', 3 )
      CALL FRAME
      ENDIF
C
C*** FRAME 3
C
C--- POLAR PLOT
C
      CALL RRANGE( 0.0, 1.0 )
      CALL TRANGE( 0.0, 6.283 )
      CALL RGB( 1.0, 0.0, 0.0 )
      CALL POLAR( X, Y, 100 )
      CALL RGB( 0.0, 1.0, 0.0 )
      CALL POLAR( Y2, X, 100 )
      CALL RGB( 0.1, 0.1, 0.1 )
      CALL POLDEF( 'COSINE', 'POLAR' )
      CALL POLOUT
      CALL POLGRD
      CALL FRAME
C
C*** FRAME 4
C
C--- PIE CHART
C
      CALL RGB( 0.1, 0.1, 0.1 )
      CALL PIECHT( 0.35, CAPT, PERC, 6, 2 )
      CALL UTITLE( 'A P*LIECHART' )
      CALL GRFRAM
      CALL FRAME
C
C*** FRAME 5
C
C--- HISTOGRAM
C
      CALL AUTOXY
      CALL HISTGR( X, Y, 25, -1.0 )
      CALL GRDEF( 'A H*LISTOGRAM', 'X', 'Y', 3 )
      CALL FRAME
C
C*** FRAME 6
C
C--- SHADED HISTOGRAM
C--- N.B. ENPANE BELOW IS *VITAL* OTHERWISE ALL THE
C--- ANNOTATION GETS PLOTTED INSIDE THE LAST HISTOGRAM
C--- BAR. THIS *MUST* BE A BUG.
C
      CALL AUTOXY
      CALL SHDEGR( X, Y, 25, 0.05, 33.0, 0.01 )
      CALL ENPANE
      CALL GRDEF( 'A*LNOTHER*U H*LISTOGRAM', 'X', 'Y', 3 )
      CALL FRAME
C
C*** FRAME 7
C
C--- GRAPH WITH MONTH AXES.
C--- N.B. THE X2 ARRAY IS OVERWRITTEN IN THIS CASE BY GRAPH
C
      CALL MONTHX( 3.0 )
      CALL AUTOXY
      CALL GRAPH( X2, Y, 25 )
      CALL GRDEF( 'A C*LOMMERCIAL *UP*LLOT', 'DATE', 'Y', 3 )
      CALL FRAME
C
C*** FRAME 8
C
C--- LOG-LIN PLOT
C
      CALL LINX
      CALL LOGY
      CALL LOGLIM( 2.0, 10000.0 )
      CALL LOGTIK( 1.0 )
      CALL XAXIS( 0.0, 6.284 )
      CALL YAXIS( 0.001, 1000.0 )
      CALL RGB( 0.1, 0.1, 1.0 )
      CALL GRAPH( X, Y3, 100 )
      CALL RGB( 0.1, 0.1, 0.1 )
      CALL GRDEF( 'A L*LOG-*UL*LIN *UP*LLOT', '*LX',
     1            '*LEXP10(3 COS(X))', 3 )
      CALL LYATIK
      CALL XYGRID
      CALL FRAME
C
C*** FRAME 9
C
C--- MULTIPLE GRAPHS ON ONE FRAME
C
      CALL LINX
      CALL LINY
C
C--- DEFINE A BLANKED BOX.
C
      CALL BLANK( 0.8, 0.95, 0.85, 0.9 )
C
C--- BOTTOM LEFT PANE.
C
      IF( .TRUE. )THEN
      CALL PANE( 0.0, 0.5, 0.0, 0.5 )
      CALL CINTER( 10 )
      CALL CLABHT( 0.009, 0.0 )
      CALL RGB( 1.0, 0.0, 0.0 )
      CALL CONTR( GRID, 5, 5, 9, CVALS )
      CALL XAXIS( -1.5, 2.5 )
      CALL YAXIS( -1.5, 2.5 )
      CALL RGB( 0.1, 0.1, 0.1 )
      CALL GRDEF( 'C*LONTOURS*U', 'X', 'Y', 3 )
      ENDIF
C
C--- BOTTOM RIGHT PANE.
C
      CALL PANE( 0.5, 1.0, 0.0, 0.5 )
      CALL AUTOXY
      CALL PTPLOT( X, Y, 100, 3 )
      CALL GRDEF( 'T*LEST PLOT*U', 'X', 'COS(X)', 3 )
C
C--- TOP LEFT PANE.
C
      CALL PANE( 0.0, 0.5, 0.5, 1.0 )      
      CALL RRANGE( 0.0, 1.0 )
      CALL TRANGE( 0.0, 6.283 )
      CALL RGB( 1.0, 0.0, 0.0 )
      CALL POLAR( X, Y, 100 )
      CALL RGB( 0.0, 1.0, 0.0 )
      CALL POLAR( Y2, X, 100 )
      CALL RGB( 0.1, 0.1, 0.1 )
      CALL POLDEF( 'COSINE', 'POLAR' )
      CALL POLOUT
      CALL POLGRD
C
C--- TOP RIGHT PANE.
C
      CALL PANE( 0.5, 1.0, 0.5, 1.0 )
      CALL LINX
      CALL LOGY
      CALL XAXIS( 0.0, 6.284 )
      CALL YAXIS( 0.001, 1000.0 )
      CALL RGB( 0.1, 0.1, 1.0 )
      CALL GRAPH( X, Y3, 100 )
      CALL RGB( 0.1, 0.1, 0.1 )
      CALL GRDEF( 'A L*LOG-*UL*LIN *UP*LLOT', '*LX',
     1            '*LEXP10(3 COS(X))', 3 )
      CALL XYGRID
C
C--- STOP CLIPPING OUT THE BLANKED BOX
C
      CALL OBLANK
      CALL ENBLNK
C
C--- NOW PUT A STRING CENTERED IN THE BLANKED BOX.
C
      SWID = STRING( 'BLANK' )
      DRWID = ( 0.9 * ( 0.95 - 0.8 ) )
      XS = 0.5 * ( ( 0.95 + 0.8 ) - DRWID )
      IF( ABS(SWID) .LT. 0.0001 )THEN
         SWID = 1.0
         PRINT *, 'SWID FIXED.'
      ENDIF
      SHT = DRWID / SWID
      YS = 0.5 * ( 0.9 + 0.85 )
      CALL OFF2( XS, YS )
      CALL SYMHT( SHT )
      CALL SYMTXT( 'BLANK' )
C
C--- OUTPUT THE FRAME
C
      CALL FRAME
C
C*** FRAME 10
C
C--- PLOT WITH ERROR BARS AND POLYNOMIAL INTERPOLATION.
C--- N.B. POLY5E DOES NOT ACTUALLY SEEM TO PLOT THE 
C---      ERROR BARS.
C
      CALL ENPANE
      CALL LINX
      CALL LINY
      CALL AUTOXY
      CALL RGB( 0.1, 0.1, 1.0 )
      CALL POLY5E( XE, YE, 8, 3, 3 )
      CALL SAMEXY
      CALL RGB( 1.0, 0.1, 0.1 )
      CALL PTPLTE( XE, YE, 8, 3, 3, 3 )
      CALL RGB( 0.1, 1.0, 0.1 )
      CALL GRAPH( X, Y, 100 )
      CALL RGB( 0.1, 0.1, 0.1 )
      CALL GRDEF( 'P*LOLYNOMIAL INTERPOLATION AND ERROR BARS', 
     1            '*LX*U', '*LSIN(X)*U', 3 )      
      CALL FRAME
C
C--- ALL DONE
C
 2121 CONTINUE
      PRINT *,'DIMEND'
      CALL DIMEND
      PRINT *,'YES, I HAVE COME TO THE END. HOORAY!'
      STOP
      END
