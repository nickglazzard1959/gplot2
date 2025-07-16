      PROGRAM DIMFONT(OUTPUT=/132,TAPE6=OUTPUT)
C
C TRY PLOTTING DIMFILM FONTS.
C
      PARAMETER(NFONTS=24)
      INTEGER I, J, IDX, JDX, IFONT
      INTEGER FNTNOS(NFONTS), CFONT
      REAL X, Y
      CHARACTER*64 A
      CHARACTER*5 B
C
      DATA FNTNOS/11,12,111,14,15,16,24,25,26,31,35,46,56,66,
     &1011,1014,1015,2015,8001,8002,8003,8004,8005,9001/
C
      CALL DIMSET
      CALL EPSDIM( 7.0, 7.0, 0.2, 0.2 )
      CALL EPSNAM( 'FON' )
      CALL DEPCOL
C      CALL DTEK4K
      CALL BOUNDS( 0.0, 11.0, 0.0, 12.0 )
      CALL LINSF( 1.0 )
C
C SET UP CHARACTERS TO TRY TO PLOT
C
      A =
     &'!"#$%& ()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_'
C ASC  345678901234567890123456789012345678901234567890123456789012345
C      333333344444444445555555555666666666677777777778888888888999999
C IDX  123456789012345678901234567890123456789012345678901234567890123
C      0        1         2         3         4         5         6
C                                     45678901234567890123456789012345
C                                     6     7         8         9  
      B = '*LX*L'
C
C STEP OVER ALL THE FONTS
C
      DO 9000 CFONT=1,NFONTS
         IFONT = FNTNOS(CFONT)
C
C STEP OVER A GRID. PLOT ALL CHARACTERS IN A SELECTED FONT.
C
         CALL RGB( 0.15, 0.15, 0.15 )
         PRINT *,'CALLING LDABET (1) FOR FONT ',IFONT
         CALL LDABET( 1, IFONT )
         DO 1 J=0,9
            Y = J + 1.0
            DO 2 I=0,9
               X = I + 1.0
               IDX = I + J * 10 - 2
               CALL OFF2( X, Y )
               IF( IDX .GT. 0 )THEN
                  IF( IDX .LT. 64 )THEN
                     CALL SYMTXT( A(IDX:IDX) )
                  ELSE IF( IDX .LT. 96 )THEN
                     JDX = IDX - 32
                     B(3:3) = A(JDX:JDX)
                     CALL SYMTXT( B )
                  ENDIF
               ENDIF
 2          CONTINUE
 1       CONTINUE
C
C DRAW ANNOTATION. ALWAYS IN FONT 11.
C
         PRINT *,'CALLING LDABET (2) FOR FONT 11'
         CALL LDABET( 1, 11 )
         CALL RGB( 1.0, 0.1, 0.1 )
         DO 3 J=0,10
            IF( J .LT. 10 )THEN
               Y = J + 1.0
               CALL OFF2( 0.02, Y )
               CALL INUM( 10*(J+3), '(I3)' )
            ENDIF
            Y = J + 0.5
            CALL OFF2( 0.5, Y )
            CALL ON2( 10.5, Y ) 
 3       CONTINUE
         DO 4 J=0,10
            IF( J .LT. 10 )THEN
               X = J + 1.0
               CALL OFF2( X, 0.1 )
               CALL INUM( J, '(I1)' )
            ENDIF
            X = J + 0.5
            CALL OFF2( X, 0.5 )
            CALL ON2( X, 10.5 ) 
 4       CONTINUE
         CALL OFF2( 0.5, 11.0 )
         CALL SYMTXT( 
     &   'C*LRAY*U X-MP DIMFILM *LFONT DISPLAY*U. FONT = ' )
         CALL OFF2( 7.0, 11.0 )
         CALL INUM( IFONT, '(I4)' )
C
C AND A BIT MORE ...
C
         CALL RGB( 0.7, 0.8, 0.7 )
         DO 11 J=0,9
            Y = J + 0.65
            DO 22 I=0,9
               X = I + 1.05
               IDX = I + J * 10 - 1
               IF( IDX .GT. 0 .AND. IDX .LT. 96 )THEN
                  CALL OFF2( X, Y )
                  CALL INUM( IDX, '(I2)' )
               ENDIF
 22         CONTINUE
 11      CONTINUE
C
C ADVANCE THE FRAME
C
         CALL FRAME
C
C END LOOP OVER FONTS
C
C         GOTO 9999
 9000 CONTINUE
C
C DONE
C
 9999 CONTINUE
      CALL DIMEND
      STOP
      END
