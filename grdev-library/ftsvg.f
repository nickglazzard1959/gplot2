      SUBROUTINE SVGNAM( SFNAME )
C---------------------------------------------------------------------
C SET THE SVG OUTPUT FILE NAME STEM TO SFNAME. 4 CHARS MAX ON NOS.
C---------------------------------------------------------------------
      CHARACTER*(*) SFNAME
C----
      INCLUDE 'svgcmn.cmn'
#ifdef UNIX
      SVGNL = MIN(72,LEN(SFNAME))
#else
      SVGNL = MIN(4,LEN(SFNAME))
#endif
      SVGN(1:SVGNL) = SFNAME(1:SVGNL)
      END
C
      SUBROUTINE SVGCLR
C----------------------------------------------------------------------
C CLOSE ANY EXISTING OUTPUT FILE AND OPEN A NEW ONE.
C----------------------------------------------------------------------
      INCLUDE 'svgcmn.cmn'
      CHARACTER*80 FNO, ALINE
      INTEGER IOS, FRNO, LNBC
      CHARACTER*53 TID
      DATA FRNO/1/
      DATA TID/'^X^M^L^N^S="^H^T^T^P://^W^W^W.^W3.^O^R^G/2000/^S^V^G"'/
C
C---- COMPLETE AND CLOSE ANY EXISTING OPEN FILE.
C---- NOTE: SVG IS CASE SENSITIVE, SO 6/12 CODE IS MANDATORY.
C
      IF( OPENED )THEN
         IF( EMPTYF )THEN
            CLOSE(UNIT=LUN, STATUS='DELETE')
            CLOSE(UNIT=SLUN, STATUS='DELETE')
         ELSE
C
C---- WRITE TRAILER TO SCRATCH FILE.
C
            IF( INGROUP )THEN
               WRITE(SLUN,9)'</^G>'
               INGROUP = .FALSE.
            ENDIF
            WRITE(SLUN,9)'</^S^V^G>'
 9          FORMAT(A)
C
C---- COMPLETE THE SVG HEADER WITH THE VIEWBOX NOW WE KNOW IT.
C
            IXMIN = INT(MAX(0.0,XMIN))
            IYMIN = 800 - INT(MIN(800.0,YMAX))
            IWIDTH = INT(XMAX - XMIN) + 1
            IHEIGHT = INT(YMAX - YMIN) + 1
            WRITE(LUN,29)IWIDTH
 29         FORMAT(' ^W^I^D^T^H="',I3,'"')
            WRITE(LUN,39)IHEIGHT
 39         FORMAT(' ^H^E^I^G^H^T="',I3,'"')
            WRITE(LUN,19)IXMIN-1, IYMIN-1, IWIDTH+1, IHEIGHT+1
 19         FORMAT(' ^V^I^E^WB^O^X="',I4,I4,I4,I4,'">')
C
C---- COPY THE CONTENTS OF THE SCRATCH FILE TO THE OUTPUT FILE.
C
            REWIND(UNIT=SLUN)
 1          CONTINUE
               READ(SLUN,9,END=2)ALINE
               WRITE(LUN,9)ALINE(1:LNBC(ALINE,1,1))
               GOTO 1
 2          CONTINUE
C
C---- CLOSE THE MAIN OUTPUT FILE, KEEPING IT, AND SCRATCH, DELETING IT.
C
            CLOSE(UNIT=LUN, STATUS='KEEP')
            CLOSE(UNIT=SLUN, STATUS='DELETE')
            FRNO = FRNO + 1
            OPENED = .FALSE.
         ENDIF
      ENDIF
C
C---- CREATE A FILE NAME FOR THE NEW FRAME.
C
#ifdef UNIX
      WRITE(FNO,100)SVGN(1:SVGNL), FRNO, '.^S^V^G'
 100  FORMAT(A,I3.3,A)
#else
      WRITE(FNO,100)SVGN(1:SVGNL), FRNO
 100  FORMAT(A,I3.3)
#endif
C
C---- CREATE THE NEW FILE. OVERWRITE EXISTING.
C
      OPEN(UNIT=LUN, FILE=FNO,
     +     STATUS='UNKNOWN', FORM='FORMATTED',
     +     IOSTAT=IOS)
      IF( IOS .NE. 0 )STOP 'CANNOT CREATE SVG OUTPUT FILE.'
      REWIND(UNIT=LUN)
      OPENED = .TRUE.
      EMPTYF = .TRUE.
      INGROUP = .FALSE.
C
C---- CREATE A SCRATCH FILE TO WRITE SVG TO PRIOR TO KNOWING VIEWBOX.
C
      OPEN(UNIT=SLUN,
     +     STATUS='SCRATCH', FORM='FORMATTED',
     +     IOSTAT=IOS)
      IF( IOS .NE. 0 )STOP 'CANNOT CREATE SVG SCRATCH FILE.'
      REWIND(UNIT=SLUN)
C
C---- WRITE OUT THE PARTIAL SVG HEADER TO THE MAIN OUTPUT FILE.
C
      WRITE(LUN,9)'<^S^V^G '
      WRITE(LUN,9)TID
C
C---- SET BOUNDS TRACKING VARIABLES.
C
      XMIN = 801.0
      YMIN = 801.0
      XMAX = -1.0
      YMAX = -1.0
C
      END
C
      SUBROUTINE SVGWID( WIDTH )
C----------------------------------------------------------------------
C SET THE WIDTH OF THE SVG PEN.
C----------------------------------------------------------------------
      REAL WIDTH
C----
      INCLUDE 'svgcmn.cmn'
      WPEN = 2.0 * WIDTH
C
C---- END GROUP SO THE NEXT LINE WILL CREATE NEW GROUP WITH THIS WIDTH.
      IF( INGROUP )THEN
         WRITE(SLUN,9)'</^G>'
 9       FORMAT(A)
         INGROUP = .FALSE.
      ENDIF
      END
C
      SUBROUTINE SVGRGBC( R, G, B )
C----------------------------------------------------------------------
C SET THE COLOUR OF THE SVG PEN.
C----------------------------------------------------------------------
      REAL R, G, B
C----
      INCLUDE 'svgcmn.cmn'
      CPEN(1) = R
      CPEN(2) = G
      CPEN(3) = B
C
C---- END GROUP SO THE NEXT LINE WILL CREATE NEW GROUP WITH THIS COLOUR.
      IF( INGROUP )THEN
         WRITE(SLUN,9)'</^G>'
 9       FORMAT(A)
         INGROUP = .FALSE.
      ENDIF
      END
C
      SUBROUTINE SVGBEGN
C----------------------------------------------------------------------
C INITIALIZE SVG OUTPUT.
C----------------------------------------------------------------------
      INCLUDE 'svgcmn.cmn'
      LUN = 12
      SLUN = 13
      OPENED = .FALSE.
      EMPTYF = .TRUE.
      INGROUP = .FALSE.
      CALL SVGCLR
      END
C
      SUBROUTINE SVGEND
C----------------------------------------------------------------------
C TERMINATE SVG OUTPUT.
C----------------------------------------------------------------------
      INCLUDE 'svgcmn.cmn'
      IF( EMPTYF )THEN
         CLOSE(UNIT=LUN, STATUS='DELETE')
      ELSE
         CLOSE(UNIT=LUN, STATUS='KEEP')
      ENDIF
      CLOSE(UNIT=SLUN, STATUS='DELETE')
      OPENED = .FALSE.
      EMPTYF = .TRUE.
      END
C
      SUBROUTINE SVGMOVE( X, Y, ON )
C----------------------------------------------------------------------
C MOVE OR DRAW TO (X,Y).
C----------------------------------------------------------------------
      REAL X, Y
      LOGICAL ON
C----
      INCLUDE 'svgcmn.cmn'
      CHARACTER*32 STYLE
      CHARACTER*24 STYWID
      CHARACTER*52 MATRIX
      CHARACTER*15 LB
      CHARACTER*68 COUT, CFMT
      INTEGER IPEN(3), I, IOUT
      DATA STYLE /'^S^T^Y^L^E="^S^T^R^O^K^E:^R^G^B('/
      DATA STYWID /'^S^T^R^O^K^E-^W^I^D^T^H:'/
      DATA MATRIX /'^T^R^A^N^S^F^O^R^M="^M^A^T^R^I^X(1 0 0  1 0   0)"'/
      DATA LB /'<^L^I^N^E*^X1="'/
      IF( ON )THEN
C
C---- IF NOT IN A GROUP, OPEN A GROUP SETTING COLOUR AND WIDTH.
         IF( .NOT. INGROUP )THEN
            DO 1 I=1,3
               IPEN(I) = INT(255.9*CPEN(I))
 1          CONTINUE
 9          FORMAT('<^G ',A)
            WRITE(SLUN,9)MATRIX
            WRITE(SLUN,102)STYLE,IPEN(1),IPEN(2),IPEN(3)
 102        FORMAT(A,I3,I4,I4,');')
            WRITE(SLUN,103)STYWID,MIN(9.9,WPEN)
 103        FORMAT(A,F3.1,'" >')
            INGROUP = .TRUE.
          ENDIF
C
C---- OUTPUT A LINE. DO NOT USE NICE FORMATTING TO SAVE SPACE.
C---- UNFORTUNATELY, SAFARI CANNOT DEAL WITH LEADING SPACES SUCH AS
C---- X1=" 23.987" (CHROME, FIREFOX, ETC. CAN, BUT NOT SAFARI).
C---- DO FIXED WIDTH OUTPUT TO CFMT, THEN REMOVE SPACES TO COUT.
         WRITE(CFMT,100)LB,XPOS,800.0-YPOS,X,800.0-Y
         IOUT = 0
         DO 2 I=1,68
            IF( CFMT(I:I) .NE. ' ' )THEN
               IOUT = IOUT + 1
               COUT(IOUT:IOUT) = CFMT(I:I)
            ENDIF
 2       CONTINUE
         DO 3 I=1,IOUT
            IF( COUT(I:I) .EQ. '*' )THEN
               COUT(I:I) = ' '
            ENDIF
 3       CONTINUE
         WRITE(SLUN,101)COUT(1:IOUT)
 101     FORMAT(A)
         EMPTYF = .FALSE.
      ENDIF
 100  FORMAT(A,F7.3,'"*^Y1="',F7.3,'"*^X2="',F7.3,'"*^Y2="',F7.3,'" />')
C
C--- KEEP TRACK OF WHERE WE ARE AND THE BOUNDS OF WHERE WE'VE BEEN.
      XPOS = X
      YPOS = Y
      XMIN = MIN(XMIN,XPOS)
      YMIN = MIN(YMIN,YPOS)
      XMAX = MAX(XMAX,XPOS)
      YMAX = MAX(YMAX,YPOS)
C
      END
