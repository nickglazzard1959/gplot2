      SUBROUTINE SVGNAM( SFNAME )
C---------------------------------------------------------------------
C SET THE SVG OUTPUT FILE NAME STEM TO SFNAME. 4 CHARS MAX ON NOS.
C 72 CHARS MAX ON "UNIX". ALLOWS FOR 4 CHAR EXT AND 3 DIGIT FRNO.
C RESET FRAME NUMBER. SFNAME SHOULD BE 79 OR MORE CHARACTERS.
C---------------------------------------------------------------------
      IMPLICIT LOGICAL (A-Z)
      CHARACTER*(*) SFNAME
C----
      INCLUDE 'svgcmn.cmn'
#ifdef UNIX
      SVGNL = MIN(72,LEN(SFNAME))
#else
      SVGNL = MIN(4,LEN(SFNAME))
#endif
      SVGN(1:SVGNL) = SFNAME(1:SVGNL)
      FRNO = 0
C
      RETURN
      END
C
      SUBROUTINE SVGCLR
C----------------------------------------------------------------------
C CLOSE ANY EXISTING OUTPUT FILE AND OPEN A NEW ONE.
C----------------------------------------------------------------------
      IMPLICIT LOGICAL (A-Z)
      INCLUDE 'svgcmn.cmn'
      CHARACTER*80 FNO, ALINE, CFMT
      INTEGER IOS, LNBC
      INTEGER IXMIN, IYMIN, IWIDTH, IHEIGHT
      CHARACTER*53 TID
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
            IYMIN = INT(DVYMAX - MIN(DVYMAX,YMAX))
            IWIDTH = INT(XMAX - XMIN) + 1
            IHEIGHT = INT(YMAX - YMIN) + 1
            WRITE(CFMT,29)IWIDTH
 29         FORMAT(' ^W^I^D^T^H="',I4,'"')
            CALL SQSPACE(CFMT,ALINE,LUN)
            WRITE(CFMT,39)IHEIGHT
 39         FORMAT(' ^H^E^I^G^H^T="',I4,'"')
            CALL SQSPACE(CFMT,ALINE,LUN)
            WRITE(CFMT,19)0, 0, INT(DVXMAX+1), INT(DVYMAX+1)
 19         FORMAT(' ^V^I^E^WB^O^X="',I5,'*',I5,'*',I5,'*',I5,'">')
            CALL SQSPACE(CFMT,ALINE,LUN)
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
C---- INCREMENT FRAME NUMBER.
C
            CLOSE(UNIT=LUN, STATUS='KEEP')
            CLOSE(UNIT=SLUN, STATUS='DELETE')
            OPENED = .FALSE.
         ENDIF
      ENDIF
C
C---- CREATE A FILE NAME FOR THE NEW FRAME.
C
      FRNO = FRNO + 1
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
      XMIN = DVYMAX + 1
      YMIN = DVYMAX + 1
      XMAX = -1.0
      YMAX = -1.0
C
      PRINT 889,DVXMAX,DVYMAX
 889  FORMAT(1X,'SIZE ',F7.3,' X ',F7.3,' PIXELS')
C
      RETURN
      END
C
      SUBROUTINE SVGWID( WIDTH )
C----------------------------------------------------------------------
C SET THE WIDTH OF THE SVG PEN.
C----------------------------------------------------------------------
      IMPLICIT LOGICAL (A-Z)
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
C
      RETURN
      END
C
      SUBROUTINE SVGRGBC( R, G, B )
C----------------------------------------------------------------------
C SET THE COLOUR OF THE SVG PEN.
C----------------------------------------------------------------------
      IMPLICIT LOGICAL (A-Z)
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
C
      RETURN
      END
C
      SUBROUTINE SVGDIM( XTOP, YTOP )
C----------------------------------------------------------------------
C SET THE SVG CANVAS SIZE IN PIXELS.
C----------------------------------------------------------------------
      IMPLICIT LOGICAL (A-Z)
      REAL XTOP, YTOP
C
      INCLUDE 'svgcmn.cmn'
      DVXMAX = MIN(9999.0,XTOP)
      DVYMAX = MIN(9999.0,YTOP)
      DVSET = .TRUE.
C
      RETURN
      END
C
      SUBROUTINE SVGDGT( UXS, UYS )
C --- ------------------------------------------------------------------
C RETURN THE SVG CANVAS SIZE PREVIOUSLY SET WITH SVGDIM().
C----------------------------------------------------------------------
      IMPLICIT LOGICAL (A-Z)
      REAL UXS, UYS
C
      INCLUDE 'svgcmn.cmn'
C
      UXS = DVXMAX
      UYS = DVYMAX
C
      RETURN
      END
C
      SUBROUTINE SVGBEGN
C----------------------------------------------------------------------
C INITIALIZE SVG OUTPUT.
C----------------------------------------------------------------------
      IMPLICIT LOGICAL (A-Z)
      INCLUDE 'svgcmn.cmn'
C      
      LUN = 12
      SLUN = 13
      OPENED = .FALSE.
      EMPTYF = .TRUE.
      INGROUP = .FALSE.
      IF( .NOT. DVSET )THEN
         DVXMAX = 799.0
         DVYMAX = 799.0
         DVSET = .TRUE.
      ENDIF
      CALL SVGCLR
C
      RETURN
      END
C
      SUBROUTINE SVGEND
C----------------------------------------------------------------------
C TERMINATE SVG OUTPUT.
C----------------------------------------------------------------------
      IMPLICIT LOGICAL (A-Z)
      INCLUDE 'svgcmn.cmn'
C
      IF( EMPTYF )THEN
         CLOSE(UNIT=LUN, STATUS='DELETE')
      ELSE
         CLOSE(UNIT=LUN, STATUS='KEEP')
      ENDIF
      CLOSE(UNIT=SLUN, STATUS='DELETE')
      OPENED = .FALSE.
      EMPTYF = .TRUE.
C
      RETURN
      END
C
      SUBROUTINE SQSPACE( CFMT, COUT, SLUN )
C----------------------------------------------------------------------
C REMOVE BLANKS FROM CFMT TO COUT.
C ANY SPACES THAT MUST BE PRESERVED IN CFMT SHOULD BE REPLACED BY *.
C ALL * IN CFMT WILL BECOME BLANKS IN COUT.
C WRITE RESULT TO LUN SLUN.
C----------------------------------------------------------------------
      IMPLICIT LOGICAL (A-Z)
      CHARACTER*(*) CFMT, COUT
      INTEGER SLUN
C
      INTEGER I, IOUT
C
      IOUT = 0
      DO 2 I=1,72
         IF( CFMT(I:I) .NE. ' ' )THEN
            IOUT = IOUT + 1
            COUT(IOUT:IOUT) = CFMT(I:I)
         ENDIF
 2    CONTINUE
C
      DO 3 I=1,IOUT
         IF( COUT(I:I) .EQ. '*' )THEN
            COUT(I:I) = ' '
         ENDIF
 3    CONTINUE
C
      WRITE(SLUN,101)COUT(1:IOUT)
 101  FORMAT(A)
C
      RETURN
      END
C
      SUBROUTINE SVGMOVE( XU, YU, ON )
C----------------------------------------------------------------------
C MOVE OR DRAW TO (XU,YU).
C----------------------------------------------------------------------
      IMPLICIT LOGICAL (A-Z)
      REAL XU, YU
      LOGICAL ON
C----
      INCLUDE 'svgcmn.cmn'
      CHARACTER*32 STYLE
      CHARACTER*24 STYWID
      CHARACTER*52 MATRIX
      CHARACTER*15 LB
      CHARACTER*72 COUT, CFMT
      INTEGER IPEN(3), I
      REAL X, Y
      DATA STYLE /'^S^T^Y^L^E="^S^T^R^O^K^E:^R^G^B('/
      DATA STYWID /'^S^T^R^O^K^E-^W^I^D^T^H:'/
      DATA MATRIX /'^T^R^A^N^S^F^O^R^M="^M^A^T^R^I^X(1 0 0 1 0 0)"'/
      DATA LB /'<^L^I^N^E*^X1="'/
C
      X = MIN(DVXMAX, MAX(0.0, XU))
      Y = MIN(DVYMAX, MAX(0.0, YU))
C
      IF( ON )THEN
C
C---- IF NOT IN A GROUP, OPEN A GROUP SETTING COLOUR AND WIDTH.
C---- NOTE THAT R, G, B VALUES MUST BE SEPARATED WITH COMMAS NOT SPACES.
         IF( .NOT. INGROUP )THEN
            DO 1 I=1,3
               IPEN(I) = INT(255.9*CPEN(I))
 1          CONTINUE
 9          FORMAT('<^G ',A)
            WRITE(SLUN,9)MATRIX
            WRITE(CFMT,102)STYLE,IPEN(1),IPEN(2),IPEN(3)
 102        FORMAT(A,I3,',',I3,',',I3,');')
            CALL SQSPACE(CFMT,COUT,SLUN)
            WRITE(CFMT,103)STYWID,MIN(9.9,WPEN)
 103        FORMAT(A,F3.1,'" >')
            CALL SQSPACE(CFMT,COUT,SLUN)
            INGROUP = .TRUE.
          ENDIF
C
C---- OUTPUT A LINE. DO NOT USE NICE FORMATTING TO SAVE SPACE.
C---- UNFORTUNATELY, SAFARI CANNOT DEAL WITH LEADING SPACES SUCH AS
C---- X1=" 23.987" (CHROME, FIREFOX, ETC. CAN, BUT NOT SAFARI).
C---- DO FIXED WIDTH OUTPUT TO CFMT, THEN REMOVE SPACES TO COUT.
         WRITE(CFMT,100)LB,XPOS,DVYMAX-YPOS,X,DVYMAX-Y
         CALL SQSPACE(CFMT,COUT,SLUN)
         EMPTYF = .FALSE.
      ENDIF
 100  FORMAT(A,F8.3,'"*^Y1="',F8.3,'"*^X2="',F8.3,'"*^Y2="',F8.3,'" />')
C
C--- KEEP TRACK OF WHERE WE ARE AND THE BOUNDS OF WHERE WE'VE BEEN.
      XPOS = X
      YPOS = Y
      XMIN = MIN(XMIN,XPOS)
      YMIN = MIN(YMIN,YPOS)
      XMAX = MAX(XMAX,XPOS)
      YMAX = MAX(YMAX,YPOS)
C
      RETURN
      END
