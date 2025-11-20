      SUBROUTINE SVGNAM( SFNAME )
C---------------------------------------------------------------------
C SET THE SVG OUTPUT FILE NAME STEM TO SFNAME.
C 4 CHARS MAX ON NOS. ALLOWS FOR 3 DIGIT FRNO.
C 72 CHARS MAX ON "UNIX". ALLOWS FOR 4 CHAR EXT AND 3 DIGIT FRNO.
C RESET FRAME NUMBER. SFNAME SHOULD BE 79 OR MORE CHARACTERS.
C---------------------------------------------------------------------
      IMPLICIT LOGICAL (A-Z)
      CHARACTER*(*) SFNAME
C----
      INCLUDE 'svgcmn.cmn'
#ifdef UNIX
      SVGNL = MIN(72,LEN(SFNAME))
#endif
#ifdef VMS
      SVGNL = MIN(72,LEN(SFNAME))
#endif
#ifndef PORTF77
      SVGNL = MIN(4,LEN(SFNAME))
#endif
      SVGN(1:SVGNL) = SFNAME(1:SVGNL)
      FRNO = 0
C
      RETURN
      END
C
      SUBROUTINE SVGSAV( DOSAVE )
C----------------------------------------------------------------------
C SAVE/REPLACE OUTPUT FILES AS PERMANENT FILES ON NOS IF DOSAVE .TRUE.
C----------------------------------------------------------------------
      IMPLICIT LOGICAL (A-Z)
      LOGICAL DOSAVE
C----
      INCLUDE 'svgcmn.cmn'
      AUTOSAV = DOSAVE
C
      RETURN
      END
C
      SUBROUTINE NOSPFRT( NAME )
C----------------------------------------------------------------------
C ON NOS, MAKE LOCAL FILE NAME PERMANENT AND DELETE THE LOCAL FILE.
C NOTE THAT IF THE LOCAL FILE IS NOT DELETED, IT IS POSSIBLE TO RUN
C INTO "LOCAL FILE LIMIT" PROBLEMS.
C----------------------------------------------------------------------
      IMPLICIT LOGICAL (A-Z)
      CHARACTER*(*) NAME
#ifndef PORTF77
      INTEGER IPFERR
C
      CALL PF('REPLACE',
     +     NAME, NAME,
     +     'NA','IGNR','RC',IPFERR)
      IF( IPFERR .NE. 0 )THEN
         WRITE(6,100)IPFERR
 100     FORMAT(1X,'NOSPFRT - WARNING - PFERROR ',I4,' OCCURRED.')
      ELSE
         OPEN(UNIT=1, FILE=NAME, STATUS='OLD', ERR=1)
         CLOSE(UNIT=1, STATUS='DELETE', ERR=1)
      ENDIF
      RETURN
 1    CONTINUE
      WRITE(6,101)
 101  FORMAT(1X,'NOSPFRT - ERROR REMOVING LOCAL FILE')
      RETURN
#else
      WRITE(6,102)NAME
 102  FORMAT(1X,'ERROR, FILE NAME = ',A)
      STOP 'NOSPFRT MUST ONLY BE CALLED WHEN OS IS NOS.'
#endif
      END
C
      SUBROUTINE SVGCLR
C----------------------------------------------------------------------
C CLOSE ANY EXISTING OUTPUT FILE AND OPEN A NEW ONE.
C----------------------------------------------------------------------
      IMPLICIT LOGICAL (A-Z)
      INCLUDE 'svgcmn.cmn'
      CHARACTER*80 ALINE, CFMT
      INTEGER IOS, LNBC
      CHARACTER*53 TID
      DATA TID/'^X^M^L^N^S="^H^T^T^P://^W^W^W.^W3.^O^R^G/2000/^S^V^G"'/
C
C---- COMPLETE AND CLOSE ANY EXISTING OPEN FILE.
C---- NOTE: SVG IS CASE SENSITIVE, SO 6/12 CODE IS MANDATORY.
C
      IF( OPENED )THEN
         IF( EMPTYF )THEN
            CLOSE(UNIT=LUN, STATUS='DELETE')
         ELSE
C
C---- WRITE THE TRAILER.
C
            IF( INGROUP )THEN
               WRITE(LUN,9)'</^G>'
               INGROUP = .FALSE.
            ENDIF
            WRITE(LUN,9)'</^S^V^G>'
 9          FORMAT(A)
C
C---- CLOSE THE OUTPUT FILE, KEEPING IT.
C---- INCREMENT FRAME NUMBER.
C
            CLOSE(UNIT=LUN, STATUS='KEEP')
            OPENED = .FALSE.
C
C---- ON NOS, MAKE THE LOCAL OUTPUT FILE PERMANENT IF DESIRED.
C
#ifndef PORTF77
            IF( AUTOSAV )THEN
               CALL NOSPFRT(FNO(1:LNBC(FNO,1,1)))
            ENDIF
#endif
         ENDIF
      ENDIF
C
C---- CREATE A FILE NAME FOR THE NEW FRAME.
C---- ON "UNIX", ADD AN EXTENSION AND FORCE THE RESULT TO LOWER CASE.
C
      FRNO = FRNO + 1
#ifdef UNIX
      WRITE(FNO,100)SVGN(1:SVGNL), FRNO, '.^S^V^G'
 100  FORMAT(A,I3.3,A)
      CALL LOCASE(FNO(1:LNBC(FNO,1,1)))
#endif
#ifdef VMS
      WRITE(FNO,100)SVGN(1:SVGNL), FRNO, '.^S^V^G'
 100  FORMAT(A,I3.3,A)
#endif
#ifndef PORTF77
      WRITE(FNO,100)SVGN(1:SVGNL), FRNO
 100  FORMAT(A,I3.3)
#endif
C
C---- CREATE THE NEW FILE. OVERWRITE EXISTING.
C
      OPEN(UNIT=LUN, FILE=FNO(1:LNBC(FNO,1,1)),
     +     STATUS='UNKNOWN', FORM='FORMATTED',
     +     IOSTAT=IOS)
      IF( IOS .NE. 0 )STOP 'CANNOT CREATE SVG OUTPUT FILE.'
      REWIND(UNIT=LUN)
      OPENED = .TRUE.
      EMPTYF = .TRUE.
      INGROUP = .FALSE.
C
C---- WRITE OUT THE SVG HEADER TO THE OUTPUT FILE.
C
      WRITE(LUN,9)'<^S^V^G '
      WRITE(LUN,9)TID
C
C---- COMPLETE THE SVG HEADER WITH THE VIEWBOX.
C---- NOTE: A PREVIOUS VERSION OF THIS CODE ACCUMULATED BOUNDS TO WRITE
C---- AN APPROPRIATE VIEWBOX, WHICH NEEDED A SCRATCH FILE THAT WAS
C---- COPIED TO THE MAIN FILE. THIS MADE SVG 3 X SLOWER THAN EPS ON NOS.
C---- I DO NOT UNDERSTAND WHAT BROWSERS DO WITH VIEWBOX INFORMATION.
C---- IF THE VIEWBOX SPECIFICATION DOES NOT MATCH WIDTH AND HEIGHT, THE
C---- SVG IMAGE IS PRESENTED UNEXPECTEDLY SMALL WITH BIG WHITE BORDERS.
C---- SETTING WIDTH AND HEIGHT TO MATCH THE VIEWBOX WIDTH AND HEIGHT
C---- SEEMS TO MAKE BROWSERS DO WHAT IS DESIRED. THIS SEEMS WRONG,
C---- BUT IT IS WHAT IT IS.
C---- THIS ALLOWS THINGS TO BE SIGNIFICANTLY SIMPLIFIED, THOUGH, AND
C---- MAYBE THAT IS WHY IT IS WHAT IT IS.
C
      WRITE(CFMT,29)INT(DVXMAX+1)
 29   FORMAT(' ^W^I^D^T^H="',I5,'"')
      CALL SQSPACE(CFMT,ALINE,LUN)
      WRITE(CFMT,39)INT(DVYMAX+1)
 39   FORMAT(' ^H^E^I^G^H^T="',I5,'"')
      CALL SQSPACE(CFMT,ALINE,LUN)
      WRITE(CFMT,19)0, 0, INT(DVXMAX+1), INT(DVYMAX+1)
 19   FORMAT(' ^V^I^E^WB^O^X="',I5,'*',I5,'*',I5,'*',I5,'">')
      CALL SQSPACE(CFMT,ALINE,LUN)
C
C      PRINT 889,DVXMAX,DVYMAX
C 889  FORMAT(1X,'SIZE ',F7.2,' X ',F7.2,' PIXELS')
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
         WRITE(LUN,9)'</^G>'
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
         WRITE(LUN,9)'</^G>'
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
      LUN = 17
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
#ifndef PORTF77
      INTEGER LNBC
#endif      
C
      IF( EMPTYF )THEN
         CLOSE(UNIT=LUN, STATUS='DELETE')
      ELSE
         CLOSE(UNIT=LUN, STATUS='KEEP')
#ifndef PORTF77
         IF( AUTOSAV )THEN
            CALL NOSPFRT(FNO(1:LNBC(FNO,1,1)))
         ENDIF
#endif         
      ENDIF
      OPENED = .FALSE.
      EMPTYF = .TRUE.
C
      RETURN
      END
C
      SUBROUTINE SQSPACE( CFMT, COUT, LUN )
C----------------------------------------------------------------------
C REMOVE BLANKS FROM CFMT TO COUT.
C ANY SPACES THAT MUST BE PRESERVED IN CFMT SHOULD BE REPLACED BY *.
C ALL * IN CFMT WILL BECOME BLANKS IN COUT.
C WRITE RESULT TO LUN LUN.
C----------------------------------------------------------------------
      IMPLICIT LOGICAL (A-Z)
      CHARACTER*(*) CFMT, COUT
      INTEGER LUN
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
      WRITE(LUN,101)COUT(1:IOUT)
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
            WRITE(LUN,9)MATRIX
            WRITE(CFMT,102)STYLE,IPEN(1),IPEN(2),IPEN(3)
 102        FORMAT(A,I3,',',I3,',',I3,');')
            CALL SQSPACE(CFMT,COUT,LUN)
            WRITE(CFMT,103)STYWID,MIN(9.9,WPEN)
 103        FORMAT(A,F3.1,'" >')
            CALL SQSPACE(CFMT,COUT,LUN)
            INGROUP = .TRUE.
          ENDIF
C
C---- OUTPUT A LINE. DO NOT USE NICE FORMATTING TO SAVE SPACE.
C---- UNFORTUNATELY, SAFARI CANNOT DEAL WITH LEADING SPACES SUCH AS
C---- X1=" 23.987" (CHROME, FIREFOX, ETC. CAN, BUT NOT SAFARI).
C---- DO FIXED WIDTH OUTPUT TO CFMT, THEN REMOVE SPACES TO COUT.
         WRITE(CFMT,100)LB,XPOS,DVYMAX-YPOS,X,DVYMAX-Y
         CALL SQSPACE(CFMT,COUT,LUN)
         EMPTYF = .FALSE.
      ENDIF
 100  FORMAT(A,F8.3,'"*^Y1="',F8.3,'"*^X2="',F8.3,'"*^Y2="',F8.3,'" />')
C
C--- KEEP TRACK OF WHERE WE ARE.
      XPOS = X
      YPOS = Y
C
      RETURN
      END
C
      SUBROUTINE SVGBORD
C----------------------------------------------------------------------
C DRAW A BLACK, 1 PIXEL WIDE,  BORDER AROUND THE EDGE OF THE DEFINED
C CANVAS. RESTORE THE STATE ON ENTRY BEFORE RETURNING.
C----------------------------------------------------------------------
      IMPLICIT LOGICAL (A-Z)
      INCLUDE 'svgcmn.cmn'
C
      REAL SCPEN(3), SWPEN
      INTEGER I
C
      DO 1 I=1,3
         SCPEN(I) = CPEN(I)
 1    CONTINUE
      SWPEN = WPEN
      CALL SVGRGBC(0.0,0.0,0.0)
      CALL SVGWID(0.5)
      CALL SVGMOVE(0.5,0.5,.FALSE.)
      CALL SVGMOVE(DVXMAX-0.5,0.5,.TRUE.)
      CALL SVGMOVE(DVXMAX-0.5,DVYMAX-0.5,.TRUE.)
      CALL SVGMOVE(0.5,DVYMAX-0.5,.TRUE.)
      CALL SVGMOVE(0.5,0.5,.TRUE.)
      CALL SVGWID(0.5*SWPEN)
      CALL SVGRGBC(SCPEN(1),SCPEN(2),SCPEN(3))
      RETURN
      END
