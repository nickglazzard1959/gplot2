      SUBROUTINE DFX006
      INCLUDE 'params.cmn'
      INCLUDE 'dfxc01.cmn'
      INCLUDE 'dfxc01s.cmn'
      INCLUDE 'dfxc23.cmn'
      INCLUDE 'dfxc24.cmn'
      INCLUDE 'dfxc06.cmn'
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc02.cmn'
      INCLUDE 'dfxc02s.cmn'
      INCLUDE 'dfxc03.cmn'
      INCLUDE 'dfxc04.cmn'
      INCLUDE 'dfxc07.cmn'
      INCLUDE 'dfxc10.cmn'
      INCLUDE 'dfxc10s.cmn'
      INCLUDE 'dfxcba.cmn'
      INCLUDE 'dfxcbc.cmn'
      INCLUDE 'dfxcbcs.cmn'
      INCLUDE 'dfxcbd.cmn'
      INCLUDE 'dfxc13.cmn'
      INCLUDE 'dfxc13s.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
C    MAXBET IS NUMBER OF SIMULTANEOUSLY IN-CORE ALPHABETS
C    DIMFILM CURRENTLY SUPPORTS MAXBET UP TO A MAXIMUM OF 3
      PARAMETER (MAXBET=3)
      INCLUDE 'dfxc20.cmn'
      INCLUDE 'dfxc20s.cmn'
      INCLUDE 'dfxc11.cmn'
      INCLUDE 'dfxc15.cmn'
C***********************************************************************
C
C***********************************************************************
C
C    DESCRIPTION OF VARIABLES IN COMMON BLOCK DFXC00
C
C***********************************************************************
C
C    NOTE - THE LENGTH OF /DFXC00/ AND ORDER OF ITS VARIABLES MUST NOT
C           BE CHANGED WITHOUT REFLECTION IN IVAL AND BNDRY
C
C***********************************************************************
C
C
C    -----SIMPLE INTEGER VARIABLES-----
C-----------------------------------------------------------------------
C    ICHECK        INDICATOR FOR DEGREE OF CHECKING
C           0   PARAMETERS CHECKED, DEFAULTS USED FOR NON-FATAL ERRORS
C           +1  AS 0 + MESSAGE                   -1 AS 0
C           +2  AS 1 + BOUNDS CHECKING           -2 AS 2 NO MESSAGE
C                      WITH MESSAGE
C           +3  AS 2 + STOP FOR OUT OF BOUNDS    -3 AS 3 MESSAGE ONLY
C                                                        FOR STOP
C                  DEFAULT IS +2
C           ALL MESSAGES ARE OUTPUT ON PRINT FILE
C    ISAVE         CURRENTLY UNUSED INTEGER SAVE ADDRESS
C    IDASH         USED TO SELECT TYPE OF PLOTTED LINE.  DEFAULT 0
C           = 0    SOLID LINE
C           = 1    DASHED LINE    - - - - - -
C           = 2    DOTTED LINE   . . . . . . . . .
C           = 3    DASH-DOT LINE   - . - . - .
C           = 4    USER SUPPLIED LINE
C    IEFILE        DIAGNOSTIC FILE DESIGNATION
C           >=0    FORTRAN UNIT NUMBER
C           = -1   OUTPUT FILE (DEFAULT)
C           < -1   NO DIAGNOSTIC OUTPUT
C    NO1           START WORD OF CURRENT PATTERN IN XLINE
C    NO2           END WORD OF CURRENT PATTERN IN XLINE
C    N4            NUMBER OF WORDS USED IN USER SUPPLIED PATTERN,  @ 10
C    N1BLP        CURRENT ELEMENT OF BROKEN LINE PATTERN
C    INTEND        DETERMINES SPECIAL SYMBOL PLOTTING AT INTERVALS
C                  ALONG PLOTTED LINES.
C           @ -1   NO INTERUPT OF LINE
C           \ 1    SPECIAL SYMBOL DRAWN AT EACH (INTEND)TH PLOT POINT
C    INTCH         COUNTER USED BY INTRPT AND UPDATED IN ON2
C    IICHAR         INDICATES THE SPECIAL SYMBOL TO BE DRAWN VIA INTRPT
C                  PERMITTED RANGE   0@ IICHAR @ 13
C    IOBCNT        USED TO COUNT NUMBER OF OUT-OF-BOUNDS OCCURRENCES
C                  WHEN IABS(ICHECK) \ 2.  INITIALISED TO ZERO AT
C                  START OF EACH NEW FRAME
C    NOFRMS       THIS HOLDS THE NUMBER OF FRAMES GENERATED SINCE THE
C                  LAST CALL OF REPEAT - IT IS USED TO PREVENT
C                  OVERLAPPING REPEATS
C    NUMWRN       HOLDS NUMBER OF WARNING MESSAGES PRINTED -
C                  USED BY DFX130 TO TERMINATE ON PRODUCTION OF
C                  EXCESSIVE WARNING MESSAGES
C    ITRDSH        CONTROLS DASHED LINE SCALING ACROSS TRTANSFORMATIONS
C            = 0   XUNIT VALUE IS UNCHANGED I.E. RETAINS NUMERIC VALUE
C                  IN NEW COORDINATE SYSTEM
C            = 1   RESET TO .01 OF X-RANGE OF WC WINDOW OR BOUNDS (AS
C                  RELEVENT)
C            = 2   RESCALED TO RETAIN SAME PHYSICAL DIMENSION IN NDC
C                  (DEFAULT ACTION)
C    MKVIS         CONTROLS PLOTTING OF MARKERS ACCORDING TO VISIBILITY
C                  OF CURRENT POSITION, VIZ:
C                  CURRENT POSITION IN VIEW       OUT OF VIEW
C             = 1          ALL MARKER PLOTTED        NONE
C             = 2            VISIBLE PARTS           NONE
C             = 3            VISIBLE PARTS         VISIBLE PARTS
C                  (DEFAULT = 3)
C
C    NPTTYP        CONTROLS TYPE OF POINT PLOT.  NEGATIVE FOR S/W SPOT,
C                  POSITIVE FOR HARDWARE SPOT.  DEFAULT -1.
C                  (CURRENTLY TYPE 1 ONLY SUPPORTED.)
C
C    -----SIMPLE LOGICAL VARIABLES-----
C-----------------------------------------------------------------------
C    WIND          INDICATOR FOR WINDOWING.  DEFAULT .FALSE.
C           = .TRUE.  WINDOWING IS ON
C           = .FALSE.  WINDOWING IS OFF
C                  N.B. WINDOW IS HERE USED IN RESPECT OF PANE, BEING
C                  THE PRE-CLIPPED AREA OF THE CURRENT WORLD
C                  COORDINATE WINDOW (I.E. BOUNDS)
C    BLNKS         IF BLANKING CURRENTLY IN OPERATION , INDICATES
C                  WHETHER BLANKED AREA LIES WITHIN WINDOWED AREA
C           = .TRUE.  BLANKING OPERATIONS MUST BE PERFORMED
C           = .FALSE.  NO BLANKING NEED BE PERFORMED
C    BLNK          INDICATOR FOR BLANKING.  DEFAULT .FALSE.
C           = .TRUE.  BLANKING HAS BEEN CALLED
C           = .FALSE.  BLANKING TURNED OFF
C    SYMCO         INDICATES WHETHER CHARACTER STRING SHOULD CONTINUE
C                  FROM END POSITION OF PREVIOUS STRING. DEFAULT .FALSE.
C           = .TRUE.  CONTINUES FROM PREVIOUS STRING
C           = .FALSE.  STARTS FROM CURRENT PLOTTER POSITION TO WHICH
C                  BEAM OFF MOVE IS MADE AFTER OUTPUT OF STRING
C    HS            INDICATES WHETHER SYMBOL ANGLES ARE RELATIVE TO
C                  FRAME OR AXIS ROTATION (N.B. CALLS TO CHAR ARE ALWAYS
C                  RELATIVE TO AXIS ROTATION)
C           = .TRUE.  RELATIVE TO FRAME (THIS IS DEFAULT)
C           = .FALSE.  RELATIVE TO AXES
C    SPOS          DETERMINES WHETHER SYMBOL CONTINUATION IS
C                  REPOSITIONED AT FRAME ADVANCE
C           = .TRUE.  REPOSITIONED
C           = .FALSE.  NO CHANGE (DEFAULT)
C    TRHT          CONTROLS SYMBOL (TEXT + MARKER) HEIGHT SCALING ACROSS
C                  TRANSFORMATIONS
C           = .FALSE.  RETAINS NUMERIC VALUE IN NEW COORDINATES
C           = .TRUE.   RESCALED TO MAINTAIN SAME PHYSICAL NDC DIMENSION
C                      (DEFAULT)
C
C
C    -----INTEGER ARRAY VARIABLES-----
C-----------------------------------------------------------------------
C    ANGRP(4)         THERE ARE FOUR GROUPS OF ANGULAR PARAMETERS, THE
C                     FIRST BY DEFAULT ARE DEGREES, THE SECOND RADIANS,
C                     THE THIRD AND FOURTH DEGREES.
C                     THE DEFAULT MAY BE CHANGED FOR EACH GROUP
C                     INDEPENDENTLY.  THE VALUE OF THE CORRESPONDING
C                     ANGRP ELEMENT DENOTES THE INPUT FORM; VIZ,
C                     1 - DEGREES, 2 - RADIANS, 3 - GRADS.
C
C            ASSIGNMENT OF ANGULAR ARGUMENTS TO GROUPS
C            -----------------------------------------
C
C   GROUP     1             2              3              4
C          -------       -------        -------        -------
C          SYMANG        TRANGE         CIRARC         PARAL2
C          ROTATE        POLAR          DTHETA         BOX
C          HATCH         POLFN          ELSEG1         POLYGN
C          CLABHT        RADSEP
C                        POLTOA
C                        POLON
C                        POLOFF
C                        TLEVS
C                        ELSEG2 -PHI1/2 ELSEG2 -PSI
C
C
C    -----SIMPLE REAL VARIABLES-----
C-----------------------------------------------------------------------
C    X0T,Y0T       TEMPORARY ORIGIN IN USERS COORDINATE SYSTEM
C                  DEFAULT (0.0,0.0)
C    XPOS,YPOS     CURRENT PLOT POSITION IN USERS ORIGINAL COORDINATES
C                  (I.E. ALLOWANCE FOR TEMPORARY ORIGIN INCLUDED).
C                  DEFAULT (0.0,0.0)
C
C    N.B.  THE FOLLOWING PARAMETERS ARE BOUNDS DESCRIBING RECTANGULAR
C    AREAS, GIVING USER COORDINATES OF XLEFT, XRIGHT, YLOW, YUP.
C
C    XB1,XB2,YB1,YB2         OVERALL BOUNDS, USED WHEN NO WINDOWING.
C                            BY DEFAULT USES CHART AREA.
C    XTB1,XTB2,YTB1,YTB2     WINDOWING BOUNDS, BY DEFAULT USES OVERALL
C                            BOUNDS.
C    XBB1,XBB2,YBB1,YBB2     BLANKING BOUNDS WHEN BLANKING IS TURNED ON
C
C    ANGLS         THE ANGULAR ORIENTATION OF A CHARACTER STRING TO BE
C                  PLOTTED VIA SYMBOL OR NUMBER.  DEFAULT 0.0
C                  (ANG GROUP 1)
C    ANGL          CURRENTLY ACTIVE VALUE OF ANGLS
C    ANGSTP        THE ANGULAR STEP TO BE USED IN CURVES DRAWN WITH
C                  FUNCTIONS.  DEFAULT 2*PI/50.  (RADIANS)
C    HT            HEIGHT TO BE USED FOR TEXT.
C    HTMK          HEIGHT TO BE USED FOR MARKERS.
C    XS,YS         USERS COORDINATES OF POINT TO START CHARACTER STRING.
C                  UPDATED AT END OF STRING TO RESUME PLOTTING OF NEXT
C                  CHARACTER STRING.
C    XQ            SCALE FACTOR FOR UNIT LENGTH OF LINE PATTERNS.
C                  PATTERN LENGTH = ORIGINAL LENGTH*XQ. FOR DEFAULTS SEE
C                  TABLE BELOW
C    ALPHA       ANGLE (GROUP 1) OF AXIS ROTATION   DEFAULT OF 0.0
C    CALPHA      COSINE OF AXIS ROTATION ANGLE    DEFAULT 1.0
C    SALPHA      SINE OF AXIS ROTATION ANGLE      DEFAULT OF 0.0
C    TL            TOTAL LENGTH OF CURRENT PATTERN BEFORE REPETITION
C                  IN CURRENT COORDINATES
C    TL4           LENGTH OF USER SUPPLIED PATTERN SCALING.
C                  DEFAULT OF 10.0 UNITS.
C    XSPOS,YSPOS   REPOSITIONING COORDINATES FOR TEXT CONTINUATION
C                  AT FRAME ADVANCE.  SET TO LOWER LEFT OF BOUNDS
C                  AT BOUNDS CHANGES.
C    XLDONE      DISTANCE OF BROKEN LINE PATTERN COMPLETED
C
C
C    -----REAL ARRAY-----
C-----------------------------------------------------------------------
C    XLINE(18)     HOLDS LINE PATTERNS;
C            1 - 10     AVAILABLE FOR USER SUPPLIED PATTERN
C                       EACH ELEMENT BY DEFAULT SET TO 1.0 UNITS.
C           11 - 12     DASHED LINE
C           13 - 14     DOTTED LINE
C           15 - 18     DASH-DOT LINE
C
C
C
C
C***********************************************************************
C
C***********************************************************************
C
C***********************************************************************
C
C    DESCRIPTION OF VARIABLES IN COMMON BLOCK DFXC02
C
C***********************************************************************
C
C
C    -----SIMPLE INTEGER VARIABLES-----
C-----------------------------------------------------------------------
C    XTYPE         DEFINES X-AXIS TYPE
C    YTYPE         DEFINES Y-AXIS TYPE
C           = 1    LINEAR
C           = 2    LOGARITHMIC (BASE 10)
C           = 4    MONTH PLOT
C                  BY DEFAULT BOTH AXES ARE OF TYPE LINEAR
C    MONX          SPECIFIES NUMBER OF MONTHS TO APPEAR ALONG MONTH
C    MONY          TYPE AXIS.  BY DEFAULT IS ZERO, WHEN AUTO SPACING
C    NDEG          SPECIFIES DEGREE OF POLYNOMIAL INTERPOLATION IN
C                  GRAPH PLOTS.
C           = 0    NO POLYNOMIAL INTERPOLATION (THIS IS DEFAULT)
C           = 3    CUBIC POLYNOMIAL INTERPOLATION
C           = 5    POLYNOMIAL OF DEGREE 5 INTERPOLATION
C    IXSTEP        NUMBER OF STEPS BETWEEN ABSCISSAE FOR POLYNOMIAL
C                  INTERPOLATION.  BY DEFAULT = 5
C    INTNDG       HOLDS VALUE OF DFXC00 VARIABLE INTEND, USED TO
C                  RESTORE VALUE IN GRAPH ROUTINES TO ENABLE INTERUPT
C                  AT SUPPLIED POINTS.
C    RTYPE         INDICATES RADIAL TYPE
C           = 1    LINEAR (DEFAULT) - VALUES MUST BE NON-NEGATIVE
C           = 2    LOGARITHMIC (BASE 10) - VALUES MUST BE \ 1.0
C    NLEV          NUMBER OF LEVELS FOR ANGULAR GRIDDING
C                  1 @ NLEV @ 4, DEFAULT IS 3 (SEE RTHLEV)
C    IHIST         DETERMINES FORM OF HISTOGRAM
C     IABS()= 1    NORMAL - STANDARD DEFAULT
C           = 2    STAIRCASE TYPE - IMPLIES AUTO-WIDTH
C           = 3    SHADED - IMPLIES NON-LINE HISTOGRAMS
C           POSITIVE - DEFAULT, NO CHECK ON VERTICAL LINES
C           NEGATIVE - VERTICAL LINES SUPRESSED IF COINCIDENT WITH EDGE
C
C
C    -----SIMPLE REAL VARIABLES-----
C-----------------------------------------------------------------------
C    XMONTH        SPECIFIES MONTH AT WHICH MONTH AXIS PLOT SHOULD
C    YMONTH        COMMENCE.  13.>X(Y)MONTH\1.   - MAY START AT FRACTION
C                  OF MONTH.  DEFAULT IS 1.0 (I.E.JANUARY 1ST)
C    XGL,XGR       LOWER AND UPPER BOUNDS FOR X-AXIS WHEN IN NON-AUTO
C                  MODE.  USER DEFINED UNITS.
C    YGL,YGU       LOWER AND UPPER BOUNDS FOR Y-AXIS WHEN IN NON-AUTO
C                  MODE.  USER DEFINED UNITS.
C    HIST          DETERMINES TYPE OF HISTOGRAM PRODUCTION AND BAR WIDTH
C           = -10.0    DEFAULT, NO HISTOGRAM PRODUCED (ACTUAL RANGE
C                                                           HIST < -2.)
C           = -1.0     AUTO-WIDTH HISTOGRAMS PRODUCED - ADJACENT BARS
C                      TOUCH (-.5 > HIST > -2.)
C           =0.0       LINE HISTOGRAM PRODUCED  (-.5@ HIST @0.0 )
C           > 0.0      HISTOGRAM BARS ARE OF SPECIFIED WIDTH IN USERS
C                      AXIS UNITS ( HIST >0.0 )
C    XAT           AXES INTERSECT AT (XAT,YAT)
C    YAT           BY DEFAULT (0.0,0.0)
C    DIV1          SPECIFIES X-AXIS DIVISION UNITS
C    DIV2          SPECIFIES Y-AXIS DIVISION UNITS
C           = -10.0    DEFAULT, AUTOMATIC MODE
C           > 0.0      DISTANCE BETWEEN TICKS ETC IN USERS AXIS UNITS
C    RGL,RGR       LOWER AND UPPER BOUNDS FOR RADIAL VALUES WHEN IN
C                  NON-AUTO MODE.
C                  USER DEFINED UNITS.  FOR LINEAR \ 0.0
C                                       FOR LOGARITHMIC \ 1.0
C    THGL,THGR     LOWER AND UPPER BOUNDS FOR ANGULAR RANGE, ANTI-CLOCK
C                  FROM THGL TO THGR - ACCORDING TO ANGRP(2)
C                  HELD INTERNALLY IN RADIANS, IN RANGE 0,TWOPI
C    RSCALE        INTERNALLY USED SCALE FACTOR
C    RXR,RYR       CENTRE OF POLAR PLOT USED INTERNALLY.
C                  IN USERS FRAME COORDINATES.
C    RADIV         BY DEFAULT -10.0.  IF > 0.0 IS USED AS TICKING/VALUES
C                  INTERVAL FOR LINEAR RADIAL AXIS
C    DEGPRT       LIMIT OF SEPARATION FOR THGL,THGR (RADIANS) FOR
C                  SEPARATE VALUES TO BE APPLIED.  AT LESS THAN TWICE
C                  THIS SEPARATION VALUES ARE ADJACENT TO RADII
C                  I.E. THEY ARE NOT SPACED BY TICK HEIGHT
C                  NEGATIVE VALUE INDICATES AUTOMATIC COMPUTATION OF
C                  VALUE FOR DEGPRT.  NON-NEGATIVE VALUE FIXES THE
C                  APPROPRIATE VALUE.  BY DEFAULT DEGPRT = -0.1 RADIAN.
C                  THE ABSOLUTE VALUE OF DEGPRT IS USED TO DETERMINE
C                  WHETHER A 90 DEGREE RADIUS IS TO BE TICKED I.E. IF
C                  THE 90 DEGREE RADIUS IS NEARER THAN ABS(DEGPRT) TO
C                  AN EXTREMA IT WILL NOT BE TICKED
C
C    LOGMIN        THIS SETS MINIMUM NUMBER OF CYCLES ON LOG AXIS FOR
C                  WHICH LOG TICKING ETC WILL BE PERFORMED.
C                  DIST.LT.LOGMIN DIVERTS TICKS ETC TO LINEAR SPACING
C                  OF LOG VALUES.  DEFAULT VALUE 2.0
C    LOGMAX        MAXIMUM BOUND FOR INTERMEDIATE TICKING OF LOG AXIS
C                  TO BE PERFORMED.  DIST.GE.LOGMAX TICKS WHOLE
C                  CYCLES ONLY.  DEFAULT VALUE 5.0
C
C    -----REAL ARRAY-----
C-----------------------------------------------------------------------
C    RTHLEV(4)     HOLDS VALUES (IN RADIANS - ACCORDING TO ANGRP(2))
C                  FOR THE NLEV STEPS IN ANGULAR GRIDDING.  BY DEFAULT
C                  NLEV = 3, AND THE CORRESPONDING VALUES OF RTHLEV
C                  (DEGREE EQUIVALENTS) 90,45,15
C
C
C    -----SIMPLE LOGICAL VARIABLES-----
C-----------------------------------------------------------------------
C    FIXX          SPECIFIES AUTO OR NON-AUTO MODE FOR EACH AXIS
C    FIXY
C           =.TRUE.   NON-AUTO - AXIS BOUNDS ARE USER SUPPLIED
C           =.FALSE.   AUTO - AXIS BOUNDS COMPUTED.  THIS IS DEFAULT
C    AXLAB         USED AS LOCAL PARAMETER TO INDICATE LABELS ETC ARE
C           =.TRUE.   TO BE PLACED AGAINST AXIS
C           =.FALSE.   TO BE PLACED AGAINST FRAME
C    AXTK         CONTROLS SUPPRESSION OF TICKING AT AXIS POINTS
C           =.TRUE.   TICKING OCCURS
C           =.FALSE.   TICKING SUPPRESSED.  THIS IS DEFAULT
C    FIXR          SPECIFIES AUTO OR NON-AUTO MODE FOR RADIAL AXES
C    FIXTH         SPECIFIES AUTO OR NON-AUTO MODE FOR ANGULAR RANGE
C           = .TRUE.  NON-AUTO - BOUNDS USER SUPPLIED
C           = .FALSE.  AUTO - AXIS BOUNDS COMPUTED.  THIS IS DEFAULT
C    IXVAL         SELECTS INTEGRAL X-AXIS VALUING FOR LINEAR AXIS
C    IYVAL         SELECTS INTEGRAL Y-AXIS VALUING FOR LINEAR AXIS
C           = .TRUE.  INTEGRAL
C           = .FALSE.  REAL (DEFAULT)
C    IZVAL         USED FOR INTERNAL COMMUNICATION
C    CANCL         USED TO INDICATE DFX300/DFX301 SEQUENCE
C           = .TRUE.  DFX300 HAS BEEN ENTERED
C             .FALSE. STATUS RESET BY DFX301
C                      (USED TO AVOID MULTIPLE STATUS PRESERVATION)
C
C
C***********************************************************************
C
C    CHARACTER VARIABLES IN COMMON BLOCK DFXC05
C
C-----------------------------------------------------------------------
C
C    ROUTIN        CURRENT ROUTINE NAME - OCCASIONALLY USED FOR
C                  DIAGNOSTIC PURPOSES
C
C    ERRREC        HOLDS UP TO 6 RECORDS OF 120 CHARACTERS FOR
C                  OUTPUT BY DFX005 (ACCORDING TO IEFILE)
C
C-----------------------------------------------------------------------
C
C
C***********************************************************************
C
C***********************************************************************
C
C    DESCRIPTION OF SIMPLE REAL VARIABLES IN COMMON BLOCK DFXC06
C
C***********************************************************************
C
C
C    THESE HOLD CONSTANTS USED BY GRAPHICAL ROUTINES TO FIT TITLES,
C    TICKS ETC AS A FRACTION OF WINDOWED BOUNDS.
C    DC1           FRACTION OCCUPIED BY GRAPH
C    DC2           MARGIN FRACTION  DC1 + 2*DC2 = 1.0
C    DC3           OUTER MARGIN
C    DC4           OFFSET FOR X TITLE = DC3 + DC5
C    DC5           HEIGHT FOR X TITLE
C    DC6           HEIGHT FOR X,Y VALUES
C    DC7           FRACTION OF DISTANCE BETWEEN TICKS THAT MAY BE USED
C                  FOR VALUES
C    DC8           MARGIN FRACTION EXCLUDING BLANK SPACE NEXT TO PLOT
C    DC9           LENGTH OF Y VALUES STRINGS = DC8 - DC4
C    DC10          LABEL HEIGHT   DC5 - DC10 > 0.0 (I.E.TITLE > Y LABEL)
C    DC11          START POSITION FOR COMMON FACTOR ALONG SIDE
C                  .5 < DC11 < DC1 + DC2
C    DC12          FRACTION OF LENGTH TO BE USED FOR LABEL
C                  DC12 @ 2.*DC11 - 1. TO AVOID COMMON FACTOR OVERLAP
C    DC13          HEIGHT FROM BOTTOM TO X LABEL (= DC4 + SPACE)
C    DC14          FRACTION OF LENGTH TO BE OCCUPIED BY DFX306
C                                             (EXTREMA VALUES)
C                  DC14 @ .5*DC1 (TO AVOID OVERLAP OF BOTH END VALUES)
C    TC1           PRINCIPAL TICK LENGTH AS FRACTION OF WINDOW DIMENSION
C    TC2           SIZE OF SECONDARY TICK AS FRACTION OF PRINCIPAL TICK
C    TD01          USED IN DEFINITION OF DIFF01 AS FRACTION OF RANGE
C                  WHICH MAY BE CONSIDERED COINCCOMDECK AT A POINT FOR
C                  OMMISSION OF TICKING ETC.
C    RLTI          DECREASE IN INTENSITY FOR INTERMEDIATE TICKS
C                  ON LOGARITHMIC AXIS.  BY DEFAULT = .2.
C
C                  VALUES ARE SET FOR ALL THESE IN DATA STATEMENT ABOVE.
C
C***********************************************************************
C
C***********************************************************************
C
C             DESCRIPTION OF REAL ARRAYS IN DFXC13
C
C-----------------------------------------------------------------------
C
C    XNORG      NEW ORIGIN X-COORDINATE
C    YNORG      NEW ORIGIN Y-COORDINATE
C    XTORG      TEMPORARY ORIGIN X-COORDINATE
C    YTORG      TEMPORARY ORIGIN Y-COORDINATE
C    AXANG      AXIS ANGLE
C
C        N.B. ALL ABOVE ARE RELEVANT TO CORRESPONDING TRANSFORMATION
C
C
C***********************************************************************
C
C***********************************************************************
C
C    DESCRIPTION OF VARIABLES IN COMMON BLOCK DFXC01
C
C***********************************************************************
C
C
C    -----SIMPLE INTEGER VARIABLES-----
C-----------------------------------------------------------------------
C    IBET          INDICATES BASE ALPHABET
C           =      1)
C           =     ..)   96 CHARACTER FONTS PLUS (OPTIONAL) 24 ACCENTS
C           = MAXBET)
C           = MAXBET+1    96 SPECIAL SYMBOLS
C           = MAXBET+2    48 CENTRED MARKER/PLOTTING SYMBOLS
C    (N.B. DEFAULT DIMFILM SYSTEM HAS MAXBET = 3, AND DOES NOT SUPPORT
C          GREATER VALUES)
C    NBET          CURRENT VALUE OF IBET
C    NCOX,NCOY     OFFSET FROM CHARACTER BOTTOM LEFT TO ORIGIN
C                  IN FONT RASTER UNITS FOR CURRENT ALPHABET
C    CPATH         STRING DIRECTION
C           = 0    RIGHT (PERMITS ALL DIMFILM ESCAPES)
C           = 1    RIGHT
C           = 2    LEFT(1-4 PERMIT 'CASE' ESCAPES ONLY)
C           = 3    UP
C           = 4    DOWN
C                       DEFAULT = 0
C    IALT          PEN VALUE USED WHEN ALT OPERABLE
C (N)LOW           LOWER CASE/UPPER CASE/MIXED CASE MAPPING
C                  (N CURRENT STATUS)
C           = -32  LOWER CASE MAPPED TO UPPER CASE
C           = 0    NO MAPPING
C           = +32  UPPER CASE MAPPED TO LOWER CASE
C
C
C    -----SIMPLE LOGICAL VARIABLES-----
C-----------------------------------------------------------------------
C    LENGTH        IF .TRUE. CALL TO SYMTXT IS TO DETERMINE LENGTH OF
C                  STRING AND NO PLOTTING IS PERFORMED
C                  BY DEFAULT IS .FALSE.
C    NTH           TO INDICATE FIRST ENTRY (.FALSE.) OR LATER (.TRUE.)
C                  WHEN TXTCN IS .TRUE. AND NO CHANGES OF TYPE HAVE
C                  BEEN MADE.  ANY CHANGE MAKES NTH .FALSE.
C    TXTCN        WHEN .TRUE. TEXT WILL CONTINUE WITH LAST TYPE OPTIONS
C                  DETERMINED BY ALTERNATORS
C                  THESE DENOTE THE TYPE TO BE USED AT COMMENCEMENT
C                  OF A STRING.  ALL ARE .FALSE. TO BEGIN
C                  TRUE ALTERNATIVE GIVEN
C         FOLLOWING ARE BASIC TYPE   N-PREFIX IS CURRENT TYPE STYLE
C                   .TRUE. / .FALSE.
C (N)HVY           HEAVY TYPE/NORMAL TYPE
C (N)ITAL          ITALICISED/UPRIGHT
C (N)ALT           ALTERNATE PEN/STANDARD PEN
C (N)MONSP         MONO SPACED/PROPORTIONALLY SPACED
C (N)UN            UNDERLINED/NON-UNDERLINED
C    SDSYM         CONTROLS SOLID/BROKEN LINES FOR ALL SYMBOLS
C           = .TRUE.  SOLID LINES (DEFAULT)
C           = .FALSE.  BROKEN LINES
C    BACK          IF BACKSPACE CURRENTLY ACTIVE
C    BLKTXT        SET TRUE IF BLOCK TEXT ACTIVE (DEFAULT FALSE)
C    RELSUP        SET TRUE (DEFAULT) IF SUPERSCRIPTS RELATIVE TO
C                  ACTUAL HEIGHT OF PRECEDING CHARACTER, ELSE
C                  RELATIVE TO CHARACTER BOX
C    NESC          SET TRUE (DEFAULT) IF ESCAPE SEQUENCES ENABLED
C
C
C    -----SIMPLE REAL VARIABLES-----
C-----------------------------------------------------------------------
C    XPASS         USED FOR LENGTH CALL TO SYMTXT, AND FROM NUM
C                  -VE IMPLIES CALL FROM NUM
C                  +VE IS LENGTH OF STRING GENERATED
C                  BY DEFAULT IS 100.0 - SHOULD BE SO SET IN DIMSET
C    MUITAL        TANGENT OF ANGLE FOR ITALICISED TYPE (DEFAULT 0.2)
C    NUITAL        CURRENT VALUE OF MUITAL
C    ZHVY          VALUE USED WHEN HVY OPERABLE
C    SUPY          SUPERSCRIPT OFFSET IN Y-DIRECTION AS FRACTION OF HT
C    SUPH          SUPERSCRIPT HEIGHT AS FRACTION OF HT
C    SUBY          SUBSCRIPT OFFSET IN Y-DIRECTION AS FRACTION OF HT
C    SUBH          SUBSCRIPT HEIGHT AS FRACTION OF HT
C    CEXP          CHARACTER EXPANSION FACTOR (APPLIED TO FONT WIDTH)
C    CSPACE        INTER CHARACTER SPACE AS FRACTION OF HT
C    CANGL         COSINE OF ANGLE FOR SYMBOL STRING
C                  BY DEFAULT SET TO 1.0
C    SANGL         SINE OF ANGLE FOR SYMBOL STRING
C                  BY DEFAULT SET TO 0.0
C    CXF           CURRENT FACTOR TO CURRENT ALPHABET X-COORDINATE
C    CYF           CURRENT FACTOR TO CURRENT ALPHABET Y-COORDINATE
C    CX,CY         CURRENT CHARACTER COORDINATE RELATIVE TO BEGINNING
C                  OF STRING
C    CSEP          ACTUAL SPACING APPLIED AFTER CHARACTER
C    SPFACT        EXPANSION FACTOR FOR BASE LEVEL SPACES
C    SPACES        AGGREGATE LENGTH OF SPACES AT BASE LEVEL
C    CMONO         ACTUAL MONO CHARACTER WIDTH (EXCLUDING CSEP)
C    C1MONO        AS CMONO FOR UNIT HEIGHT - USED IN GRAPHICAL ROUTINES
C    C1SEP         AS CSEP FOR UNIT HEIGHT - USED IN GRAPHICAL ROUTINES
C    UNYSP         POSITIONING OF UNDERLINING BELOW BASE OF CHARACTER
C                  STRING AS FRACTION OF HEIGHT
C    FRACHT        HEIGHT OF NUMERATOR/DENOMINATOR AS FRACTION OF
C                  CURRENT HEIGHT
C    FRACM         POSITIVE Y-OFFSET OF FRACTION MID-LINE AS FRACTION
C                  OF CURRENT HEIGHT
C    FRACGP        GAP, ABOVE AND BELOW MID-LINE, AS FRACTION OF
C                  CURRENT HEIGHT
C
C                  NOTE - UNYSP*FRACHT SHOULD BE LESS THAN FRACGP
C                         TO AVOID UNDERLINING FALLING BELOW MID-LINE
C
C    OVERH         OVER/UNDER HEIGHT AS FRACTION
C    OVERY         FRACTIONAL GAP BETWEEN PREVIOUS CHARACTER HEIGHT
C                  AND BASE OF OVER CHARACTER
C    UNDERY        FRACTIONAL GAP BETWEEN BASE OF PREVIOUS CHARACTER
C                  AND HEIGHT OF UNDER CHARACTER
C    RXT           REVERSE DISTANCE TO CHARACTER CENTRE FOR BACKSPACE
C                  AND ACCENTS
C
C***********************************************************************
C
C
C
C***********************************************************************
C
C    DESCRIPTION OF VARIABLES IN COMMON BLOCK DFXC23
C
C***********************************************************************
C
C
C    -----SIMPLE INTEGER VARIABLES-----
C-----------------------------------------------------------------------
C
C    FITPT1        LOW STRING POINTER (I.E. STRING LESS THAN XFIT)
C
C    FITPT2        HIGH STRING POINTER (I.E. STRING EXCEEDS XFIT) -
C                     IF ZERO COMPLETE STRING IS LESS THAN XFIT
C
C
C    -----SIMPLE LOGICAL VARIABLES-----
C-----------------------------------------------------------------------
C    FIT           SET .TRUE. ONLY WHEN TEXT FITTING/JUSTIFICATION
C                  BEING CALCULATED - MUST BE INITIALISED AS .FALSE.
C
C
C    -----SIMPLE REAL VARIABLES-----
C-----------------------------------------------------------------------
C    XFIT          TARGET STRING LENGTH
C
C    XFIT1         LOW STRING LENGTH (I.E. AT PRECEDING SPACE)
C
C    XFIT2         HIGH STRING LENGTH (I.E. AT SPACE FOLLOWING XFIT)
C
C    SPACE1        ACCUMULATED SPACES (LOW)
C
C    SPACE2        ACCUMULATED SPACES (HIGH)
C
C
C***********************************************************************
C
C
C
C***********************************************************************
C
C    DESCRIPTION OF VARIABLES IN COMMON BLOCK DFXC24
C
C***********************************************************************
C
C
C    -----SIMPLE INTEGER VARIABLES-----
C-----------------------------------------------------------------------
C
C    RASTDX        CELL MAPPED AREA X-DIMENSION
C
C    RASTDY        CELL MAPPED AREA Y-DIMENSION
C
C    RSTERR        ERROR FLAG FOR CELL MAPPED AREA
C
C    RASTLN        SCAN LINE NUMBER FOR CURRENT SCAN LINE
C
C
C
C    -----SIMPLE REAL VARIABLES-----
C-----------------------------------------------------------------------
C    RASTPX        CELL MAPPED AREA STARTING X-COORDINATE
C
C    RASTQX        CELL MAPPED AREA FINISHING X-COORDINATE
C
C    RASTPY        CELL MAPPED AREA STARTING Y-COORDINATE
C
C    RASTQY        CELL MAPPED AREA FINISHING Y-COORDINATE
C
C
C
C***********************************************************************
C
C
C***********************************************************************
C
C    DESCRIPTION OF VARIABLES IN COMMON BLOCK DFXC07
C
C***********************************************************************
C
C
C    -----SIMPLE LOGICAL VARIABLE-----
C-----------------------------------------------------------------------
C    IVAR          INDICATES WHETHER HEIGHT PROPORTIONAL INTENSITY
C                  VARIATION FOR SYMBOLS IS ACTIVE.
C           = .FALSE.  INACTIVE (DEFAULT)
C           = .TRUE.  ACTIVE, AND CONTROLLED THROUGH DFX205
C
C    -----SIMPLE REAL VARIABLES-----
C-----------------------------------------------------------------------
C    HTFRAC        FRACTIONAL DECREASE IN HEIGHT FOR WHICH INTENSITY
C                  IS DECREASED BY ZISTEP.  (0. < HTFRAC < 1.)
C    DWAS          INTERNAL CHARACTER HEIGHT OF PREVIOUS CHARACTER.
C                  WHEN IVAR = .TRUE. USED TO DETERMINE FRACTIONAL HT
C                  CHANGE - WHEN SUCH CHANGE OCCURS.
C    DIN           INTERNAL CHARACTER HEIGHT ON ENTRY TO SYMTXT.
C                  USED WHEN IVAR = .TRUE.
C    ZISTEP        INTENSITY STEP WHEN IVAR IS SET FOR EACH FRACTIONAL
C                  HEIGHT CHANGE HTFRAC.  ZISTEP AND HTFRAC ARE
C                  USER SET THROUGH SUBROUTINE IZVAR.
C
C***********************************************************************
C
C***********************************************************************
C***********************************************************************
C
C    DESCRIPTION OF VARIABLES IN COMMON BLOCK DFXC04
C
C-----------------------------------------------------------------------
C
C    INTEGER VARIABLES
C-----------------------------------------------------------------------
C
C    IRGBN              RANGE 0:NRGB; POINTS INTO ICOLPT WHENCE
C                       REDIRECTION INTO NLUT YIELDS POINTER VALUE:
C             > 0       SUBSCRIPT (RANGE 1:NRGB) TO R,G,B MODIFIED BY
C                       ZRGB, OR NLUT - DEPENDENT ON VALUE IN NLUT
C             = 0       SELECTS FROM LOOK UP TABLE AS DETERMINED
C                       BY NLUT(0)
C                       IRGBN SPECIFIES THE CSTYPE OF THE PLOT
C                       FUNCTION, AND SO GOVERNS SELECTION OF
C                       COLOUR/INTENSITY
C
C    NLUT               ARRAY (RANGE 0:NRGB); ENTRY 0 IS POINTER INTO
C                       LOOK UP TABLE, FOR ENTRIES 1:NRGB NON-NEGATIVE
C                       IS LOOK UP TABLE POINTER, NEGATIVE IMPLIES
C                       CORRESPONDING RGB/ZRGB VALUE IS TO BE SELECTED
C                       NLUT CONTROLS ASSIGNMENT OF COLOUR/INTENSITY
C                       FOR THE CSGROUPS
C
C    ICOLPT             ARRAY (0:NRGB) SIMPLY REMAPS POINTER IRGBN
C                             - MAPS SELECTIONS FOR SECONDARY/TERTIARY
C                       COLOURS TO ALTERNATE (E.G. ENABLES ALL SECONDARY
C                       COLOUR/INTENSITY TO EQUATE TO PRIMARY SETTING)
C                       DEFAULT IS ICOLPT(I) = I. ICOLPT(0) = 0 AND
C                       ICOLPT(NRGB) = NRGB ALWAYS.  ENTRY NRGB IS USED
C                       FOR INTENSITY ALTERNATION.
C                       I.E. ICOLPT IS THE MAPPING BETWEEN THE
C                       CSTYPE AND ASSOCIATED CSGROUP
C
C             *.N.B.*   DEVCOL IN /DFXCBA/ IS SET .TRUE. FOR ALL ENTRIES
C                       WHENEVER A COLOUR SELECTION IS MADE
C
C
C    NCMODE             CURRENT USER COLOUR MODEL (SEE DFX160 FOR LIST)
C                       INITIALISED TO 0 (= RGB MODEL)
C
C      REAL ARRAY VARIABLES
C-----------------------------------------------------------------------
C
C    R,G,B              RED/GREEN/BLUE COLOUR COMPONENTS FOR EACH
C                       COLOUR SELECTION (CURRENTLY ACTIVE)
C
C    R0,G0,B0           COMPONENTS AS SET BY USER
C
C    ZRGB               INTENSITY FACTOR APPLIED TO EACH COLOUR
C
C    ZINT               INTENSITY EQUIVALENT OF COLOUR TRIPLET
C                       (USING CURRENT RELATIONSHIP OPTION)
C
C    ZINT0              INTENSITY AS SET BY USER
C
C    SFLIN              LINE WIDTH SCALE FACTOR FOR EACH COLOUR SET
C
C    SFSPOT             SPOT SCALE FACTOR FOR EACH COLOUR SET
C
C
C      SIMPLE REAL VARIABLES
C-----------------------------------------------------------------------
C    DZINT              FRACTIONAL INTENSITY DIFFERENCE FOR EQUALITY
C
C***********************************************************************
C
C***********************************************************************
C***********************************************************************
C
C***********************************************************************
C
C    DESCRIPTION OF SIMPLE REAL VARIABLES IN COMMON BLOCK DFXC03
C
C***********************************************************************
C
C
C-----------------------------------------------------------------------
C    THESE HOLD TRIGONOMETRIC CONSTANTS, USED IN GRAPHICAL AND
C    FUNCTIONAL ROUTINES, WITH ACCURACY TO SINGLE PRECISION MACHINE
C    LIMIT.  THE INTERNAL FLOATING POINT REPRESENTATIONS MAY BE GIVEN
C    FOR GREATER ACCURACY ON ANY PARTICULAR MACHINE.  THE VALUES IN THE
C    TABLE ARE ACCURATE FOR 48-BIT INTEGER COEFFICIENTS, AND SO ARE
C    SUFFICIENT FOR 60-BIT MACHINES.
C-----------------------------------------------------------------------
C
C    CON           NO. OF RADIANS IN 1 DEGREE   0.017453292519943
C    CON1          NO. OF DEGREES IN 1 RADIAN  57.295779513082
C    PIBY2         PI/2                         1.5707963267949
C    PI            PI                           3.1415926535898
C    TWOPI         2*PI                         6.2831853071796
C    ANGCON(I,J)   I = 1 CONVERSION TO RADIANS, I = 2 FROM RADIANS
C                  J = 1 DEGREES, J = 2 RADIANS, J = 3 GRADS
C                  USED FOR ALL ANGULAR INPUT/OUTPUT
C                  DIMFILM USES RADIANS INTERNALLY
C***********************************************************************
C
C***********************************************************************
C
C    DESCRIPTION OF VARIABLES IN COMMON BLOCK DFXC10
C
C***********************************************************************
C
C
C    -----REAL ARRAY-----
C-----------------------------------------------------------------------
C    DL             HOLDS VALUES OF ALOG10(I+1) - ALOG10(I)
C                   USED IN TICKING AND VALUING ROUTINES WHEN THERE
C                   IS A LOGARITHMIC AXIS
C
C***********************************************************************
C
C***********************************************************************
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C***********************************************************************
      INTEGER DEFALF(5)
      REAL XLSET(18),DCSET(17),DCEQ(17)
      SAVE XLSET, DCSET, DEFALF, DEFINT
      EQUIVALENCE (DCEQ(1),DC1)
C    EQUIVALENCE USED TO ENABLE PRESETTING
      DATA XLSET/10*1.0,1.0,-1.0,0.0,-1.0,1.0,-1.0,0.0,-1.0/
      DATA DCSET/.83,.085,.01,.03,.02,.01,.8,.07,.04,.015,.85,
     1           .68,.035,.125,.01,.5,.001/
C    SET DEFAULT FONTS FOR LOADING
      DATA DEFALF/11,15,1015,8001,9001/
C    SET DEFAULT INTENSITY FOR R,G,B TRIPLET
      DATA DEFINT/0.8/
      ICHECK = 2
      INTEND = -1
      IOBCNT = 0
      SYSERR = 0
      SYSCOD = ' '
      CON = 0.017453292519943
      CON1 = 57.295779513082
      PIBY2 = 1.5707963267949
      PI = 3.1415926535898
      TWOPI = 6.2831853071796
      ANGCON(1,1) = CON
      ANGCON(2,1) = CON1
      ANGCON(1,2) = 1.0
      ANGCON(2,2) = 1.0
      ANGCON(1,3) = TWOPI*.01
      ANGCON(2,3) = 100./TWOPI
      ANGRP(1) = 1
      ANGRP(2) = 2
      ANGRP(3) = 1
      ANGRP(4) = 1
      ANGL = 0.0
      ANGLS = 0.0
C    ANGSTP = 2*PI/50
      ANGSTP = .12566370614359
      NO1 = 1
      NO2 = 10
      N4 = 10
      TL4 = 10.
      TL = 1.0
      XQ = 1.0
      DO 1 I=1,18
    1 XLINE(I) = XLSET(I)
      DO 2 I=1,17
    2 DCEQ(I) = DCSET(I)
      N1BLP = 1
      XLDONE = 0.0
      NOFRMS = 1
      NUMWRN = 0
C    FLAG NO DATA IN ALL DIAGNOSTIC RECORDS
      DO 3 I=1,6
    3 ERRREC(I) = '*'
C    DO GLOBAL CHECK TO SEE IF INTERACTIVE SESSION
C    THIS ROUTINE MAY ALSO PERFORM OTHER SYSTEM SPECIFIC INITIALISATION
C    (E.G. DETECTING REAL/INTEGER DEFAULT LENGTHS FOR DFXM31/32/33)
      CALL DFXMS7(IA)
      SYSIA = IA.NE.0
      RLTI = .2
      IDASH = 0
      CALL DFX135(0.01)
      XS = 0.0
      YS = 0.0
      XSPOS = 0.0
      YSPOS = 0.0
      SPOS = .FALSE.
      XOT = 0.0
      YOT = 0.0
      CALPHA = 1.0
      SALPHA = 0.0
      TRHT = .TRUE.
      ITRDSH = 2
      DO 4 I=0,MAXTRS
      XNORG(I) = 0.0
      YNORG(I) = 0.0
      XTORG(I) = 0.0
      YTORG(I) = 0.0
      AXANG(I) = 0.0
      PRECLP(I) = .FALSE.
      PREBLK(I) = .FALSE.
      PREBND(I) = .FALSE.
    4 WCTRN(I) = .FALSE.
      ALPHA = 0.0
      XPOS = 0.0
      YPOS = 0.0
      SYMCO = .FALSE.
      XTYPE = 1
      YTYPE = 1
      MONX = 0
      MONY = 0
      NDEG = 0
      IXSTEP = 5
      XMONTH = 1.0
      YMONTH = 1.0
      HIST = -10.0
      DIV1 = -10.0
      DIV2 = -10.0
      IHIST = 1
      IXVAL = .FALSE.
      IYVAL = .FALSE.
      IZVAL = .FALSE.
      XAT = 0.0
      YAT = 0.0
      FIXX = .FALSE.
      FIXY = .FALSE.
      AXLAB = .FALSE.
      AXTK = .FALSE.
      RTYPE = 1
      NLEV = 3
      FIXR = .FALSE.
      FIXTH = .FALSE.
      RXR = 0.0
      RYR = 0.0
      RGL = 1.0
      RGR = 1.0
      RSCALE = 1.0
      XGL = 0.0
      XGR = 0.0
      YGL = 0.0
      YGU = 0.0
      THGL = 0.0
      THGR = 0.0
      RADIV = -10.0
      LOGMIN = 2.0
      LOGMAX = 5.0
      RTHLEV(1) = 90.0*CON
      RTHLEV(2) = 45.0*CON
      RTHLEV(3) = 15.0*CON
C    SET CONTENTS OF DL FOR GRAPHICAL ROUTINES
      DO 6 I=1,9
    6 DL(I) = LOG10(REAL(I+1)) - LOG10(REAL(I))
      CANCL = .FALSE.
C    PRESET CONTOURING VARIABLES
      IFMT = '(G9.3)'
      INTERP = .FALSE.
      CLAB = .TRUE.
      CS = 1.0
      IQUAD = 1
      PHI = 0.0
      CCHT = -1.0
      XB1 = 0.0
      YB1 = 0.0
      XTB1 = 0.0
      YTB1 = 0.0
      HS = .TRUE.
C    LOAD FONT FILE
      CALL DFXM10(FATPT)
      IF (FATPT.NE.0) CALL DFX001('DFX006',17)
C    LOAD DEFAULT ALPHABETS
      DO 7 I=1,MAXBET+2
      J = I
      IF (I.GT.MAXBET) J = 3 + I - MAXBET
      FATPT2 = DEFALF(J)
      CALL DFXM11(I,FATPT2,FATPT,INO)
      IF (FATPT.NE.0) CALL DFX001('DFX006',18)
      BETCHS(I) = INO
    7 CFONT(I) = DEFALF(J)
C    SWITCH BLOCK TEXT MODE OFF
      BLKTXT = .FALSE.
C    SET JUSTIFICATION OFF
      FIT = .FALSE.
C    SET ALL CHARACTER DEFAULTS - USER MAY RESET VIA RESTXT
      CALL DFX206
C    DEFAULT SYMBOL HEIGHT TO 1/50
      HT = .02
      HTMK = .02
      MKVIS = 3
C    CHARDEF NOT REQUIRED IN SETUP ROUTINE AS SET IN SYMTXT/SYM
C    INITIALISE COLOUR SELECTIONS, ETC.
C
C    SET INTENSITY DIFFERENCE FOR EQUALITY
      DZINT = 0.001
      DO 5 I=1,NRGB
C     NO INTENSITY REDUCTION FACTOR
      ZRGB(I) = 1.0
C    SET LINE WIDTH SCALE FACTOR FOR ALL COLOUR SETS TO 1.0
      SFLIN(I) = 1.0
C    SET SPOT SCALE FACTOR FOR ALL COLOUR SETS TO 1.0
      SFSPOT(I) = 1.0
C     SET DEFAULT POINTERS
      ICOLPT(I) = I
C     N.B. NLUT WILL BE SET NEGATIVE BY REFERENCE TO RGB -
C          INCLUDED HERE TO EMPHASISE INITIAL STATE
    5 NLUT(I) = -1
C    ZERO POINTER VALUE ALWAYS EQUATES TO ZERO
      ICOLPT(0) = 0
      NLUT(0) = 0
C    IRGBN TEMPORARILY SET TO ZERO TO FORCE DEVCOL SETTING BY DFX140
C    AND ENSURE CORRECT FUNCTIONING IN DFX143
      IRGBN = 0
C    SET ALL COLOURS TO WHITE AT DEFINT (SET IN DATA)
C    EQUIVALENT TO RGB(DEFINT,DEFINT,DEFINT)
      DO 8 I=1,NRGB
    8 CALL DFX143(I,DEFINT,DEFINT,DEFINT)
C     SELECT PRIMARY COLOUR/INTENSITY
      CALL DFX140(1)
C    LOAD DIMFILM DEFAULT LUT
      CALL DFX162
      LUTTAB = +1
C    SET COLOUR MODEL TO RGB
      NCMODE = 0
C     SELECT DIMFILM (S/W) POINTS AND SET NDC SIZE
C     (TRY TO MATCH HARDWARE POINT IN SIZE)
      DXNDPT = .00016
      NPTTYP = -1
C     SELECT GKS DEFAULT TRANSFORMATION 0
      CALL DFX0AA(0)
C    SET ACTIVE PLOT FUNCTION FLAG FOR DFX000 DIAGNOSTIC PURPOSES
      ACTFN = .TRUE.
C    NO IMMEDIATE WORK STATIONS INITIALLY
      IMM = .FALSE.
C    FLAG CELL MAPPED AREA AS UNSET
      RASTDX = 0
      RETURN
      END
