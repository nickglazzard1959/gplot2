C
C CHARS - ROUTINES FOR CHARACTER STRING MANIPULATION AND PARSING.
C================================================================
C MANY WRITTEN BY DR. ADRIAN CLARK SOMETIME IN THE EARLY-MID 1980'S.
C MODIFIED FOR CDC NOS COMPATIBILITY AND ADDITIONS BY NICK GLAZZARD.
C================================================================
C
#ifndef PORTF77
      INTEGER FUNCTION IACHAR(C)
C----------------------------------------------------(CHARS)-----------
C RETURN THE ASCII CODE FOR C REGARDLESS OF THE LOCAL CHARACTER SET.
C THIS VERSION IS FOR CDC NOS. C IS DISPLAY CODE AND THE RESULT
C WILL BE IN THE RANGE 32 TO 95 INCLUSIVE. 6/12 CODE IS NOT EXPLICITLY
C DEALT WITH (NO LOWER CASE IS POSSIBLE IN 1 CHARACTER).
C----------------------------------------------------------------------
      CHARACTER*1 C
C----
      IACHAR = ICHAR(C) + 32
      RETURN
      END
          IDENT IDCHAR
          ENTRY IDCHAR
*
* INTEGER FUNCTION IDCHAR( CHARACTER*1 C )
*----------------------------------------------------(CHARS)-----------
*    RETURN THE DISPLAY CODE CODE FOR A SINGLE CHARACTER.
*----------------------------------------------------------------------
*
 IDCHAR   BSS    1           ENTRY.
          BX2    X1          X2 = DESCRIPTOR WORD.
          SX3    X2          X3 = FIRST WORD ADDRESS (LS 18 BITS).
          SB2    24          SHIFT TO 1ST CHAR POS INFO IN DESC
          AX4    B2,X2
          MX5    -6          X5 = 6 BIT CHAR MASK.
          BX4    -X5*X4      X4 = 1ST CHAR POSITION
          SX6    0           X6 = 0 = RESULT
          SX5    9           X5 = 9
          IX5    X5-X4       X5 = (9 - CHAR POS)
          SX7    6           X7 = BITS PER CHAR
          IX5    X7*X5       X5 = 6 * (9 - CHAR POS)
          SB7    X5          B7 = INPUT SHIFT
          SA1    X3          X1 = C1
          AX6    B7,X1       X6 = SHIFT(C1,INPUT SHIFT)
          MX5    -6          X5 = 6 BIT CHAR MASK.
          BX6    -X5*X6      X6 = C1(INPUTPOS)
          EQ     IDCHAR      RETURN
          END
#endif
C
      INTEGER FUNCTION LNBC(S,B,R)
C----------------------------------------------------(CHARS)-----------
C LAST NONE BLANK CHARACTER.
C SEARCH FOR NONE BLANK IN S FROM END TO B. RETURN R IF NONE FOUND.
C----------------------------------------------------------------------
      CHARACTER*(*) S
      INTEGER B, R
C----
      INTEGER J, I, BB, IACHAR
C
      LNBC = R
      BB = MAX( B, 1 )
      DO 1 I = LEN(S), BB, -1
         J = IACHAR( S(I:I) )
         IF (J .NE. 32 .AND. J .NE. 9) THEN
            LNBC = I
            GOTO 2
         ENDIF
1     CONTINUE
2     RETURN
      END
C
      INTEGER FUNCTION NNBC(S,B,R)
C----------------------------------------------------(CHARS)-----------
C NEXT NONE BLANK CHARACTER.
C SEARCH FOR NONE BLANK IN S FROM B TO END. RETURN R IF NONE FOUND.
C----------------------------------------------------------------------
      CHARACTER*(*) S
      INTEGER B, R
C----
      INTEGER I, J, BB, IACHAR
C
      NNBC = R
      BB = MAX( B, 1 )
      DO 1 I = BB, LEN(S)
         J = IACHAR( S(I:I) )
         IF ( J .NE. 32 .AND. J .NE. 9) THEN
            NNBC = I
            GOTO 2
         ENDIF
1     CONTINUE
2     RETURN
      END
C
      INTEGER FUNCTION NOCC(S,C,B,R)
C----------------------------------------------------(CHARS)-----------
C NEXT MEMBER OF CHARACTER CLASS.
C SEARCH S FROM B TO END FOR ANY CHARACTER IN C. RETURN R IF NONE FOUND.
C----------------------------------------------------------------------
      CHARACTER*(*) S, C
      INTEGER B, R
C----
      INTEGER I, BB
      CHARACTER*1 CC
C
      NOCC = R
      BB = MAX( B, 1 )
      DO 1 I = BB, LEN(S)
         CC = S(I:I)
         IF ( INDEX(C,CC) .GT. 0 )THEN
            NOCC = I
            GOTO 2
         ENDIF
1     CONTINUE
2     RETURN
      END
C
      INTEGER FUNCTION LOCC(S,C,B,R)
C----------------------------------------------------(CHARS)-----------
C LAST OF CHARACTER CLASS.
C SEARCH S FROM END TO B FOR ANY CHARACTER IN C. RETURN R IF NONE FOUND.
C----------------------------------------------------------------------
      CHARACTER*(*) S, C
      INTEGER B, R
C----
      INTEGER I, BB
      CHARACTER*1 CC
C
      LOCC = R
      BB = MAX( B, 1 )
      DO 1 I = LEN(S), BB, -1
         CC = S(I:I)
         IF ( INDEX(C,CC) .GT. 0 )THEN
            LOCC = I
            GOTO 2
         ENDIF
1     CONTINUE
2     RETURN
      END
C
      INTEGER FUNCTION NBC(S,B,R)
C----------------------------------------------------(CHARS)-----------
C NEXT BLANK CHARACTER.
C SEARCH S FROM B TO END FOR A BLANK. RETURN R IF NONE FOUND.
C----------------------------------------------------------------------
      CHARACTER*(*) S
      INTEGER B, R
C----
      INTEGER I, J, BB, IACHAR
C
      NBC = R
      BB = MAX( B, 1 )
      DO 1 I = BB, LEN(S)
         J = IACHAR( S(I:I) )
         IF( J .EQ. 32 .OR. J .EQ. 9 )THEN
            NBC = I
            GOTO 2
         ENDIF
1     CONTINUE
2     RETURN
      END
C
      INTEGER FUNCTION LBC(S,B,R)
C----------------------------------------------------(CHARS)-----------
C LAST BLANK CHARACTER.
C SEARCH S FROM END TO B FOR A BLANK. RETURN R IF NONE FOUND.
C----------------------------------------------------------------------
      CHARACTER*(*) S
      INTEGER B, R
C----
      INTEGER I, J, BB, IACHAR
C
      LBC = R
      BB = MAX( B, 1 )
      DO 1 I = LEN(S), BB, -1
         J = IACHAR( S(I:I) )
         IF( J .EQ. 32 .OR. J .EQ. 9 )THEN
            LBC = I
            GOTO 2
         ENDIF
1     CONTINUE
2     RETURN
      END
C
      INTEGER FUNCTION LOCATE( LIST, NLIST, ITEM, FLAG )
C--------------------------------------------------------(CHARS)-------
C SEARCH FOR ITEM AS A MEMBER OF THE LIST LIST(NLIST).
C IF FLAG TRUE, DO ALL COMPARISONS IN UPPER CASE.
C----------------------------------------------------------------------
C
      INTEGER NLIST
      CHARACTER*(*) LIST(NLIST), ITEM
      LOGICAL FLAG
C----
      INCLUDE 'cmchars.cmn'
      CHARACTER*(MAXLN) UPITEM, UPLIST
      INTEGER I
C
      UPITEM = ITEM
      IF( FLAG )CALL UPCASE( UPITEM )
C
      DO 1 I = 1, NLIST
         UPLIST = LIST(I)
         IF( FLAG )CALL UPCASE( UPLIST )
         IF( UPITEM .EQ. UPLIST ) GO TO 2
    1 CONTINUE
      LOCATE = 0
      RETURN
C
    2 CONTINUE
      LOCATE = I
      RETURN
      END
C
      INTEGER FUNCTION LOOKUP( LIST, NLIST, ITEM, UPNLOW, PART )
C---------------------------------------------------------(CHARS)------
C SEARCH FOR ITEM AS A MEMBER OF THE LIST LIST(NLIST). IF PART IS TRUE
C THEN ALLOW A MATCH WHEN S IS A UNIQUE ABBREVIATION OF A MEMBER OF
C LIST. RETURN 0 FOR NO MATCH, [1:NLIST] FOR A MATCH INDICATING THE
C ELEMENT MATCHED, OR -1 FOR AMBIGUOUS MATCH WHEN PART IS TRUE.
C----------------------------------------------------------------------
C
      INTEGER NLIST
      LOGICAL UPNLOW, PART
      CHARACTER*(*) LIST(NLIST), ITEM
C----
C
      CHARACTER*1 CLIST, CITEM
      INTEGER NCIT, NC, ILIST, IT, MATCH, IC
C
C --- FUNCTIONS.
C
      INTEGER LNBC
C
C --- SETUP.
C
      NCIT = LNBC( ITEM, 1, 0 )
      MATCH = 0
      NC = LEN( LIST(1) )
C
C --- IF THE ITEM LENGTH IS GREATER THAN THE LIST MEMBER LENGTH, ITEM
C --- CANNOT BE A SUB-STRING (OR ALL OF) ANY LIST ITEM. FAIL NOW.
C
      IF( NCIT .GT. NC ) GO TO 5
C
C --- THE OUTER LOOP RUNS OVER THE LIST. FOR EACH POSITION IN THE LIST,
C --- AN ATTEMPT IS MADE TO MATCH ITEM WITH THAT LIST ELEMENT. IF A
C --- PARTIAL MATCH IS MADE, MATCH IS SET TO A NEGATIVE VALUE AND THE
C --- SCAN CONTINUES. IF AN EXACT MATCH IS FOUND, MATCH IS SET TO A
C --- POSITIVE VALUE AND THE LOOP IS EXITED.
C
      DO 4 ILIST = 1, NLIST
         DO 1 IT = 1, NCIT
            CITEM = ITEM(IT:IT)
            IF( UPNLOW ) CALL UPCASE( CITEM )
            CLIST = LIST(ILIST)(IT:IT)
            IF( UPNLOW ) CALL UPCASE( CLIST )
            IF( CLIST .NE. CITEM ) GO TO 4
    1    CONTINUE
C
C --- THE REST OF THE LIST ELEMENT MUST BE BLANK FOR AN EXACT MATCH.
C --- IF IT IS, RETURN IMMEDIATELY WITH ILIST AS THE EXACT MATCH.
C
         DO 2 IC = IT+1, NC
            IF( LIST(ILIST)(IC:IC) .NE. ' ' ) GO TO 3
    2    CONTINUE
         MATCH = ILIST
         GO TO 5
C
C --- PARTIAL MATCH CASE. IF WE DO NOT WANT PARTIAL MATCHES SKIP
C --- TO THE NEXT LIST ITEM.
C
    3    CONTINUE
         IF( .NOT. PART )GO TO 4
C
C --- ITEM IS A LEADING SUB-STRING OF LIST ENTRY ILIST. IF WE HAVE
C --- ALREADY RECORDED A MATCH, THE PREVIOUS PARTIAL MATCH WAS NOT
C --- UNIQUE AND WE MUST RETURN -VE LIST INDEX AS THE MATCH.
C
         IF( MATCH .NE. 0 ) GO TO 5
         MATCH = - ILIST
    4 CONTINUE
C
C --- IF WE REACH HERE, WE MUST HAVE, AT BEST, A UNIQUE PARTIAL MATCH.
C --- NEGATE MATCH (MAKING A PARTIAL MATCH POSITIVE), THEN RETURN.
C
      MATCH = - MATCH
C
    5 CONTINUE
      LOOKUP = MATCH
C
      RETURN
      END
C
      LOGICAL FUNCTION ALPHA(S)
C-------------------------------------------------------(CHARS)--------
C IF EVERY CHARACTER IN S IS ALPHABETIC (A-ZA-Z$_) RETURN TRUE.
C----------------------------------------------------------------------
      CHARACTER*(*) S
C----
      INTEGER IACHAR
C
      ALPHA = .TRUE.
      DO 1 I = 1, LEN(S)
         J = IACHAR( S(I:I) )
         IF( ( J .GE. 65 .AND. J .LE. 90 ) .OR.
     &       ( J .GE. 97 .AND. J .LE.122 ) ) GOTO 1
         IF( J .EQ. 36 ) GO TO 1
         IF( J .EQ. 95 ) GO TO 1
         ALPHA = .FALSE.
         GOTO 2
1     CONTINUE
2     RETURN
      END
C
      LOGICAL FUNCTION ALFNUM(S)
C-------------------------------------------------------(CHARS)--------
C IF EVERY CHARACTER IN S IS ALPHANUMERIC (A-ZA-Z$_0-9) RETURN TRUE.
C----------------------------------------------------------------------
      CHARACTER*(*) S
C----
      INTEGER IACHAR
C
      ALFNUM = .TRUE.
      DO 1 I = 1, LEN(S)
         J = IACHAR( S(I:I) )
         IF( ( J .GE. 65 .AND. J .LE. 90 ) .OR.
     &       ( J .GE. 97 .AND. J .LE.122 ) ) GOTO 1
         IF( (J .GE. 48 ) .AND. (J.LE.57) ) GO TO 1
         IF( J .EQ. 36 ) GO TO 1
         IF( J .EQ. 95 ) GO TO 1
         ALFNUM = .FALSE.
         GOTO 2
1     CONTINUE
2     RETURN
      END
C
      LOGICAL FUNCTION NUMRIC(S)
C-------------------------------------------------------(CHARS)--------
C IF EVERY CHARACTER IN S IN NUMERIC RETURN TRUE ELSE FALSE.
C----------------------------------------------------------------------
      CHARACTER*(*) S
C----
      INTEGER IACHAR
C
      NUMRIC = .TRUE.
      DO 1 I = 1, LEN(S)
         J = IACHAR( S(I:I) )
         IF( J .GT. 47 .AND. J .LT. 58 )GOTO 1
         NUMRIC = .FALSE.
         GOTO 2
1     CONTINUE
2     RETURN
      END
C
      LOGICAL FUNCTION CTOI(S,V)
C-------------------------------------------------------(CHARS)--------
C ASSUMING S IS WHOLLY NUMERIC, CONVERT IT TO AN INTEGER IN V.  IF
C IT IS FOUND THAT S IS NOT NUMERIC, RETURN FALSE ELSE TRUE.
C----------------------------------------------------------------------
      CHARACTER*(*) S
      INTEGER V
C----
      INTEGER I, J, ISIGN
C
      IB = 1
C
C---- DEAL WITH LEADING SIGN
C
      CTOI = .TRUE.
      ISIGN = 1
      IF( S(IB:IB) .EQ. '-' )THEN
         ISIGN = -1
         IB = IB + 1
      ELSE IF( S(IB:IB) .EQ. '+' )THEN
         IB = IB + 1
      ENDIF
C
C---- CONVERT THE VALUE
C
      V = 0
      DO 1 I = IB, LEN(S)
         J = INDEX('0123456789', S(I:I))
         IF( J .GT. 0 )THEN
            V = 10 * V + (J - 1)
         ELSE
            CTOI = .FALSE.
            GOTO 2
         ENDIF
1     CONTINUE
2     V = V * ISIGN
      RETURN
      END
C
      LOGICAL FUNCTION CXTOI(S,V)
C--------------------------------------------------------(CHARS)-----
C IF S CONSISTS OF HEXADECIMAL CHARACTERS, CONVERT IT TO AN INTEGER.
C SIGN CHARACTER IS NOT PROCESSED, SO THE RESULT WILL BE POSITIVE.
C IF S IS NOT VALID HEX, RETURN FALSE, ELSE TRUE.
C--------------------------------------------------------------------
      CHARACTER*(*) S
      INTEGER V
C----
      INTEGER I, J
C
      CXTOI = .TRUE.
C
C---- CONVERT THE VALUE
C
      V = 0
      DO 1 I = 1, LEN(S)
         J = INDEX('0123456789ABCDEF', S(I:I))
         IF( J .GT. 0 )THEN
            V = 16 * V + (J - 1)
         ELSE
            WRITE(6,*)' OOPS AT ',S(I:I),' I=',I
            CXTOI = .FALSE.
            GOTO 2
         ENDIF
 1    CONTINUE
 2    CONTINUE
      RETURN
      END
C
      LOGICAL FUNCTION IFROMC(S,V,B,P)
C---------------------------------------------------------(CHARS)------
C ASSUMING S IS PARTLY NUMERIC, CONVERT IT TO AN INTEGER IN V.  IF
C IT IS FOUND THAT S IS NOT NUMERIC, RETURN FALSE ELSE TRUE.
C BEGIN THE CONVERSION FROM CHARACTER B. RETURN THE POSITION OF THE
C LAST NON-DIGIT CHARACTER SCANNED IN P (OR LEN(S) ).
C----------------------------------------------------------------------
      CHARACTER*(*) S
      INTEGER V, B, P
C----
      INTEGER I, J, ISIGN
C
      IB = B
C
C---- DEAL WITH LEADING SIGN
C
      IFROMC = .TRUE.
      ISIGN = 1
      IF( S(IB:IB) .EQ. '-' )THEN
         ISIGN = -1
         IB = IB + 1
      ELSE IF( S(IB:IB) .EQ. '+' )THEN
         IB = IB + 1
      ENDIF
C
C---- CONVERT THE VALUE
C
      V = 0
      P = LEN(S)
      DO 1 I = IB, LEN(S)
         J = INDEX('0123456789', S(I:I))
         IF( J .GT. 0 )THEN
            V = 10 * V + (J - 1)
         ELSE
            IF( I .EQ. IB )IFROMC = .FALSE.
            P = I
            GOTO 2
         ENDIF
1     CONTINUE
2     V = V * ISIGN
      RETURN
      END
C
      LOGICAL FUNCTION XFROMC(S,V,B,P)
C------------------------------------------------------(CHARS)---------
C ASSUMING S IS PARTLY HEXADECIMAL, CONVERT IT TO AN INTEGER IN V.  IF
C IT IS FOUND THAT S IS NOT NUMERIC, RETURN FALSE ELSE TRUE.
C BEGIN THE CONVERSION FROM CHARACTER B. RETURN THE POSITION OF THE
C LAST NON-DIGIT CHARACTER SCANNED IN P (OR LEN(S) ).
C----------------------------------------------------------------------
      CHARACTER*(*) S
      INTEGER V, B, P
C----
      INTEGER I, J, ISIGN
C
      IB = B
C
      XFROMC = .TRUE.
      ISIGN = 1
      IF( S(IB:IB) .EQ. '-' )THEN
         ISIGN = -1
         IB = IB + 1
      ELSE IF( S(IB:IB) .EQ. '+' )THEN
         IB = IB + 1
      ENDIF
C
C---- CONVERT THE VALUE
C
      V = 0
      P = LEN(S)
      DO 1 I = IB, LEN(S)
         J = INDEX('0123456789ABCDEF', S(I:I))
         IF( J .GT. 0 )THEN
            V = 16 * V + (J - 1)
         ELSE
            IF( I .EQ. IB )XFROMC = .FALSE.
            P = I
            GOTO 2
         ENDIF
1     CONTINUE
2     V = V * ISIGN
      RETURN
      END
C
      SUBROUTINE UPCASE(S)
C-------------------------------------------------------(CHARS)--------
C CONVERT LOWERCASE ALPHABETICS IN S TO UPPERCASE.
C THIS VERSION WORKS WITH 6/12 CODE BY REMOVING ALL CARAT CHARACTERS.
C THIS WILL NOT WORK CORRECTLY FOR STRINGS WHICH CONTAIN AT (AT A) AND
C CARAT (AT B) CHARACTERS.
C----------------------------------------------------------------------
      CHARACTER*(*) S
C----
      INTEGER I, J, IACHAR, L
C
#ifndef PORTF77
      INCLUDE 'cmchars.cmn'
      CHARACTER*(MAXLN) ST
      J = 1
#endif
      L = LEN(S)
#ifndef PORTF77
      L = MIN(L,MAXLN)
#endif
      DO 1 I = 1, L
#ifdef PORTF77
         J = IACHAR( S(I:I) )
         IF( J .GE. 97 .AND. J .LE. 122 )THEN
            J = J - 32
            S(I:I) = CHAR( J )
         ENDIF
#else
         IF( S(I:I) .NE. '^' )THEN
            ST(J:J) = S(I:I)
            J = J + 1
         ENDIF
#endif
1     CONTINUE
C
#ifndef PORTF77
      S(1:J-1) = ST(1:J-1)
      DO 2 I = J, LEN(S)
         S(I:I) = ' '
2     CONTINUE
#endif
      RETURN
      END
C
      INTEGER FUNCTION UPCASE6( S, US )
C-------------------------------------------------------(CHARS)--------
C CONVERT A STRING,S, TO UPPER CASE, US, FOLLOWING ALL 6/12 RULES.
C RETURN THE LENGTH OF THE CONVERTED STRING.
C----------------------------------------------------------------------
      CHARACTER*(*) S, US
C----
C
      INCLUDE 'cmchars.cmn'
      INTEGER LNBC, I, J, L
C
C---- DO A BASIC CONVERSION, IGNORING @ ESCAPES.
C
      CALL UPCASE(S)
      LST = LNBC(S,1,LEN(S))
C
C---- PROCESS ANY @ ESCAPES.
C
      L = MIN(LEN(US),LST)
      J = 1
      I = 1
 1    CONTINUE
         IF( S(I:I) .EQ. '@' )THEN
            IF( I .LT. LST )THEN
               IF( S(I+1:I+1) .EQ. 'A' )THEN
                  US(J:J) = S(I:I)
                  I = I + 2
               ELSE IF( S(I+1:I+1) .EQ. 'B' )THEN
                  US(J:J) = '^'
                  I = I + 2
               ENDIF
            ENDIF
         ELSE
            US(J:J) = S(I:I)
            I = I + 1
         ENDIF
         IF( I .GT. L )GOTO 2
         J = J + 1
      GOTO 1
 2    CONTINUE
      UPCASE6 = J
      RETURN
      END
C
      SUBROUTINE LOCASE(S)
C-------------------------------------------------------(CHARS)--------
C CONVERT UPPERCASE ALPHABETICS IN S TO LOWERCASE.
C DOES NOTHING USEFUL ON NOS.
C----------------------------------------------------------------------
      CHARACTER*(*) S
C----
      INTEGER I, J, IACHAR
C
      DO 1 I = 1, LEN(S)
         J = IACHAR( S(I:I) )
         IF( J .GE. 65 .AND. J .LE. 90 )THEN
            J = J + 32
            S(I:I) = CHAR( J )
         ENDIF
1     CONTINUE
C
      RETURN
      END
C
      INTEGER FUNCTION LOCASE6( S, LS )
C-------------------------------------------------------(CHARS)--------
C CONVERT S TO LOWERCASE IN LS.  LS MAY NEED TO BE TWICE AS LONG AS S.
C WORKS CORRECTLY WITH 6/12 CODE ON NOS.
C RETURN THE LENGTH OF THE LOWER CASE STRING.
C----------------------------------------------------------------------
      CHARACTER*(*) S, LS
C----
      INTEGER I, J, IC
C
      J = 1
      DO 1 I = 1, LEN(S)
         IF( S(I:I) .EQ. '^' )THEN
            LS(J:J+1) = '@B'
            J = J + 2
         ELSE IF( S(I:I) .EQ. '@' )THEN
            LS(J:J+1) = '@A'
            J = J + 2
         ELSE
            IC = IACHAR( S(I:I) )
            IF( IC .GE. 65 .AND. IC .LE. 90 )THEN
               LS(J:J) = '^'
               J = J + 1
               LS(J:J) = S(I:I)
               J = J + 1
            ELSE
               LS(J:J) = S(I:I)
               J = J + 1
            ENDIF
         ENDIF
 1    CONTINUE
      LOCASE6 = J - 1
      RETURN
      END
C
      INTEGER FUNCTION NNANC(S,B,R)
C------------------------------------------------------(CHARS)---------
C SEARCH S FROM B TO END FOR NON-ALPHANUMERIC. RETURN R IF NONE FOUND.
C ALPHANUMERIC IS A-Z OR A-Z OR $ OR _ OR 0-9.
C----------------------------------------------------------------------
      CHARACTER*(*) S
      INTEGER B, R
C----
      INTEGER I
      LOGICAL ALFNUM
C
      NNANC = R
      DO 1 I = B, LEN(S)
         IF( ALFNUM( S(I:I) ) ) GO TO 1
         NNANC = I
         GOTO 2
1     CONTINUE
2     RETURN
      END
C
      INTEGER FUNCTION NANC(S,B,R)
C------------------------------------------------------(CHARS)---------
C SEARCH S FROM B TO END FOR ALPHANUMERIC. RETURN R IF NONE FOUND.
C ALPHANUMERIC IS A-Z OR A-Z OR $ OR _ OR 0-9.
C----------------------------------------------------------------------
      CHARACTER*(*) S
      INTEGER B, R
C----
      INTEGER I
      LOGICAL ALFNUM
C
      NANC = R
      DO 1 I = B, LEN(S)
         IF( ALFNUM( S(I:I) ) ) THEN
            NANC = I
            GOTO 2
         ENDIF
1     CONTINUE
2     RETURN
      END
C
      LOGICAL FUNCTION RFROMC( S, V, B, P )
C------------------------------------------------------(CHARS)---------
C CONVERT TEXT IN STRING S INTO A REAL VALUE IN V STARTING FROM
C CHARACTER B.  MOVE TEXT POINTER P TO POSITION OF NEXT CHARACTER
C NOT BELONGING TO REAL NUMBER.  RETURN TRUE IF A REAL NUMBER
C WAS CONVERTED WITHOUT ERROR, ELSE FALSE.
C----------------------------------------------------------------------
      CHARACTER*(*) S
      INTEGER B, P
      REAL V
C----
      LOGICAL GIMAN, GDMAN, GE, NUMRIC, IFROMC, GDP
C
C---- START WITH NO INTEGER PART OF MANTISSA, AND NO DECIMAL PART.
C---- AND NO EXPONENT.
C
      GIMAN = .FALSE.
      GDMAN = .FALSE.
      GE = .FALSE.
      GDP = .FALSE.
      ISLEN = LEN( S )
      IMAN = 0
      IDMAN = 0
      IEXP = 0
      V = 0.0
C
C---- TEXT POINTER TO START OF TEXT TO CONVERT
C
      P = B
C
C---- ACCOUNT FOR LEADING SIGN, IF ANY.
C
      SIGN = 1.0
      IF( S(P:P) .EQ. '+' )THEN
         P = P + 1
         IF( P .GT. ISLEN )GOTO 999
      ELSE IF( S(P:P) .EQ. '-' )THEN
         P = P + 1
         IF( P .GT. ISLEN )GOTO 999
         SIGN = -1.0
      ENDIF
C
C---- ATTEMPT TO CONVERT INTEGER PART OF MANTISSA.
C---- IF WE START WITH . THEN THIS IS ZERO.
C
      IF( S(P:P) .EQ. '.' )THEN
         IMAN = 0
         GDP = .TRUE.
         P = P + 1
         IF( P .GT. ISLEN )GOTO 999
      ELSE
         IF( NUMRIC( S(P:P) ) )THEN
            IF( .NOT. IFROMC( S, IMAN, P, NP ) )THEN
               GOTO 999
            ENDIF
            P = NP
            GIMAN = .TRUE.
         ELSE
            GOTO 999
         ENDIF
      ENDIF
C
C---- NO LEADING . BUT MAYBE . NOW.  ILLEGAL IF ALREADY GOT .
C
      IF( S(P:P) .EQ. '.' )THEN
         IF( .NOT. GIMAN )THEN
            GOTO 999
         ELSE
            GDP = .TRUE.
            P = P + 1
            IF( P .GT. ISLEN )GOTO 999
         ENDIF
      ENDIF
C
C---- THE NEXT CHARACTER MUST BE: A) START OF EXPONENT, OR
C---- B) START OF DECIMAL PART OF MANTISSA OR C) END OF NUMBER
C
      IF( S(P:P) .EQ. 'E' .OR. S(P:P) .EQ. 'E' )THEN
         GE = .TRUE.
         IDMAN = 0
         P = P + 1
         IF( P .GT. ISLEN )GOTO 999
      ELSE
         IF( NUMRIC( S(P:P) ) .AND. GDP )THEN
            IF( .NOT. IFROMC( S, IDMAN, P, NP ) )THEN
               GOTO 999
            ELSE
               NDMAN = NP - P
               IF( NP .EQ. ISLEN )NDMAN = NDMAN + 1
               P = NP
               GDMAN = .TRUE.
            ENDIF
         ENDIF
      ENDIF
C
C---- WE MUST NOW HAVE EITHER THE INTEGER OR DECIMAL PARTS OF
C---- THE MANTISSA OR BOTH.  IF NEITHER, ERROR.
C
      IF( .NOT. GIMAN .AND. .NOT. GDMAN )THEN
         GOTO 999
      ENDIF
C
C---- IF NEXT CHARACTER IS E OR E AND WE HAVE NOT ALREADY GOT
C---- EXPONENT START, THEN IT COULD BE START OF EXPONENT
C
      IF( .NOT. GE .AND. ( S(P:P) .EQ. 'E' .OR. S(P:P) .EQ. 'E' ) )THEN
         GE = .TRUE.
         P = P + 1
         IF( P .GT. ISLEN )GOTO 999
      ENDIF
C
C---- IF WE ARE AT START OF EXPONENT, GET IT
C
      IF( .NOT. GE )THEN
         IEXP = 0
      ELSE
         ESIGN = 1.0
         IF( S(P:P) .EQ. '+' )THEN
            P = P + 1
            IF( P .GT. ISLEN )GOTO 999
         ELSE IF( S(P:P) .EQ. '-' )THEN
            P = P + 1
            IF( P .GT. ISLEN )GOTO 999
            ESIGN = -1.0
         ENDIF
         IF( .NOT. NUMRIC( S(P:P) ) )THEN
            GOTO 999
         ELSE
            IF( .NOT. IFROMC( S, IEXP, P, NP ) )THEN
               GOTO 999
            ELSE
               P = NP
            ENDIF
         ENDIF
      ENDIF
C
C---- WE ARE NOW READY TO ASSEMBLE V FROM THE BITS.
C
      IF( GDMAN )THEN
         DMAN = FLOAT(IDMAN) / ( 10.0 ** NDMAN )
      ELSE
         DMAN = 0.0
      ENDIF
      V = (SIGN * (FLOAT(IMAN) + DMAN))
      IF( GE )THEN
         V = V * (10.0 ** (ESIGN * FLOAT(IEXP)))
      ENDIF
C
C---- END
C
      RFROMC = .TRUE.
      RETURN
C
C---- ABEND
C
999   RFROMC = .FALSE.
      RETURN
      END
C
      INTEGER FUNCTION GETWORD( IWORD, LINE, WORD )
C------------------------------------------------------(CHARS)---------
C GET THE IWORD'TH WORD FROM IWORD INTO WORD. RETURN -1 IF NOT FOUND.
C RETURN NUMBER OF CHARS IN WORD IF OK. WORDS ARE NUMBERED 1 UP.
C----------------------------------------------------------------------
      INTEGER IWORD
      CHARACTER*(*) LINE, WORD
C----
      INTEGER DIMLINE, DIMWORD, ILINE, I, NWORD
      GETWORD = -1
      ILINE = 1
      NWORD = 0
      DIMLINE = LEN(LINE)
      DIMWORD = LEN(WORD)
C
C---- WHILE LOOP UNTIL DESIRED WORD FOUND.
C
 1    CONTINUE
C---- EAT SPACES.
 3       CONTINUE
         IF( ILINE .GT. DIMLINE )RETURN
         IF( LINE(ILINE:ILINE) .NE. ' ')GOTO 4
         ILINE = ILINE + 1
         GOTO 3
 4       CONTINUE
C---- ILINE IS AT NON-SPACE. INCREMENT NWORD AND SEE IF IT IS IWORD.
         NWORD = NWORD + 1
         IF( NWORD .EQ. IWORD )GOTO 2
C---- EAT NON-SPACES.
 5       CONTINUE
         IF( ILINE .GT. DIMLINE )RETURN
         IF( LINE(ILINE:ILINE) .EQ. ' ')GOTO 6
         ILINE = ILINE + 1
         GOTO 5
 6       CONTINUE
         GOTO 1
 2    CONTINUE
C
C---- IWORD'TH WORD FOUND, COPY IT TO WORD.
C
      DO 7 I=1,DIMWORD
         IF( ILINE .GT. DIMLINE )GOTO 8
         IF( LINE(ILINE:ILINE) .EQ. ' ' )GOTO 8
         WORD(I:I) = LINE(ILINE:ILINE)
         ILINE = ILINE + 1
 7    CONTINUE
 8    CONTINUE
C
C---- RETURN NUMBER OF NON-SPACE CHARS IN WORD AND SPACE FILL WORD.
C
      GETWORD = I - 1
      DO 9 I=(GETWORD+1),DIMLINE
         WORD(I:I) = ' '
 9    CONTINUE
      RETURN
      END
