      SUBROUTINE DFX207(IC,N1,NN,IS,IT,RRES,IRES,ILIM)
C    PROCESS NUMBER FROM CHARACTER STRING IC -
C    IN POINTER N1 IS UPDATED
C    NN IS STRING LIMIT
C    PERMISSIBLE DELIMITERS IN STRING IS
C    IT - TYPE OF NUMBER TO BE EXTRACTED
C       -N  UNSIGNED INTEGER OF (MAXIMUM) N DIGITS
C        0  UNSIGNED INTEGER
C        1  OPTIONALLY SIGNED INTEGER
C        2  REAL
C        3  REAL WITH OPTIONAL EXPONENT
C    RRES IS RESULTANT VALUE FOR REALS
C    IRES IS RESULTANT VALUE FOR INTEGERS
C    ILIM POINTS TO DELIMITER FOUND IN IS
C        +VE  VALID DELIMITER
C         0   INVALID CHARACTER (NON-DELIMITER) FOUND
C        -1   END OF STRING ENCOUNTERED - VALID VALUE
C        -2   END OF STRING ENCOUNTERED - INVALID VALUE
C        -3   END OF STRING ENCOUNTERED - TOTALLY BLANK FIELD
C        -10-N  BLANK FIELD, DELIMITER N FOUND
      CHARACTER*(*) IC,IS
      REAL RES(2),SIGN(2)
      CHARACTER*1 C
      CHARACTER*10 DIG
      LOGICAL FIRST,BLANK,NB
      SAVE DIG
      DATA DIG/'0123456789'/
      NB = .FALSE.
      FIRST = .TRUE.
      BLANK = (INDEX(IS,' ').LE.0)
C    BLANK ENABLES BLANKS AS VALID (NOT PERMITTED IF BLANK DELIMITER)
      K = 1
      RES(1) = 0.0
      RES(2) = 0.0
      IRES = 0
      ISIGN = +1
      R1 = 10.0
      R2 = 1.0
      SIGN(1) = +1.0
      SIGN(2) = +1.0
C    SETUP NMAX FOR CASE OF IT -VE AS LIMIT OF FIELD
      NMAX = N1 - IT
    1 C = IC(N1:N1)
      N1 = N1 + 1
C    N1 POINTS TO NEXT CHARACTER
      IPT = INDEX(DIG,C)
      IF (IPT.LE.0) GO TO 2
C    HAVE DIGIT
      FIRST = .FALSE.
      NB = .TRUE.
      BLANK = .FALSE.
      IF (IT.LE.1) THEN
                        IRES = IRES*10 + (IPT-1)
                   ELSE
                        RES(K) = RES(K)*R1 + FLOAT(IPT-1)*R2
                        IF (R2.LT.1.) R2 = R2*0.1
      ENDIF
C    STRING EXHAUSTED
      IF (N1.GT.NN) GO TO 100
C    FIELD EXHAUSTED
      IF (N1.EQ.NMAX) GO TO 101
      GO TO 1
C    HERE FOR NON-DIGIT
    2 IF (BLANK.AND.(C.EQ.' ')) GO TO 1
C    UNSIGNED INTEGER ENDED
      IF (IT.LT.0) GO TO 102
C    CHECK VALID DELIMITER
      IPT = INDEX(IS,C)
      IF (IPT.GT.0) GO TO 99
      IF (IT.EQ.0) GO TO 102
      IF (C.NE.' ') NB = .TRUE.
C    NOW CHECK VALID SIGNS, ETC.
      IF (IT.EQ.1) THEN
C         OPTIONALLY SIGNED INTEGER HERE
C         CHECK FOR INVALID STRING (UNLESS ALL TRAILING BLANKS)
          IF (.NOT.FIRST) GO TO 103
          IF ((C.NE.'+').AND.(C.NE.'-')) GO TO 103
          IF (C.EQ.'-') ISIGN = -1
          BLANK = (INDEX(IS,' ').LE.0)
          FIRST = .FALSE.
      ELSEIF (IT.EQ.2) THEN
C         REAL (NO EXPONENT) HERE
          IPT = INDEX('+-.',C)
C         CHECK FOR INVALID STRING (UNLESS ALL TRAILING BLANKS)
          IF (IPT.LE.0) GO TO 103
          IF (FIRST) THEN
                  IF (IPT.EQ.2) SIGN(K) = -1.0
          ELSE
                  IF (IPT.NE.3) GO TO 103
          ENDIF
          FIRST = .FALSE.
          IF (IPT.NE.3) GO TO 1
          R1 = 1.0
          R2 = 0.1
          BLANK = .FALSE.
      ELSEIF (IT.EQ.3) THEN
C         REAL WITH EXPONENT HERE
          IPT = INDEX('+- .E',C)
C         INVALID STRING
          IF (IPT.LE.0) GO TO 104
          IF (K.EQ.2) THEN
C                    EXPONENT
C                    INVALID STRING
                     IF (IPT.GT.4) GO TO 104
C                    INVALID STRING (UNLESS ALL TRAILING BLANKS)
                     IF (.NOT.FIRST.AND.(IPT.EQ.3)) GO TO 103
          ENDIF
          IF (K.EQ.1) THEN
C                     HERE IF MANTISSA
                      IF (.NOT.FIRST) THEN
                                 IF (IPT.LE.2.OR.IPT.EQ.5) GO TO 10
                                 IF (IPT.NE.3) GO TO 11
C                   IF BLANKS PERMITTED SKIP THEM
    9                            IF (N1.GT.NN) GO TO 100
                                 C = IC(N1:N1)
                                 N1 = N1 + 1
                                 IF (C.EQ.' ') GO TO 9
                                 IPT = INDEX('+- .E',C)
                                 IF((IPT.LE.0).OR.(IPT.EQ.4)) GO TO 104
   10                            K = 2
                                 R1 = 10.0
                                 R2 = 1.0
                                 BLANK = (INDEX(IS,' ').LE.0)
   11                            CONTINUE
                      ENDIF
          ENDIF
          IF (IPT.EQ.2) SIGN(K) = -1.0
          IF (IPT.EQ.4) THEN
                          R1 = 1.0
                          R2 = 0.1
                          BLANK = .FALSE.
          ENDIF
          FIRST = (IPT.EQ.5)
      ENDIF
      GO TO 1
   99 ILIM = IPT
 998  IF (.NOT.FIRST) THEN
#ifdef NOSVE
        IF((IT.EQ.3).AND.(K.EQ.2)) RES(1)=RES(1)*(E10VE(RES(2)*SIGN(2)))
#else
        IF((IT.EQ.3).AND.(K.EQ.2)) RES(1)=RES(1)*(10.**(RES(2)*SIGN(2)))
#endif
       IF (IT.GE.2) RRES = RES(1)*SIGN(1)
                      ELSE
       ILIM = -10 - IPT
      ENDIF
      IF (IT.EQ.1) IRES = IRES*ISIGN
  999 RETURN
  100 ILIM = -1
C    VALID VALUE IF NEITHER FIRST NOR NB
      IF (.NOT.NB) ILIM = -3
      IF (FIRST) ILIM = -2
      GO TO 998
  101 ILIM = -3
      IF (NB) ILIM = 1
      GO TO 999
  102 ILIM = 0
C    AND LEAVE POINTING TO INVALID DELIMITER
      N1 = N1 - 1
      GO TO 999
C    TRAILING BLANKS PERMITTED HERE
  103 IF (C.NE.' ') GO TO 104
      IF (N1.GT.NN) GO TO 100
      C = IC(N1:N1)
      N1 = N1 + 1
      GO TO 103
  104 IPT = INDEX(IS,C)
      IF (IPT.LE.0) GO TO 102
      GO TO 99
      END
