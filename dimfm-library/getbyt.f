      INTEGER FUNCTION GETBYT( WORD, BYTE )
C --- ------------------------------------------------------------------
C
C --- EXTRACT BYTE NUMBER BYTE FROM INTEGER WORD.
C
      INTEGER WORD, BYTE
      INTEGER IGET8B
      GETBYT = IGET8B( WORD, BYTE )
      IF( GETBYT .GT. 127 )GETBYT = GETBYT - 256
      RETURN
      END
          IDENT  IBSET
          ENTRY  IBSET       INT IBSET( INT V, INT B )
*
* SET BIT B (0 TO 59) OF V AND RETURN V. V NOT MODIFIED.
*
 IBSET    BSS    1           STORES RETURN ADDRESS
          SA2    A1+1        X2 = &B
          SA1    X1          X1 = V. NOTE THIS OVERWRITES ARGUMENT LIST
          SA2    X2          X2 = B (X2 = *X2)
          SX3    1           X3 = 1
          SB1    X2          B1 = B
          LX3    B1          X3 = 1 << B
          BX6    X3+X1       IBSET = V .OR. ( 1 << B )
          EQ     IBSET       RETURN
          END
          IDENT  IBCLR
          ENTRY  IBCLR       INT IBCLR( INT V, INT B )
*
* CLEAR BIT B (0 TO 59) OF V AND RETURN V. V NOT MODIFIED.
*
 IBCLR    BSS    1           SAVE RETURN ADDRESS
          SA2    A1+1        X2 = &B
          SA1    X1          X1 = V
          SA2    X2          X2 = B
          SX3    1           X3 = 1
          SB1    X2          B1 = B
          LX3    B1          X3 = 1 << B
          BX6    -X3*X1      IBCLR = V .AND. .NOT.( 1 << B )
          EQ     IBCLR       RETURN
          END
          IDENT  IBTEST
          ENTRY  IBTEST      INT IBTEST( INT V, INT B )
*
* TEST BIT B (0 TO 59) OF V. RETURN 1 IF SET, 0 IF CLEAR.
*
 IBTEST   BSS    1           SAVE RETURN ADDRESS
          SA2    A1+1        X2 = &B
          SA1    X1          X1 = V
          SA2    X2          X2 = B
          SX3    1           X3 = 1
          SB1    X2          B1 = B
          LX3    B1          X3 = 1 << B
          BX5    X3*X1       X5 = V .AND. ( 1 << B )
          SX6    0           IBTEST = 0
          ZR     X5,DONE     IF ( ( V .AND. ( 1 << B ) ) == 0 )GOTO DONE
          SX6    1           IBTEST = 1
 DONE     EQ     IBTEST      RETURN
          END
          IDENT  IGET8B
          ENTRY  IGET8B      INT IGET8B( INT V, INT B ) EXTRACT 8 BIT BY
*
* RETURN 8 BIT BYTE (0 TO 6) FROM V.
*
 IGET8B   BSS    1
          SA2    A1+1        X2 = &B
          SA1    X1          X1 = V
          SA2    X2          X2 = B
          LX2    3           X2 = 8 * B
          SB1    X2          B1 = 8 * B
          SX3    377B        X3 = 377B
          AX1    B1          X1 = V >> ( 8 * B ) (ARITHMETIC)
          BX6    X1*X3       IGETB8 = ( V >> ( 8 * B ) ) .AND. 377B
          EQ     IGET8B
          END
C
C
C
C ==================================================
C === STUB ROUTINES = FUNCTIONALITY NOT REQUIRED ===
C ==================================================
C
C --- -----------------------------------------------------------------
