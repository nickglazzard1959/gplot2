      SUBROUTINE DFXM32( VAL )
C --- -----------------------------------------------------------------
C
C --- THIS ROUTINE CLEARS THE LOW-ORDER BIT IN THE MANTISSA OF THE REAL
C --- VARIABLE VAL. WE DO IT BY TREATING VAL AS INTEGER AND SETTING
C --- BIT  #16 (VAX FLOAT) OR #0 (IEEE FLOAT).
C
      INTEGER VAL, LSB
      PARAMETER( LSB=0 )
      INTEGER IBCLR
C
      VAL = IBCLR( VAL, LSB )
C
      RETURN
      END
C --- -----------------------------------------------------------------
      LOGICAL FUNCTION DFXM33( VAL )
C --- -----------------------------------------------------------------
C
C --- THIS ROUTINE RETURNS WHETHER THE LOW-ORDER BIT IN THE MANTISSA
C --- OF THE REAL VARIABLE VAL IS SET. WE DO IT BY TREATING VAL AS AN
C --- INTEGER AND TESTING BIT  #16 (VAX FLOAT) OR #0 (IEEE FLOAT).
C
      INTEGER VAL, LSB
      PARAMETER( LSB=0 )
      INTEGER IBTEST
C
      DFXM33 = ( IBTEST( VAL, LSB ) .NE. 0 )
C
      RETURN
      END
C
C
C
C =============================
C === FONT ACCESS FUNCTIONS ===
C =============================
C
C --- -----------------------------------------------------------------
