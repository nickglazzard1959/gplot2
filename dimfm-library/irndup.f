      INTEGER FUNCTION IRNDUP( RX )
C --- -----------------------------------------------------------------
C
C --- FUNCTION TO ROUND UP POSITIVE REALS INTO AN INTEGER
C
      REAL RX
      IRNDUP = INT(RX+1) - INT(FLOAT(INT(RX+1))-RX)
      END
C
C
C --- -----------------------------------------------------------------
