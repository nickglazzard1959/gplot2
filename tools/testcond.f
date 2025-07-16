      PROGRAM A
      REAL B
#ifdef DEF1
      REAL DEF1
#endif
      INTEGER C
#ifndef DEF2
      REAL DEF2
#endif
      INTEGER D
#ifdef DEF3
      REAL DEF3
#else
      REAL NDEF3
      REAL NDEFX
#endif
      INTEGER E
#ifdef DEF4
      REAL DEF4
#else
      REAL NDEF4
      REAL NDEFX4
#ifdef DEFNEST
      REAL DEFNEST
#else
      REAL NDEFNEST
#endif
#endif
      INTEGER F
#if SYM1 != 2
      REAL SYM12
#else
      REAL NOSYM12
#endif
      B = C
#if SINGLEIF
      D = B + SINGLEIF
#endif
      STOP
      END
      
