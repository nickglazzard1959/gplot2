      SUBROUTINE DFX162
C     LOADS THE DIMFILM DEFAULT COLOUR LOOK UP TABLE
C
      REAL L
C
C     FIRST 16 ENTRIES ALWAYS:
C              0 - BACKGROUND )   THESE ARE INITIALLY DEVICE DEFAULTS
C              1 - FOREGROUND )
C              2 - RED
C              3 - GREEN
C              4 - BLUE
C              5 - YELLOW
C              6 - CYAN
C              7 - MAGENTA
C              8 - RED-YELLOW
C              9 - YELLOW-GREEN
C             10 - GREEN-CYAN
C             11 - CYAN-BLUE
C             12 - BLUE-MAGENTA
C             13 - MAGENTA-RED
C             14 - WHITE
C             15 - BLACK
C
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      INCLUDE 'dfxc17.cmn'
      S = 0.0
      L = 0.0
      CALL DFX160(6,H,L,S,RGBLUT(1,0),RGBLUT(2,0),RGBLUT(3,0),IERR)
      L = 1.0
      CALL DFX160(6,H,L,S,RGBLUT(1,1),RGBLUT(2,1),RGBLUT(3,1),IERR)
      N = 2
      S = 1.0
      L = 0.5
      CALL DFX163(N,0.0,L,S,3,.FALSE.)
      CALL DFX163(N,60.,L,S,3,.FALSE.)
      L = 0.5
      S = 1.0
      H = 30.0
      DH = 60.0
      DO 3 I=1,6
      CALL DFX160(6,H,L,S,RGBLUT(1,N),RGBLUT(2,N),RGBLUT(3,N),IERR)
      N = N + 1
    3 H = H + DH
      L = 1.0
      S = 0.0
      CALL DFX160(6,H,L,S,RGBLUT(1,N),RGBLUT(2,N),RGBLUT(3,N),IERR)
      N = N + 1
      L = 0.0
      CALL DFX160(6,H,L,S,RGBLUT(1,N),RGBLUT(2,N),RGBLUT(3,N),IERR)
C    FIRST ROW FINISHED HERE
      N = 16
      CALL DFX163(N,0.0,0.2,1.0,12,.FALSE.)
      N = 32
      CALL DFX163(N,0.0,0.3,1.0,12,.FALSE.)
      N = 48
      CALL DFX163(N,0.0,0.4,1.0,12,.FALSE.)
      N = 64
      CALL DFX163(N,0.0,0.5,1.0,12,.FALSE.)
      N = 80
      CALL DFX163(N,0.0,0.6,1.0,12,.FALSE.)
      N = 96
      CALL DFX163(N,0.0,0.7,1.0,12,.FALSE.)
      N = 112
      CALL DFX163(N,0.0,0.8,1.0,12,.FALSE.)
      N = 128
      CALL DFX163(N,0.0,0.3,0.6,12,.FALSE.)
      N = 144
      CALL DFX163(N,0.0,0.4,0.6,12,.FALSE.)
      N = 160
      CALL DFX163(N,0.0,0.5,0.6,12,.FALSE.)
      N = 176
      CALL DFX163(N,0.0,0.6,0.6,12,.FALSE.)
      N = 192
      CALL DFX163(N,0.0,0.7,0.6,12,.FALSE.)
      N = 208
      CALL DFX163(N,0.0,0.4,0.3,12,.FALSE.)
      N = 224
      CALL DFX163(N,0.0,0.5,0.3,12,.FALSE.)
      N = 240
      CALL DFX163(N,0.0,0.6,0.3,12,.FALSE.)
      N = 28
      CALL DFX164(N,0.0,1.0,15)
      N = 29
      CALL DFX164(N,120.0,1.0,15)
      N = 30
      CALL DFX164(N,240.0,1.0,15)
      N = 31
      CALL DFX164(N,0.0,0.0,15)
      RETURN
      END
C
C----------------------------------------------
C
