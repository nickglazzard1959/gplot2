C
C ================================================
C === ENCAPSULATED POSTSCRIPT OUTPUT FUNCTIONS ===
C ================================================
C
C = ENCAPSULATED POSTSCRIPT OUTPUT FUNCTIONS =
C --- PSBEGN: INITIALIZE EPS OUTPUT.
C --- PSEND : END POSTSCRIPT OUTPUT.
C --- PSCLR : CLOSE EPS OUTPUT FILE AND OPEN A NEW ONE.
C --- PSMOVE: EPS MOVE AND DRAW.
C --- PSRGBC: SET RGB COLOUR TO DRAW IN.
C --- PSWID : SET WIDTH OF LINE TO DRAW.
C
C --- ------------------------------------------------------------------
      BLOCK DATA EPSBDB
C --- ------------------------------------------------------------------
C
      INCLUDE 'dfxpsn.cmn'
      INCLUDE 'dfxpcn.cmn'
C
      DATA XSIZE/5.0/, YSIZE/5.0/, XOFFST/0.5/, YOFFST/0.5/
      DATA XPOS/0.0/, YPOS/0.0/, XMIN/0.0/, YMIN/0.0/, LUN/11/
      DATA OPENED/.FALSE./, EMPTYF/.FALSE./, DIMXXX/.FALSE./, EPFNL/4/
      DATA EPRTL/4/
      DATA EPFN/'EPSF'/
      DATA EPRT/'NONE'/
      END
C
C
C --- ------------------------------------------------------------------
