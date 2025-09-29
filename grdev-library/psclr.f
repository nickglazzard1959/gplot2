      SUBROUTINE PSCLR
C --- ------------------------------------------------------------------
C --- THIS ROUTINE CLOSES AN EXISTING OUTPUT FILE AND OPENS A NEW ONE.
C --- OR, IF EVERY IS .FALSE. ONLY OPEN A SINGLE OUTPUT FILE AND PUT ALL
C --- FRAMES IN TO IT.
C --- ------------------------------------------------------------------
      IMPLICIT LOGICAL (A-Z)
      REAL INPS
      PARAMETER( INPS = 72.0 )
C
      LOGICAL EVERY
      PARAMETER( EVERY = .TRUE. )
C
      INTEGER IOS
      INTEGER LNBC
C
      INCLUDE 'dfxpsn.cmn'
      INCLUDE 'dfxpcn.cmn'
C
      CHARACTER*28 CMTID
      CHARACTER*45 CMTIT
      CHARACTER*36 CMTBB
      CHARACTER*22 CMTEC
      CHARACTER*31 DEFL1
      CHARACTER*24 DEFL2
      CHARACTER*34 DEFL3
      CHARACTER*42 DEFM
      CHARACTER*37 DEFC
      CHARACTER*50 DEFW
      CHARACTER*18 CMTEP
      CHARACTER*28 SETWD
      CHARACTER*16 SHOPG
      CHARACTER*15 CMTLR
      CHARACTER*24 CMTBE
      CHARACTER*7  EPEXT
      DATA CMTID/'%%!PS-A^D^O^B^E-3.0 EPSF-3.0'/
      DATA CMTIT/'%%T^I^T^L^E: D^I^M^F^I^L^M ^P^L^O^T ^F^I^L^E.'/
      DATA CMTBB/'%%B^O^U^N^D^I^N^GB^O^X: (^A^T^E^N^D)'/
      DATA CMTEC/'%%E^N^DC^O^M^M^E^N^T^S'/
      DATA DEFL1/'/^L ^0^M^O^V^E^T^O ^L^I^N^E^T^O'/
      DATA DEFL2/'^C^U^R^R^E^N^T^P^O^I^N^T'/
      DATA DEFL3/'^S^T^R^O^K^E ^M^O^V^E^T^O^2 ^D^E^F'/
      DATA DEFM /'/^M ^0^N^E^W^P^A^T^H ^M^O^V^E^T^O^2 ^D^E^F'/
      DATA DEFC /'/^C ^0^S^E^T^R^G^B^C^O^L^O^R^2 ^D^E^F'/
      DATA DEFW /'/^W ^00.5 ^M^U^L ^S^E^T^L^I^N^E^W^I^D^T^H^2 ^D^E^F'/
      DATA CMTEP/'%%E^N^DP^R^O^L^O^G'/
      DATA SETWD/'0.5 ^S^E^T^L^I^N^E^W^I^D^T^H'/
      DATA SHOPG/'^S^H^O^W^P^A^G^E'/
      DATA CMTLR/'%%T^R^A^I^L^E^R'/
      DATA CMTBE/'%%B^O^U^N^D^I^N^GB^O^X: '/
      DATA EPEXT/'.^E^P^S'/
C
C --- CLOSE ANY EXISTING FILLED FILE, AFTER WRITING THE TRAILER.
C
      IF( EVERY )THEN
         IF( OPENED ) THEN
            IF( EMPTYF ) THEN
               CLOSE( UNIT=LUN, STATUS='DELETE' )
C               PRINT 888
C 888           FORMAT(1X,'PSCLR ASKED FOR EMPTY FILE TO BE DELETED.')
            ELSE
               WRITE(LUN,9)SHOPG
 9             FORMAT(A)
               WRITE(LUN,9)CMTLR
               WRITE(LUN, 100) CMTBE, INT(XMIN), INT(YMIN),
     &                         INT(XMAX+0.5), INT(YMAX+0.5)
               CLOSE( UNIT=LUN, STATUS='KEEP' )
C
C---- ON NOS, MAKE THE LOCAL OUTPUT FILE PERMANENT IF DESIRED.
C
#ifndef PORTF77
               IF( AUTOSAV )THEN
                  CALL NOSPFRT(FNO(1:LNBC(FNO,1,1)))
               ENDIF
#endif
            END IF
         END IF
C
C --- OR JUST WRITE THE TRAILER IF ONLY ONE OUTPUT FILE.
C
      ELSE
         IF( OPENED )THEN
               WRITE(LUN,9)SHOPG
               WRITE(LUN,9)CMTLR
               WRITE(LUN, 100) CMTBE, INT(XMIN), INT(YMIN),
     &                         INT(XMAX+0.5), INT(YMAX+0.5)
         ENDIF
      ENDIF
C
C --- OPEN THE OUTPUT FILE. IF DOING ONE FRAME PER FILE,
C --- CONSTRUCT A FILE NAME WITH THE FRAME NUMBER IN IT.
C --- FOR "UNIX", FORCE THE RESULT TO BE LOWER CASE.
C
      IF( .NOT. OPENED .OR. EVERY )THEN
         FRNO = FRNO + 1
         IF( EVERY )THEN
#ifdef UNIX
            WRITE( FNO, 200 )EPFN(1:EPFNL),FRNO,EPEXT
 200        FORMAT( A,I3.3,A )
            CALL LOCASE(FNO(1:LNBC(FNO,1,1)))
#else
            WRITE( FNO, 200 )EPFN(1:EPFNL),FRNO
 200        FORMAT( A,I3.3 )
#endif            
            OPEN( UNIT=LUN, FILE=FNO(1:LNBC(FNO,1,1)),
     &       STATUS='UNKNOWN', FORM='FORMATTED',
     &       IOSTAT=IOS )
C
C --- OR JUST OPEN THE SINGLE OUTPUT FILE IF NOT OPEN YET.
C
         ELSE
            OPEN( UNIT=LUN, FILE=EPFN(1:EPFNL),
     &       STATUS='UNKNOWN', FORM='FORMATTED',
     &       IOSTAT=IOS )
         ENDIF
         IF( IOS .NE. 0 ) STOP 'CANNOT OPEN PS PLOTFILE.'
         OPENED = .TRUE.
         EMPTYF = .TRUE.
      ENDIF
C
C --- INITIALIZE BOUNDS AND POSITION.
C
      XMAX = (XSIZE + XOFFST) * INPS
      YMAX = (YSIZE + YOFFST) * INPS
      XMIN = XOFFST * INPS
      YMIN = YOFFST * INPS
      XPOS = XMAX
      YPOS = YMAX
C
C      PRINT 889,XSIZE,YSIZE,XOFFST,YOFFST
C 889  FORMAT(1X,'SIZE ',F7.3,' X ',F7.3,' OFFSET ',F7.3,', ',F7.3,' IN')
C
C
C --- WRITE OUT THE POSTSCRIPT HEADER.
C
      WRITE(LUN,9)CMTID
      WRITE(LUN,9)CMTIT
      WRITE(LUN,9)CMTBB
      WRITE(LUN,9)CMTEC
      WRITE(LUN,9)DEFL1
      WRITE(LUN,9)DEFL2
      WRITE(LUN,9)DEFL3
      WRITE(LUN,9)DEFM
      WRITE(LUN,9)DEFC
      WRITE(LUN,9)DEFW
      WRITE(LUN,9)CMTEP
      WRITE(LUN,9)SETWD
      WRITE(LUN,300)FNO(1:50)
      WRITE(LUN,301)EPRT(1:EPRTL)
C
C --- NOTE - BOUNDING BOX MUST BE INTEGER.
C
 100  FORMAT(A,I6,I6,I6,I6)
 300  FORMAT('%%PLOTFILE ',A)
 301  FORMAT('%%ROUTE ',A)
      END
C
      SUBROUTINE EPSSAV( DOSAVE )
C --- ------------------------------------------------------------------
C --- SAVE LOCAL OUTPUT FILES TO PERMANENT FILES ON NOS IF DOSAVE .TRUE.
C --- ------------------------------------------------------------------
      IMPLICIT LOGICAL (A-Z)
      LOGICAL DOSAVE
C
      INCLUDE 'dfxpsn.cmn'
C
      AUTOSAV = DOSAVE
C
      RETURN
      END
