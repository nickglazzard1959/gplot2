      SUBROUTINE DFXM04
C --- -----------------------------------------------------------------
C
C --- THIS ROUTINE AND ITS ASSOCIATED ENTRY POINTS IMPLEMENT DEVICE
C --- NOMINATION AND THE INTERFACE TO THE DEVICE DRIVERS (DFXDNN).
C --- THIS VERSION IS SOMEWHAT INTERIM.
C
C --- PARAMETERS.
C
      INTEGER MAXDEV
      PARAMETER( MAXDEV=5 )
C
C --- ARGUMENTS.
C
      REAL XARG(*),YARG(*),ZARG(*)
      INTEGER NARG(*)
C
C --- LOCAL STORAGE.
C
      LOGICAL INUSE(MAXDEV)
C
C --- COMMONS.
C
      INCLUDE 'dfxcbd.cmn'
C
      SAVE INUSE
      DATA INUSE /MAXDEV*.FALSE./
C
C --- -----------------------------------------------------------------
      ENTRY DFXM06( DRIVER, N )
C --- -----------------------------------------------------------------
C
      IF( (N.GT.0) .AND. (N.LE.MAXDEV) ) THEN
         INUSE(N) = .TRUE.
      ELSE
         STOP 'DFXM06: ILLEGAL DEVICE NUMBER.'
      END IF
C
      RETURN
C
C --- -----------------------------------------------------------------
      ENTRY DFXM05( I, XARG, YARG, ZARG, NARG )
C --- -----------------------------------------------------------------
C
      IF( INUSE(NWS) ) THEN
         IF( NWS .EQ. 1 ) THEN
            CALL DFXD01( I, XARG, YARG, ZARG, NARG )
         ELSE IF( NWS .EQ. 2 ) THEN
            CALL DFXD02( I, XARG, YARG, ZARG, NARG )
         ELSE IF( NWS .EQ. 3 ) THEN
            CALL DFXD03( I, XARG, YARG, ZARG, NARG )
         ELSE IF( NWS .EQ. 4 ) THEN
            CALL DFXD04( I, XARG, YARG, ZARG, NARG )
         ELSE IF( NWS .EQ. 5 ) THEN
            CALL DFXD05( I, XARG, YARG, ZARG, NARG )
         ELSE
            STOP 'DFXM05: ILLEGAL WORKSTATION NUMBER.'
         END IF
      ELSE
         STOP 'DFXM05: WORKSTATION NOT IN USE.'
      END IF
C
      RETURN
      END
