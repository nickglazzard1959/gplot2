      SUBROUTINE DFX138(MODEL)
C
C++++++++++++++CODE MUST REPLICATE THAT IN USER ACCESSIBLE CMODEL+++++++
C
      CHARACTER*(*) MODEL
      CHARACTER*3 MODELS(0:7),CMOD
      INCLUDE 'dfxc00.cmn'
      INCLUDE 'dfxc00s.cmn'
      INCLUDE 'dfxc04.cmn'
      INCLUDE 'dfxc05.cmn'
      INCLUDE 'dfxc12.cmn'
      SAVE MODELS
      DATA MODELS/'RGB','CMY','YIQ','YUV','YTV','HSV','HLS','USR'/
C   ++++++++++++N.B. USR NOT YET IMPLEMENTED - INCREASE LOOP COUNT TO 7
C   ++++++++++++     WHEN RELEASED
      CALL DFX010(MODEL,CMOD,1,L,IERR)
      IF (IERR.NE.0) GO TO 2
      DO 1 I=0,6
      IF (CMOD.EQ.MODELS(I)) THEN
                  NCMODE = I
                  GO TO 2
      ENDIF
    1 CONTINUE
C    NO MATCH FOR MODEL
    2 CONTINUE
      RETURN
      END
