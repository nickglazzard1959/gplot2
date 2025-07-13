      SUBROUTINE DFXG50(ITR,XMIN,XMAX,YMIN,YMAX)
      INCLUDE 'params.cmn'
      INCLUDE 'dfxcaa.cmn'
      INCLUDE 'dfxcac.cmn'
      INCLUDE 'dfxcacs.cmn'
      INCLUDE 'dfxcbd.cmn'
      GKSERR = 8
      IF (GKSOS.EQ.0) GO TO 99
      GKSERR = 50
      IF (ITR.LE.0.OR.ITR.GT.MAXTRS) GO TO 99
      GKSERR = 51
      IF (XMIN.GE.XMAX) GO TO 99
      IF (YMIN.GE.YMAX) GO TO 99
      GKSERR = 52
      IF (AMIN1(XMIN,YMIN).LT.0.0) GO TO 99
      IF (AMAX1(XMAX,YMAX).GT.1.0) GO TO 99
      GKSERR = 0
      NDCVP(1,ITR) = XMIN
      NDCVP(2,ITR) = XMAX
      NDCVP(3,ITR) = YMIN
      NDCVP(4,ITR) = YMAX
   99 RETURN
      END
