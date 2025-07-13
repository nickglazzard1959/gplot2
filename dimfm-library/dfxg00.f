      SUBROUTINE DFXG00
      INCLUDE 'params.cmn'
      INCLUDE 'dfxcaa.cmn'
      INCLUDE 'dfxcab.cmn'
      INCLUDE 'dfxcac.cmn'
      INCLUDE 'dfxcacs.cmn'
      INCLUDE 'dfxcad.cmn'
      INCLUDE 'dfxcba.cmn'
      INCLUDE 'dfxcbb.cmn'
      INCLUDE 'dfxcbc.cmn'
      INCLUDE 'dfxcbcs.cmn'
      INCLUDE 'dfxcbd.cmn'
      INCLUDE 'dfxcd0.cmn'
      INCLUDE 'dfxcd0s.cmn'
      IF (GKSOS.NE.0) GO TO 1000
C    CHECK VALID TO OPEN GKS
C
C    SET DUMMY VARIABLES IN DFXCD0 - USED TO AVOID ARITHMETIC ERRORS
C               (E.G. IN DFX000 WHEN SIMPLE VARIABLE EQUATED TO
C                        FIRST ELEMENT OF ARRAY)
C
      ZDUMMY = 0.0
      NDUMMY = 0
C
      GKSOS = 1
      NCOPWS = 0
      NCACWS = 0
      NOTRP = 0
      PLI = 1
      PLT = 1
      PLWS = 1.0
      PLCI = 1
      PLTASF = .TRUE.
      PLWASF = .TRUE.
      PLCASF = .TRUE.
      PMI = 1
      PMT = 3
      PMS = 1.0
      PMCI = 1
      PMTASF = .TRUE.
      PMSASF = .TRUE.
      PMCASF = .TRUE.
      TI = 1
      TF = 1
      TP = 0
      TEX = 0.0
      TSP = 0.0
      TCI = 1
      TFPASF = .TRUE.
      TEXASF = .TRUE.
      TSPASF = .TRUE.
      TCIASF = .TRUE.
      TCHHT = 0.01
      TCHUPX = 0.0
      TCHUPY = 1.0
      TPATH = 0
      TAH = 0
      TAV = 0
      AI = 1
      AS = 0
      ASI = 1
      ACI = 1
      ASASF = .TRUE.
      ASIASF = .TRUE.
      ACIASF = .TRUE.
      PSIZX = 1.0
      PSIZY = 1.0
      PREFX = 0.0
      PREFY = 0.0
      PICKID = 0
      XSWCND = 1.0
      YSWCND = 1.0
      NOTR = 0
      DO 1 I=0,MAXTRS
      NOTRPR(I) = I
      WCWIN(1,I) = 0.0
      WCWIN(2,I) = 1.0
      WCWIN(3,I) = 0.0
      WCWIN(4,I) = 1.0
      NDCVP(1,I) = 0.0
      NDCVP(2,I) = 1.0
      NDCVP(3,I) = 0.0
    1 NDCVP(4,I) = 1.0
      DO 2 I=1,MSOPWS
    2 WSID(I) = -1000
      VPCLIP = .TRUE.
      NOPS = 0
C     INITIALISE DIMFILM
      CALL DFX006
      RETURN
 1000 CONTINUE
C****************
C     ERROR RETURN HERE FOR NOT CLOSED ON ENTRY
C****************
      END
