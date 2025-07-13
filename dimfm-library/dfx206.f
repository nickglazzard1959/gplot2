      SUBROUTINE DFX206
      INCLUDE 'dfxc01.cmn'
      INCLUDE 'dfxc01s.cmn'
      INCLUDE 'dfxc07.cmn'
C    RESETS ALL NON-STRING POSITIONING STRING PARAMETERS
C    EXCEPT HEIGHT
C    (I.E. EXCLUDES HT,HS,SYMCO,CANGL,SANGL,SPOS,ANGLS,ANGL)
      IBET = 1
      LOW = 0
      HVY = .FALSE.
      ITAL = .FALSE.
      ALT = .FALSE.
      MONSP = .FALSE.
      UN = .FALSE.
      SDSYM = .TRUE.
      TXTCN = .FALSE.
      RELSUP = .TRUE.
      MUITAL = 0.2
      UNYSP = 0.1
      FRACHT = 0.6
      FRACM = 3./7.
      FRACGP = 0.13
      ZHVY = 0.15
      SUPY = -0.4
C    CHANGE TO +0.1 WHEN CHARACTER HEIGHTS AVAILABLE
      SUPH = 0.4
      SUBY = 0.0
      SUBH = 0.4
      OVERH = 0.4
      OVERY = 0.1
      UNDERY = 0.1
      CEXP = 1.0
      CSPACE = 0.0
      SPFACT = 1.0
      IALT = 3
      CPATH = 0
      LENGTH = .FALSE.
      XPASS = 100.0
      IVAR = .FALSE.
      NTH = .FALSE.
      NESC = .TRUE.
      RETURN
      END
