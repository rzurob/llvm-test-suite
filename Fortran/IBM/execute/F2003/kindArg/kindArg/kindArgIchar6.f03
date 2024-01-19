!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 16, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ICHAR
!*
!*  REFERENCE                  : Feature Number 289083
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Entities with different attubute used for kind arg - associate/select type
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgIchar6


  INTEGER(1), ALLOCATABLE :: I01, II01(:), K01
  INTEGER(2), POINTER     :: I02, II02(:), K02
  CLASS(*),   ALLOCATABLE :: I04, II04(:), K04
  CLASS(*),   POINTER     :: I08, II08(:), K08

  CHARACTER(:), POINTER :: CC(:)

  ALLOCATE(I01)
  ALLOCATE(I02)
  ALLOCATE(INTEGER(4)::I04)
  ALLOCATE(INTEGER(8)::I08)

  ALLOCATE(CC(128), SOURCE=(/(CHAR(I=I, KIND=1), I=0, 127)/))

  ALLOCATE(K01, SOURCE=1_1)
  ALLOCATE(K02, SOURCE=1_2)
  ALLOCATE(K04, SOURCE=1_4)
  ALLOCATE(K08, SOURCE=1_8)

  ALLOCATE(II01(128))
  ALLOCATE(II02(128))
  ALLOCATE(INTEGER(4)::II04(128))
  ALLOCATE(INTEGER(8)::II08(128))


  ASSOCIATE (I1=>I01,II1=>II01,K1=>K01, I2=>I02,II2=>II02,K2=>K02)
  SELECT TYPE (I4 => I04)
  TYPE IS (INTEGER(4))
  SELECT TYPE (II4 => II04)
  TYPE IS (INTEGER(4))
  SELECT TYPE (K4 => K04)
  TYPE IS (INTEGER(4))
  SELECT TYPE (I8 => I08)
  TYPE IS (INTEGER(8))
  SELECT TYPE (II8 => II08)
  TYPE IS (INTEGER(8))
  SELECT TYPE (K8 => K08)
  TYPE IS (INTEGER(8))



  DO I1 = 0, 127
    IF (ICHAR(CHAR(I1), KIND=KIND((/K1/))  )     .NE. I1)                   ERROR STOP 11
    IF (ICHAR(CHAR(I1), KIND=KIND((/K2/))-1)     .NE. I1)                   ERROR STOP 12
    IF (ICHAR(CHAR(I1), KIND=KIND((/K4/))-3)     .NE. I1)                   ERROR STOP 13
    IF (ICHAR(CHAR(I1), KIND=KIND((/K8/))-7)     .NE. I1)                   ERROR STOP 14
  END DO

  DO I2 = 0, 127
    IF (ICHAR(CHAR(I2), KIND=KIND((/K1/))  )     .NE. I2)                   ERROR STOP 21
    IF (ICHAR(CHAR(I2), KIND=KIND((/K2/))-1)     .NE. I2)                   ERROR STOP 22
    IF (ICHAR(CHAR(I2), KIND=KIND((/K4/))-3)     .NE. I2)                   ERROR STOP 23
    IF (ICHAR(CHAR(I2), KIND=KIND((/K8/))-7)     .NE. I2)                   ERROR STOP 24
  END DO

  DO I4 = 0, 127
    IF (ICHAR(CHAR(I4), KIND=KIND((/K1/))  )     .NE. I4)                   ERROR STOP 41
    IF (ICHAR(CHAR(I4), KIND=KIND((/K2/))-1)     .NE. I4)                   ERROR STOP 42
    IF (ICHAR(CHAR(I4), KIND=KIND((/K4/))-3)     .NE. I4)                   ERROR STOP 43
    IF (ICHAR(CHAR(I4), KIND=KIND((/K8/))-7)     .NE. I4)                   ERROR STOP 44
  END DO

  DO I8 = 0, 127
    IF (ICHAR(CHAR(I8), KIND=KIND((/K1/))  )     .NE. I8)                   ERROR STOP 81
    IF (ICHAR(CHAR(I8), KIND=KIND((/K2/))-1)     .NE. I8)                   ERROR STOP 82
    IF (ICHAR(CHAR(I8), KIND=KIND((/K4/))-3)     .NE. I8)                   ERROR STOP 83
    IF (ICHAR(CHAR(I8), KIND=KIND((/K8/))-7)     .NE. I8)                   ERROR STOP 84
  END DO


  II1=(/(I,I=0,127)/)
  II2 = II1
  II4 = II2
  II8 = II4

  IF (ANY( ICHAR(C=CC, KIND=CC%KIND   ) .NE. II1)) ERROR STOP 110
  IF (ANY( ICHAR(C=CC, KIND=II1%KIND  ) .NE. II1)) ERROR STOP 111
  IF (ANY( ICHAR(C=CC, KIND=II2%KIND-1) .NE. II1)) ERROR STOP 112
  IF (ANY( ICHAR(C=CC, KIND=II4%KIND-3) .NE. II1)) ERROR STOP 113
  IF (ANY( ICHAR(C=CC, KIND=II8%KIND-7) .NE. II1)) ERROR STOP 114


  CLASS DEFAULT
    STOP 91
  END SELECT
  CLASS DEFAULT
    STOP 92
  END SELECT
  CLASS DEFAULT
    STOP 93
  END SELECT
  CLASS DEFAULT
    STOP 94
  END SELECT
  CLASS DEFAULT
    STOP 95
  END SELECT
  CLASS DEFAULT
    STOP 96
  END SELECT

  END ASSOCIATE

  END

