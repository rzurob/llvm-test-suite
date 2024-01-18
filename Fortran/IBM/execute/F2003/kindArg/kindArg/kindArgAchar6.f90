!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : ACHAR
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
!*  (322978)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgAchar6


  CLASS(*), ALLOCATABLE :: I01, II01(:)

!  CHARACTER :: CC=(/(ACHAR(I=I, KIND=1), I=0, 127)/)


  CHARACTER :: CC(0:127)
  CC=(/(ACHAR(I=I, KIND=1), I=0, 127)/)

  ALLOCATE(I01, SOURCE=1_1)
  ALLOCATE(II01(128), SOURCE=3_1)


  ASSOCIATE (K1=>1_1, K2=>2_1, K4=>4_1, K8=>8_1)
  SELECT TYPE (I1 => I01)
  TYPE IS (INTEGER(1))
  SELECT TYPE (II1 => II01)
  TYPE IS (INTEGER(1))


  DO I1 = 0, 127
    IF (IACHAR(ACHAR(I1, KIND=K1%KIND ))        .NE. I1)                   ERROR STOP 11
    IF (IACHAR(ACHAR(I1, KIND=K2%KIND ))        .NE. I1)                   ERROR STOP 12
    IF (IACHAR(ACHAR(I1, KIND=K4%KIND ))        .NE. I1)                   ERROR STOP 13
    IF (IACHAR(ACHAR(I1, KIND=K8%KIND ))        .NE. I1)                   ERROR STOP 14
    IF (ACHAR(IACHAR(ACHAR(I1, KIND=K1%KIND ))) .NE. ACHAR(I1, KIND=K1%KIND ))  ERROR STOP 15
    IF (ACHAR(IACHAR(ACHAR(I1, KIND=K2%KIND ))) .NE. ACHAR(I1, KIND=K2%KIND ))  ERROR STOP 16
    IF (ACHAR(IACHAR(ACHAR(I1, KIND=K4%KIND ))) .NE. ACHAR(I1, KIND=K4%KIND ))  ERROR STOP 17
    IF (ACHAR(IACHAR(ACHAR(I1, KIND=K8%KIND ))) .NE. ACHAR(I1, KIND=K8%KIND ))  ERROR STOP 18
  END DO

  DO I2 = 0, 127
    IF (IACHAR(ACHAR(I2, KIND=I1%KIND ))         .NE. I2)                         ERROR STOP 21
    IF (IACHAR(ACHAR(I2, KIND=II1%KIND ))        .NE. I2)                         ERROR STOP 22
    IF (ACHAR(IACHAR(ACHAR(I2, KIND=I1%KIND )))  .NE. ACHAR(I2, KIND=I1%KIND ))   ERROR STOP 25
    IF (ACHAR(IACHAR(ACHAR(I2, KIND=II1%KIND ))) .NE. ACHAR(I2, KIND=II1%KIND ))  ERROR STOP 26
  END DO

  IF (ANY( ACHAR(I=(/(I1, I1=0,127)/), KIND=K8%KIND) .NE. CC)) ERROR STOP 111
  IF (ANY( ACHAR(I=(/(I2, I2=0,127)/), KIND=K4%KIND) .NE. CC)) ERROR STOP 112
  IF (ANY( ACHAR(I=(/(I4, I4=0,127)/), KIND=K2%KIND) .NE. CC)) ERROR STOP 113
  IF (ANY( ACHAR(I=(/(I8, I8=0,127)/), KIND=K1%KIND) .NE. CC)) ERROR STOP 114

  IF (ANY( IACHAR(ACHAR(I=(/(I1, I1=0,127)/), KIND=I1%KIND))  .NE. (/(I1, I1=0,127)/))) ERROR STOP 115
  IF (ANY( IACHAR(ACHAR(I=(/(I2, I2=0,127)/), KIND=II1%KIND)) .NE. (/(I2, I2=0,127)/))) ERROR STOP 116


  CLASS DEFAULT
    STOP 91
  END SELECT
  CLASS DEFAULT
    STOP 92
  END SELECT

  END ASSOCIATE

  END

