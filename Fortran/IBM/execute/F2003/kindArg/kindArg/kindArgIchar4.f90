!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 12, 2006
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
!*  Ichar is the inverse of the CHAR function
!*
!*  (322601)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgIchar4


  INTEGER(1) :: I1, II1(128)
  INTEGER(2) :: I2, II2(128)
  INTEGER(4) :: I4, II4(128)
  INTEGER(8) :: I8, II8(128)

  CHARACTER(:), POINTER :: CC(:)

  ALLOCATE(CC(128), SOURCE=(/(CHAR(I=I, KIND=1), I=0, 127)/))

  DO I1 = 0, 127
    IF (ICHAR(C=CHAR(I1, KIND=1_1), KIND=8 )        .NE. I1)                   STOP 11
    IF (ICHAR(C=CHAR(I1, KIND=1_2), KIND=4 )        .NE. I1)                   STOP 12
    IF (ICHAR(C=CHAR(I1, KIND=1_4), KIND=2 )        .NE. I1)                   STOP 13
    IF (ICHAR(C=CHAR(I1, KIND=1_8), KIND=1 )        .NE. I1)                   STOP 14
  END DO

  DO I2 = 0, 127
    IF (ICHAR(C=CHAR(I2, KIND=1_1), KIND=2 )        .NE. I2)                   STOP 21
    IF (ICHAR(C=CHAR(I2, KIND=1_2), KIND=4 )        .NE. I2)                   STOP 22
    IF (ICHAR(C=CHAR(I2, KIND=1_4), KIND=8 )        .NE. I2)                   STOP 23
    IF (ICHAR(C=CHAR(I2, KIND=1_8), KIND=1 )        .NE. I2)                   STOP 24
  END DO

  DO I4 = 0, 127
    IF (ICHAR(C=CHAR(I4, KIND=1_1), KIND=1 )        .NE. I4)                   STOP 41
    IF (ICHAR(C=CHAR(I4, KIND=1_2), KIND=2 )        .NE. I4)                   STOP 42
    IF (ICHAR(C=CHAR(I4, KIND=1_4), KIND=4 )        .NE. I4)                   STOP 43
    IF (ICHAR(C=CHAR(I4, KIND=1_8), KIND=8 )        .NE. I4)                   STOP 44
  END DO

  DO I8 = 0, 127
    IF (ICHAR(C=CHAR(I8, KIND=1_1), KIND=4 )        .NE. I8)                   STOP 81
    IF (ICHAR(C=CHAR(I8, KIND=1_2), KIND=8 )        .NE. I8)                   STOP 82
    IF (ICHAR(C=CHAR(I8, KIND=1_4), KIND=2 )        .NE. I8)                   STOP 83
    IF (ICHAR(C=CHAR(I8, KIND=1_8), KIND=1 )        .NE. I8)                   STOP 84
  END DO



  IF (ANY( ICHAR(C=(/(CHAR(I1), I1=0,127)/), KIND=1_8) .NE. ICHAR(CC))) STOP 111
  IF (ANY( ICHAR(C=(/(CHAR(I2), I2=0,127)/), KIND=1_4) .NE. ICHAR(CC))) STOP 112
  IF (ANY( ICHAR(C=(/(CHAR(I4), I4=0,127)/), KIND=1_2) .NE. ICHAR(CC))) STOP 113
  IF (ANY( ICHAR(C=(/(CHAR(I8), I8=0,127)/), KIND=1_1) .NE. ICHAR(CC))) STOP 114


  IF (ANY( ICHAR(CHAR(I=(/(I1, I1=0,127)/), KIND=KIND((/ICHAR(CC, KIND=1_1)/)))) .NE. (/ICHAR(CC)/))) STOP 115
  IF (ANY( ICHAR(CHAR(I=(/(I1, I1=0,127)/), KIND=KIND((/ICHAR(CC, KIND=1_2)/)))) .NE. (/ICHAR(CC)/))) STOP 116
  IF (ANY( ICHAR(CHAR(I=(/(I1, I1=0,127)/), KIND=KIND((/ICHAR(CC, KIND=1_4)/)))) .NE. (/ICHAR(CC)/))) STOP 117
  IF (ANY( ICHAR(CHAR(I=(/(I1, I1=0,127)/), KIND=KIND((/ICHAR(CC, KIND=1_8)/)))) .NE. (/ICHAR(CC)/))) STOP 118

  END

