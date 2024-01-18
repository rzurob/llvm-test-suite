! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/kindArg/kindArg/kindArgLen5.f
! opt variations: -qnol

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgLen5
!*
!*  DATE                       : Jun. 22, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : LEN
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
!*  Result Characteristics.
!*  Integer scalar. If KIND is present, the kind type parameter is that specified by the value of KIND;
!*  otherwise the kind type parameter is that of default integer type.
!*
!*  (322675)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLen5
  IMPLICIT NONE

  TYPE :: DT(N1,D1,D2,D3,D4)    ! (20,1,2,4,8)
    INTEGER, KIND :: D1,D2,D3,D4
    INTEGER, LEN  :: N1
    INTEGER(D1)   :: K1(1:1)=1
    INTEGER(D2)   :: K2(2:3)=2
    INTEGER(D3)   :: K4(4:7)=4
    INTEGER(D4)   :: K8(8:15)=8
  END TYPE

  TYPE (DT(20,1,2,4,8)), PARAMETER :: T=DT(20,1,2,4,8)()

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8

  CHARACTER(127), PARAMETER :: CC(128)=" "


  DO I1 = 0, 127
    IF (     LEN(STRING=CC(I1)(:I1), KIND=LBOUND(T%K1, DIM=1))  .NE. I1)   STOP 11
    IF (KIND(LEN(STRING=CC(I1)(:I1), KIND=LBOUND(T%K1, DIM=1))) .NE. 1)    STOP 12
    IF (     LEN(STRING=CC(I1)(:I1), KIND=LBOUND(T%K2, DIM=1))  .NE. I1)   STOP 13
    IF (KIND(LEN(STRING=CC(I1)(:I1), KIND=LBOUND(T%K2, DIM=1))) .NE. 2)    STOP 14
    IF (     LEN(STRING=CC(I1)(:I1), KIND=LBOUND(T%K4, DIM=1))  .NE. I1)   STOP 15
    IF (KIND(LEN(STRING=CC(I1)(:I1), KIND=LBOUND(T%K4, DIM=1))) .NE. 4)    STOP 16
    IF (     LEN(STRING=CC(I1)(:I1), KIND=LBOUND(T%K8, DIM=1))  .NE. I1)   STOP 17
    IF (KIND(LEN(STRING=CC(I1)(:I1), KIND=LBOUND(T%K8, DIM=1))) .NE. 8)    STOP 18
  END DO

  DO I2 = 0, 127
    IF (     LEN(STRING=CC(I2:)(:), KIND=LBOUND(T%K1, DIM=1))  .NE. 127)   STOP 21
    IF (KIND(LEN(STRING=CC(I2:)(:), KIND=LBOUND(T%K1, DIM=1))) .NE. 1)     STOP 22
    IF (     LEN(STRING=CC(I2:)(:), KIND=LBOUND(T%K2, DIM=1))  .NE. 127)   STOP 23
    IF (KIND(LEN(STRING=CC(I2:)(:), KIND=LBOUND(T%K2, DIM=1))) .NE. 2)     STOP 24
    IF (     LEN(STRING=CC(I2:)(:), KIND=LBOUND(T%K4, DIM=1))  .NE. 127)   STOP 25
    IF (KIND(LEN(STRING=CC(I2:)(:), KIND=LBOUND(T%K4, DIM=1))) .NE. 4)     STOP 26
    IF (     LEN(STRING=CC(I2:)(:), KIND=LBOUND(T%K8, DIM=1))  .NE. 127)   STOP 27
    IF (KIND(LEN(STRING=CC(I2:)(:), KIND=LBOUND(T%K8, DIM=1))) .NE. 8)     STOP 28
  END DO

  DO I4 = 0, 127
    IF (     LEN(STRING=CC(:I4)(I4:I4), KIND=LBOUND(T%K1, DIM=1))  .NE. 1)  STOP 41
    IF (KIND(LEN(STRING=CC(:I4)(I4:I4), KIND=LBOUND(T%K1, DIM=1))) .NE. 1)  STOP 42
    IF (     LEN(STRING=CC(:I4)(I4:I4), KIND=LBOUND(T%K2, DIM=1))  .NE. 1)  STOP 43
    IF (KIND(LEN(STRING=CC(:I4)(I4:I4), KIND=LBOUND(T%K2, DIM=1))) .NE. 2)  STOP 44
    IF (     LEN(STRING=CC(:I4)(I4:I4), KIND=LBOUND(T%K4, DIM=1))  .NE. 1)  STOP 45
    IF (KIND(LEN(STRING=CC(:I4)(I4:I4), KIND=LBOUND(T%K4, DIM=1))) .NE. 4)  STOP 46
    IF (     LEN(STRING=CC(:I4)(I4:I4), KIND=LBOUND(T%K8, DIM=1))  .NE. 1)  STOP 47
    IF (KIND(LEN(STRING=CC(:I4)(I4:I4), KIND=LBOUND(T%K8, DIM=1))) .NE. 8)  STOP 48
  END DO

  DO I8 = 0, 127
    IF (     LEN(STRING=CC(I8+1:I8)(:), KIND=LBOUND(T%K1, DIM=1))  .NE. 127)  STOP 81
    IF (KIND(LEN(STRING=CC(I8+1:I8)(:), KIND=LBOUND(T%K1, DIM=1))) .NE. 1)    STOP 82
    IF (     LEN(STRING=CC(I8+1:I8)(:), KIND=LBOUND(T%K2, DIM=1))  .NE. 127)  STOP 83
    IF (KIND(LEN(STRING=CC(I8+1:I8)(:), KIND=LBOUND(T%K2, DIM=1))) .NE. 2)    STOP 84
    IF (     LEN(STRING=CC(I8+1:I8)(:), KIND=LBOUND(T%K4, DIM=1))  .NE. 127)  STOP 85
    IF (KIND(LEN(STRING=CC(I8+1:I8)(:), KIND=LBOUND(T%K4, DIM=1))) .NE. 4)    STOP 86
    IF (     LEN(STRING=CC(I8+1:I8)(:), KIND=LBOUND(T%K8, DIM=1))  .NE. 127)  STOP 87
    IF (KIND(LEN(STRING=CC(I8+1:I8)(:), KIND=LBOUND(T%K8, DIM=1))) .NE. 8)    STOP 88
  END DO


  END

