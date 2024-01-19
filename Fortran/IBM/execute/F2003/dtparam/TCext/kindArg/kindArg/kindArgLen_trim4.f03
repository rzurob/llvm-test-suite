! GB DTP extension using:
! ftcx_dtp -qck -qk -qnol /tstdev/F2003/kindArg/kindArg/kindArgLen_trim4.f
! opt variations: -qnock -qnok -ql

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 23, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : LEN_TRIM
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
!*  Returns the length of the character argument without counting trailing blank characters.
!*  - string of spaces
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM kindArgLen_trim4

  TYPE :: DT(D1)    ! (4)
    INTEGER, len :: D1
    CHARACTER(d1), ALLOCATABLE :: CC(:,:)
  END TYPE

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8

  TYPE(DT(:)),  POINTER :: T
  INTEGER            :: III(128,128)=0

  ALLOCATE(DT(128) :: T)
  ALLOCATE(CHARACTER(128) :: T%CC(128,128))

  T%CC=" "


  DO I1 =1, 127
    IF (LEN_TRIM(STRING=T%CC(I1,I1), KIND=1)   .NE. 0 )   ERROR STOP 11
  END DO

  DO I2 =1, 128
    IF (ANY( LEN_TRIM(STRING=T%CC(I2,:)(1:), KIND=2)   .NE. 0 ))    ERROR STOP 12
  END DO

  DO I4 =1, 128
    IF ( ANY (LEN_TRIM(STRING=T%CC(:,I4)(I4:), KIND=4) .NE. 0 )) ERROR STOP 14
  END DO

  DO I8 =1, 128
    IF (ANY( LEN_TRIM(STRING=T%CC(I8,:), kind=8) .NE. 0))     ERROR STOP 18
  END DO


  IF (ANY(LEN_TRIM(STRING=T%CC(:,:)(:), KIND=4)         .NE. III)) ERROR STOP 92

  END
