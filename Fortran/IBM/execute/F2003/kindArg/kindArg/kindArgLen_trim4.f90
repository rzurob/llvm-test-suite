!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgLen_trim4
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 23, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : LEN_TRIM 
!*
!*  REFERENCE                  : Feature Number 289083 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   
!*   
!*  Returns the length of the character argument without counting trailing blank characters. 
!*  - string of spaces  
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLen_trim4


  TYPE :: DT
    CHARACTER(:), ALLOCATABLE :: CC(:,:)
  END TYPE

  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8

  TYPE(DT),  POINTER :: T
  INTEGER            :: III(128,128)=0

  ALLOCATE(T)
  ALLOCATE(CHARACTER(128) :: T%CC(128,128))

  T%CC=" "


  DO I1 =1, 127
    IF (LEN_TRIM(STRING=T%CC(I1,I1), KIND=LEN_TRIM("1   "(1:4), KIND=1) )   .NE. 0 )   STOP 11
  END DO

  DO I2 =1, 128
    IF (ANY( LEN_TRIM(STRING=T%CC(I2,:)(1:), KIND=LEN_TRIM(" 2  "(1:4), KIND=2) )   .NE. 0 ))    STOP 12
  END DO

  DO I4 =1, 128
    IF ( ANY (LEN_TRIM(STRING=T%CC(:,I4)(I4:), KIND=LEN_TRIM("   4"(1:4), KIND=4) ) .NE. 0 )) STOP 14
  END DO

  DO I8 =1, 128
    IF (ANY( LEN_TRIM(STRING=T%CC(I8,:), KIND=LEN_TRIM("12345678"(1:8), KIND=8) ) .NE. 0))     STOP 18
  END DO


  IF (ANY(LEN_TRIM(STRING=T%CC(:,:)(:), KIND=LEN_TRIM("1234    "(1:8), KIND=8))         .NE. III)) STOP 92


  END

