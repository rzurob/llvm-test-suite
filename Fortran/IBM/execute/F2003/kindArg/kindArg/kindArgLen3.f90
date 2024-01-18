!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgLen3
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 22, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : LEN 
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
!*  Description. Returns the length of a character entity. 
!*    
!*  (322672) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLen3

  TYPE :: DT
    CHARACTER(:), ALLOCATABLE :: CC(:,:)
  END TYPE
  
  INTEGER(1) :: I1
  INTEGER(2) :: I2
  INTEGER(4) :: I4
  INTEGER(8) :: I8
    
  TYPE(DT), POINTER :: T
 
  ALLOCATE(T)
  ALLOCATE(CHARACTER(128) :: T%CC(128,128))

  DO I=1,128
  DO J=1, 128
  DO K=1, 128
    T%CC(I,J)(K:K)=ACHAR(K)
  END DO
  END DO
  END DO

  DO I1 =1, 127
    IF (LEN(STRING=T%CC(:,:)(I1+1:), KIND=I1%KIND )   .NE. 128-I1 )   STOP 11
  END DO

  DO I2 =1, 128
    IF (LEN(STRING=T%CC(:,:)(1:), KIND=I2%KIND )   .NE. 128)    STOP 12
  END DO

  DO I4 =1, 128
    IF (LEN(STRING=T%CC(:,I4)(I4:), KIND=I4%KIND ) .NE. 129-I4) STOP 14
  END DO

  DO I8 =1, 128
    IF (LEN(STRING=T%CC(I8,:)(:I8), KIND=I8%KIND ) .NE. I8)     STOP 18
  END DO


  IF (LEN(STRING=T%CC(1,1), KIND=2_1 ) .NE. 128) STOP 91
  IF (LEN(STRING=T%CC(:,:)(:))         .NE. 128) STOP 92



  END

