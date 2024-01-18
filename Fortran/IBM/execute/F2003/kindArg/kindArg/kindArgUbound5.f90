!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case            IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgUbound5
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jul. 05, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics 
!*
!*  SECONDARY FUNCTIONS TESTED : UBOUND 
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
!*  Result Characteristics.
!*  Integer. If KIND is present, the kind type parameter is that specified by the value of KIND; 
!*  otherwise the kind type parameter is that of default integer type. The result is scalar if DIM is present;
!*  otherwise, the result is an array of rank one and size n, where n is the rank of ARRAY.
!*    
!*  (322397) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgUbound5
  IMPLICIT NONE

  INTEGER(1) :: I1
  INTEGER(4) :: I

  INTEGER, PARAMETER    :: L1=-2**7
  INTEGER, PARAMETER    :: L2=-2**7+1

  CHARACTER(128) :: C
  CHARACTER(:), POINTER :: CC(:,:,:,:,:,:,:,:,:)
  integer :: ii(L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2)
  ALLOCATE(CHARACTER(128) :: CC(L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2,L1:L2))


  DO I1 = 1, 9
    IF (     UBOUND(ARRAY=CC, DIM=I1, KIND=INT(KIND(CC)+7, KIND=8))   .NE. L2)               STOP 11
    IF (KIND(UBOUND(ARRAY=CC, DIM=I1, KIND=INT(KIND(CC)+7, KIND=8)))  .NE. 8)                STOP 12
    IF (     UBOUND(ARRAY=CC, DIM=I1, KIND=INT(KIND(CC)+3, KIND=4))   .NE. L2)               STOP 13
    IF (KIND(UBOUND(ARRAY=CC, DIM=I1, KIND=INT(KIND(CC)+3, KIND=4)))  .NE. 4)                STOP 14
    IF (     UBOUND(ARRAY=CC, DIM=I1, KIND=INT(KIND(CC)+1, KIND=2))   .NE. L2)               STOP 15
    IF (KIND(UBOUND(ARRAY=CC, DIM=I1, KIND=INT(KIND(CC)+1, KIND=2)))  .NE. 2)                STOP 16
    IF (     UBOUND(ARRAY=CC, DIM=I1, KIND=INT(KIND(CC)+0, KIND=1))   .NE. L2)               STOP 17
    IF (KIND(UBOUND(ARRAY=CC, DIM=I1, KIND=INT(KIND(CC)+0, KIND=1)))  .NE. 1)                STOP 18
  END DO


  IF (SIZE(UBOUND(ARRAY=CC, KIND=INT(KIND(CC)+7, KIND=8)))  .NE. 9)                STOP 20
  IF (ANY( UBOUND(ARRAY=CC, KIND=INT(KIND(CC)+7, KIND=8))   .NE. L2))              STOP 21
  IF (KIND(UBOUND(ARRAY=CC, KIND=INT(KIND(CC)+7, KIND=8)))  .NE. 8)                STOP 22

  IF (SIZE(UBOUND(ARRAY=CC, KIND=INT(KIND(CC)+3, KIND=8)))  .NE. 9)                STOP 30
  IF (ANY( UBOUND(ARRAY=CC, KIND=INT(KIND(CC)+3, KIND=8))   .NE. L2))              STOP 31
  IF (KIND(UBOUND(ARRAY=CC, KIND=INT(KIND(CC)+3, KIND=8)))  .NE. 4)                STOP 32

  IF (SIZE(UBOUND(ARRAY=CC, KIND=INT(KIND(CC)+1, KIND=8)))  .NE. 9)                STOP 40
  IF (ANY( UBOUND(ARRAY=CC, KIND=INT(KIND(CC)+1, KIND=8))   .NE. L2))              STOP 41
  IF (KIND(UBOUND(ARRAY=CC, KIND=INT(KIND(CC)+1, KIND=8)))  .NE. 2)                STOP 42

  IF (SIZE(UBOUND(ARRAY=CC, KIND=INT(KIND(CC)+0, KIND=8)))  .NE. 9)                STOP 50
  IF (ANY( UBOUND(ARRAY=CC, KIND=INT(KIND(CC)+0, KIND=8))   .NE. L2))              STOP 51
  IF (KIND(UBOUND(ARRAY=CC, KIND=INT(KIND(CC)+0, KIND=8)))  .NE. 1)                STOP 52

  IF (ANY( UBOUND(ARRAY=CC)   .NE. L2))                      STOP 31
  IF (KIND(UBOUND(ARRAY=CC))  .NE. 4)                        STOP 32


  END

