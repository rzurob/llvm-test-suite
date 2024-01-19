!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : LBOUND
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
!*  characteristics :: the keyword - KIND
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLbound2

  INTEGER(1),    PARAMETER :: I(3) = (/1,1,1/)
  INTEGER(1),    PARAMETER :: Array(3) = (/1,1,1/)
  INTEGER(1),    PARAMETER :: KIND(3)  = (/1,1,2/)

  PRINT*, LBOUND(Array, KINDD=I(1))
  PRINT*, LBOUND(Array, DIMKIND=I(1))
  PRINT*, LBOUND(ARRAY=KIND, KIND=Array(1))
  PRINT*, LBOUND(ARRAY=Array, KIND=1, KIND=1)
  PRINT*, LBOUND(KIND=KIND(1), ARRAY=Array, KIND=1_2)

  PRINT*, LBOUND(I, kInD=I(2)) ! this is fine
  PRINT*, LBOUND(ARRAY=Array, kInD=KIND(I(1))) ! this is fine

  END

