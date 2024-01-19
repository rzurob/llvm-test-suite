!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jul. 05, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : UBOUND
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
!*  characteristics :: value of kind
!*
!*  (325884)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgUbound1

  INTEGER,    PARAMETER :: I(3) = (/-1,0,3/)
  INTEGER,    PARAMETER :: J(3) = (/1,-1,-1/)


  PRINT*, UBOUND(ARRAY=I, KIND=I(1))
  PRINT*, UBOUND(ARRAY=I(:), KIND=I(2))
  PRINT*, UBOUND(KIND=I(3), DIM=I(1), ARRAY=I)

  PRINT*, UBOUND(ARRAY=I, KIND=SUM(I)) ! ok

  PRINT*, UBOUND(KIND=SUM(I+J), ARRAY=I) !ok

  END

