!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgMaxlog1
!*
!*  DATE                       : Jun. 26, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : MAXLOC
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
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgMaxlog1

  INTEGER(1),    PARAMETER :: I(3) = (/-1,0,3/)
  INTEGER(2),    PARAMETER :: J(3) = (/1,1,1/)


  PRINT*, MAXLOC((/CHAR(1_1)/), 1_1, KIND=I(1))
  PRINT*, MAXLOC(I, .FALSE., KIND=I(2))
  PRINT*, MAXLOC(KIND=I(3), MASK=.FALSE.,  DIM=1_1, ARRAY=J)

  PRINT*, MAXLOC((/"A","B","C"/), KIND=KIND(I+J))

  PRINT*, MAXLOC(KIND=J(J(KIND(I+J))), ARRAY=(/"A","B","C"/), MASK=.TRUE.) !ok

  END

