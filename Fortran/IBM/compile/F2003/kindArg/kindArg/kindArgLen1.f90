!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 15, 2006
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
!*  characteristics :: value of kind
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLen1

  INTEGER,       PARAMETER :: I(3) = (/-1,0,3/)
  INTEGER,       PARAMETER :: J(3) = (/1,1,1/)
  CHARACTER(*),  PARAMETER :: CC(2) = (/"123", "abc"/)

  PRINT*, LEN(STRING=CC(2), KIND=I(1))
  PRINT*, LEN(STRING=CC, KIND=I(2))
  PRINT*, LEN(STRING=CC, KIND=LEN(CC))

  PRINT*, LEN(STRING=CC, KIND=SUM(J))

  PRINT*, LEN(STRING=CC, KIND=J(LEN(CC, KIND=1)) ) !ok

  END

