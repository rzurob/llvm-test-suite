!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgIndex2
!*
!*  DATE                       : Jun. 12, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : INDEX
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


  PROGRAM kindArgIndex2

  INTEGER(1),    PARAMETER :: I(3) = (/INTEGER(4)::1,1,1/)
  CHARACTER,     PARAMETER :: C(3) = (/character::"I","B","M"/)

  PRINT*, INDEX(SUBSTRING=C(1), STRING=C(2), KINDD=I(1))
  PRINT*, INDEX(SUBSTRING=C(1), STRING=C(2), KID=kind(1))
  PRINT*, INDEX(KIND=I(1),SUBSTRING=C, STRING=C, KIND=2)
  PRINT*, INDEX(SUBSTRING=C(1), STRING=C(2), SUBSTRING=C(1), KIND=1_2)

  PRINT*, INDEX(SUBSTRING=C, STRING=C, kInD=I(2)) ! this is fine
  PRINT*, INDEX(SUBSTRING=C, STRING=C, kInD=KIND(C)) ! this is fine

  END

