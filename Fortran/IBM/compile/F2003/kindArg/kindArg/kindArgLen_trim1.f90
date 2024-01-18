!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgLen_trim1
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
!*  characteristics :: value of kind
!*
!*  (325879)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLen_trim1

  INTEGER,       PARAMETER :: I(3) = (/-1,0,3/)
  INTEGER,       PARAMETER :: J(3) = (/1,1,1/)
  CHARACTER(*),  PARAMETER :: CC(2) = (/" 123 ", " abc "/)

  PRINT*, LEN_TRIM(STRING=CC(2), KIND=I(1))
  PRINT*, LEN_TRIM(STRING=CC, KIND=I(2))
  PRINT*, LEN_TRIM(STRING=CC, KIND=LEN(CC))

  PRINT*, LEN_TRIM(STRING=CC, KIND=SUM(J))

  PRINT*, LEN_TRIM(STRING=CC, KIND=J(LEN_TRIM(CC(2)(4:5), KIND=1)) ) !ok

  END

