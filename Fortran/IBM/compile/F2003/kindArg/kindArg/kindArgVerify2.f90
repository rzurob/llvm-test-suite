!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jul. 06, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : VERIFY
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


  PROGRAM kindArgVerify2

  INTEGER(1),    PARAMETER :: I(3) = (/INTEGER(4)::1,1,1/)
  CHARACTER,     PARAMETER :: C(3) = (/character::"I","B","M"/)

  PRINT*, VERIFY(SET=C(1), STRING=C(2), KINDD=I(1))
  PRINT*, VERIFY(SET=C(1), STRING=C(2), KID=kind(1))
  PRINT*, VERIFY(KIND=I(1),SET=C, STRING=C, KIND=2)
  PRINT*, VERIFY(SET=C(1), STRING=C(2), SET=C(1), KIND=1_2)

  PRINT*, VERIFY(SET=C, STRING=C, kInD=I(2)) ! this is fine
  PRINT*, VERIFY(SET=C, STRING=C, kInD=KIND(C), BACK=.FALSE.) ! this is fine

  END

