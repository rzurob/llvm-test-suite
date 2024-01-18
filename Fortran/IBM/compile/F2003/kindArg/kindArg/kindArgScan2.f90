!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 28, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : New Kind argumnet for existing intrinsics
!*
!*  SECONDARY FUNCTIONS TESTED : SCAN
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


  PROGRAM kindArgScan2

  INTEGER(1),    PARAMETER :: I(3) = (/INTEGER(4)::1,1,1/)
  CHARACTER,     PARAMETER :: C(3) = (/character::"I","B","M"/)
  LOGICAL,       PARAMETER :: BACK=.true.

  PRINT*, SCAN(SET=C(1), STRING=C(2), KINDD=I(1))
  PRINT*, SCAN(SET=C(1), STRING=C(2), KID=kind(1))
  PRINT*, SCAN(KIND=I(1),SET=C, STRING=C, KIND=2)
  PRINT*, SCAN(SET=C(1), STRING=C(2), BACKK=.FALSE., KINDD=1_2)

  PRINT*, SCAN(SET=C, STRING=C, kInD=KIND(I(:))) ! this is fine
  PRINT*, SCAN(SET=C, STRING=C, kInD=KIND(I), BACK=BACK) ! this is fine

  END

