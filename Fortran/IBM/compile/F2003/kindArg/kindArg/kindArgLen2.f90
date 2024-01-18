!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : kindArgLen2
!*
!*  DATE                       : Jun. 22, 2006
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
!*  characteristics :: the keyword - KIND
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLen2

  INTEGER(1),    PARAMETER :: I(3) = (/1,1,1/)
  CHARACTER,     PARAMETER :: C(3) = (/"I","B","M"/)
  INTEGER,       PARAMETER :: Kind(2)=(/1,2/)

  PRINT*, LEN(STRING=C(1), KINDD=I(1))
  PRINT*, LEN(STRING=C(2), KID=Kind(1))
  PRINT*, LEN(KIND=I(1), KIND=Kind(2))
  PRINT*, LEN(STRING=(/"I","B","M"/), KIND=1, STRING=(/"I","B","M"/))
  PRINT*, LEN(LEN=.TRUE., STRING=(/"1","2"/))

  PRINT*, LEN(StrinG=C, kInD=I(2)) ! this is fine
  PRINT*, LEN(string=C, kInD=C%KIND) ! this is fine

  END

