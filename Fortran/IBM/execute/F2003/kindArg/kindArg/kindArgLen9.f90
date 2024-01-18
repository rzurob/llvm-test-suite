!*********************************************************************
!*  ===================================================================
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
!*  -qintsize
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM kindArgLen9
  CHARACTER(127) :: CC(128)


  DO I = 0, 127
    IF (     LEN(STRING=CC(I)(1:I), KIND=K%KIND)    .NE. I )         ERROR STOP 11
    IF (KIND(LEN(STRING=CC(I)(1:I), KIND=K%KIND))   .NE. K%KIND ) ERROR STOP 12
  END DO


  END

