! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 20, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*  Wrong semantics check on //
!*
!* (293110)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc19

  ASSOCIATE ( P1 => "ABC", P2 => P1 // P1)
  ! PRINT*, P1
  ! PRINT*, P2
  END ASSOCIATE
  END


