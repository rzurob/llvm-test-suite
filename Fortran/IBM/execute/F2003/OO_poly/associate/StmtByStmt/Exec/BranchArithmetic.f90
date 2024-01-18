! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 25, 2005
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
!*  Branching with arithmetic GOTO
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM BranchArithmetic
  IMPLICIT NONE

  CALL Sub("ok", -1)
  CONTAINS
  SUBROUTINE Sub(Arg, Exp)
  CHARACTER(*) :: Arg
  CLASS(*) :: Exp

  SELECT TYPE ( Exp )
  TYPE IS (INTEGER)
    IF (Exp) 1,2,3
  END SELECT

1 ASSOCIATE ( ASSOCIATE => Arg )
    IF ( ASSOCIATE .NE. "ok" ) STOP 20
    SELECT TYPE ( Exp )
    TYPE IS (INTEGER)
      If (Exp + 1) 1, 2, 3
    END SELECT
2 END ASSOCIATE

  STOP 0

3 PRINT *, "Should not be here!"
  STOP 11

  END SUBROUTINE
  END
