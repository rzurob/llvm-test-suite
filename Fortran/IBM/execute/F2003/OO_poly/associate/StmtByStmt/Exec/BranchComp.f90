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
!*  Branching with computed GOTO
!*  (ICE-300400)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM BranchComp
  IMPLICIT NONE

  CALL Sub("ok", 1)
  CONTAINS
  SUBROUTINE Sub(Arg, Label)
  CHARACTER(*) :: Arg
  CLASS(*) :: Label

  SELECT TYPE ( Label )
  TYPE IS (INTEGER)
    GOTO (1,2) Label
  END SELECT

1 ASSOCIATE ( ASSOCIATE => Arg )
    IF ( ASSOCIATE .NE. "ok" ) STOP 20
    SELECT TYPE ( Label)
    TYPE IS (INTEGER)
      GOTO (1,2) Label + 1
    END SELECT
2 END ASSOCIATE
  END SUBROUTINE

  END
