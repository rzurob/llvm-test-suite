! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 3, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C812
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The selector is a substring appearing in variable definition context.
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C812SubStr
  IMPLICIT NONE

  CALL Sub( "0123456789" )

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*), INTENT(IN) :: Arg

  SELECT TYPE ( As => Arg )
    CLASS DEFAULT
      SELECT TYPE ( As => As )
        CLASS DEFAULT
        As = "9876543210"
      END SELECT
    TYPE IS (REAL(8))
      STOP 20
    TYPE IS (CHARACTER(*))
      STOP 30
  END SELECT
  STOP 40

  END SUBROUTINE

  END

