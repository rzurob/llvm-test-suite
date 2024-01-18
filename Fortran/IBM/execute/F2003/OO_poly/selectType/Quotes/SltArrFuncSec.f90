! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 17, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
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
!*
!*   The selector is associate name associating to  an array section from function call
!*
!*    (Undefiend symbol on Fun-298310)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM SltArrFuncSec
  IMPLICIT NONE

  CALL Sub()

  CONTAINS

  FUNCTION Fun()
  CLASS(*), ALLOCATABLE :: Fun(:,:)
  INTEGER :: i
    ALLOCATE(Fun(3,3), SOURCE=RESHAPE((/(INT(i, 8), i=1,9)/),(/3,3/)))
  END FUNCTION

  SUBROUTINE Sub()

  SELECT TYPE ( As => Fun() )
  CLASS DEFAULT
  SELECT TYPE ( As => As(1:SIZE(As,1):2,1:SIZE(As,2):2 ) )

    TYPE IS (INTEGER(4))
      STOP 40
    TYPE IS (INTEGER(2))
      STOP 41
    TYPE IS (INTEGER(8))
      IF ( ANY (SHAPE(As) .NE. (/2,2/) )) STOP 20
      IF ( ANY (As .NE. RESHAPE((/1_8, 3_8, 7_8, 9_8/),(/2,2/)) )) STOP 20
    CLASS DEFAULT

  END SELECT
  END SELECT

  END SUBROUTINE

  END



