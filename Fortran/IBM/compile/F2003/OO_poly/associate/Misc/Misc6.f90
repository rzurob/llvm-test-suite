! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 12, 2004
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
!*  Missing actual argument in associate selector caused ICE
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
    CONTAINS
      PROCEDURE, NoPASS   :: ReturnBase
    END TYPE

    CONTAINS

    FUNCTION ReturnBase(Arg)
    CLASS(Base) :: Arg
    CLASS(Base), ALLOCATABLE  :: ReturnBase
      ALLOCATE(ReturnBase, SOURCE=Arg)
    END FUNCTION

  END MODULE

  PROGRAM  Misc6
  USE M
  IMPLICIT NONE

  ASSOCIATE (As => ReturnBase()) !Missing argument here
    PRINT*, SAME_TYPE_AS(As, Base())
  END ASSOCIATE

  END

