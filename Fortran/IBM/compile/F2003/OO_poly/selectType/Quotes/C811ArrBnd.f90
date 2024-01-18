! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 2, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C811
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
!*    The selector is an array constructor without ssociate-name =>
!*    Array formed by a binding call
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base
    END TYPE

    TYPE, EXTENDS(Base) :: Child
    CONTAINS
      PROCEDURE, PASS   :: GetBase
    END TYPE

    CONTAINS

    FUNCTION GetBase(Arg)
    CLASS(Child)              :: Arg
    CLASS(Base), ALLOCATABLE  :: GetBase(:)
      ALLOCATE(GetBase(3))
      SELECT TYPE (GetBase)
        TYPE IS (Base)
          GetBase = Arg%Base
        CLASS DEFAULT
          STOP 33
      END SELECT
    END FUNCTION

  END MODULE

  PROGRAM C811ArrBnd
  USE M
  IMPLICIT NONE

  CLASS(*), POINTER    :: Ptr
  CLASS(Child), POINTER :: Ptr1

  ALLOCATE(Child :: Ptr )

  SELECT TYPE ( Ptr%GetBase() )
  END SELECT

  SELECT TYPE ( Ptr1%GetBase() )
  END SELECT
  STOP 40

  END

