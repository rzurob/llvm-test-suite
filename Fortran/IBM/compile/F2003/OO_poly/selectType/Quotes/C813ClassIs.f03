! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 3, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C813
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
!*    The selector is an associating entity of poly with an  extension type
!*    to thst of "TYPE IS"
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT :: Ground
    END TYPE

    TYPE, EXTENDS(Ground) :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM C813ClassIs
  USE M
  IMPLICIT NONE

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*) :: ARg


  SELECT TYPE (Arg)
  CLASS IS (child)
    SELECT TYPE ( As => Arg)
      TYPE IS (Base)
        STOP 51
      CLASS DEFAULT
        STOP 31
    END SELECT
  END SELECT

  END SUBROUTINE

  END
