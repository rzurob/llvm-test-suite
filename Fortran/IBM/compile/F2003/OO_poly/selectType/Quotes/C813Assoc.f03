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
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    END TYPE

  END MODULE

  PROGRAM C813Assoc
  USE M
  IMPLICIT NONE

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*) :: ARg


  SELECT TYPE (Arg)
  TYPE IS (child)
    ASSOCIATE ( As =>Arg )
    SELECT TYPE ( As )
      TYPE IS (Child)
      CLASS DEFAULT
    END SELECT
    END ASSOCIATE
  END SELECT

  END SUBROUTINE

  END

