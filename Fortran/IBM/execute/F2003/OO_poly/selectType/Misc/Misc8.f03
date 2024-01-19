! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 16, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED :
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
!*  Associating entity is poly by class is
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  Misc8

    TYPE :: Base
    END TYPE
    TYPE, EXTENDS(Base) :: Child
    END TYPE

    SELECT TYPE ( As => Fun() )
      CLASS IS (Child)
        SELECT TYPE ( As )
          CLASS DEFAULT
        END SELECT
  END SELECT

  CONTAINS

  FUNCTION Fun()
    CLASS(base), ALLOCATABLE :: Fun
    ALLOCATE(Child :: Fun)
  END FUNCTION

  END

