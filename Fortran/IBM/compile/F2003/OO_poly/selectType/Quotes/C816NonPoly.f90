! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp C816NonPoly.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C816NonPoly
!*
!*  DATE                       : Dec. 3, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C816
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
!*    The selector is a non poly entity
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT :: Level0
    END TYPE

    TYPE, EXTENDS(Level0) :: Level1
      INTEGER :: Level1Id = 1
    END TYPE

    TYPE, EXTENDS(Level1) :: Level2
      INTEGER :: Level2Id = 2
    END TYPE

    TYPE, EXTENDS(Level2) :: Level3
      INTEGER :: Level3Id = 3
    END TYPE

    TYPE, EXTENDS(Level3) :: Level4
      INTEGER :: Level4Id = 4
    END TYPE

  END MODULE

  PROGRAM C816NonPoly
  USE M
  IMPLICIT NONE

  CLASS(Level4), POINTER :: Var

  ALLOCATE(Var )

  ASSOCIATE ( As => Var )
  SELECT TYPE ( As )
  TYPE IS (LEVEL4)

  SELECT TYPE ( As )
    TYPE IS (Level4)
      STOP 50
    TYPE IS (Level3)
      STOP 51
    TYPE IS (Level2)
      STOP 52
    TYPE IS (Level1)
      STOP 53
    TYPE IS (Level0)
      STOP 54
    CLASS DEFAULT
      STOP 30
  END SELECT

  END SELECT
  END ASSOCIATE

  END

