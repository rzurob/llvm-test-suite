! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 3, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C817
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
!*    The type guard CLASS IS is specified with the same type twice
!*    ()
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

  PROGRAM C817ClassIsPoly
  USE M
  IMPLICIT NONE

  CLASS(Level1), POINTER :: Ptr
  TYPE(Level1), TARGET  :: Tar

  Ptr  => Tar

  SELECT TYPE ( Ptr )

    CLASS IS (Level4)
      STOP 50
    CLASS IS (Level3)
      STOP 51
    CLASS IS (Level1)
      !STOP 52
      PRINT*, "OK!, Still run here"
    CLASS IS (Level3)
      STOP 53
    CLASS IS (Level1)
      STOP 54

    CLASS DEFAULT
      STOP 30
  END SELECT


  END

