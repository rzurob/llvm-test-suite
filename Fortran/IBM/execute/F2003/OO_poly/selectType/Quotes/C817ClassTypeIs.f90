! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: C817ClassTypeIs.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C817ClassTypeIs
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
!*    The same type is specified for both type guard CLASS IS
!*    and TYPE IS
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

  PROGRAM C817ClassTypeIs
  USE M
  IMPLICIT NONE

  CLASS(*), POINTER :: Ptr
  TYPE(LEVEL2), TARGET  :: Tar

  Ptr  => Tar
  CALL Sub(Ptr)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*) :: Arg

  SELECT TYPE ( Arg )
    TYPE IS (Level1)
      STOP 50
    CLASS IS (Level1)
      STOP 51
    TYPE IS (Level2)
    ! STOP 53
    CLASS IS (Level2)
      STOP 55
    CLASS IS (Level4)
      STOP 55
    TYPE IS (Level4)
      STOP 56
    CLASS DEFAULT
      STOP 30
  END SELECT

  END SUBROUTINE

  END

