! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp InTypeIs1.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : InTypeIs1
!*
!*  DATE                       : Jan. 24, 2005
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
!*   Within the TYPE IS, the associating entity is not polymorphic
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero
      INTEGER :: Id = 0
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base
      INTEGER :: BaseId = 1
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    END TYPE

  END MODULE

  PROGRAM InTypeIs1
  USE M
  IMPLICIT NONE
  TYPE(Child) :: V(2,2)

  CALL Sub(V)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*) :: Arg(2:3,3:4)

    SELECT TYPE (Arg=>Arg(2, 3))
    TYPE IS (Child)
      IF (.NOT. SAME_TYPE_AS(Arg, V)) STOP 20
      SELECT TYPE (Arg)
      END SELECT
    CLASS DEFAULT
      STOP 21
    END SELECT

    SELECT TYPE (Arg)
    TYPE IS (Child)
      SELECT TYPE (Arg)
      END SELECT
      IF (.NOT. SAME_TYPE_AS(Arg, V)) STOP 30
    CLASS DEFAULT
      STOP 41
    END SELECT

  END SUBROUTINE

  END



