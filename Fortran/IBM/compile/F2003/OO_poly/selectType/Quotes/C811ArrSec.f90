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
! %POSTCMD: tcomp C811ArrSec.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C811ArrSec
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
!*    The selector is an array section  without ssociate-name =>
!*
!*    (Wrong check)
!*   Relax the err msg check to the current err msg
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

  PROGRAM C811ArrSec
  USE M
  IMPLICIT NONE

  CLASS(Base), POINTER :: Ptr(:,:)

  ALLOCATE( Child :: Ptr(2:10, 3:12) )

  SELECT TYPE ( Ptr(::2, ::1) )
    TYPE IS (Base)
      STOP 20
    CLASS DEFAULT
      STOP 30
  END SELECT
  STOP 40

  END

