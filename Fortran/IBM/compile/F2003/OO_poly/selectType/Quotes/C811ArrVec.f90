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
! %POSTCMD: tcomp C811ArrVec.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C811ArrVec
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
!*    Array section with a vector subscript.
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base
    END TYPE

    TYPE, EXTENDS(Base) :: Child
    END TYPE

  END MODULE

  PROGRAM C811ArrVec
  USE M
  IMPLICIT NONE

  CLASS(Base), POINTER :: Ptr(:,:)

  ALLOCATE( Child :: Ptr(2:10, 3:12) )

  SELECT TYPE ( Ptr((/10,7,7,2/), (/12,3,3,12/)) )
    TYPE IS (Base)
      STOP 20
    CLASS DEFAULT
      STOP 30
  END SELECT
  STOP 40

  END

