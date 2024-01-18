! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodeferredlp /tstdev/OO_poly/selectType/Quotes/C811ArrBnd.f
! opt variations: -qnok -qnol -qdeferredlp

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
! %POSTCMD: tcomp C811ArrBnd.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C811ArrBnd
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

    TYPE :: Base(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
    CONTAINS
      PROCEDURE, PASS   :: GetBase
    END TYPE

    CONTAINS

    FUNCTION GetBase(Arg)
    CLASS(Child(4,*))              :: Arg
    CLASS(Base(4,20)), ALLOCATABLE  :: GetBase(:)
      ALLOCATE(GetBase(3))
      SELECT TYPE (GetBase)
        TYPE IS (Base(4,*))
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
  CLASS(Child(4,20)), POINTER :: Ptr1

  ALLOCATE(Child(4,20) :: Ptr )

  SELECT TYPE ( Ptr%GetBase() )
  END SELECT

  SELECT TYPE ( Ptr1%GetBase() )
  END SELECT
  STOP 40

  END

