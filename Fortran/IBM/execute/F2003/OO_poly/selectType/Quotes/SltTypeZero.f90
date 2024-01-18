! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: SltTypeZero.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltTypeZero
!*
!*  DATE                       : Dec. 14, 2004
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
!*   The type spec is specified with a type of zero size
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
    END TYPE

    TYPE, ABSTRACT,  EXTENDS(Base) :: Base1
    END TYPE

    TYPE, EXTENDS(Base1) :: Child
    END TYPE

  END MODULE

  PROGRAM SltTypeZero
  USE M
  IMPLICIT NONE

  CLASS(Base) ,ALLOCATABLE :: Var

  ALLOCATE(Var, SOURCE=Child())

  SELECT TYPE (Var)
   CLASS IS (Base)
     STOP 20
   TYPE IS (Base)
     STOP 21
   CLASS IS (Child)
     STOP 22
   TYPE IS (Child)
     PRINT*, "OK!"
   CLASS DEFAULT
     STOP 20
  END SELECT

  END

