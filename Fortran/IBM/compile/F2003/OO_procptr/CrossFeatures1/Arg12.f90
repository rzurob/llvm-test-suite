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
! %POSTCMD: tcomp Arg12.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Arg12.f
!*
!*  DATE                       : May. 23, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Explicit dummy procedure - Characteristics
!*  Implicit/Explicit Interface
!*  (304109) - Defered to feature 304991
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      CHARACTER(3) :: C
    END TYPE

    INTERFACE
      FUNCTION IntF(Arg)
      IMPORT
        TYPE(Base) :: Arg
        TYPE(Base):: IntF
      END FUNCTION
    END INTERFACE

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(Base) :: Arg
  TYPE(Base) :: ExtFun
    ExtFun = Arg
  END FUNCTION


  PROGRAM Arg12
  USE M
  IMPLICIT NONE

  PROCEDURE(IntF) :: ExtFun
  PROCEDURE(IntF), POINTER :: ProcPtr1
  PROCEDURE(TYPE(Base)), POINTER :: ProcPtr2

  ProcPtr1 => ExtFun
  CALL IntSub(ProcPtr1)

  ProcPtr2 => ExtFun
  CALL IntSub1(ProcPtr2)  !This is wrong


  CONTAINS

    SUBROUTINE IntSub(Arg)
    IMPLICIT NONE
    PROCEDURE(TYPE(Base)), POINTER :: Arg
    END SUBROUTINE

    SUBROUTINE IntSub1(Arg)
    IMPLICIT  NONE
    PROCEDURE(IntF), POINTER :: Arg
    END SUBROUTINE


  END

