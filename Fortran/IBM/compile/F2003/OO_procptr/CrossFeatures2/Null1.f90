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
! %POSTCMD: tcomp Null1.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Null1.f
!*
!*  DATE                       : May. 10, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
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
!*   null()
!*   The characteristics
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER :: Id
      PROCEDURE(Fun), POINTER :: ProcPtr=>NULL()
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    TYPE(DT) :: Fun
    CLASS(DT) :: Arg
      Fun = Arg
    END FUNCTION

  END MODULE

  PROGRAM Null1
  USE M
  IMPLICIT NONE

  TYPE (DT) :: V

  INTERFACE
    TYPE(DT) FUNCTION ExtFun()
    IMPORT DT
    END FUNCTION
  END INTERFACE

  PROCEDURE(Fun), POINTER :: ProcPtr=>NULL()
  PROCEDURE(ExtFun), POINTER :: ProcPtr1=>NULL()

  ProcPtr1 => ExtFun

  ProcPtr => ExtFun
  ProcPtr => NULL(ProcPtr)
  ProcPtr => NULL(ProcPtr1)


  V%ProcPtr => NULL(Fun)
  V%ProcPtr => Fun
  V%ProcPtr => NULL(ProcPtr1)

  ProcPtr1 => Fun   ! This is wrong
  ProcPtr1 => NULL(ProcPtr)


  END

