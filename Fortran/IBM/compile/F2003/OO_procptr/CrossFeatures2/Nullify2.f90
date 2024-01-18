! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Nullify2.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Nullify2.f
!*
!*  DATE                       : May. 10, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : nullify
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
!*  The nullify stmt
!*  (315148)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER :: Id
    END TYPE

    CONTAINS

    FUNCTION ReturnProcPtr(Arg)
    PROCEDURE(Fun), POINTER :: ReturnProcPtr
    PROCEDURE(Fun)          :: Arg
      ReturnProcPtr => Arg
    END FUNCTION

    FUNCTION Fun(Arg)
    CLASS(*) :: Arg
    CLASS(*), POINTER :: Fun
      ALLOCATE(Fun, SOURCE=Arg)
    END FUNCTION

  END MODULE

  PROGRAM Nullify2
  USE M
  IMPLICIT NONE
  PROCEDURE(ReturnProcPtr), POINTER :: ProcPtr=>NULL()

  ProcPtr => ReturnProcPtr
  NULLIFY(ProcPtr(Fun))

  END


