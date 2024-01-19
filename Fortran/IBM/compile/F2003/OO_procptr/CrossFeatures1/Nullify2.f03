! *********************************************************************
!*  ===================================================================
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
!*  The nullify stmt
!*  (304562)
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
    INTEGER :: Fun
    INTEGER :: Arg
      Fun = Arg
    END FUNCTION

  END MODULE

  PROGRAM Nullify2
  USE M
  IMPLICIT NONE


  PROCEDURE(ReturnProcPtr), POINTER :: ProcPtr=>NULL()
  PROCEDURE(Fun),           POINTER :: ProcPtr1=>NULL()

  ProcPtr => ReturnProcPtr
  NULLIFY(ProcPtr(Fun))

  ProcPtr1 => Fun
  CALL IntSub(ProcPtr1, Fun)

  CONTAINS

  SUBROUTINE IntSub(Arg1, Arg2)
  PROCEDURE(Fun), POINTER, INTENT(IN) :: Arg1
  PROCEDURE(Fun)                      :: Arg2

    NULLIFY(Arg1)
    NULLIFY(Arg2)

  END SUBROUTINE

  END


