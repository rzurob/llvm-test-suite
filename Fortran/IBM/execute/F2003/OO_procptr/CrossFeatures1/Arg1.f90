! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 19, 2005
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
!*  Dummy argument is a procedure pointer - function/Null
!*  (304727)(305366)(305719)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M0

    TYPE :: Base
      CHARACTER(3) :: C
    END TYPE

  END MODULE

  MODULE M
  USE M0

    TYPE  :: DT
      PROCEDURE(TYPE(Base)), NOPASS, POINTER :: ProcPtr
    END TYPE

    CONTAINS

    FUNCTION ModFun(Arg)
    TYPE(Base) :: Arg, ModFun
      ModFun = Arg
    END FUNCTION

  END MODULE

  FUNCTION RetPtr(Fun)
  USE M
  PROCEDURE(ModFun), POINTER :: Fun
  PROCEDURE(ModFun), POINTER :: RetPtr
    RetPtr => Fun
  END FUNCTION

  PROGRAM Arg1
  USE M
  IMPLICIT NONE

  INTERFACE
    FUNCTION RetPtr(Fun)
      IMPORT
      PROCEDURE(ModFun), POINTER :: Fun
      PROCEDURE(ModFun), POINTER :: RetPtr
    END FUNCTION
  END INTERFACE

  PROCEDURE(ModFun), POINTER :: ProcPtr

  ProcPtr => Modfun
  CALL IntSub(NULL())
  CALL IntSub(NULL(ProcPtr))
  CALL IntSub(RetPtr(NULL()))

  CONTAINS

  SUBROUTINE IntSub(Ptr)
  PROCEDURE(ModFun), POINTER :: Ptr
    IF ( ASSOCIATED(Ptr)) STOP 41
  END SUBROUTINE

  END

