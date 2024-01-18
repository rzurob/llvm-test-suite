! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 24, 2005
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
!*  1. Proc-ptr name conflict with type bound names
!*  2. pure subprogram
!*  (315494/315506)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE  :: DT1
      PROCEDURE(ModFun1), PASS, POINTER :: ProcPtr
    END TYPE

    TYPE, EXTENDS(DT1)  :: DT2
    CONTAINS
      PROCEDURE, PASS :: ProcPtr => ModFun2
    END TYPE

    TYPE  :: DT3
      PROCEDURE(), NoPASS, POINTER :: ProcPtr
    CONTAINS
      PROCEDURE, NoPASS :: ProcPtr => ModFun3
    END TYPE

    CONTAINS

    FUNCTION ModFun1(Arg)
    CLASS(DT1) :: Arg
    TYPE(DT1) :: ModFun1
      ModFun1 = Arg
    END FUNCTION

    FUNCTION ModFun2(Arg)
    CLASS(DT2) :: Arg
    TYPE(DT2) :: ModFun2
      ModFun2 = Arg
    END FUNCTION

  END MODULE


  PURE SUBROUTINE ExtSub(Proc, ProcPtr)
  PROCEDURE()          :: Proc
  PROCEDURE(), POINTER :: ProcPtr
  END SUBROUTINE

  PURE SUBROUTINE ExtSub1(Proc, ProcPtr)
  INTERFACE
    SUBROUTINE IS()
    END SUBROUTINE
  END INTERFACE
  PROCEDURE(IS)          :: Proc
  PROCEDURE(IS), POINTER :: ProcPtr
  END SUBROUTINE

  PROGRAM StrComp5

  END

