! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 12, 2005
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
!*  C727 (R742) A procedure-name shall be the name of an external, module,
!*  or dummy procedure, a specific intrinsic function listed in 13.6
!*  and not marked with a bullet (.), or a procedure pointer.
!*
!*  (Wrong result at 22)
!*  (306625)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  CONTAINS

    FUNCTION ModFun(Arg)
    COMPLEX(8)          :: Arg
    COMPLEX(8), POINTER :: ModFun
      !ALLOCATE(ModFun, SOURCE=Arg)
      ALLOCATE(ModFun)
      ModFun = Arg
    END FUNCTION

    SUBROUTINE ModSub(Arg1, Arg2)
    PROCEDURE(ModFun) :: Arg1
    COMPLEX(8)        :: Arg2
      IF ( Arg1(Arg2) .NE. Arg2 ) ERROR STOP 21
    END SUBROUTINE

  END MODULE


  PROGRAM PtrAssignProcNameMod
  USE M
  IMPLICIT NONE

  PROCEDURE(IFun),   POINTER :: Ptr
  PROCEDURE(ModSub), POINTER :: Ptr1
  COMPLEX(8)                 :: Cmpx

    Ptr => ModFun
    Cmpx = Ptr((1.0_8,-1.0_8))
    IF (Cmpx .NE. (1.0_8,-1.0_8) ) ERROR STOP 22

    Ptr1 => ModSub
    CALL Ptr1(ModFun, (1.0_8,-1.0_8))

    Ptr => ModFun
    CALL Ptr1(Ptr, (-1.0_8,1.0_8))

  CONTAINS

    FUNCTION IFun(Arg)
    COMPLEX(8)          :: Arg
    COMPLEX(8), POINTER :: IFun
      !ALLOCATE(IFun, SOURCE=Arg)
      ALLOCATE(IFun)
      IFun = Arg
    END FUNCTION

  END

