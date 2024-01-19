! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 08, 2005
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
!* Implicit stmt
!* ! in the derived type definition construct, the typing rule is not applied.
!*
!*  (CHaracteristics check , ice-306550)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  IMPLICIT INTEGER(P)

  TYPE :: DT
    INTEGER :: I=1
    PROCEDURE(), POINTER, NOPASS :: ProcPtr => NULL()
  END TYPE

  TYPE :: DT1
    SEQUENCE
    INTEGER :: I=1
    PROCEDURE(), POINTER, NOPASS :: ProcPtr => NULL()
  END TYPE

  CONTAINS

  FUNCTION ModFun(Arg)
  TYPE(DT) :: ModFun, Arg
    ModFun = Arg
  END FUNCTION

  FUNCTION ModFun1(Arg)
  INTEGER :: ModFun1, Arg
    ModFun1 = Arg
  END FUNCTION

  END MODULE

  MODULE M1
  USE M

  CONTAINS

  SUBROUTINE ModSub(Arg)
  TYPE(DT) :: arg
    Arg%ProcPtr=>ModFun
  END SUBROUTINE

  SUBROUTINE ModSub1(Arg)
  TYPE(DT1) :: arg
    Arg%ProcPtr=>ModFun
  END SUBROUTINE

  END MODULE

  PROGRAM Misc11
  USE M
  IMPLICIT INTEGER(P)

  PROCEDURE(), POINTER :: ProcPtr

  TYPE(DT)  :: V
  TYPE(DT1) :: U

    V%PRocPtr => ModFun
    U%PRocPtr => ModFun
    U%PRocPtr => ModFun1
    IF (V%ProcPtr(-1) .NE. -1 ) ERROR STOP 21  ! causing ice

    ProcPtr => ModFun  ! this might be ok

  END


