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
! %POSTCMD: tcomp PtrAssignCharacteristics7.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignCharacteristics7.f
!*
!*  DATE                       : Jun. 16, 2005
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
!*  Characteristics : Subroutine/Function/Bind
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  USE ISO_C_BINDING

  TYPE, BIND(C) :: DT
    INTEGER(C_INT) :: Int=1
  END TYPE

  CONTAINS

  SUBROUTINE ModSub1(Arg)
  TYPE(DT) :: Arg
  END SUBROUTINE

  FUNCTION ModFun1(Arg)
  TYPE(DT)          :: ModFun1
  TYPE(DT)         :: Arg
    ModFun1 = DT()
  END FUNCTION

  SUBROUTINE ModSub2(Arg) BIND(C)
  TYPE(DT) :: Arg
  END SUBROUTINE

  FUNCTION ModFun2(Arg)  BIND(C)
  TYPE(DT)          :: ModFun2
  TYPE(DT)         :: Arg
    ModFun2 = DT()
  END FUNCTION

  END MODULE

  PROGRAM PtrAssignCharacteristics2
  USE M
  IMPLICIT NONE

  INTERFACE
    SUBROUTINE IntF1(A)
      IMPORT
      TYPE(DT) :: A
    END SUBROUTINE
  END INTERFACE

  PROCEDURE(IntF1), POINTER :: ProcPtr1

  INTERFACE
    FUNCTION IntF2(Arg)
      IMPORT
      TYPE(DT)         :: IntF2
      TYPE(DT)         :: Arg
    END FUNCTION
  END INTERFACE

  PROCEDURE(IntF2), POINTER :: ProcPtr2

  INTERFACE
    SUBROUTINE IntF3(A)
      IMPORT
      TYPE(DT) :: A
    END SUBROUTINE
  END INTERFACE

  PROCEDURE(IntF3), POINTER :: ProcPtr3

  INTERFACE
    FUNCTION IntF4(Arg)
      IMPORT
      TYPE(DT)         :: IntF4
      TYPE(DT)         :: Arg
    END FUNCTION
  END INTERFACE

  PROCEDURE(IntF4), POINTER :: ProcPtr4


  ProcPtr1 => ModFun1

  ProcPtr2 => ModSub2

  ProcPtr3 => ModSub2

  ProcPtr4 => ModFun2

  END

