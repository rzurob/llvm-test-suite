! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_procptr/CrossFeatures2/PtrAssignCharacteristics2.f
! opt variations: -qnok -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 18, 2005
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
!*  Characteristics are diff
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT0(K1)    ! (4)
      INTEGER, KIND :: K1
  END TYPE

  TYPE, EXTENDS(DT0) :: DT    ! (4)
  END TYPE

  CONTAINS

  SUBROUTINE ModSub1(Arg)
  TYPE(DT0(4)) :: Arg
  END SUBROUTINE

  SUBROUTINE ModSub2(Arg)
  CLASS(DT(4)) :: Arg
  END SUBROUTINE

  SUBROUTINE ModSub3(Arg)
  CLASS(DT(4)) :: Arg
  END SUBROUTINE

  FUNCTION ModFun1(Arg)
  TYPE(DT(4))          :: ModFun1
  TYPE(DT0(4))         :: Arg
    ModFun1 = DT(4)()
  END FUNCTION

  FUNCTION ModFun2(Arg)
  TYPE(DT(4))         :: ModFun2
  TYPE(DT(4))         :: Arg
    ModFun2 = Arg
  END FUNCTION

  END MODULE

  PROGRAM PtrAssignCharacteristics2
  USE M
  IMPLICIT NONE

  INTERFACE
    SUBROUTINE IntF1(A)
      IMPORT
      TYPE(DT(4)) :: A
    END SUBROUTINE
  END INTERFACE

  PROCEDURE(IntF1), POINTER :: ProcPtr1

  INTERFACE
    SUBROUTINE IntF2(A)
      IMPORT
      TYPE(DT(4)) :: A
    END SUBROUTINE
  END INTERFACE

  PROCEDURE(IntF2), POINTER :: ProcPtr2

  INTERFACE
    SUBROUTINE IntF3(A)
      IMPORT
      CLASS(DT0(4)) :: A
    END SUBROUTINE
  END INTERFACE

  PROCEDURE(IntF3), POINTER :: ProcPtr3

  INTERFACE
    FUNCTION IntF4(Arg)
      IMPORT
      TYPE(DT0(4))         :: IntF4
      TYPE(DT0(4))         :: Arg
    END FUNCTION
  END INTERFACE

  PROCEDURE(IntF4), POINTER :: ProcPtr4

  INTERFACE
    FUNCTION IntF5(Arg)
      IMPORT
      CLASS(DT(4)), POINTER :: IntF5
      TYPE(DT(4))           :: Arg
    END FUNCTION
  END INTERFACE

  PROCEDURE(IntF5), POINTER :: ProcPtr5

  ProcPtr1 => ModSub1

  ProcPtr2 => ModSub2

  ProcPtr3 => ModSub3

  ProcPtr4 => ModFun1

  ProcPtr5 => ModFun2


  END
