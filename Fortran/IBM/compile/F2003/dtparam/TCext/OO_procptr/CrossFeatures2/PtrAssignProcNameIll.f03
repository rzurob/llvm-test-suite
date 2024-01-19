! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_procptr/CrossFeatures2/PtrAssignProcNameIll.f
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
!*  C727 (R742) A procedure-name shall be the name of an external, module,
!*  or dummy procedure, a specific intrinsic function listed in 13.6
!*  and not marked with a bullet (.), or a procedure pointer.
!*
!*  The target is an internal procedure
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K1)    ! (4)
      INTEGER, KIND :: K1
  END TYPE

  END MODULE

  MODULE M1
  USE M

  CONTAINS

  SUBROUTINE ModSub(ArgPtr)
  PROCEDURE(IntFun), POINTER  :: Ptr

    Ptr => IntFun

  CONTAINS

    FUNCTION IntFun()
    CLASS(DT(4)), ALLOCATABLE :: IntFun
      ALLOCATE(IntFun, SOURCE=DT(4)())
    END FUNCTION

  END SUBROUTINE

  END MODULE


  PROGRAM PtrAssignProcNameProcPtr
  USE M
  IMPLICIT NONE
  PROCEDURE(IntFun), POINTER  :: Ptr

    Ptr => IntFun

  CONTAINS

  FUNCTION IntFun()
  CLASS(DT(4)), POINTER :: IntFun
    ALLOCATE(IntFun, SOURCE=DT(4)())
  END FUNCTION

  FUNCTION IntFun1()
  INTEGER :: IntFun1
  PROCEDURE(IntFun), POINTER  :: Ptr

    Ptr => IntFun
    IntFun1 = 1

  END FUNCTION

  END

