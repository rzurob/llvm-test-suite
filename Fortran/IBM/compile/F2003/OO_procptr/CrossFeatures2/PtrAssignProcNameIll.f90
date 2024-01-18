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
! %POSTCMD: tcomp PtrAssignProcNameIll.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignProcNameProcPtr.f
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

  TYPE :: DT
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
    CLASS(DT), ALLOCATABLE :: IntFun
      ALLOCATE(IntFun, SOURCE=DT())
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
  CLASS(DT), POINTER :: IntFun
    ALLOCATE(IntFun, SOURCE=DT())
  END FUNCTION

  FUNCTION IntFun1()
  INTEGER :: IntFun1
  PROCEDURE(IntFun), POINTER  :: Ptr

    Ptr => IntFun
    IntFun1 = 1

  END FUNCTION

  END

