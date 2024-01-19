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

  CONTAINS

  SUBROUTINE ModSub(ArgPtr)
  PROCEDURE(IntFun), POINTER  :: Ptr

    Ptr => IntFun

  CONTAINS
    FUNCTION IntFun()
    INTEGER :: IntFun
      IntFun = 1
    END FUNCTION

  END SUBROUTINE

  END MODULE


  PROGRAM PtrAssignProcNameProcPtr
  IMPLICIT NONE
  PROCEDURE(IntFun), POINTER  :: Ptr

    Ptr => IntFun

  CONTAINS

  FUNCTION IntFun()
  INTEGER :: IntFun

  Ptr => IntFun

  IntFun = 1

  END FUNCTION


  FUNCTION ExtFun()
  INTEGER :: ExtFun
  PROCEDURE(IntFun), POINTER  :: Ptr

  Ptr => IntFun

  ExtFun = 1

  END FUNCTION

  END

