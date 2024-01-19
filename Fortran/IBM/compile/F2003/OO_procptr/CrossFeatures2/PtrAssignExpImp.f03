! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 25, 2005
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
!*  If the characteristics of proc-pointer-object or proc-target are such that
!*  an explicit interface is required, both proc-pointer-object and proc-target
!*  shall have an explicit interface.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  CONTAINS

  FUNCTION ModFun1()
  CLASS(*), POINTER :: ModFun1
    ALLOCATE(ModFun1, SOURCE=-1)
  END FUNCTION

  END MODULE

  FUNCTION ExtFun1()
  INTEGER (1) :: ExtFun1
    ExtFun1 = 1
  END FUNCTION

  PROGRAM PtrAssignExpImp
  USE M
  IMPLICIT NONE

  PROCEDURE(ModFun1), POINTER :: ProcPtr1
  PROCEDURE(INTEGER(1))  :: ExtFun1

  PROCEDURE(INTEGER(1)), POINTER :: ProcPtr2


  ProcPtr1 => ExtFun1

  ProcPtr2 => ModFun1


  END

