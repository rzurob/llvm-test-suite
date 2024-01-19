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


  FUNCTION ExtFun1()
  INTEGER (1) :: ExtFun1
    ExtFun1 = 1
  END FUNCTION

  FUNCTION ExtFun2(Arg)
  INTEGER (1) :: ExtFun2
  INTEGER (1) :: Arg(:)
    ExtFun2 = 1
  END FUNCTION

  PROGRAM PtrAssignExpImp
  IMPLICIT NONE

  INTERFACE
    FUNCTION F1()
      INTEGER (1) :: F1
    END FUNCTION
    FUNCTION F2(Arg)
      INTEGER (1) :: F2
      INTEGER (1) :: Arg(:)
    END FUNCTION
  END INTERFACE

  PROCEDURE(F1), POINTER :: ProcPtr1
  PROCEDURE(INTEGER(1))  :: ExtFun1

  PROCEDURE(INTEGER(1)), POINTER :: ProcPtr2
  PROCEDURE(F2)                  :: ExtFun2


  ProcPtr1 => ExtFun1

  ProcPtr2 => ExtFun2

  ProcPtr2 => ExtFun1

  END

