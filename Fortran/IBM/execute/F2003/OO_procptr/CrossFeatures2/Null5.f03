! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 11, 2005
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
!*   null()
!*   the defered parameter of the contextual entity
!*  (315124)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Null5
  IMPLICIT NONE

  INTERFACE
    FUNCTION IFun()
    PROCEDURE(), POINTER :: IFun
    END FUNCTION

    FUNCTION IFun1(Arg)
    PROCEDURE(), POINTER :: Arg
    CHARACTER(:), POINTER :: IFun1
    END FUNCTION

    FUNCTION IFun2()
    CHARACTER, ALLOCATABLE :: IFun2(:)
    END FUNCTION
  END INTERFACE

  PROCEDURE(IFun), POINTER :: ProcPtr
  PROCEDURE(IFun1), POINTER :: ProcPtr1
  PROCEDURE(IFun2), POINTER :: ProcPtr2

  DATA  ProcPtr /NULL()/
  DATA  ProcPtr1 /NULL()/
  DATA  ProcPtr2 /NULL()/

  IF (ASSOCIATED(ProcPtr))  ERROR STOP 11
  IF (ASSOCIATED(ProcPtr1))  ERROR STOP 12
  IF (ASSOCIATED(ProcPtr2))  ERROR STOP 13

  END

