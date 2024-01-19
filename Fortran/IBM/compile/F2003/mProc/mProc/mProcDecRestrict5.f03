!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 10, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Generaliztion of PROCEDURE statement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 296676
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Within a scoping unit, two procedures that have the same generic name shall
!*  both be subroutines or both be functions
!*
!*  (317262)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M


  TYPE :: DT
    CHARACTER  :: ID
  CONTAINS
    GENERIC    :: ASSIGNMENT(=) => ModSub
    GENERIC    :: OPERATOR(.OP.) => ModFun
    PROCEDURE, PASS(Arg2)  :: ModSub
    PROCEDURE  :: ModFun
  END TYPE

  TYPE, EXTENDS(DT) :: DT1
  END TYPE

  CONTAINS

  SUBROUTINE ModSub(Arg1, Arg2)
  TYPE(DT),  INTENT(OUT) :: Arg1
  CLASS(DT), INTENT(IN)  :: Arg2
  END SUBROUTINE

  FUNCTION ModFun(Arg1, Arg2)
  TYPE(DT) :: ModFun
  CLASS(DT), INTENT(IN) :: Arg1, Arg2
    ModFun%ID = Arg1%ID // Arg2%ID
  END FUNCTION

  END MODULE

  PROGRAM mProcDecRestrict5
  USE M
  PROCEDURE(ModFun), POINTER :: ProcPtr

  CONTAINS

  SUBROUTINE IntSub(Proc)
  PROCEDURE(ModSub) :: Proc

  INTERFACE ASSIGNMENT(=)
    PROCEDURE ProcPtr
  END INTERFACE

  INTERFACE OPERATOR( .OP. )
    PROCEDURE Proc
  END INTERFACE

  END SUBROUTINE

  END


