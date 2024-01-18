!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 02, 2006
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
!*  If a generic procedure is accessed from a module, the rules apply to all the
!*  specific versions even the rules apply to all the specific versions even
!*  if some of them are inaccessible by their specific names.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT
    CHARACTER(9)  :: ID
  END TYPE

  INTERFACE Fun
    PROCEDURE ModFun
  END INTERFACE

  CONTAINS

  FUNCTION ModFun(arg)
  TYPE(DT) :: Arg
  TYPE(DT)  :: ModFun
    ModFun%ID = "M-" // Arg%ID
  END FUNCTION

  END MODULE


  MODULE M1
  USE M, ONLY : DT

  TYPE, EXTENDS(DT) :: DT1
  END TYPE

  PROCEDURE(ModFun), POINTER :: ProcPtr

  INTERFACE Fun
    PROCEDURE ProcPtr
  END INTERFACE

  CONTAINS

  FUNCTION ModFun(arg)
  TYPE(DT1) :: Arg
  TYPE(DT1)  :: ModFun
    ModFun%ID = "M1-" // Arg%ID
  END FUNCTION

  END MODULE

  MODULE M2
  USE M1

  CONTAINS

  SUBROUTINE ModSub()
    ProcPtr => ModFun
  END SUBROUTINE

  END MODULE

  PROGRAM  mProcDecRestrict1
  USE M, ONLY: Fun, DT
  USE M1, ONLY: Fun, DT1
  USE M2

  TYPE(DT)  :: T
  TYPE(DT1) :: T1

  CALL ModSub

  T  = Fun(DT ("0"))
  T1 = Fun(DT1("1"))

  IF (TRIM(T%ID)   .NE. "M-0" )  STOP 11
  IF (TRIM(T1%ID)  .NE. "M1-1" ) STOP 12

  END

