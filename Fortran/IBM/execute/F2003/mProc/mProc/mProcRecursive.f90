!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 03, 2006
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
!*  Recursive
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT
    CHARACTER(20) :: ID
  END TYPE

  PROCEDURE(ModFun1), POINTER  :: ProcPtr

  INTERFACE Fun
    RECURSIVE FUNCTION ExtFun(Arg, Arg1, Arg2)
      IMPORT  DT
      CLASS(DT), INTENT(IN) :: Arg, Arg1, Arg2
      TYPE(DT)  :: ExtFun
    END FUNCTION
  END INTERFACE

  CONTAINS

  SUBROUTINE ModInit()
    ProcPtr => ModFun1
  END SUBROUTINE

  RECURSIVE FUNCTION ModFun(Arg)
  CLASS(DT), INTENT(IN)   :: Arg
  TYPE(DT)  :: ModFun
  INTEGER, SAVE :: Count = 0
    IF (Count .NE. 4 ) THEN
    ModFun%ID = "ModFun-" // Arg%ID
    ELSE
      Count=Cout + 1
      ModFun = ModFun(Arg)
    END IF
  END FUNCTION

  RECURSIVE FUNCTION ModFun1(Arg, Arg1)
  CLASS(DT), INTENT(IN) :: Arg, Arg1
  TYPE(DT)  :: ModFun1
  INTEGER, SAVE :: Count = 0
    IF (Count .NE. 4 ) THEN
      ModFun1%ID = "ModFun1-" // Arg%ID
    ELSE
      Count=Cout + 1
      ModFun1 = ModFun1(Arg, Arg1)
    END IF
  END FUNCTION

  END MODULE

  RECURSIVE FUNCTION ExtFun(Arg, Arg1, Arg2)
  USE M, ONLY : DT
  CLASS(DT), INTENT(IN) :: Arg, Arg1, Arg2
  TYPE(DT)  :: ExtFun
  INTEGER, SAVE :: Count = 0
    IF (Count .NE. 4 ) THEN
      ExtFun%ID = "ExtFun-" // Arg%ID
    ELSE
      Count=Cout + 1
      ExtFun = ExtFun(Arg, Arg1, Arg2)
    END IF
  END FUNCTION


  PROGRAM mProcRecursive
  USE M

  INTERFACE Fun
    PROCEDURE ModFun
    PROCEDURE ProcPtr
  END INTERFACE

  TYPE(DT) :: T, T1, T2

  CALL ModInit()

  T  = Fun(DT("0"))
  T1 = Fun(DT("00"), DT("1"))
  T2 = Fun(DT("000"), DT("1"), DT("2"))

  IF (T%ID     .NE. "ModFun-0" )   ERROR STOP 11
  IF (T1%ID    .NE. "ModFun1-00" ) ERROR STOP 12
  IF (T2%ID    .NE. "ExtFun-000" ) ERROR STOP 13

  END

