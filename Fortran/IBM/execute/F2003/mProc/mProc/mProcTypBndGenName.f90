!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 06, 2006
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
!*  Interaction with type bound generics
!*
!*  -- Generic name
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT
    CHARACTER(10) :: ID
    CONTAINS
    GENERIC    :: GName => ModFun
    PROCEDURE  :: ModFun
  END TYPE

  INTERFACE  GName
    FUNCTION ExtFun(Arg1, Arg2, Arg3)
      IMPORT DT
      TYPE(DT), INTENT(IN) :: Arg1
      TYPE(DT), INTENT(IN) :: Arg2
      TYPE(DT), INTENT(IN) :: Arg3
      TYPE(DT)             :: ExtFun
    END FUNCTION
    PROCEDURE ExtFun
  END INTERFACE

  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(DT), INTENT(IN) :: Arg
  TYPE(DT)              :: ModFun
    ModFun%ID = "ModFun"
  END FUNCTION

  FUNCTION ModFun1(Arg1, Arg2)
  TYPE(DT), INTENT(IN) :: Arg1
  TYPE(DT), INTENT(IN) :: Arg2
  TYPE(DT)             :: ModFun1
    ModFun1%ID = "ModFun1"
  END FUNCTION

  END MODULE

  FUNCTION ExtFun(Arg1, Arg2, Arg3)
  USE M, ONLY: DT
  TYPE(DT), INTENT(IN) :: Arg1
  TYPE(DT), INTENT(IN) :: Arg2
  TYPE(DT), INTENT(IN) :: Arg3
  TYPE(DT)             :: ExtFun
    ExtFun%ID = "ExtFun"
  END FUNCTION


  PROGRAM mProcTypBndGenName
  USE M

  PROCEDURE(ModFun1), POINTER  :: ProcPtr

  INTERFACE  GName
    PROCEDURE ProcPtr
  END INTERFACE

  TYPE(DT) :: T, T1, T2

  ProcPtr => ModFun1

  T   = T%GName()
  T1  = GName(DT(""), DT(""))
  T2  = GName(DT(""), DT(""), DT(""))

  IF (TRIM(T%ID)   .NE. "ModFun"  ) STOP 11
  IF (TRIM(T1%ID)  .NE. "ModFun1" ) STOP 12
  IF (TRIM(T2%ID)  .NE. "ExtFun" ) STOP 14


  END

