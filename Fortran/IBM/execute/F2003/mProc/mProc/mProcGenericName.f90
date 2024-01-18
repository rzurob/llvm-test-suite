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
!*  A generic name specifies a single name to reference all of the procedure names in
!*  the interface block.  A generic name may be the same as any one of the procedure names
!*  in the interface block, or the same as any accessible generic name.
!*
!*  -- External
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT
    INTEGER :: ID
  END TYPE

  TYPE, EXTENDS(DT) :: DT1
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2
  END TYPE

  TYPE, EXTENDS(DT2) :: DT3
  END TYPE

  PROCEDURE(ModFun1), POINTER  :: ProcPtr

  INTERFACE  ExtFun
    FUNCTION ExtFun(Arg1, Arg2)
      IMPORT
      TYPE(DT3), INTENT(IN) :: Arg1
      TYPE(DT3), INTENT(IN) :: Arg2
      TYPE(DT3)             :: ExtFun
    END FUNCTION
    PROCEDURE ExtFun
    PROCEDURE ModFun
    PROCEDURE ProcPtr
  END INTERFACE

  !PROCEDURE(ModFun1), POINTER  :: ProcPtr

  CONTAINS

  FUNCTION ModFun(Arg1, Arg2)
  TYPE(DT), INTENT(IN) :: Arg1
  TYPE(DT), INTENT(IN) :: Arg2
  TYPE(DT)             :: ModFun
    ModFun%ID = Arg1%ID + Arg2%ID
  END FUNCTION

  FUNCTION ModFun1(Arg1, Arg2)
  TYPE(DT1), INTENT(IN) :: Arg1
  TYPE(DT1), INTENT(IN) :: Arg2
  TYPE(DT1)             :: ModFun1
    ModFun1%ID = Arg1%ID + Arg2%ID
  END FUNCTION

  END MODULE

  FUNCTION ExtFun(Arg1, Arg2)
  USE M, ONLY: DT3
  TYPE(DT3), INTENT(IN) :: Arg1
  TYPE(DT3), INTENT(IN) :: Arg2
  TYPE(DT3)             :: ExtFun
    ExtFun%ID = Arg1%ID + Arg2%ID
  END FUNCTION


  PROGRAM mProcGenericName
  USE M

  TYPE(DT)  :: T=DT(-1)
  TYPE(DT1) :: T1=DT1(1)
  TYPE(DT2) :: T2=DT2(2)
  TYPE(DT3) :: T3=DT3(3)

  ProcPtr => ModFun1

  T   = ExtFun(DT(-1),  DT(-1))
  T1  = ExtFun(DT1(-2), DT1(-2))
  T3  = ExtFun(DT3(-3), DT3(-3))

  IF (T%ID   .NE. -2 ) ERROR STOP 11
  IF (T1%ID  .NE. -4 ) ERROR STOP 12
  IF (T3%ID  .NE. -6 ) ERROR STOP 14


  END

