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
!*  A generic interface block specifies a generic interface for each of the
!*  procedures in the interface block. The PROCEDURE statement lists procedure
!*  pointers, external procedures, du mmy procedures, or module procedures
!*  that have this generic interface. A generic interface is always explicit.
!*  -- Defined Operator
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

  END MODULE

  MODULE M1
  USE M

  INTERFACE OPERATOR( + )
    PROCEDURE ModFun
  END INTERFACE

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

  FUNCTION ModFun2(Arg1, Arg2)
  TYPE(DT2), INTENT(IN) :: Arg1
  TYPE(DT2), INTENT(IN) :: Arg2
  TYPE(DT2)             :: ModFun2
    ModFun2%ID = Arg1%ID + Arg2%ID
  END FUNCTION

  END MODULE

  FUNCTION ExtFun(Arg1, Arg2)
  USE M
  TYPE(DT3), INTENT(IN) :: Arg1
  TYPE(DT3), INTENT(IN) :: Arg2
  TYPE(DT3)             :: ExtFun
    ExtFun%ID = Arg1%ID + Arg2%ID
  END FUNCTION


  PROGRAM mProcDefOp
  USE M
  USE M1


  INTERFACE  OPERATOR( + )
    FUNCTION ExtFun(Arg1, Arg2)
      IMPORT
      TYPE(DT3), INTENT(IN) :: Arg1
      TYPE(DT3), INTENT(IN) :: Arg2
      TYPE(DT3)             :: ExtFun
    END FUNCTION
  END INTERFACE

  INTERFACE  OPERATOR( + )
    PROCEDURE ExtFun
  END INTERFACE

  CALL IntSub(ModFun1)

  CONTAINS

  SUBROUTINE IntSub(Proc)
  PROCEDURE(ModFun1)           :: Proc
  PROCEDURE(ModFun2), POINTER  :: ProcPtr

  INTERFACE  OPERATOR( + )
    PROCEDURE Proc
  END INTERFACE

  INTERFACE  OPERATOR( + )
    PROCEDURE ProcPtr
  END INTERFACE

  TYPE(DT) :: T=DT(-1)
  TYPE(DT1) :: T1=DT1(1)
  TYPE(DT2) :: T2=DT2(2)
  TYPE(DT3) :: T3=DT3(3)

  ProcPtr => ModFun2


  T  = DT(-1) + DT(-1)
  T1 = DT1(1) + DT1(1)
  T2 = DT2(2) + DT2(2)
  T3 = DT3(3) + DT3(3)

  IF (T%ID  .NE. -2 ) ERROR STOP 11
  IF (T1%ID  .NE. 2 ) ERROR STOP 12
  IF (T2%ID  .NE. 4 ) ERROR STOP 13
  IF (T3%ID  .NE. 6 ) ERROR STOP 14

  END  SUBROUTINE

  END
