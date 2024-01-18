!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcAssign.f
!*
!*  DATE                       : Mar 01, 2006
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
!*  -- Defined assignment
!*  (316834)
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

  INTERFACE ASSIGNMENT(=)
    PROCEDURE ModSub
  END INTERFACE

  CONTAINS

  SUBROUTINE ModSub(Arg1, Arg2)
  TYPE(DT), INTENT(INOUT) :: Arg1
  TYPE(DT), INTENT(IN)    :: Arg2
    Arg1%ID = Arg2%ID
  END SUBROUTINE

  SUBROUTINE ModSub1(Arg1, Arg2)
  TYPE(DT1), INTENT(INOUT) :: Arg1
  TYPE(DT1), INTENT(IN)    :: Arg2
    Arg1 = Arg2
  END SUBROUTINE

  SUBROUTINE ModSub2(Arg1, Arg2)
  TYPE(DT2), INTENT(INOUT) :: Arg1
  TYPE(DT2), INTENT(IN)    :: Arg2
    Arg1 = Arg2
  END SUBROUTINE

  END MODULE

  SUBROUTINE ExtSub(Arg1, Arg2)
  USE M
  TYPE(DT3), INTENT(INOUT) :: Arg1
  TYPE(DT3), INTENT(IN)    :: Arg2
    Arg1 = Arg2
  END SUBROUTINE


  PROGRAM mProcAssign
  USE M
  USE M1

  INTERFACE ASSIGNMENT(=)
    SUBROUTINE ExtSub(Arg1, Arg2)
      IMPORT
      TYPE(DT3), INTENT(INOUT) :: Arg1
      TYPE(DT3), INTENT(IN)    :: Arg2
    END SUBROUTINE
  END INTERFACE

  INTERFACE ASSIGNMENT(=)
    PROCEDURE ExtSub
  END INTERFACE

  CALL IntSub(ModSub1)

  CONTAINS

  SUBROUTINE IntSub(Proc)
  PROCEDURE(ModSub1)           :: Proc
  PROCEDURE(ModSub2), POINTER  :: ProcPtr

  INTERFACE ASSIGNMENT(=)
    PROCEDURE Proc
  END INTERFACE

  INTERFACE ASSIGNMENT(=)
    PROCEDURE ProcPtr
  END INTERFACE

  TYPE(DT) :: T=DT(-1)
  TYPE(DT1) :: T1=DT1(1)
  TYPE(DT2) :: T2=DT2(2)
  TYPE(DT3) :: T3=DT3(3)

  ProcPtr => ModSub2


  T  = DT(-1)
  T1 = DT1(1)
  T2 = DT2(2)
  T3 = DT3(3)

  IF (T%ID  .NE. -1 ) STOP 11
  IF (T1%ID  .NE. 1 ) STOP 12
  IF (T2%ID  .NE. 2 ) STOP 13
  IF (T3%ID  .NE. 3 ) STOP 14

  END  SUBROUTINE

  END

